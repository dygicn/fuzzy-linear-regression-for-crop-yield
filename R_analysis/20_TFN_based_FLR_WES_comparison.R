rm(list = ls())

library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")

tfn_dir <- file.path(output_dir, "TFN_based_FLR_WES_comparison")
if (!dir.exists(tfn_dir)) dir.create(tfn_dir, recursive = TRUE)

eps <- 1e-12

fuzzyreg_pred_path <- file.path(
  output_dir,
  "explained_fuzzyreg_expanding_window",
  "explained_fuzzyreg_expanding_window_predictions.csv"
)
fuzzyreg_scaling_path <- file.path(
  output_dir,
  "explained_fuzzyreg_expanding_window",
  "explained_fuzzyreg_expanding_window_scaling_log.csv"
)
tfn_mc_pred_path <- file.path(
  output_dir,
  "TFN_MC_linear",
  "TFN_MC_linear_predictions.csv"
)
tfn_mc_scaling_path <- file.path(
  output_dir,
  "TFN_MC_linear",
  "TFN_MC_linear_scaling_log.csv"
)

required_files <- c(
  fuzzyreg_pred_path,
  fuzzyreg_scaling_path,
  tfn_mc_pred_path,
  tfn_mc_scaling_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

unique_scaling <- function(path, year_col) {
  sc <- read.csv(path, stringsAsFactors = FALSE)
  names(sc)[names(sc) == year_col] <- "test_year"
  unique(sc[, c(
    "fuzzification_rate",
    "fuzzification_percent",
    "crop",
    "model",
    "test_year",
    "y_center",
    "y_scale"
  )])
}

standardize_predictions <- function(pred_path, scaling_path, source_label, year_col) {
  pred <- read.csv(pred_path, stringsAsFactors = FALSE)
  names(pred)[names(pred) == year_col] <- "test_year"
  pred$model_family <- source_label

  sc <- unique_scaling(scaling_path, year_col)
  pred <- merge(
    pred,
    sc,
    by = c(
      "fuzzification_rate",
      "fuzzification_percent",
      "crop",
      "model",
      "test_year"
    ),
    all.x = TRUE
  )

  if (anyNA(pred$y_center) || anyNA(pred$y_scale)) {
    stop("Scaling information is missing for some prediction rows in ", source_label)
  }

  y_std <- (pred$observed - pred$y_center) / pred$y_scale
  spread_std <- abs(y_std) * pred$fuzzification_rate

  pred$obs_lower <- (y_std - spread_std) * pred$y_scale + pred$y_center
  pred$obs_center <- pred$observed
  pred$obs_upper <- (y_std + spread_std) * pred$y_scale + pred$y_center

  pred
}

area_tfn <- function(lower, center, upper) {
  pmax((upper - lower) / 2, eps)
}

membership_tfn <- function(x, lower, center, upper) {
  ifelse(
    x < lower | x > upper,
    0,
    ifelse(
      x <= center,
      ifelse(abs(center - lower) < eps, 1, (x - lower) / (center - lower)),
      ifelse(abs(upper - center) < eps, 1, (upper - x) / (upper - center))
    )
  )
}

single_tef <- function(obs_l, obs_c, obs_u, pred_l, pred_c, pred_u) {
  lower_bound <- min(obs_l, pred_l)
  upper_bound <- max(obs_u, pred_u)

  if (!is.finite(lower_bound) || !is.finite(upper_bound) ||
      abs(upper_bound - lower_bound) < eps) {
    return(NA_real_)
  }

  x_grid <- seq(lower_bound, upper_bound, length.out = 1001)
  obs_mu <- membership_tfn(x_grid, obs_l, obs_c, obs_u)
  pred_mu <- membership_tfn(x_grid, pred_l, pred_c, pred_u)

  numerator <- sum(diff(x_grid) * (head(abs(obs_mu - pred_mu), -1) + tail(abs(obs_mu - pred_mu), -1)) / 2)
  denominator <- area_tfn(obs_l, obs_c, obs_u)

  numerator / denominator
}

calculate_tfn_metrics <- function(df) {
  df <- df[df$fit_status == "ok", ]

  split_key <- paste(
    df$model_family,
    df$fuzzification_rate,
    df$fuzzification_percent,
    df$crop,
    df$model,
    sep = "||"
  )

  metric_list <- lapply(split(df, split_key), function(x) {
    gof <- mean(
      (x$obs_lower - x$pred_lower)^2 +
        (x$obs_center - x$pred_center)^2 +
        (x$obs_upper - x$pred_upper)^2,
      na.rm = TRUE
    )

    tef_values <- mapply(
      single_tef,
      x$obs_lower,
      x$obs_center,
      x$obs_upper,
      x$pred_lower,
      x$pred_center,
      x$pred_upper
    )

    mae_tfn <- mean(
      abs(x$obs_lower - x$pred_lower) +
        abs(x$obs_center - x$pred_center) +
        abs(x$obs_upper - x$pred_upper),
      na.rm = TRUE
    )

    rmse_tfn <- sqrt(mean(
      (
        (x$obs_lower - x$pred_lower)^2 +
          (x$obs_center - x$pred_center)^2 +
          (x$obs_upper - x$pred_upper)^2
      ) / 3,
      na.rm = TRUE
    ))

    data.frame(
      model_family = x$model_family[1],
      fuzzification_rate = x$fuzzification_rate[1],
      fuzzification_percent = x$fuzzification_percent[1],
      crop = x$crop[1],
      model = x$model[1],
      n_test = nrow(x),
      test_year_start = min(x$test_year),
      test_year_end = max(x$test_year),
      GOF = gof,
      TEF = sum(tef_values, na.rm = TRUE),
      MAE_TFN = mae_tfn,
      RMSE_TFN = rmse_tfn,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, metric_list)
}

add_wes <- function(metrics) {
  metrics$WES <- NA_real_

  for (crop_name in unique(metrics$crop)) {
    idx <- which(metrics$crop == crop_name)
    metric_names <- c("GOF", "TEF", "MAE_TFN", "RMSE_TFN")
    norm_mat <- matrix(NA_real_, nrow = length(idx), ncol = length(metric_names))
    colnames(norm_mat) <- metric_names

    for (metric_name in metric_names) {
      values <- metrics[idx, metric_name]
      range_value <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
      if (!is.finite(range_value) || range_value < eps) {
        norm_mat[, metric_name] <- 0
      } else {
        norm_mat[, metric_name] <- (values - min(values, na.rm = TRUE)) / range_value
      }
    }

    variances <- apply(norm_mat, 2, var, na.rm = TRUE)
    if (!is.finite(sum(variances)) || sum(variances) < eps) {
      weights <- rep(1 / length(metric_names), length(metric_names))
    } else {
      weights <- variances / sum(variances)
    }

    metrics[idx, paste0("weight_", metric_names)] <- matrix(
      rep(weights, each = length(idx)),
      nrow = length(idx)
    )

    metrics[idx, "WES"] <- as.numeric(norm_mat %*% weights)
  }

  metrics
}

fuzzyreg_predictions <- standardize_predictions(
  fuzzyreg_pred_path,
  fuzzyreg_scaling_path,
  "TFN fuzzyreg package",
  "test_year"
)

tfn_mc_predictions <- standardize_predictions(
  tfn_mc_pred_path,
  tfn_mc_scaling_path,
  "TFN Monte Carlo linear",
  "year"
)

tfn_predictions <- rbind(
  fuzzyreg_predictions[, intersect(names(fuzzyreg_predictions), names(tfn_mc_predictions))],
  tfn_mc_predictions[, intersect(names(fuzzyreg_predictions), names(tfn_mc_predictions))]
)

tfn_metrics <- calculate_tfn_metrics(tfn_predictions)
tfn_metrics <- add_wes(tfn_metrics)
tfn_metrics <- tfn_metrics[order(tfn_metrics$crop, tfn_metrics$WES), ]

best_tfn_by_crop_wes <- tfn_metrics[!duplicated(tfn_metrics$crop), ]

winner_counts <- as.data.frame(table(
  best_tfn_by_crop_wes$model_family,
  best_tfn_by_crop_wes$model,
  best_tfn_by_crop_wes$fuzzification_percent
))
names(winner_counts) <- c("model_family", "model", "fuzzification_percent", "n_crop_wins")
winner_counts <- winner_counts[winner_counts$n_crop_wins > 0, ]
winner_counts <- winner_counts[order(-winner_counts$n_crop_wins), ]

write.csv(
  tfn_metrics,
  file.path(tfn_dir, "TFN_based_FLR_all_model_WES_metrics.csv"),
  row.names = FALSE
)

write.csv(
  best_tfn_by_crop_wes,
  file.path(tfn_dir, "TFN_based_FLR_best_by_crop_WES.csv"),
  row.names = FALSE
)

write.csv(
  winner_counts,
  file.path(tfn_dir, "TFN_based_FLR_WES_winner_counts.csv"),
  row.names = FALSE
)

excel_path <- file.path(tfn_dir, "TFN_based_FLR_WES_comparison.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  data.frame(
    Item = c(
      "Purpose",
      "Included models",
      "Selection criterion",
      "Validation",
      "Notes"
    ),
    Description = c(
      "Internal comparison of TFN-based fuzzy linear regression models.",
      "FLAR, PLR, PLRLS, and TFN-MC-FLR.",
      "Minimum Weighted Error Score (WES) for each crop.",
      "Expanding-window test years 2015-2024.",
      "Observed TFNs are reconstructed from standardized yields using the corresponding training-window scaling parameters."
    ),
    stringsAsFactors = FALSE
  )
)

addWorksheet(wb, "Best_By_Crop_WES")
writeData(wb, "Best_By_Crop_WES", best_tfn_by_crop_wes)

addWorksheet(wb, "All_TFN_Model_Metrics")
writeData(wb, "All_TFN_Model_Metrics", tfn_metrics)

addWorksheet(wb, "Winner_Counts")
writeData(wb, "Winner_Counts", winner_counts)

header_style <- createStyle(
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

for (sheet_name in names(wb)) {
  freezePane(wb, sheet_name, firstRow = TRUE)
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:80, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("TFN-based FLR WES comparison completed.\n")
cat("Outputs written to:", tfn_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n\n")
cat("Best TFN-based FLR models by WES:\n")
print(best_tfn_by_crop_wes[, c(
  "crop",
  "model_family",
  "model",
  "fuzzification_percent",
  "GOF",
  "TEF",
  "MAE_TFN",
  "RMSE_TFN",
  "WES"
)])

cat("\nWinner counts:\n")
print(winner_counts)
