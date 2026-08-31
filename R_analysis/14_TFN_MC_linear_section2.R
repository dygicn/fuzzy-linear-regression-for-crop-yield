# Section 2: TFN-based fuzzy linear regression with Monte Carlo method
#
# This script implements the Monte Carlo fuzzy linear regression approach for
# Case-II data: crisp inputs and fuzzy output.
#
# Methodological logic:
#   1. Standardize predictors and yield using training-window statistics.
#   2. Convert standardized observed crop yield into a triangular fuzzy number (TFN).
#   2. Generate random TFN coefficient vectors.
#   3. For each candidate vector, compute fuzzy predictions on the training set.
#   4. Select the candidate vector that minimizes the fuzzy MSE error (MSEe).
#   5. Predict the next held-out year using expanding-window validation.
#   6. Defuzzify the TFN prediction using center of gravity (COG).
#   7. Compute crop-wise MAE, RMSE, MAPE, and out-of-sample R2.
#
# Outputs are saved under:
#   output/TFN_MC_linear

rm(list = ls())

library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found. Run 00_build_panel_dataset.R first.")
}

mc_dir <- file.path(output_dir, "TFN_MC_linear")
if (!dir.exists(mc_dir)) dir.create(mc_dir, recursive = TRUE)

panel <- read.csv(panel_path, stringsAsFactors = FALSE, check.names = FALSE)

predictors <- c(
  "Mean_NDVI",
  "Mean_EVI",
  "Mean_LST_Day_1km",
  "Precipitation_mm",
  "ET_mm",
  "SPEI",
  "GDD",
  "HeatStress",
  "ColdStress"
)

response <- "yield_kg_ha"
fuzzification_rates <- c(0.05, 0.10, 0.15)
min_train_years <- 15

# Icen and Demirhan (2016) report MC settings such as 10^4 or 10^5.
# We use 10^4 for the main crop-wise expanding-window analysis.
N_candidates <- 10000
chunk_size <- 1000
interval_multiplier <- 3

model_name <- "TFN_MC_linear"

mae <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mape <- function(obs, pred) mean(abs((obs - pred) / obs), na.rm = TRUE) * 100
r2_score <- function(obs, pred) {
  1 - sum((obs - pred)^2, na.rm = TRUE) /
    sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

make_tfn_response <- function(y, delta) {
  spread <- delta * abs(y)
  cbind(
    lower = y - spread,
    center = y,
    upper = y + spread
  )
}

scale_train_test_y <- function(train_x, test_x, train_y) {
  center <- apply(train_x, 2, mean)
  scalev <- apply(train_x, 2, sd)
  scalev[scalev == 0 | is.na(scalev)] <- 1
  y_center <- mean(train_y, na.rm = TRUE)
  y_scale <- sd(train_y, na.rm = TRUE)
  if (is.na(y_scale) || y_scale == 0) y_scale <- 1

  train_scaled <- sweep(sweep(train_x, 2, center, "-"), 2, scalev, "/")
  test_scaled <- sweep(sweep(test_x, 2, center, "-"), 2, scalev, "/")
  train_y_scaled <- (train_y - y_center) / y_scale

  list(
    train = train_scaled,
    test = test_scaled,
    train_y = train_y_scaled,
    y_center = y_center,
    y_scale = y_scale,
    x_center = center,
    x_scale = scalev
  )
}

back_transform_tfn_matrix <- function(pred_tfn, y_center, y_scale) {
  out <- pred_tfn
  out[, "lower"] <- out[, "lower"] * y_scale + y_center
  out[, "center"] <- out[, "center"] * y_scale + y_center
  out[, "upper"] <- out[, "upper"] * y_scale + y_center
  out
}

ols_center_intervals <- function(x_design, y, interval_multiplier) {
  fit <- lm.fit(x = x_design, y = y)
  beta_hat <- as.numeric(fit$coefficients)
  beta_hat[!is.finite(beta_hat)] <- 0

  n <- nrow(x_design)
  p <- ncol(x_design)
  residual_df <- max(n - p, 1)
  rss <- sum(fit$residuals^2, na.rm = TRUE)
  sigma2 <- rss / residual_df

  xtx_inv <- try(solve(crossprod(x_design)), silent = TRUE)
  if (inherits(xtx_inv, "try-error")) {
    xtx_inv <- MASS::ginv(crossprod(x_design))
  }

  se <- sqrt(pmax(diag(xtx_inv) * sigma2, 0))
  fallback <- max(sd(y, na.rm = TRUE) * 0.01, 1e-6)
  half_width <- interval_multiplier * pmax(se, 0.05 * abs(beta_hat), fallback)

  data.frame(
    coefficient = colnames(x_design),
    beta_center = beta_hat,
    interval_lower = beta_hat - half_width,
    interval_upper = beta_hat + half_width,
    half_width = half_width,
    stringsAsFactors = FALSE
  )
}

generate_random_tfn_coefficients <- function(intervals, n_candidates) {
  q <- nrow(intervals)
  raw <- array(runif(n_candidates * q * 3), dim = c(n_candidates, q, 3))

  for (j in seq_len(q)) {
    low <- intervals$interval_lower[j]
    high <- intervals$interval_upper[j]
    raw[, j, ] <- low + (high - low) * raw[, j, ]
    raw[, j, ] <- t(apply(raw[, j, ], 1, sort))
  }

  raw
}

predict_tfn_chunk <- function(x_design, coeff_chunk) {
  coef_l <- coeff_chunk[, , 1, drop = FALSE][, , 1]
  coef_c <- coeff_chunk[, , 2, drop = FALSE][, , 1]
  coef_u <- coeff_chunk[, , 3, drop = FALSE][, , 1]

  x_pos <- pmax(x_design, 0)
  x_neg <- pmin(x_design, 0)

  pred_lower <- coef_l %*% t(x_pos) + coef_u %*% t(x_neg)
  pred_center <- coef_c %*% t(x_design)
  pred_upper <- coef_u %*% t(x_pos) + coef_l %*% t(x_neg)

  list(lower = pred_lower, center = pred_center, upper = pred_upper)
}

msee_for_chunk <- function(y_tfn, pred) {
  err <- sweep(pred$lower, 2, y_tfn[, "lower"], "-")^2 +
    sweep(pred$center, 2, y_tfn[, "center"], "-")^2 +
    sweep(pred$upper, 2, y_tfn[, "upper"], "-")^2
  rowMeans(err, na.rm = TRUE)
}

e2_for_one <- function(y_tfn, pred_tfn) {
  sum(abs(y_tfn[, "lower"] - pred_tfn[, "lower"]) +
        abs(y_tfn[, "center"] - pred_tfn[, "center"]) +
        abs(y_tfn[, "upper"] - pred_tfn[, "upper"]))
}

mapee_for_one <- function(y_tfn, pred_tfn) {
  mean(
    abs((pred_tfn[, "lower"] - y_tfn[, "lower"]) / pmax(abs(y_tfn[, "lower"]), 1e-8)) +
      abs((pred_tfn[, "center"] - y_tfn[, "center"]) / pmax(abs(y_tfn[, "center"]), 1e-8)) +
      abs((pred_tfn[, "upper"] - y_tfn[, "upper"]) / pmax(abs(y_tfn[, "upper"]), 1e-8)),
    na.rm = TRUE
  ) * 100
}

select_best_candidate <- function(x_design, y_tfn, intervals, n_candidates, chunk_size) {
  best_error <- Inf
  best_coeff <- NULL
  best_index <- NA_integer_
  generated <- 0

  while (generated < n_candidates) {
    current_n <- min(chunk_size, n_candidates - generated)
    coeff_chunk <- generate_random_tfn_coefficients(intervals, current_n)
    pred <- predict_tfn_chunk(x_design, coeff_chunk)
    errors <- msee_for_chunk(y_tfn, pred)
    local_best <- which.min(errors)

    if (length(local_best) == 1 && is.finite(errors[local_best]) && errors[local_best] < best_error) {
      best_error <- errors[local_best]
      best_coeff <- coeff_chunk[local_best, , , drop = FALSE][1, , ]
      best_index <- generated + local_best
    }

    generated <- generated + current_n
  }

  list(coefficients = best_coeff, train_msee = best_error, candidate_index = best_index)
}

predict_tfn_one_candidate <- function(x_design, coeff_matrix) {
  coeff_array <- array(coeff_matrix, dim = c(1, nrow(coeff_matrix), 3))
  pred <- predict_tfn_chunk(x_design, coeff_array)
  cbind(
    lower = as.numeric(pred$lower[1, ]),
    center = as.numeric(pred$center[1, ]),
    upper = as.numeric(pred$upper[1, ])
  )
}

predictions <- list()
metrics <- list()
coefficients <- list()
interval_logs <- list()
scaling_logs <- list()

pred_counter <- 1
metric_counter <- 1
coef_counter <- 1
interval_counter <- 1
scaling_counter <- 1

set.seed(12311)

for (delta in fuzzification_rates) {
  cat("Running TFN-MC linear fuzzification rate:", delta, "\n")

  for (crop_name in sort(unique(panel$crop))) {
    crop_data <- panel[panel$crop == crop_name, ]
    crop_data <- crop_data[order(crop_data$year), ]
    years <- crop_data$year
    test_years <- years[(min_train_years + 1):length(years)]

    crop_model_predictions <- list()
    crop_pred_counter <- 1

    for (test_year in test_years) {
      train <- crop_data[crop_data$year < test_year, ]
      test <- crop_data[crop_data$year == test_year, ]

      train_x <- as.matrix(train[, predictors, drop = FALSE])
      test_x <- as.matrix(test[, predictors, drop = FALSE])
      scaled <- scale_train_test_y(train_x, test_x, train[[response]])

      x_train_design <- cbind("(Intercept)" = 1, scaled$train)
      x_test_design <- cbind("(Intercept)" = 1, scaled$test)

      scaling_logs[[scaling_counter]] <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        y_center = scaled$y_center,
        y_scale = scaled$y_scale,
        predictor = predictors,
        x_center = as.numeric(scaled$x_center),
        x_scale = as.numeric(scaled$x_scale),
        stringsAsFactors = FALSE
      )
      scaling_counter <- scaling_counter + 1

      y_train_tfn <- make_tfn_response(scaled$train_y, delta)

      intervals <- ols_center_intervals(
        x_design = x_train_design,
        y = scaled$train_y,
        interval_multiplier = interval_multiplier
      )

      selected <- select_best_candidate(
        x_design = x_train_design,
        y_tfn = y_train_tfn,
        intervals = intervals,
        n_candidates = N_candidates,
        chunk_size = chunk_size
      )

      pred_tfn <- predict_tfn_one_candidate(x_test_design, selected$coefficients)
      pred_tfn <- back_transform_tfn_matrix(pred_tfn, scaled$y_center, scaled$y_scale)
      pred_defuzz_cog <- rowMeans(pred_tfn)

      train_pred_tfn <- predict_tfn_one_candidate(x_train_design, selected$coefficients)
      train_e2 <- e2_for_one(y_train_tfn, train_pred_tfn)
      train_mapee <- mapee_for_one(y_train_tfn, train_pred_tfn)

      pred_row <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        observed = test[[response]],
        pred_lower = pred_tfn[, "lower"],
        pred_center = pred_tfn[, "center"],
        pred_upper = pred_tfn[, "upper"],
        pred_defuzz_cog = pred_defuzz_cog,
        train_msee = selected$train_msee,
        train_e2 = train_e2,
        train_mapee = train_mapee,
        n_candidates = N_candidates,
        candidate_index = selected$candidate_index,
        fit_status = ifelse(is.null(selected$coefficients), "failed", "ok"),
        stringsAsFactors = FALSE
      )

      predictions[[pred_counter]] <- pred_row
      crop_model_predictions[[crop_pred_counter]] <- pred_row

      coefficients[[coef_counter]] <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        coefficient = intervals$coefficient,
        coef_lower = selected$coefficients[, 1],
        coef_center = selected$coefficients[, 2],
        coef_upper = selected$coefficients[, 3],
        stringsAsFactors = FALSE
      )

      interval_logs[[interval_counter]] <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        coefficient = intervals$coefficient,
        beta_center = intervals$beta_center,
        interval_lower = intervals$interval_lower,
        interval_upper = intervals$interval_upper,
        half_width = intervals$half_width,
        interval_multiplier = interval_multiplier,
        stringsAsFactors = FALSE
      )

      pred_counter <- pred_counter + 1
      crop_pred_counter <- crop_pred_counter + 1
      coef_counter <- coef_counter + 1
      interval_counter <- interval_counter + 1
    }

    df <- do.call(rbind, crop_model_predictions)
    ok <- !is.na(df$pred_defuzz_cog)
    metrics[[metric_counter]] <- data.frame(
      fuzzification_rate = delta,
      fuzzification_percent = paste0(delta * 100, "%"),
      crop = crop_name,
      model = model_name,
      n_test = sum(ok),
      test_year_start = min(df$year),
      test_year_end = max(df$year),
      MAE = mae(df$observed[ok], df$pred_defuzz_cog[ok]),
      RMSE = rmse(df$observed[ok], df$pred_defuzz_cog[ok]),
      MAPE = mape(df$observed[ok], df$pred_defuzz_cog[ok]),
      R2 = r2_score(df$observed[ok], df$pred_defuzz_cog[ok]),
      mean_train_msee = mean(df$train_msee[ok], na.rm = TRUE),
      mean_train_e2 = mean(df$train_e2[ok], na.rm = TRUE),
      mean_train_mapee = mean(df$train_mapee[ok], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    metric_counter <- metric_counter + 1
  }
}

predictions <- do.call(rbind, predictions)
metrics <- do.call(rbind, metrics)
coefficients <- do.call(rbind, coefficients)
interval_logs <- do.call(rbind, interval_logs)
scaling_logs <- do.call(rbind, scaling_logs)

rate_model_summary <- aggregate(
  cbind(MAE, RMSE, MAPE, R2, mean_train_msee, mean_train_e2, mean_train_mapee) ~
    fuzzification_rate + fuzzification_percent + model,
  metrics,
  mean,
  na.rm = TRUE
)

best_by_crop_all_rates <- metrics[order(metrics$crop, metrics$RMSE), ]
best_by_crop_all_rates <- best_by_crop_all_rates[!duplicated(best_by_crop_all_rates$crop), ]

write.csv(predictions, file.path(mc_dir, "TFN_MC_linear_predictions.csv"), row.names = FALSE)
write.csv(metrics, file.path(mc_dir, "TFN_MC_linear_metrics.csv"), row.names = FALSE)
write.csv(coefficients, file.path(mc_dir, "TFN_MC_linear_selected_coefficients.csv"), row.names = FALSE)
write.csv(interval_logs, file.path(mc_dir, "TFN_MC_linear_candidate_intervals.csv"), row.names = FALSE)
write.csv(scaling_logs, file.path(mc_dir, "TFN_MC_linear_scaling_log.csv"), row.names = FALSE)
write.csv(rate_model_summary, file.path(mc_dir, "TFN_MC_linear_rate_summary.csv"), row.names = FALSE)
write.csv(best_by_crop_all_rates, file.path(mc_dir, "TFN_MC_linear_best_by_crop_all_rates.csv"), row.names = FALSE)

excel_path <- file.path(mc_dir, "TFN_MC_linear_results.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(wb, "README", data.frame(
  Item = c(
    "Purpose",
    "Method",
    "Fuzzification rates",
    "Standardization",
    "TFN response",
    "Candidate vectors",
    "Selection criterion",
    "Validation",
    "Defuzzification"
  ),
  Description = c(
    "Section 2 results for TFN-based fuzzy linear regression with Monte Carlo coefficient search.",
    "Random TFN coefficient vectors are generated and the vector minimizing training MSEe is selected.",
    "5%, 10%, and 15%",
    "Predictors and yield are standardized within each training window; predictions are back-transformed to kg/ha before error calculation.",
    "On the standardized yield scale: Y_tilde = (y_std - delta*abs(y_std), y_std, y_std + delta*abs(y_std)).",
    paste0(N_candidates, " candidate vectors per crop-test-year fit"),
    "Minimum training MSEe",
    "Expanding window; test years 2015-2024",
    "Center of gravity: (lower + center + upper) / 3"
  )
), startRow = 1)

addWorksheet(wb, "Rate_Summary")
writeData(wb, "Rate_Summary", rate_model_summary)

addWorksheet(wb, "Best_By_Crop_All_Rates")
writeData(wb, "Best_By_Crop_All_Rates", best_by_crop_all_rates)

addWorksheet(wb, "All_Metrics")
writeData(wb, "All_Metrics", metrics)

for (delta in fuzzification_rates) {
  sheet_name <- paste0("Rate_", delta * 100, "pct")
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, metrics[metrics$fuzzification_rate == delta, ])
}

addWorksheet(wb, "All_Predictions")
writeData(wb, "All_Predictions", predictions)

addWorksheet(wb, "Selected_Coefficients")
writeData(wb, "Selected_Coefficients", coefficients)

addWorksheet(wb, "Candidate_Intervals")
writeData(wb, "Candidate_Intervals", interval_logs)

addWorksheet(wb, "Scaling_Log")
writeData(wb, "Scaling_Log", scaling_logs)

header_style <- createStyle(
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

for (sheet_name in names(wb)) {
  freezePane(wb, sheet_name, firstRow = TRUE)
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:60, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:60, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("TFN-MC linear analysis completed.\n")
cat("Outputs written to:", mc_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n")
