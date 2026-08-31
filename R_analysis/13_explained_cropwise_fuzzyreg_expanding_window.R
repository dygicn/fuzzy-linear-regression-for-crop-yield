# Explained crop-wise fuzzyreg expanding-window analysis
#
# Purpose:
# This script shows, step by step, how fuzzyreg-based fuzzy linear regression
# predictions and crop-specific error metrics are obtained.
#
# Main idea:
# For each fuzzification rate, crop, and fuzzy regression model:
#   1. Split the data by year using expanding-window validation.
#   2. Standardize predictors and yield using training-window statistics.
#   3. Train the fuzzy model only on past years.
#   3. Predict the next held-out year.
#   4. Back-transform fuzzy predictions to the original yield scale.
#   5. Defuzzify the fuzzy prediction using center of gravity (COG).
#   6. Compute crop-specific MAE, RMSE, MAPE, and R2 over all test years.
#
# Example:
#   Train 2000-2014 -> Test 2015
#   Train 2000-2015 -> Test 2016
#   ...
#   Train 2000-2023 -> Test 2024

rm(list = ls())

library(fuzzyreg)
library(openxlsx)

# -------------------------------------------------------------------------
# 1. File paths and data
# -------------------------------------------------------------------------

analysis_dir <- "/Users/duyguicen/Desktop/crop_yield_results_2026_07_20_no_projection"
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found. Run 00_build_panel_dataset.R first.")
}

explained_dir <- file.path(output_dir, "explained_fuzzyreg_expanding_window")
if (!dir.exists(explained_dir)) dir.create(explained_dir, recursive = TRUE)

panel <- read.csv(panel_path, stringsAsFactors = FALSE, check.names = FALSE)

# -------------------------------------------------------------------------
# 2. Model variables
# -------------------------------------------------------------------------

# Environmental predictors used for crop yield prediction.
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

# Crisp crop yield response.
response <- "yield_kg_ha"

# Fuzzyreg methods to be compared.
fuzzy_methods <- c("FLAR", "PLR", "PLRLS")

# Fuzzification rates for the response TFN.
# Example with delta = 0.10 after yield standardization:
# y_std becomes TFN(y_std - 0.10*abs(y_std), y_std, y_std + 0.10*abs(y_std)).
#
# We save results for each rate separately because reviewers may ask how
# sensitive the fuzzy regression results are to the chosen fuzzification level.
fuzzification_rates <- c(0.05, 0.10, 0.15)

# In the fuzzyreg package, FLAR and PLR use fuzzy response spreads through
# fuzzy.left.y and fuzzy.right.y. PLRLS estimates fuzzy coefficients from a
# crisp response in the package implementation, so the response fuzzification
# rate is not applicable to PLRLS. We therefore run PLRLS once and label its
# fuzzification level as "N/A" instead of duplicating identical PLRLS results
# under 5%, 10%, and 15%.
method_fuzzification_rates <- function(method) {
  if (method == "PLRLS") {
    NA_real_
  } else {
    fuzzification_rates
  }
}

format_fuzzification_percent <- function(fuzzification_rate) {
  if (is.na(fuzzification_rate)) {
    "N/A"
  } else {
    paste0(fuzzification_rate * 100, "%")
  }
}

# First 15 years are used as the initial training window.
# With 2000-2024 data, this gives test years 2015-2024.
min_train_years <- 15

formula_text <- paste(response, "~", paste(predictors, collapse = " + "))
model_formula <- as.formula(formula_text)

# -------------------------------------------------------------------------
# 3. Error metric functions
# -------------------------------------------------------------------------

mae <- function(obs, pred) {
  mean(abs(obs - pred), na.rm = TRUE)
}

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

mape <- function(obs, pred) {
  mean(abs((obs - pred) / obs), na.rm = TRUE) * 100
}

r2_score <- function(obs, pred) {
  1 - sum((obs - pred)^2, na.rm = TRUE) /
    sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

scale_train_test_xy <- function(train, test, predictors, response) {
  train_x <- as.matrix(train[, predictors, drop = FALSE])
  test_x <- as.matrix(test[, predictors, drop = FALSE])

  x_center <- apply(train_x, 2, mean)
  x_scale <- apply(train_x, 2, sd)
  x_scale[x_scale == 0 | is.na(x_scale)] <- 1

  y_center <- mean(train[[response]], na.rm = TRUE)
  y_scale <- sd(train[[response]], na.rm = TRUE)
  if (is.na(y_scale) || y_scale == 0) y_scale <- 1

  train[, predictors] <- sweep(sweep(train_x, 2, x_center, "-"), 2, x_scale, "/")
  test[, predictors] <- sweep(sweep(test_x, 2, x_center, "-"), 2, x_scale, "/")

  train[[response]] <- (train[[response]] - y_center) / y_scale

  list(
    train = train,
    test = test,
    y_center = y_center,
    y_scale = y_scale,
    x_scaling = data.frame(
      predictor = predictors,
      x_center = as.numeric(x_center),
      x_scale = as.numeric(x_scale),
      stringsAsFactors = FALSE
    )
  )
}

back_transform_tfn <- function(pred, y_center, y_scale) {
  pred$pred_lower <- pred$pred_lower * y_scale + y_center
  pred$pred_center <- pred$pred_center * y_scale + y_center
  pred$pred_upper <- pred$pred_upper * y_scale + y_center
  pred$pred_defuzz_cog <- pred$pred_defuzz_cog * y_scale + y_center
  pred
}

# -------------------------------------------------------------------------
# 4. Fit fuzzyreg model on one training set
# -------------------------------------------------------------------------

fit_fuzzy_model <- function(train, method, fuzzification_rate) {
  # fuzzyreg expects the left and right spreads of the fuzzy response.
  # We define a symmetric triangular fuzzy number around each observed yield:
  #   Y_tilde = (y - delta*y, y, y + delta*y)
  # After yield standardization, y may be negative. Therefore, both left and
  # right spreads are defined as delta*abs(y_std).
  if (!is.na(fuzzification_rate)) {
    train$yield_left_spread <- abs(train[[response]]) * fuzzification_rate
    train$yield_right_spread <- abs(train[[response]]) * fuzzification_rate
  }

  if (method %in% c("FLAR", "PLR")) {
    # FLAR and PLR use fuzzy response spreads explicitly.
    call <- substitute(
      fuzzylm(
        FORMULA,
        data = train,
        method = METHOD,
        fuzzy.left.y = "yield_left_spread",
        fuzzy.right.y = "yield_right_spread"
      ),
      list(
        FORMULA = model_formula,
        METHOD = tolower(method)
      )
    )
    eval(call)

  } else if (method == "PLRLS") {
    # PLRLS in fuzzyreg is fitted with the package's own least-squares setup.
    # The package implementation does not use fuzzy.left.y/fuzzy.right.y for
    # PLRLS in the same way as FLAR and PLR, so fuzzification_rate is N/A.
    call <- substitute(
      fuzzylm(
        FORMULA,
        data = train,
        method = "plrls"
      ),
      list(FORMULA = model_formula)
    )
    eval(call)

  } else {
    stop(paste("Unknown fuzzy method:", method))
  }
}

# -------------------------------------------------------------------------
# 5. Manual fuzzy prediction and COG defuzzification
# -------------------------------------------------------------------------

manual_fuzzy_predict <- function(fit, newdata, predictors) {
  # fuzzyreg stores fuzzy coefficients with center, left spread, and right spread.
  coef_mat <- fit$coef

  # Negative spreads are not meaningful for TFNs, so we truncate them at zero.
  coef_mat[, "left.spread"] <- pmax(0, coef_mat[, "left.spread"])
  coef_mat[, "right.spread"] <- pmax(0, coef_mat[, "right.spread"])

  # Design matrix for the new observation/year.
  X <- cbind("(Intercept)" = 1, newdata[, predictors, drop = FALSE])
  X <- as.matrix(X)

  # Center prediction.
  pred_center <- as.numeric(X %*% coef_mat[rownames(coef_mat), "center"])

  # Lower and upper prediction bounds are calculated using interval arithmetic.
  pred_lower <- rep(0, nrow(X))
  pred_upper <- rep(0, nrow(X))

  for (j in seq_len(ncol(X))) {
    term_name <- colnames(X)[j]
    x <- X[, j]

    beta_center <- coef_mat[term_name, "center"]
    beta_left_spread <- coef_mat[term_name, "left.spread"]
    beta_right_spread <- coef_mat[term_name, "right.spread"]

    beta_lower <- beta_center - beta_left_spread
    beta_upper <- beta_center + beta_right_spread

    # If x is positive, lower uses beta_lower and upper uses beta_upper.
    # If x is negative, multiplication reverses the interval bounds.
    term_lower <- ifelse(x >= 0, beta_lower * x, beta_upper * x)
    term_upper <- ifelse(x >= 0, beta_upper * x, beta_lower * x)

    pred_lower <- pred_lower + term_lower
    pred_upper <- pred_upper + term_upper
  }

  # Center of gravity defuzzification for triangular fuzzy prediction:
  #   y_hat_COG = (lower + center + upper) / 3
  pred_defuzz_cog <- (pred_lower + pred_center + pred_upper) / 3

  data.frame(
    pred_lower = pred_lower,
    pred_center = pred_center,
    pred_upper = pred_upper,
    pred_defuzz_cog = pred_defuzz_cog
  )
}

# -------------------------------------------------------------------------
# 6. Crop-wise expanding-window prediction
# -------------------------------------------------------------------------

all_predictions <- list()
scaling_rows <- list()
counter <- 1
scaling_counter <- 1

for (method in fuzzy_methods) {
  for (fuzzification_rate in method_fuzzification_rates(method)) {
    fuzzification_percent <- format_fuzzification_percent(fuzzification_rate)
    cat("Running method:", method, "| fuzzification:", fuzzification_percent, "\n")

    for (crop_name in sort(unique(panel$crop))) {
    # Work crop by crop.
    crop_data <- panel[panel$crop == crop_name, ]
    crop_data <- crop_data[order(crop_data$year), ]

    years <- crop_data$year

    # The first test year comes after the initial training window.
    # With 25 years and min_train_years = 15, test years are 2015-2024.
    test_years <- years[(min_train_years + 1):length(years)]

      for (test_year in test_years) {
        # Expanding-window split:
        # Train uses only years before the test year.
        # Test is the single held-out year.
        train <- crop_data[crop_data$year < test_year, ]
        test <- crop_data[crop_data$year == test_year, ]
        scaled <- scale_train_test_xy(train, test, predictors, response)
        train_scaled <- scaled$train
        test_scaled <- scaled$test

        scaling_rows[[scaling_counter]] <- data.frame(
          fuzzification_rate = fuzzification_rate,
          fuzzification_percent = fuzzification_percent,
          crop = crop_name,
          model = method,
          test_year = test_year,
          y_center = scaled$y_center,
          y_scale = scaled$y_scale,
          scaled$x_scaling,
          stringsAsFactors = FALSE
        )
        scaling_counter <- scaling_counter + 1

        # Fit fuzzy linear regression on the training window only.
        fit <- try(fit_fuzzy_model(train_scaled, method, fuzzification_rate), silent = TRUE)

        if (inherits(fit, "try-error")) {
          # If a model fails, keep the row so the failure is visible.
          all_predictions[[counter]] <- data.frame(
            fuzzification_rate = fuzzification_rate,
            fuzzification_percent = fuzzification_percent,
            crop = crop_name,
            model = method,
            train_start = min(train$year),
            train_end = max(train$year),
            test_year = test_year,
            observed = test[[response]],
            pred_lower = NA_real_,
            pred_center = NA_real_,
            pred_upper = NA_real_,
            pred_defuzz_cog = NA_real_,
            absolute_error = NA_real_,
            squared_error = NA_real_,
            percentage_error = NA_real_,
            fit_status = "failed",
            stringsAsFactors = FALSE
          )

        } else {
          # Predict the held-out test year.
          pred <- manual_fuzzy_predict(fit, test_scaled, predictors)
          pred <- back_transform_tfn(pred, scaled$y_center, scaled$y_scale)

          # Year-specific prediction errors.
          abs_err <- abs(test[[response]] - pred$pred_defuzz_cog)
          sq_err <- (test[[response]] - pred$pred_defuzz_cog)^2
          pct_err <- abs((test[[response]] - pred$pred_defuzz_cog) / test[[response]]) * 100

          all_predictions[[counter]] <- data.frame(
            fuzzification_rate = fuzzification_rate,
            fuzzification_percent = fuzzification_percent,
            crop = crop_name,
            model = method,
            train_start = min(train$year),
            train_end = max(train$year),
            test_year = test_year,
            observed = test[[response]],
            pred_lower = pred$pred_lower,
            pred_center = pred$pred_center,
            pred_upper = pred$pred_upper,
            pred_defuzz_cog = pred$pred_defuzz_cog,
            absolute_error = abs_err,
            squared_error = sq_err,
            percentage_error = pct_err,
            fit_status = "ok",
            stringsAsFactors = FALSE
          )
        }

        counter <- counter + 1
      }
      }
  }
}

all_predictions <- do.call(rbind, all_predictions)
scaling_log <- do.call(rbind, scaling_rows)

# -------------------------------------------------------------------------
# 7. Crop-wise error metrics from the expanding-window predictions
# -------------------------------------------------------------------------

crop_model_metrics <- do.call(
  rbind,
  lapply(split(
    all_predictions,
    list(all_predictions$fuzzification_percent, all_predictions$crop, all_predictions$model),
    drop = TRUE
  ), function(df) {
    ok <- !is.na(df$pred_defuzz_cog)

    data.frame(
      fuzzification_rate = df$fuzzification_rate[1],
      fuzzification_percent = df$fuzzification_percent[1],
      crop = df$crop[1],
      model = df$model[1],
      n_test = sum(ok),
      test_year_start = min(df$test_year),
      test_year_end = max(df$test_year),

      # These metrics summarize the 10 held-out yearly predictions.
      MAE = mae(df$observed[ok], df$pred_defuzz_cog[ok]),
      RMSE = rmse(df$observed[ok], df$pred_defuzz_cog[ok]),
      MAPE = mape(df$observed[ok], df$pred_defuzz_cog[ok]),
      R2 = r2_score(df$observed[ok], df$pred_defuzz_cog[ok]),
      stringsAsFactors = FALSE
    )
  })
)

# Helper used only for deterministic tie-breaking in tables.
# If two rows have identical RMSE, the lower fuzzification rate is preferred.
# PLRLS has N/A fuzzification and is placed after numeric rates only when
# there is an exact tie.
crop_model_metrics$fuzzification_sort <- ifelse(
  is.na(crop_model_metrics$fuzzification_rate),
  Inf,
  crop_model_metrics$fuzzification_rate
)

# Best fuzzyreg model per crop and fuzzification level according to RMSE.
# This table includes "N/A" for PLRLS because PLRLS is not response-rate
# sensitive in the fuzzyreg implementation.
best_model_by_crop_by_rate <- crop_model_metrics[
  order(crop_model_metrics$fuzzification_sort, crop_model_metrics$crop, crop_model_metrics$RMSE),
]
best_model_by_crop_by_rate <- best_model_by_crop_by_rate[
  !duplicated(paste(best_model_by_crop_by_rate$fuzzification_percent, best_model_by_crop_by_rate$crop)),
]

# Best fuzzyreg model per crop across all fuzzification rates according to RMSE.
best_model_by_crop_all_rates <- crop_model_metrics[
  order(crop_model_metrics$crop, crop_model_metrics$RMSE, crop_model_metrics$fuzzification_sort),
]
best_model_by_crop_all_rates <- best_model_by_crop_all_rates[
  !duplicated(best_model_by_crop_all_rates$crop),
]

# Remove the internal sorting helper from saved tables.
crop_model_metrics$fuzzification_sort <- NULL
best_model_by_crop_by_rate$fuzzification_sort <- NULL
best_model_by_crop_all_rates$fuzzification_sort <- NULL

# Backward-compatible alias for the older output file name.
best_model_by_crop <- best_model_by_crop_all_rates

# Average performance summaries by fuzzification rate.
# These summaries exclude PLRLS because its fuzzification level is N/A.
rate_sensitive_metrics <- crop_model_metrics[!is.na(crop_model_metrics$fuzzification_rate), ]
rate_sensitive_best_by_crop <- best_model_by_crop_by_rate[
  !is.na(best_model_by_crop_by_rate$fuzzification_rate),
]

rate_summary_all_models <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~ fuzzification_rate + fuzzification_percent,
  rate_sensitive_metrics,
  mean,
  na.rm = TRUE
)

rate_summary_best_by_crop <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~ fuzzification_rate + fuzzification_percent,
  rate_sensitive_best_by_crop,
  mean,
  na.rm = TRUE
)

# -------------------------------------------------------------------------
# 8. Save outputs
# -------------------------------------------------------------------------

write.csv(
  all_predictions,
  file.path(explained_dir, "explained_fuzzyreg_expanding_window_predictions.csv"),
  row.names = FALSE
)

write.csv(
  scaling_log,
  file.path(explained_dir, "explained_fuzzyreg_expanding_window_scaling_log.csv"),
  row.names = FALSE
)

write.csv(
  crop_model_metrics,
  file.path(explained_dir, "explained_fuzzyreg_expanding_window_metrics.csv"),
  row.names = FALSE
)

write.csv(
  best_model_by_crop,
  file.path(explained_dir, "explained_fuzzyreg_best_model_by_crop_rmse.csv"),
  row.names = FALSE
)

write.csv(
  best_model_by_crop_by_rate,
  file.path(explained_dir, "explained_fuzzyreg_best_model_by_crop_by_rate_rmse.csv"),
  row.names = FALSE
)

write.csv(
  best_model_by_crop_all_rates,
  file.path(explained_dir, "explained_fuzzyreg_best_model_by_crop_all_rates_rmse.csv"),
  row.names = FALSE
)

write.csv(
  rate_summary_all_models,
  file.path(explained_dir, "explained_fuzzyreg_rate_summary_all_models.csv"),
  row.names = FALSE
)

write.csv(
  rate_summary_best_by_crop,
  file.path(explained_dir, "explained_fuzzyreg_rate_summary_best_by_crop.csv"),
  row.names = FALSE
)

# -------------------------------------------------------------------------
# 9. Save all results into one Excel workbook
# -------------------------------------------------------------------------

excel_path <- file.path(explained_dir, "explained_fuzzyreg_fuzzification_rates.xlsx")

wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(wb, "README", data.frame(
  Item = c(
    "Purpose",
    "Fuzzification rates",
    "TFN definition",
    "PLRLS note",
    "Standardization",
    "Validation",
    "Models",
    "Defuzzification",
    "Error metrics"
  ),
  Description = c(
    "Crop-wise fuzzyreg expanding-window predictions and errors for all fuzzification rates.",
    "5%, 10%, and 15% for FLAR and PLR; N/A for PLRLS.",
    "On the standardized yield scale: Y_tilde = (y_std - delta*abs(y_std), y_std, y_std + delta*abs(y_std)).",
    "PLRLS is fitted once because the fuzzyreg implementation estimates PLRLS from a crisp response rather than fuzzy response spreads.",
    "Predictors and yield are standardized within each training window; predictions are back-transformed to kg/ha before error calculation.",
    "Expanding window: train on past years, predict the next year; test years 2015-2024.",
    "FLAR, PLR, PLRLS",
    "Center of gravity: (lower + center + upper) / 3",
    "MAE, RMSE, MAPE, and out-of-sample R2"
  )
), startRow = 1)

addWorksheet(wb, "Rate_Summary_All")
writeData(wb, "Rate_Summary_All", rate_summary_all_models)

addWorksheet(wb, "Rate_Summary_Best")
writeData(wb, "Rate_Summary_Best", rate_summary_best_by_crop)

addWorksheet(wb, "Best_By_Crop_All_Rates")
writeData(wb, "Best_By_Crop_All_Rates", best_model_by_crop_all_rates)

addWorksheet(wb, "Best_By_Crop_By_Rate")
writeData(wb, "Best_By_Crop_By_Rate", best_model_by_crop_by_rate)

addWorksheet(wb, "All_Metrics")
writeData(wb, "All_Metrics", crop_model_metrics)

for (fuzzification_rate in fuzzification_rates) {
  sheet_name <- paste0("Rate_", fuzzification_rate * 100, "pct")
  addWorksheet(wb, sheet_name)
  rate_rows <- crop_model_metrics[crop_model_metrics$fuzzification_rate == fuzzification_rate, ]
  writeData(wb, sheet_name, rate_rows)
}

addWorksheet(wb, "Rate_NA_PLRLS")
writeData(wb, "Rate_NA_PLRLS", crop_model_metrics[is.na(crop_model_metrics$fuzzification_rate), ])

addWorksheet(wb, "All_Predictions")
writeData(wb, "All_Predictions", all_predictions)

addWorksheet(wb, "Scaling_Log")
writeData(wb, "Scaling_Log", scaling_log)

header_style <- createStyle(
  fgFill = "#D9EAF7",
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

for (sheet_name in names(wb)) {
  freezePane(wb, sheet_name, firstRow = TRUE)
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:50, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("Explained fuzzyreg expanding-window analysis completed.\n")
cat("Outputs written to:", explained_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n")
