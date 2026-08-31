# Section 2.2: GFN-based fuzzy linear regression with Monte Carlo coefficient search
#
# This script follows the same Monte Carlo coefficient-search logic used in
# the TFN-MC linear regression analysis, but represents each regression
# coefficient as a Gaussian fuzzy number (GFN):
#
#   beta_tilde_j = (mu_beta_j, sigma_beta_j^2)
#
# For each expanding-window training set:
#   1. Predictors and yield are standardized using training-window statistics.
#   2. An OLS model is fitted on the standardized scale to obtain coefficient-
#      specific centers.
#   2. For each coefficient j, an OLS-centered interval is constructed:
#        [beta_hat_j - h_j, beta_hat_j + h_j]
#   3. Monte Carlo candidate GFN coefficient vectors are generated:
#        mu_beta_j      ~ U(beta_hat_j - h_j, beta_hat_j + h_j)
#        sigma_beta_j   ~ U(0, h_j)
#        sigma_beta_j^2 = sigma_beta_j^2
#   4. Each candidate vector is evaluated using a scalarized GFN-MSE.
#   5. The best candidate is used to predict the next held-out year.
#   6. GFN predictions are defuzzified on the standardized scale and then
#      back-transformed to the original yield scale for all reported errors.
#
# This differs from Gaussian fuzzy response sampling: the response is not
# repeatedly resampled. Instead, fuzzy regression coefficients are directly
# generated and selected, making the procedure parallel to TFN-MC.

rm(list = ls())

library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found. Run 00_build_panel_dataset.R first.")
}

mc_dir <- file.path(output_dir, "GFN_MC_coefficient_search")
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
N_candidates <- 10000
N_candidates_tune <- 3000
chunk_size <- 1000
interval_multiplier <- 3
inner_validation_years <- 3
model_name <- "GFN_MC_coefficient_search"

mae <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mape <- function(obs, pred) mean(abs((obs - pred) / obs), na.rm = TRUE) * 100
r2_score <- function(obs, pred) {
  1 - sum((obs - pred)^2, na.rm = TRUE) /
    sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

make_gfn_response <- function(y, delta) {
  sigma <- pmax(delta * abs(y), 1e-8)
  cbind(mu = y, variance = sigma^2)
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

back_transform_gfn_prediction <- function(pred_gfn, y_center, y_scale) {
  data.frame(
    pred_mu = pred_gfn[, "pred_mu"] * y_scale + y_center,
    pred_variance = pred_gfn[, "pred_variance"] * y_scale^2,
    stringsAsFactors = FALSE
  )
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

  half_width <- interval_multiplier * pmax(
    se,
    0.05 * abs(beta_hat),
    fallback
  )

  data.frame(
    coefficient = colnames(x_design),
    beta_center = beta_hat,
    interval_lower = beta_hat - half_width,
    interval_upper = beta_hat + half_width,
    half_width = half_width,
    variance_upper = half_width^2,
    stringsAsFactors = FALSE
  )
}

generate_random_gfn_coefficients <- function(intervals, n_candidates) {
  q <- nrow(intervals)
  mu <- matrix(NA_real_, nrow = n_candidates, ncol = q)
  sigma <- matrix(NA_real_, nrow = n_candidates, ncol = q)
  variance <- matrix(NA_real_, nrow = n_candidates, ncol = q)

  for (j in seq_len(q)) {
    mu[, j] <- runif(
      n_candidates,
      min = intervals$interval_lower[j],
      max = intervals$interval_upper[j]
    )

    # The coefficient-specific half-width h_j determines the uncertainty
    # scale for the GFN coefficient. We draw sigma from [0, h_j] and square it
    # to obtain sigma^2. This keeps coefficient uncertainty tied to the OLS-
    # centered search interval instead of using the marginal variance of X_j.
    sigma[, j] <- runif(
      n_candidates,
      min = 0,
      max = max(intervals$half_width[j], 1e-6)
    )
    variance[, j] <- sigma[, j]^2
  }

  list(mu = mu, sigma = sigma, variance = variance)
}

predict_gfn_chunk <- function(x_design, coeff_chunk) {
  pred_mu <- coeff_chunk$mu %*% t(x_design)
  pred_variance <- coeff_chunk$variance %*% t(x_design^2)
  list(mu = pred_mu, variance = pmax(pred_variance, 1e-12))
}

gfn_msee_for_chunk <- function(y_gfn, pred) {
  diff_mu <- sweep(pred$mu, 2, y_gfn[, "mu"], "-")
  diff_variance <- sweep(pred$variance, 2, y_gfn[, "variance"], "+")

  # GFN multiplication for squared error:
  # (m, v)^2 has mean m^2 and variance 2*v*m^2 + v^2.
  squared_error_mu <- diff_mu^2
  squared_error_variance <- 2 * diff_variance * diff_mu^2 + diff_variance^2

  msee_mu <- rowMeans(squared_error_mu, na.rm = TRUE)
  msee_variance <- rowMeans(squared_error_variance, na.rm = TRUE)

  # Scalar criterion for Monte Carlo selection. It keeps the squared-error
  # mean as the main target and penalizes high GFN error variance.
  msee_score <- msee_mu + sqrt(pmax(msee_variance, 0))

  data.frame(
    msee_mu = msee_mu,
    msee_variance = msee_variance,
    msee_score = msee_score
  )
}

select_best_candidate <- function(x_design, y_gfn, intervals, n_candidates, chunk_size) {
  best_score <- Inf
  best_mu <- NULL
  best_variance <- NULL
  best_index <- NA_integer_
  best_msee_mu <- NA_real_
  best_msee_variance <- NA_real_
  generated <- 0

  while (generated < n_candidates) {
    current_n <- min(chunk_size, n_candidates - generated)
    coeff_chunk <- generate_random_gfn_coefficients(intervals, current_n)
    pred <- predict_gfn_chunk(x_design, coeff_chunk)
    errors <- gfn_msee_for_chunk(y_gfn, pred)
    local_best <- which.min(errors$msee_score)

    if (
      length(local_best) == 1 &&
        is.finite(errors$msee_score[local_best]) &&
        errors$msee_score[local_best] < best_score
    ) {
      best_score <- errors$msee_score[local_best]
      best_msee_mu <- errors$msee_mu[local_best]
      best_msee_variance <- errors$msee_variance[local_best]
      best_mu <- coeff_chunk$mu[local_best, ]
      best_variance <- coeff_chunk$variance[local_best, ]
      best_index <- generated + local_best
    }

    generated <- generated + current_n
  }

  list(
    mu = best_mu,
    variance = best_variance,
    train_msee_mu = best_msee_mu,
    train_msee_variance = best_msee_variance,
    train_msee_score = best_score,
    candidate_index = best_index
  )
}

predict_gfn_one_candidate <- function(x_design, selected) {
  coeff_chunk <- list(
    mu = matrix(selected$mu, nrow = 1),
    variance = matrix(selected$variance, nrow = 1)
  )
  pred <- predict_gfn_chunk(x_design, coeff_chunk)
  cbind(
    pred_mu = as.numeric(pred$mu[1, ]),
    pred_variance = as.numeric(pred$variance[1, ])
  )
}

gfn_defuzz <- function(mu, variance, alpha = 0, kappa = 1, d_threshold = Inf) {
  sigma <- sqrt(pmax(variance, 1e-12))
  delta_abs <- abs(mu / sigma)
  adjustment <- alpha / (1 + exp(-kappa * (delta_abs - d_threshold))) * variance
  ifelse(delta_abs < d_threshold, mu + adjustment, mu)
}

tune_gfn_defuzz_params <- function(train, delta, n_candidates_tune, chunk_size) {
  train <- train[order(train$year), ]
  candidate_years <- tail(unique(train$year), inner_validation_years)
  validation_rows <- list()
  row_counter <- 1

  for (validation_year in candidate_years) {
    inner_train <- train[train$year < validation_year, ]
    inner_validation <- train[train$year == validation_year, ]

    if (nrow(inner_train) <= length(predictors) + 1 || nrow(inner_validation) == 0) {
      next
    }

    inner_train_x <- as.matrix(inner_train[, predictors, drop = FALSE])
    inner_validation_x <- as.matrix(inner_validation[, predictors, drop = FALSE])
    scaled <- scale_train_test_y(
      inner_train_x,
      inner_validation_x,
      inner_train[[response]]
    )

    x_inner_train <- cbind(`(Intercept)` = 1, scaled$train)
    x_inner_validation <- cbind(`(Intercept)` = 1, scaled$test)
    y_inner_gfn <- make_gfn_response(scaled$train_y, delta)

    intervals <- ols_center_intervals(
      x_design = x_inner_train,
      y = scaled$train_y,
      interval_multiplier = interval_multiplier
    )

    selected <- select_best_candidate(
      x_design = x_inner_train,
      y_gfn = y_inner_gfn,
      intervals = intervals,
      n_candidates = n_candidates_tune,
      chunk_size = chunk_size
    )

    if (is.null(selected$mu)) {
      next
    }

    pred_gfn <- predict_gfn_one_candidate(x_inner_validation, selected)

    validation_rows[[row_counter]] <- data.frame(
      observed = (inner_validation[[response]] - scaled$y_center) / scaled$y_scale,
      pred_mu = pred_gfn[, "pred_mu"],
      pred_variance = pred_gfn[, "pred_variance"],
      stringsAsFactors = FALSE
    )
    row_counter <- row_counter + 1
  }

  if (length(validation_rows) == 0) {
    return(list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = NA_real_))
  }

  validation_df <- do.call(rbind, validation_rows)
  sigma <- sqrt(pmax(validation_df$pred_variance, 1e-12))
  delta_abs <- abs(validation_df$pred_mu / sigma)
  finite_delta <- delta_abs[is.finite(delta_abs)]

  if (length(finite_delta) == 0) {
    d_grid <- Inf
  } else {
    d_grid <- unique(as.numeric(quantile(finite_delta, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)))
    d_grid <- d_grid[is.finite(d_grid) & d_grid > 0]
    if (length(d_grid) == 0) d_grid <- median(finite_delta, na.rm = TRUE)
  }

  alpha_grid <- c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)
  kappa_grid <- c(0.5, 1, 2, 5)

  best <- list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = Inf)

  for (alpha in alpha_grid) {
    for (kappa in kappa_grid) {
      for (d_threshold in d_grid) {
        pred_defuzz <- gfn_defuzz(
          mu = validation_df$pred_mu,
          variance = validation_df$pred_variance,
          alpha = alpha,
          kappa = kappa,
          d_threshold = d_threshold
        )
        score <- rmse(validation_df$observed, pred_defuzz)

        if (is.finite(score) && score < best$inner_rmse) {
          best <- list(
            alpha = alpha,
            kappa = kappa,
            d_threshold = d_threshold,
            inner_rmse = score
          )
        }
      }
    }
  }

  if (!is.finite(best$inner_rmse)) {
    best <- list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = NA_real_)
  }

  best
}

predictions <- list()
metrics <- list()
coefficients <- list()
interval_logs <- list()
defuzz_tuning_rows <- list()
scaling_logs <- list()
pred_counter <- 1
metric_counter <- 1
coef_counter <- 1
interval_counter <- 1
defuzz_counter <- 1
scaling_counter <- 1

set.seed(12311)

for (delta in fuzzification_rates) {
  cat("Running GFN-MC coefficient search fuzzification rate:", delta, "\n")

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

      x_train_design <- cbind(`(Intercept)` = 1, scaled$train)
      x_test_design <- cbind(`(Intercept)` = 1, scaled$test)

      y_train_gfn <- make_gfn_response(scaled$train_y, delta)

      intervals <- ols_center_intervals(
        x_design = x_train_design,
        y = scaled$train_y,
        interval_multiplier = interval_multiplier
      )

      selected <- select_best_candidate(
        x_design = x_train_design,
        y_gfn = y_train_gfn,
        intervals = intervals,
        n_candidates = N_candidates,
        chunk_size = chunk_size
      )

      pred_gfn <- predict_gfn_one_candidate(x_test_design, selected)

      defuzz_params <- tune_gfn_defuzz_params(
        train = train,
        delta = delta,
        n_candidates_tune = N_candidates_tune,
        chunk_size = chunk_size
      )

      # Lemma 2 GFN defuzzification:
      # A = mu + alpha / (1 + exp(-kappa * (|delta| - d))) * sigma^2
      # when |delta| < d, and A = mu otherwise, where |delta| = |mu / sigma|.
      pred_defuzz_gfn <- gfn_defuzz(
        mu = pred_gfn[, "pred_mu"],
        variance = pred_gfn[, "pred_variance"],
        alpha = defuzz_params$alpha,
        kappa = defuzz_params$kappa,
        d_threshold = defuzz_params$d_threshold
      )
      pred_gfn_original <- back_transform_gfn_prediction(
        pred_gfn,
        y_center = scaled$y_center,
        y_scale = scaled$y_scale
      )
      pred_defuzz_original <- pred_defuzz_gfn * scaled$y_scale + scaled$y_center

      pred_row <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        observed = test[[response]],
        pred_mu = pred_gfn_original$pred_mu,
        pred_variance = pred_gfn_original$pred_variance,
        pred_sd = sqrt(pmax(pred_gfn_original$pred_variance, 0)),
        pred_lower_95 = pmax(0, pred_gfn_original$pred_mu - 1.96 * sqrt(pmax(pred_gfn_original$pred_variance, 0))),
        pred_upper_95 = pred_gfn_original$pred_mu + 1.96 * sqrt(pmax(pred_gfn_original$pred_variance, 0)),
        pred_defuzz_gfn = pred_defuzz_original,
        pred_mu_std = pred_gfn[, "pred_mu"],
        pred_variance_std = pred_gfn[, "pred_variance"],
        pred_defuzz_gfn_std = pred_defuzz_gfn,
        y_center = scaled$y_center,
        y_scale = scaled$y_scale,
        defuzz_alpha = defuzz_params$alpha,
        defuzz_kappa = defuzz_params$kappa,
        defuzz_d = defuzz_params$d_threshold,
        defuzz_inner_rmse = defuzz_params$inner_rmse,
        train_msee_mu = selected$train_msee_mu,
        train_msee_variance = selected$train_msee_variance,
        train_msee_score = selected$train_msee_score,
        n_candidates = N_candidates,
        n_candidates_tune = N_candidates_tune,
        candidate_index = selected$candidate_index,
        fit_status = ifelse(is.null(selected$mu), "failed", "ok"),
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
        coef_mu = selected$mu,
        coef_sigma = sqrt(pmax(selected$variance, 0)),
        coef_variance = selected$variance,
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
        variance_upper = intervals$variance_upper,
        interval_multiplier = interval_multiplier,
        stringsAsFactors = FALSE
      )

      defuzz_tuning_rows[[defuzz_counter]] <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        alpha = defuzz_params$alpha,
        kappa = defuzz_params$kappa,
        d_threshold = defuzz_params$d_threshold,
        inner_rmse = defuzz_params$inner_rmse,
        n_candidates_tune = N_candidates_tune,
        stringsAsFactors = FALSE
      )

      scaling_logs[[scaling_counter]] <- data.frame(
        fuzzification_rate = delta,
        fuzzification_percent = paste0(delta * 100, "%"),
        crop = crop_name,
        year = test_year,
        model = model_name,
        predictor = predictors,
        x_center = as.numeric(scaled$x_center),
        x_scale = as.numeric(scaled$x_scale),
        y_center = scaled$y_center,
        y_scale = scaled$y_scale,
        stringsAsFactors = FALSE
      )

      pred_counter <- pred_counter + 1
      crop_pred_counter <- crop_pred_counter + 1
      coef_counter <- coef_counter + 1
      interval_counter <- interval_counter + 1
      defuzz_counter <- defuzz_counter + 1
      scaling_counter <- scaling_counter + 1
    }

    df <- do.call(rbind, crop_model_predictions)
    ok <- !is.na(df$pred_defuzz_gfn)
    metrics[[metric_counter]] <- data.frame(
      fuzzification_rate = delta,
      fuzzification_percent = paste0(delta * 100, "%"),
      crop = crop_name,
      model = model_name,
      n_test = sum(ok),
      test_year_start = min(df$year),
      test_year_end = max(df$year),
      MAE = mae(df$observed[ok], df$pred_defuzz_gfn[ok]),
      RMSE = rmse(df$observed[ok], df$pred_defuzz_gfn[ok]),
      MAPE = mape(df$observed[ok], df$pred_defuzz_gfn[ok]),
      R2 = r2_score(df$observed[ok], df$pred_defuzz_gfn[ok]),
      mean_interval_width_95 = mean(df$pred_upper_95[ok] - df$pred_lower_95[ok]),
      empirical_coverage_95 = mean(df$observed[ok] >= df$pred_lower_95[ok] &
                                     df$observed[ok] <= df$pred_upper_95[ok]),
      mean_train_msee_mu = mean(df$train_msee_mu[ok], na.rm = TRUE),
      mean_train_msee_variance = mean(df$train_msee_variance[ok], na.rm = TRUE),
      mean_train_msee_score = mean(df$train_msee_score[ok], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    metric_counter <- metric_counter + 1
  }
}

predictions <- do.call(rbind, predictions)
metrics <- do.call(rbind, metrics)
coefficients <- do.call(rbind, coefficients)
interval_logs <- do.call(rbind, interval_logs)
defuzz_tuning_rows <- do.call(rbind, defuzz_tuning_rows)
scaling_logs <- do.call(rbind, scaling_logs)

rate_model_summary <- aggregate(
  cbind(
    MAE,
    RMSE,
    MAPE,
    R2,
    mean_interval_width_95,
    empirical_coverage_95,
    mean_train_msee_mu,
    mean_train_msee_variance,
    mean_train_msee_score
  ) ~ fuzzification_rate + fuzzification_percent + model,
  metrics,
  mean,
  na.rm = TRUE
)

best_by_crop_all_rates <- metrics[order(metrics$crop, metrics$RMSE), ]
best_by_crop_all_rates <- best_by_crop_all_rates[!duplicated(best_by_crop_all_rates$crop), ]

write.csv(predictions, file.path(mc_dir, "GFN_MC_coefficient_search_predictions.csv"), row.names = FALSE)
write.csv(metrics, file.path(mc_dir, "GFN_MC_coefficient_search_metrics.csv"), row.names = FALSE)
write.csv(coefficients, file.path(mc_dir, "GFN_MC_coefficient_search_coefficients.csv"), row.names = FALSE)
write.csv(interval_logs, file.path(mc_dir, "GFN_MC_coefficient_search_candidate_intervals.csv"), row.names = FALSE)
write.csv(defuzz_tuning_rows, file.path(mc_dir, "GFN_MC_coefficient_search_defuzzification_tuning.csv"), row.names = FALSE)
write.csv(scaling_logs, file.path(mc_dir, "GFN_MC_coefficient_search_scaling_log.csv"), row.names = FALSE)
write.csv(rate_model_summary, file.path(mc_dir, "GFN_MC_coefficient_search_rate_summary.csv"), row.names = FALSE)
write.csv(best_by_crop_all_rates, file.path(mc_dir, "GFN_MC_coefficient_search_best_by_crop_all_rates.csv"), row.names = FALSE)

excel_path <- file.path(mc_dir, "GFN_MC_coefficient_search_results.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  data.frame(
    Item = c(
      "Purpose",
      "GFN response",
      "Standardization",
      "GFN coefficients",
      "Candidate intervals",
      "Selection criterion",
      "Defuzzification",
      "Fuzzification rates",
      "Validation"
    ),
    Description = c(
      "Section 2.2 results for GFN coefficient-search fuzzy linear regression using Monte Carlo candidate vectors.",
      "Standardized observed yield is represented as Y_tilde = (mu = y_std, sigma^2 = (delta * |y_std|)^2).",
      "For each expanding-window test year, predictors and yield are standardized using only the training-window statistics. GFN predictions are defuzzified on the standardized scale and back-transformed to kg/ha before calculating MAE, RMSE, MAPE, and R2.",
      "Each coefficient is represented as beta_tilde = (mu_beta, sigma_beta^2).",
      "Coefficient means are generated within OLS-centered intervals. For coefficient j, sigma_beta_j is generated between 0 and the interval half-width h_j, and sigma_beta_j^2 is used as the GFN variance.",
      "The candidate GFN coefficient vector with minimum scalarized training GFN-MSE is selected.",
      "GFN predictions are transformed to crisp values using the Lemma 2 asymmetry-adjusted defuzzification rule. The alpha, kappa, and d parameters are tuned using the last three years of each training window.",
      "5%, 10%, and 15%",
      "Expanding window; test years 2015-2024."
    ),
    stringsAsFactors = FALSE
  )
)

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

addWorksheet(wb, "GFN_Coefficients")
writeData(wb, "GFN_Coefficients", coefficients)

addWorksheet(wb, "Candidate_Intervals")
writeData(wb, "Candidate_Intervals", interval_logs)

addWorksheet(wb, "Defuzz_Tuning")
writeData(wb, "Defuzz_Tuning", defuzz_tuning_rows)

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
  addStyle(
    wb,
    sheet_name,
    header_style,
    rows = 1,
    cols = 1:60,
    gridExpand = TRUE,
    stack = TRUE
  )
  setColWidths(wb, sheet_name, cols = 1:60, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("GFN-MC coefficient search completed.\n")
cat("Outputs written to:", mc_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n")
