rm(list = ls())

library(fuzzyreg)
library(glmnet)
library(randomForest)
library(e1071)
library(xgboost)
library(FNN)
library(nnet)
library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")
winners_path <- file.path(
  output_dir,
  "fuzzy_sections_vs_tuned_ml",
  "best_overall_model_by_crop_rmse.csv"
)

if (!file.exists(panel_path)) {
  stop("Panel dataset not found: ", panel_path)
}
if (!file.exists(winners_path)) {
  stop("Winner model file not found. Run 19_compare_fuzzy_sections_with_tuned_ml.R first.")
}

forecast_dir <- file.path(output_dir, "forecast_2025_2030_winning_models")
if (!dir.exists(forecast_dir)) dir.create(forecast_dir, recursive = TRUE)

panel <- read.csv(panel_path, stringsAsFactors = FALSE, check.names = FALSE)
winners <- read.csv(winners_path, stringsAsFactors = FALSE)

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
future_years <- 2025:2030
inner_validation_years <- 3
N_candidates <- 10000
N_candidates_tune <- 3000
chunk_size <- 1000
interval_multiplier <- 3
trend_alpha <- 0.05
recent_years_for_constant_forecast <- 5

model_formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

clip_future_predictor <- function(variable, value) {
  if (variable %in% c("Mean_NDVI", "Mean_EVI")) {
    return(pmin(pmax(value, 0), 1))
  }
  if (variable %in% c("Precipitation_mm", "ET_mm", "GDD", "HeatStress", "ColdStress")) {
    return(pmax(value, 0))
  }
  value
}

forecast_predictors_for_crop <- function(crop_data, future_years) {
  rows <- data.frame(
    crop = crop_data$crop[1],
    crop_faostat = crop_data$crop_faostat[1],
    year = future_years,
    stringsAsFactors = FALSE
  )
  method_rows <- list()
  method_counter <- 1

  for (pred in predictors) {
    trend_data <- data.frame(
      year = crop_data$year,
      value = crop_data[[pred]]
    )
    fit <- lm(value ~ year, data = trend_data)
    fit_summary <- summary(fit)
    trend_p_value <- coef(fit_summary)["year", "Pr(>|t|)"]
    trend_slope <- coef(fit)["year"]
    use_linear_trend <- is.finite(trend_p_value) && trend_p_value < trend_alpha

    if (use_linear_trend) {
      pred_values <- as.numeric(predict(
        fit,
        newdata = data.frame(year = future_years),
        interval = "prediction"
      )[, "fit"])
      method <- "linear trend extrapolation"
    } else {
      recent_values <- tail(trend_data$value[order(trend_data$year)], recent_years_for_constant_forecast)
      pred_values <- rep(mean(recent_values, na.rm = TRUE), length(future_years))
      method <- paste0("last ", recent_years_for_constant_forecast, "-year mean")
    }

    rows[[pred]] <- clip_future_predictor(pred, pred_values)
    method_rows[[method_counter]] <- data.frame(
      crop = crop_data$crop[1],
      predictor = pred,
      trend_slope = trend_slope,
      trend_p_value = trend_p_value,
      selected_forecast_method = method,
      trend_alpha = trend_alpha,
      recent_years_for_constant_forecast = recent_years_for_constant_forecast,
      stringsAsFactors = FALSE
    )
    method_counter <- method_counter + 1
  }

  rows$Tbase_C <- tail(crop_data$Tbase_C, 1)
  rows$Tupper_C <- tail(crop_data$Tupper_C, 1)
  list(
    future_predictors = rows,
    predictor_methods = do.call(rbind, method_rows)
  )
}

future_predictor_outputs <- lapply(split(panel, panel$crop), function(crop_data) {
    crop_data <- crop_data[order(crop_data$year), ]
    forecast_predictors_for_crop(crop_data, future_years)
})

future_predictors <- do.call(
  rbind,
  lapply(future_predictor_outputs, function(x) x$future_predictors)
)

future_predictor_methods <- do.call(
  rbind,
  lapply(future_predictor_outputs, function(x) x$predictor_methods)
)

mae <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))

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

back_transform_y <- function(pred_scaled, y_center, y_scale) {
  as.numeric(pred_scaled) * y_scale + y_center
}

scale_data_for_fuzzyreg <- function(train, test) {
  scaled <- scale_train_test_y(
    as.matrix(train[, predictors, drop = FALSE]),
    as.matrix(test[, predictors, drop = FALSE]),
    train[[response]]
  )
  train[, predictors] <- scaled$train
  test[, predictors] <- scaled$test
  train[[response]] <- scaled$train_y
  list(
    train = train,
    test = test,
    y_center = scaled$y_center,
    y_scale = scaled$y_scale
  )
}

back_transform_tfn <- function(pred, y_center, y_scale) {
  pred$pred_lower <- pred$pred_lower * y_scale + y_center
  pred$pred_center <- pred$pred_center * y_scale + y_center
  pred$pred_upper <- pred$pred_upper * y_scale + y_center
  pred$pred_defuzz <- pred$pred_defuzz * y_scale + y_center
  pred
}

fit_fuzzy_model <- function(train, method, fuzzification_rate) {
  train$yield_left_spread <- abs(train[[response]]) * fuzzification_rate
  train$yield_right_spread <- abs(train[[response]]) * fuzzification_rate

  if (method %in% c("FLAR", "PLR")) {
    call <- substitute(
      fuzzylm(
        FORMULA,
        data = train,
        method = METHOD,
        fuzzy.left.y = "yield_left_spread",
        fuzzy.right.y = "yield_right_spread"
      ),
      list(FORMULA = model_formula, METHOD = tolower(method))
    )
    eval(call)
  } else if (method == "PLRLS") {
    call <- substitute(
      fuzzylm(FORMULA, data = train, method = "plrls"),
      list(FORMULA = model_formula)
    )
    eval(call)
  } else {
    stop("Unknown fuzzyreg method: ", method)
  }
}

manual_fuzzy_predict <- function(fit, newdata) {
  coef_mat <- fit$coef
  coef_mat[, "left.spread"] <- pmax(0, coef_mat[, "left.spread"])
  coef_mat[, "right.spread"] <- pmax(0, coef_mat[, "right.spread"])

  X <- cbind("(Intercept)" = 1, newdata[, predictors, drop = FALSE])
  X <- as.matrix(X)
  pred_center <- as.numeric(X %*% coef_mat[rownames(coef_mat), "center"])

  pred_lower <- rep(0, nrow(X))
  pred_upper <- rep(0, nrow(X))

  for (j in seq_len(ncol(X))) {
    term_name <- colnames(X)[j]
    x <- X[, j]
    beta_center <- coef_mat[term_name, "center"]
    beta_left <- coef_mat[term_name, "left.spread"]
    beta_right <- coef_mat[term_name, "right.spread"]
    beta_lower <- beta_center - beta_left
    beta_upper <- beta_center + beta_right
    pred_lower <- pred_lower + ifelse(x >= 0, beta_lower * x, beta_upper * x)
    pred_upper <- pred_upper + ifelse(x >= 0, beta_upper * x, beta_lower * x)
  }

  data.frame(
    pred_lower = pred_lower,
    pred_center = pred_center,
    pred_upper = pred_upper,
    pred_defuzz = (pred_lower + pred_center + pred_upper) / 3
  )
}

split_inner_temporal <- function(train) {
  train <- train[order(train$year), ]
  n <- nrow(train)
  val_n <- min(inner_validation_years, max(1, floor(n * 0.25)))
  list(
    tune_train = train[1:(n - val_n), ],
    val = train[(n - val_n + 1):n, ]
  )
}

grid_for_model <- function(model_name, n_train) {
  if (model_name == "MLR") {
    list(list())
  } else if (model_name %in% c("Ridge", "Lasso")) {
    lapply(10^seq(-4, 4, length.out = 17), function(lambda) list(lambda = lambda))
  } else if (model_name == "ElasticNet") {
    grid <- expand.grid(
      alpha = c(0.25, 0.50, 0.75),
      lambda = 10^seq(-4, 4, length.out = 17)
    )
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))
  } else if (model_name == "RandomForest") {
    grid <- expand.grid(
      mtry = unique(pmax(1, pmin(length(predictors), c(2, 3, 5, length(predictors))))),
      nodesize = c(1, 3, 5),
      ntree = 500
    )
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))
  } else if (model_name == "SVR") {
    grid <- expand.grid(
      cost = c(0.1, 1, 10, 100),
      gamma = c(0.01, 0.05, 0.1),
      epsilon = c(0.01, 0.1, 0.2)
    )
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))
  } else if (model_name == "XGBoost") {
    grid <- expand.grid(
      nrounds = c(30, 60, 100),
      max_depth = c(1, 2, 3),
      eta = c(0.03, 0.05, 0.1),
      subsample = 0.9,
      colsample_bytree = 0.9
    )
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))
  } else if (model_name == "kNN") {
    k_values <- unique(pmax(1, pmin(n_train, c(1, 2, 3, 4, 5, 7, 9))))
    lapply(k_values, function(k) list(k = k))
  } else if (model_name == "MLP") {
    grid <- expand.grid(size = c(1, 3, 5), decay = c(0, 0.001, 0.01, 0.1))
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))
  } else {
    stop("Unknown ML model: ", model_name)
  }
}

predict_ml_with_params <- function(model_name, train, test, params) {
  train_x <- as.matrix(train[, predictors, drop = FALSE])
  test_x <- as.matrix(test[, predictors, drop = FALSE])
  train_y <- train[[response]]

  scaled <- scale_train_test_y(train_x, test_x, train_y)
  sx_train <- scaled$train
  sx_test <- scaled$test
  train_y_scaled <- scaled$train_y

  if (model_name == "MLR") {
    fit <- lm(y ~ ., data = data.frame(y = train_y_scaled, sx_train))
    pred_scaled <- as.numeric(predict(fit, newdata = data.frame(sx_test)))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "Ridge") {
    fit <- glmnet(sx_train, train_y_scaled, alpha = 0, lambda = params$lambda, standardize = FALSE)
    pred_scaled <- as.numeric(predict(fit, newx = sx_test, s = params$lambda))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "Lasso") {
    fit <- glmnet(sx_train, train_y_scaled, alpha = 1, lambda = params$lambda, standardize = FALSE)
    pred_scaled <- as.numeric(predict(fit, newx = sx_test, s = params$lambda))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "ElasticNet") {
    fit <- glmnet(
      sx_train,
      train_y_scaled,
      alpha = params$alpha,
      lambda = params$lambda,
      standardize = FALSE
    )
    pred_scaled <- as.numeric(predict(fit, newx = sx_test, s = params$lambda))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "RandomForest") {
    fit <- randomForest(
      y ~ .,
      data = data.frame(y = train_y_scaled, sx_train),
      ntree = params$ntree,
      mtry = params$mtry,
      nodesize = params$nodesize
    )
    pred_scaled <- as.numeric(predict(fit, newdata = data.frame(sx_test)))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "SVR") {
    fit <- svm(
      y ~ .,
      data = data.frame(y = train_y_scaled, sx_train),
      type = "eps-regression",
      kernel = "radial",
      cost = params$cost,
      gamma = params$gamma,
      epsilon = params$epsilon,
      scale = FALSE
    )
    pred_scaled <- as.numeric(predict(fit, newdata = data.frame(sx_test)))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "XGBoost") {
    fit <- xgb.train(
      data = xgb.DMatrix(data = sx_train, label = train_y_scaled),
      nrounds = params$nrounds,
      objective = "reg:squarederror",
      max_depth = params$max_depth,
      eta = params$eta,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      verbose = 0
    )
    pred_scaled <- as.numeric(predict(fit, newdata = sx_test))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "kNN") {
    pred_scaled <- as.numeric(knn.reg(train = sx_train, test = sx_test, y = train_y_scaled, k = params$k)$pred)
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else if (model_name == "MLP") {
    fit <- nnet(
      y ~ .,
      data = data.frame(y = train_y_scaled, sx_train),
      size = params$size,
      decay = params$decay,
      linout = TRUE,
      maxit = 1000,
      trace = FALSE,
      MaxNWts = 5000
    )
    pred_scaled <- as.numeric(predict(fit, newdata = data.frame(sx_test)))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)
  } else {
    stop("Unknown ML model: ", model_name)
  }
}

tune_ml_model <- function(model_name, train) {
  inner <- split_inner_temporal(train)
  grid <- grid_for_model(model_name, nrow(inner$tune_train))
  best_rmse <- Inf
  best_params <- grid[[1]]

  for (params in grid) {
    pred <- try(predict_ml_with_params(model_name, inner$tune_train, inner$val, params), silent = TRUE)
    if (!inherits(pred, "try-error")) {
      score <- rmse(inner$val[[response]], pred)
      if (is.finite(score) && score < best_rmse) {
        best_rmse <- score
        best_params <- params
      }
    }
  }

  list(params = best_params, inner_rmse = best_rmse)
}

make_gfn_response <- function(y, delta) {
  sigma <- pmax(delta * abs(y), 1e-8)
  cbind(mu = y, variance = sigma^2)
}

gfn_defuzz <- function(mu, variance, alpha, kappa, d_threshold) {
  sigma <- sqrt(pmax(variance, 1e-12))
  delta_abs <- abs(mu / sigma)
  adjustment <- alpha / (1 + exp(-kappa * (delta_abs - d_threshold))) * variance
  ifelse(delta_abs < d_threshold, mu + adjustment, mu)
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

generate_random_gfn_coefficients <- function(intervals, n_candidates) {
  q <- nrow(intervals)
  mu <- matrix(NA_real_, nrow = n_candidates, ncol = q)
  variance <- matrix(NA_real_, nrow = n_candidates, ncol = q)

  for (j in seq_len(q)) {
    mu[, j] <- runif(
      n_candidates,
      min = intervals$interval_lower[j],
      max = intervals$interval_upper[j]
    )
    sigma <- runif(
      n_candidates,
      min = 0,
      max = max(intervals$half_width[j], 1e-6)
    )
    variance[, j] <- sigma^2
  }

  list(mu = mu, variance = variance)
}

predict_gfn_chunk <- function(x_design, coeff_chunk) {
  list(
    mu = coeff_chunk$mu %*% t(x_design),
    variance = pmax(coeff_chunk$variance %*% t(x_design^2), 1e-12)
  )
}

gfn_msee_for_chunk <- function(y_gfn, pred) {
  diff_mu <- sweep(pred$mu, 2, y_gfn[, "mu"], "-")
  diff_variance <- sweep(pred$variance, 2, y_gfn[, "variance"], "+")
  squared_error_mu <- diff_mu^2
  squared_error_variance <- 2 * diff_variance * diff_mu^2 + diff_variance^2
  msee_mu <- rowMeans(squared_error_mu, na.rm = TRUE)
  msee_variance <- rowMeans(squared_error_variance, na.rm = TRUE)
  msee_mu + sqrt(pmax(msee_variance, 0))
}

select_best_gfn_candidate <- function(x_design, y_gfn, intervals, n_candidates, chunk_size) {
  best_score <- Inf
  best_mu <- NULL
  best_variance <- NULL
  generated <- 0

  while (generated < n_candidates) {
    current_n <- min(chunk_size, n_candidates - generated)
    coeff_chunk <- generate_random_gfn_coefficients(intervals, current_n)
    pred <- predict_gfn_chunk(x_design, coeff_chunk)
    scores <- gfn_msee_for_chunk(y_gfn, pred)
    local_best <- which.min(scores)

    if (
      length(local_best) == 1 &&
        is.finite(scores[local_best]) &&
        scores[local_best] < best_score
    ) {
      best_score <- scores[local_best]
      best_mu <- coeff_chunk$mu[local_best, ]
      best_variance <- coeff_chunk$variance[local_best, ]
    }

    generated <- generated + current_n
  }

  list(mu = best_mu, variance = best_variance, train_msee_score = best_score)
}

predict_gfn_one_candidate <- function(x_design, selected) {
  pred <- predict_gfn_chunk(
    x_design,
    list(
      mu = matrix(selected$mu, nrow = 1),
      variance = matrix(selected$variance, nrow = 1)
    )
  )
  cbind(
    pred_mu = as.numeric(pred$mu[1, ]),
    pred_variance = as.numeric(pred$variance[1, ])
  )
}

gfn_coeff_predict <- function(train, test, delta, n_candidates) {
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
  selected <- select_best_gfn_candidate(
    x_design = x_train_design,
    y_gfn = y_train_gfn,
    intervals = intervals,
    n_candidates = n_candidates,
    chunk_size = chunk_size
  )
  pred_std <- predict_gfn_one_candidate(x_test_design, selected)
  pred_mu <- pred_std[, "pred_mu"] * scaled$y_scale + scaled$y_center
  pred_variance <- pred_std[, "pred_variance"] * scaled$y_scale^2

  list(
    pred_mu = pred_mu,
    pred_variance = pred_variance,
    pred_sd = sqrt(pred_variance),
    pred_lower_95 = pmax(0, pred_mu - 1.96 * sqrt(pred_variance)),
    pred_upper_95 = pred_mu + 1.96 * sqrt(pred_variance),
    pred_mu_std = pred_std[, "pred_mu"],
    pred_variance_std = pred_std[, "pred_variance"],
    y_center = scaled$y_center,
    y_scale = scaled$y_scale,
    train_msee_score = selected$train_msee_score
  )
}

tune_gfn_defuzz_params <- function(train, delta, n_candidates_tune) {
  train <- train[order(train$year), ]
  candidate_years <- tail(unique(train$year), inner_validation_years)
  validation_rows <- list()
  counter <- 1

  for (validation_year in candidate_years) {
    inner_train <- train[train$year < validation_year, ]
    inner_validation <- train[train$year == validation_year, ]
    if (nrow(inner_train) <= length(predictors) + 1) next

    pred <- gfn_coeff_predict(inner_train, inner_validation, delta, n_candidates_tune)
    validation_rows[[counter]] <- data.frame(
      observed = (inner_validation[[response]] - pred$y_center) / pred$y_scale,
      pred_mu = pred$pred_mu_std,
      pred_variance = pred$pred_variance_std
    )
    counter <- counter + 1
  }

  if (length(validation_rows) == 0) {
    return(list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = NA_real_))
  }

  validation_df <- do.call(rbind, validation_rows)
  sigma <- sqrt(pmax(validation_df$pred_variance, 1e-12))
  delta_abs <- abs(validation_df$pred_mu / sigma)
  d_grid <- unique(as.numeric(quantile(delta_abs[is.finite(delta_abs)], c(0.25, 0.50, 0.75), na.rm = TRUE)))
  d_grid <- d_grid[is.finite(d_grid) & d_grid > 0]
  if (length(d_grid) == 0) d_grid <- Inf

  alpha_grid <- c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)
  kappa_grid <- c(0.5, 1, 2, 5)

  best <- list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = Inf)
  for (alpha in alpha_grid) {
    for (kappa in kappa_grid) {
      for (d_threshold in d_grid) {
        pred_defuzz <- gfn_defuzz(validation_df$pred_mu, validation_df$pred_variance, alpha, kappa, d_threshold)
        score <- rmse(validation_df$observed, pred_defuzz)
        if (is.finite(score) && score < best$inner_rmse) {
          best <- list(alpha = alpha, kappa = kappa, d_threshold = d_threshold, inner_rmse = score)
        }
      }
    }
  }

  if (!is.finite(best$inner_rmse)) {
    best <- list(alpha = 0, kappa = 1, d_threshold = Inf, inner_rmse = NA_real_)
  }
  best
}

forecast_rows <- list()
tuning_rows <- list()
counter <- 1
tuning_counter <- 1

set.seed(12311)

for (i in seq_len(nrow(winners))) {
  winner <- winners[i, ]
  crop_name <- winner$crop
  crop_train <- panel[panel$crop == crop_name, ]
  crop_train <- crop_train[order(crop_train$year), ]
  crop_future <- future_predictors[future_predictors$crop == crop_name, ]
  crop_future <- crop_future[order(crop_future$year), ]

  if (winner$section == "Section 1") {
    delta <- winner$fuzzification_rate
    method <- winner$model
    scaled <- scale_data_for_fuzzyreg(crop_train, crop_future)
    fit <- fit_fuzzy_model(scaled$train, method, delta)
    pred <- manual_fuzzy_predict(fit, scaled$test)
    pred <- back_transform_tfn(pred, scaled$y_center, scaled$y_scale)

    forecast_rows[[counter]] <- data.frame(
      crop = crop_name,
      year = crop_future$year,
      selected_family = winner$family,
      selected_section = winner$section,
      selected_model_family = winner$model_family,
      selected_model = method,
      fuzzification_rate = delta,
      fuzzification_percent = winner$fuzzification_percent,
      forecast_lower = pmax(0, pred$pred_lower),
      forecast_center = pmax(0, pred$pred_center),
      forecast_upper = pmax(0, pred$pred_upper),
      forecast_defuzz = pmax(0, pred$pred_defuzz),
      forecast_sd = NA_real_,
      forecast_lower_95 = NA_real_,
      forecast_upper_95 = NA_real_,
      predictor_forecast_method = "hybrid: significant linear trend or last five-year mean",
      stringsAsFactors = FALSE
    )
    counter <- counter + 1

    tuning_rows[[tuning_counter]] <- data.frame(
      crop = crop_name,
      model = method,
      fuzzification_rate = delta,
      alpha = NA_real_,
      kappa = NA_real_,
      d_threshold = NA_real_,
      inner_rmse = NA_real_,
      params = paste0("y_center=", round(scaled$y_center, 6), "; y_scale=", round(scaled$y_scale, 6)),
      stringsAsFactors = FALSE
    )
    tuning_counter <- tuning_counter + 1

  } else if (winner$section == "Section 2.2") {
    delta <- winner$fuzzification_rate
    defuzz_params <- tune_gfn_defuzz_params(crop_train, delta, N_candidates_tune)
    pred <- gfn_coeff_predict(crop_train, crop_future, delta, N_candidates)
    pred_defuzz_std <- gfn_defuzz(
      pred$pred_mu_std,
      pred$pred_variance_std,
      defuzz_params$alpha,
      defuzz_params$kappa,
      defuzz_params$d_threshold
    )
    pred_defuzz <- pred_defuzz_std * pred$y_scale + pred$y_center

    forecast_rows[[counter]] <- data.frame(
      crop = crop_name,
      year = crop_future$year,
      selected_family = winner$family,
      selected_section = winner$section,
      selected_model_family = winner$model_family,
      selected_model = winner$model,
      fuzzification_rate = delta,
      fuzzification_percent = winner$fuzzification_percent,
      forecast_lower = NA_real_,
      forecast_center = pmax(0, pred$pred_mu),
      forecast_upper = NA_real_,
      forecast_defuzz = pmax(0, pred_defuzz),
      forecast_sd = pred$pred_sd,
      forecast_lower_95 = pred$pred_lower_95,
      forecast_upper_95 = pred$pred_upper_95,
      predictor_forecast_method = "hybrid: significant linear trend or last five-year mean",
      stringsAsFactors = FALSE
    )
    counter <- counter + 1

    tuning_rows[[tuning_counter]] <- data.frame(
      crop = crop_name,
      model = winner$model,
      fuzzification_rate = delta,
      alpha = defuzz_params$alpha,
      kappa = defuzz_params$kappa,
      d_threshold = defuzz_params$d_threshold,
      inner_rmse = defuzz_params$inner_rmse,
      params = paste0(
        "N_candidates=", N_candidates,
        "; N_candidates_tune=", N_candidates_tune,
        "; train_msee_score=", round(pred$train_msee_score, 6)
      ),
      stringsAsFactors = FALSE
    )
    tuning_counter <- tuning_counter + 1

  } else if (winner$family == "Machine Learning") {
    model_name <- winner$model
    tuned <- tune_ml_model(model_name, crop_train)
    pred <- predict_ml_with_params(model_name, crop_train, crop_future, tuned$params)
    pred <- pmax(0, as.numeric(pred))

    forecast_rows[[counter]] <- data.frame(
      crop = crop_name,
      year = crop_future$year,
      selected_family = winner$family,
      selected_section = winner$section,
      selected_model_family = winner$model_family,
      selected_model = model_name,
      fuzzification_rate = NA_real_,
      fuzzification_percent = NA_character_,
      forecast_lower = NA_real_,
      forecast_center = pred,
      forecast_upper = NA_real_,
      forecast_defuzz = pred,
      forecast_sd = NA_real_,
      forecast_lower_95 = NA_real_,
      forecast_upper_95 = NA_real_,
      predictor_forecast_method = "hybrid: significant linear trend or last five-year mean",
      stringsAsFactors = FALSE
    )
    counter <- counter + 1

    tuning_rows[[tuning_counter]] <- data.frame(
      crop = crop_name,
      model = model_name,
      fuzzification_rate = NA_real_,
      alpha = NA_real_,
      kappa = NA_real_,
      d_threshold = NA_real_,
      inner_rmse = tuned$inner_rmse,
      params = paste(names(tuned$params), unlist(tuned$params), sep = "=", collapse = "; "),
      stringsAsFactors = FALSE
    )
    tuning_counter <- tuning_counter + 1

  } else {
    stop("Unsupported winning model for crop ", crop_name, ": ", winner$section, " / ", winner$model)
  }
}

forecasts <- do.call(rbind, forecast_rows)
tuning_log <- if (length(tuning_rows) > 0) do.call(rbind, tuning_rows) else data.frame()

write.csv(future_predictors, file.path(forecast_dir, "future_environmental_predictors_2025_2030.csv"), row.names = FALSE)
write.csv(future_predictor_methods, file.path(forecast_dir, "future_predictor_forecast_methods.csv"), row.names = FALSE)
write.csv(forecasts, file.path(forecast_dir, "winning_model_yield_forecasts_2025_2030.csv"), row.names = FALSE)
write.csv(winners, file.path(forecast_dir, "winning_models_used_for_forecast.csv"), row.names = FALSE)
write.csv(tuning_log, file.path(forecast_dir, "forecast_tuning_log.csv"), row.names = FALSE)

excel_path <- file.path(forecast_dir, "winning_model_yield_forecasts_2025_2030.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  data.frame(
    Item = c(
      "Purpose",
      "Forecast horizon",
      "Predictor forecast assumption",
      "Trend decision rule",
      "Model refitting",
      "Interpretation caution"
    ),
    Description = c(
      "Yield projections using the crop-specific winning model from the fuzzy sections vs tuned ML comparison.",
      "2025-2030",
      "Each environmental predictor is forecast crop-wise using a hybrid rule: significant linear trend extrapolation or the most recent five-year mean.",
      "A linear trend fitted over 2000-2024 is used when the slope p-value is below 0.05; otherwise, the average of the most recent five years is used as a constant forecast.",
      "Each winning model is refitted using all available 2000-2024 observations before generating 2025-2030 forecasts.",
      "These are trend-based scenario projections, not externally forced climate scenario forecasts."
    ),
    stringsAsFactors = FALSE
  )
)

addWorksheet(wb, "Forecasts_2025_2030")
writeData(wb, "Forecasts_2025_2030", forecasts)

addWorksheet(wb, "Future_Predictors")
writeData(wb, "Future_Predictors", future_predictors)

addWorksheet(wb, "Predictor_Methods")
writeData(wb, "Predictor_Methods", future_predictor_methods)

addWorksheet(wb, "Winning_Models")
writeData(wb, "Winning_Models", winners)

addWorksheet(wb, "Tuning_Log")
writeData(wb, "Tuning_Log", tuning_log)

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

cat("Winning-model yield forecasts completed.\n")
cat("Outputs written to:", forecast_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n\n")
cat("Forecast preview:\n")
print(forecasts[, c(
  "crop",
  "year",
  "selected_family",
  "selected_section",
  "selected_model",
  "fuzzification_percent",
  "forecast_defuzz",
  "forecast_lower",
  "forecast_upper",
  "forecast_lower_95",
  "forecast_upper_95"
)], row.names = FALSE)
