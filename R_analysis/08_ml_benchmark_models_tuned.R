# Tuned machine learning benchmark analysis
# Models: MLR, Random Forest, SVR, XGBoost, kNN, MLP
# Validation: expanding-window by crop
# Hyperparameter tuning: inner temporal validation using the last 3 years
# of the training window.
#
# For comparability with fuzzy Monte Carlo models, predictors and yield are
# standardized inside each training window. All predictions are back-
# transformed to kg/ha before MAE, RMSE, MAPE, and R2 are computed.

rm(list = ls())

library(randomForest)
library(e1071)
library(xgboost)
library(FNN)
library(nnet)
library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found. Run 00_build_panel_dataset.R first.")
}

ml_dir <- file.path(output_dir, "ml_models_tuned")
if (!dir.exists(ml_dir)) dir.create(ml_dir, recursive = TRUE)

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
min_train_years <- 15
inner_validation_years <- 3

ml_models <- c(
  "MLR",
  "RandomForest",
  "SVR",
  "XGBoost",
  "kNN",
  "MLP"
)

mae <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mape <- function(obs, pred) mean(abs((obs - pred) / obs), na.rm = TRUE) * 100
r2_score <- function(obs, pred) {
  1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
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

back_transform_y <- function(pred_scaled, y_center, y_scale) {
  as.numeric(pred_scaled) * y_scale + y_center
}

split_inner_temporal <- function(train) {
  train <- train[order(train$year), ]
  n <- nrow(train)
  val_n <- min(inner_validation_years, max(1, floor(n * 0.25)))
  tune_train <- train[1:(n - val_n), ]
  val <- train[(n - val_n + 1):n, ]
  list(tune_train = tune_train, val = val)
}

predict_with_params <- function(model_name, train, test, params) {
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

  } else if (model_name == "RandomForest") {
    train_df <- data.frame(y = train_y_scaled, sx_train)
    test_df <- data.frame(sx_test)
    fit <- randomForest(
      y ~ .,
      data = train_df,
      ntree = params$ntree,
      mtry = params$mtry,
      nodesize = params$nodesize
    )
    pred_scaled <- as.numeric(predict(fit, newdata = test_df))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)

  } else if (model_name == "SVR") {
    train_df <- data.frame(y = train_y_scaled, sx_train)
    test_df <- data.frame(sx_test)
    fit <- svm(
      y ~ .,
      data = train_df,
      type = "eps-regression",
      kernel = "radial",
      cost = params$cost,
      gamma = params$gamma,
      epsilon = params$epsilon,
      scale = FALSE
    )
    pred_scaled <- as.numeric(predict(fit, newdata = test_df))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)

  } else if (model_name == "XGBoost") {
    dtrain <- xgb.DMatrix(data = sx_train, label = train_y_scaled)
    fit <- xgb.train(
      data = dtrain,
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
    train_df <- data.frame(y = train_y_scaled, sx_train)
    test_df <- data.frame(sx_test)
    fit <- nnet(
      y ~ .,
      data = train_df,
      size = params$size,
      decay = params$decay,
      linout = TRUE,
      maxit = 1000,
      trace = FALSE,
      MaxNWts = 5000
    )
    pred_scaled <- as.numeric(predict(fit, newdata = test_df))
    back_transform_y(pred_scaled, scaled$y_center, scaled$y_scale)

  } else {
    stop(paste("Unknown ML model:", model_name))
  }
}

grid_for_model <- function(model_name, n_train) {
  if (model_name == "MLR") {
    list(list())

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
    grid <- expand.grid(
      size = c(1, 3, 5),
      decay = c(0, 0.001, 0.01, 0.1)
    )
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, ]))

  } else {
    stop(paste("Unknown ML model:", model_name))
  }
}

tune_model <- function(model_name, train) {
  inner <- split_inner_temporal(train)
  tune_train <- inner$tune_train
  val <- inner$val
  grid <- grid_for_model(model_name, nrow(tune_train))

  best_rmse <- Inf
  best_params <- grid[[1]]

  for (params in grid) {
    pred <- try(predict_with_params(model_name, tune_train, val, params), silent = TRUE)
    if (!inherits(pred, "try-error")) {
      score <- rmse(val[[response]], pred)
      if (!is.na(score) && score < best_rmse) {
        best_rmse <- score
        best_params <- params
      }
    }
  }

  list(params = best_params, inner_rmse = best_rmse)
}

predictions <- list()
tuning_log <- list()
scaling_log <- list()
counter <- 1
log_counter <- 1
scaling_counter <- 1

set.seed(123)

for (crop_name in sort(unique(panel$crop))) {
  crop_data <- panel[panel$crop == crop_name, ]
  crop_data <- crop_data[order(crop_data$year), ]
  years <- crop_data$year
  test_years <- years[(min_train_years + 1):length(years)]

  for (model_name in ml_models) {
    for (test_year in test_years) {
      train <- crop_data[crop_data$year < test_year, ]
      test <- crop_data[crop_data$year == test_year, ]

      tuned <- try(tune_model(model_name, train), silent = TRUE)

      if (inherits(tuned, "try-error")) {
        pred <- NA_real_
        fit_status <- "failed_tuning"
        params <- list()
        inner_rmse <- NA_real_
      } else {
        params <- tuned$params
        inner_rmse <- tuned$inner_rmse
        pred_try <- try(predict_with_params(model_name, train, test, params), silent = TRUE)
        if (inherits(pred_try, "try-error")) {
          pred <- NA_real_
          fit_status <- "failed_prediction"
        } else {
          pred <- pred_try
          fit_status <- "ok"
        }
      }

      predictions[[counter]] <- data.frame(
        crop = crop_name,
        year = test_year,
        model = model_name,
        observed = test[[response]],
        predicted = as.numeric(pred),
        fit_status = fit_status,
        stringsAsFactors = FALSE
      )

      tuning_log[[log_counter]] <- data.frame(
        crop = crop_name,
        test_year = test_year,
        model = model_name,
        inner_rmse = inner_rmse,
        params = paste(names(params), unlist(params), sep = "=", collapse = "; "),
        fit_status = fit_status,
        stringsAsFactors = FALSE
      )

      train_x_for_log <- as.matrix(train[, predictors, drop = FALSE])
      train_y_for_log <- train[[response]]
      x_center_for_log <- apply(train_x_for_log, 2, mean)
      x_scale_for_log <- apply(train_x_for_log, 2, sd)
      x_scale_for_log[x_scale_for_log == 0 | is.na(x_scale_for_log)] <- 1
      y_center_for_log <- mean(train_y_for_log, na.rm = TRUE)
      y_scale_for_log <- sd(train_y_for_log, na.rm = TRUE)
      if (is.na(y_scale_for_log) || y_scale_for_log == 0) y_scale_for_log <- 1
      scaling_log[[scaling_counter]] <- data.frame(
        crop = crop_name,
        test_year = test_year,
        model = model_name,
        predictor = predictors,
        x_center = as.numeric(x_center_for_log),
        x_scale = as.numeric(x_scale_for_log),
        y_center = y_center_for_log,
        y_scale = y_scale_for_log,
        stringsAsFactors = FALSE
      )

      counter <- counter + 1
      log_counter <- log_counter + 1
      scaling_counter <- scaling_counter + 1
    }
  }
}

predictions <- do.call(rbind, predictions)
tuning_log <- do.call(rbind, tuning_log)
scaling_log <- do.call(rbind, scaling_log)

metrics <- do.call(
  rbind,
  lapply(split(predictions, list(predictions$crop, predictions$model), drop = TRUE), function(df) {
    ok <- !is.na(df$predicted)
    data.frame(
      crop = df$crop[1],
      model = df$model[1],
      n_test = sum(ok),
      test_year_start = min(df$year),
      test_year_end = max(df$year),
      MAE = mae(df$observed[ok], df$predicted[ok]),
      RMSE = rmse(df$observed[ok], df$predicted[ok]),
      MAPE = mape(df$observed[ok], df$predicted[ok]),
      R2 = r2_score(df$observed[ok], df$predicted[ok]),
      stringsAsFactors = FALSE
    )
  })
)

best_model_by_crop <- metrics[order(metrics$crop, metrics$RMSE, metrics$MAE, metrics$MAPE), ]
best_model_by_crop <- best_model_by_crop[!duplicated(best_model_by_crop$crop), ]

model_summary <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~ model,
  metrics,
  mean,
  na.rm = TRUE
)
model_summary <- model_summary[order(model_summary$RMSE), ]

write.csv(predictions, file.path(ml_dir, "ml_tuned_expanding_window_predictions.csv"), row.names = FALSE)
write.csv(metrics, file.path(ml_dir, "ml_tuned_expanding_window_metrics.csv"), row.names = FALSE)
write.csv(tuning_log, file.path(ml_dir, "ml_tuned_hyperparameter_log.csv"), row.names = FALSE)
write.csv(scaling_log, file.path(ml_dir, "ml_tuned_scaling_log.csv"), row.names = FALSE)
write.csv(best_model_by_crop, file.path(ml_dir, "ml_tuned_best_model_by_crop_rmse.csv"), row.names = FALSE)
write.csv(model_summary, file.path(ml_dir, "ml_tuned_model_summary.csv"), row.names = FALSE)

excel_path <- file.path(ml_dir, "ml_tuned_results.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(wb, "README", data.frame(
  Item = c(
    "Purpose",
    "Models",
    "Validation",
    "Hyperparameter tuning",
    "Standardization",
    "Primary criterion",
    "Outputs"
  ),
  Description = c(
    "Crop-wise tuned machine-learning benchmark results.",
    "MLR, Random Forest, SVR, XGBoost, kNN, and MLP.",
    "Expanding-window test years 2015-2024.",
    "For each crop-test-year, hyperparameters are selected using the last three available training years as inner temporal validation.",
    "For each crop-test-year and inner validation split, predictors and yield are standardized using only the training data. Model predictions are back-transformed to kg/ha before error metrics are calculated.",
    "Minimum out-of-sample RMSE for crop-wise model selection; MAE, MAPE, and R2 are also reported.",
    "Predictions, all metrics, best model by crop, model summary, and hyperparameter log."
  ),
  stringsAsFactors = FALSE
))

addWorksheet(wb, "Best_By_Crop")
writeData(wb, "Best_By_Crop", best_model_by_crop)

addWorksheet(wb, "All_Metrics")
writeData(wb, "All_Metrics", metrics)

addWorksheet(wb, "Model_Summary")
writeData(wb, "Model_Summary", model_summary)

addWorksheet(wb, "All_Predictions")
writeData(wb, "All_Predictions", predictions)

addWorksheet(wb, "Hyperparameter_Log")
writeData(wb, "Hyperparameter_Log", tuning_log)

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
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:80, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("Tuned ML benchmark analysis completed.\n")
cat("Outputs written to:", ml_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n")
