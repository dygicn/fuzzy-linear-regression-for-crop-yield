rm(list = ls())

library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")

comparison_dir <- file.path(output_dir, "all_fuzzy_sections_cropwise_comparison")
if (!dir.exists(comparison_dir)) dir.create(comparison_dir, recursive = TRUE)

metric_sources <- data.frame(
  section = c(
    "Section 1",
    "Section 2",
    "Section 2.2"
  ),
  model_family = c(
    "TFN fuzzyreg package",
    "TFN Monte Carlo linear",
    "GFN Monte Carlo coefficient search"
  ),
  path = c(
    file.path(
      output_dir,
      "explained_fuzzyreg_expanding_window",
      "explained_fuzzyreg_expanding_window_metrics.csv"
    ),
    file.path(
      output_dir,
      "TFN_MC_linear",
      "TFN_MC_linear_metrics.csv"
    ),
    file.path(
      output_dir,
      "GFN_MC_coefficient_search",
      "GFN_MC_coefficient_search_metrics.csv"
    )
  ),
  stringsAsFactors = FALSE
)

missing_files <- metric_sources$path[!file.exists(metric_sources$path)]
if (length(missing_files) > 0) {
  stop("Missing metric files:\n", paste(missing_files, collapse = "\n"))
}

standardize_metrics <- function(path, section, model_family) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$section <- section
  df$model_family <- model_family

  if (!"fuzzification_percent" %in% names(df)) {
    df$fuzzification_percent <- paste0(df$fuzzification_rate * 100, "%")
  }

  keep_cols <- c(
    "section",
    "model_family",
    "fuzzification_rate",
    "fuzzification_percent",
    "crop",
    "model",
    "n_test",
    "test_year_start",
    "test_year_end",
    "MAE",
    "RMSE",
    "MAPE",
    "R2"
  )

  optional_cols <- c(
    "mean_interval_width_95",
    "empirical_coverage_95",
    "mean_train_msee",
    "mean_train_e2",
    "mean_train_mapee",
    "mean_train_msee_mu",
    "mean_train_msee_variance",
    "mean_train_msee_score"
  )

  for (col in optional_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }

  df[, c(keep_cols, optional_cols)]
}

all_results <- do.call(
  rbind,
  Map(
    standardize_metrics,
    metric_sources$path,
    metric_sources$section,
    metric_sources$model_family
  )
)

all_results <- all_results[order(all_results$crop, all_results$RMSE), ]
best_by_crop_rmse <- all_results[!duplicated(all_results$crop), ]

best_by_crop_mae <- all_results[order(all_results$crop, all_results$MAE), ]
best_by_crop_mae <- best_by_crop_mae[!duplicated(best_by_crop_mae$crop), ]

best_by_crop_mape <- all_results[order(all_results$crop, all_results$MAPE), ]
best_by_crop_mape <- best_by_crop_mape[!duplicated(best_by_crop_mape$crop), ]

best_by_crop_by_rate <- all_results[
  order(all_results$crop, all_results$fuzzification_rate, all_results$RMSE),
]
best_by_crop_by_rate <- best_by_crop_by_rate[
  !duplicated(paste(best_by_crop_by_rate$crop, best_by_crop_by_rate$fuzzification_rate)),
]

rate_summary <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~
    section + model_family + model + fuzzification_rate + fuzzification_percent,
  all_results,
  mean,
  na.rm = TRUE
)
rate_summary <- rate_summary[order(rate_summary$RMSE), ]

winner_counts <- as.data.frame(table(
  best_by_crop_rmse$section,
  best_by_crop_rmse$model_family,
  best_by_crop_rmse$model,
  best_by_crop_rmse$fuzzification_percent
))
names(winner_counts) <- c(
  "section",
  "model_family",
  "model",
  "fuzzification_percent",
  "n_crop_wins"
)
winner_counts <- winner_counts[winner_counts$n_crop_wins > 0, ]
winner_counts <- winner_counts[order(-winner_counts$n_crop_wins), ]

write.csv(
  all_results,
  file.path(comparison_dir, "all_fuzzy_sections_all_model_metrics.csv"),
  row.names = FALSE
)
write.csv(
  best_by_crop_rmse,
  file.path(comparison_dir, "all_fuzzy_sections_best_by_crop_rmse.csv"),
  row.names = FALSE
)
write.csv(
  best_by_crop_by_rate,
  file.path(comparison_dir, "all_fuzzy_sections_best_by_crop_by_rate_rmse.csv"),
  row.names = FALSE
)
write.csv(
  rate_summary,
  file.path(comparison_dir, "all_fuzzy_sections_rate_summary.csv"),
  row.names = FALSE
)

excel_path <- file.path(comparison_dir, "all_fuzzy_sections_cropwise_best_models.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  data.frame(
    Item = c(
      "Purpose",
      "Compared sections",
      "Primary selection criterion",
      "Validation",
      "Defuzzification"
    ),
    Description = c(
      "Crop-wise comparison of all fuzzy linear regression sections.",
      paste(metric_sources$section, metric_sources$model_family, sep = ": ", collapse = " | "),
      "Minimum out-of-sample RMSE for each crop.",
      "Expanding-window test years 2015-2024.",
      "TFN predictions use COG; GFN predictions use the Gaussian fuzzy defuzzified value reported by each model."
    ),
    stringsAsFactors = FALSE
  )
)

addWorksheet(wb, "Best_By_Crop_RMSE")
writeData(wb, "Best_By_Crop_RMSE", best_by_crop_rmse)

addWorksheet(wb, "Best_By_Crop_MAE")
writeData(wb, "Best_By_Crop_MAE", best_by_crop_mae)

addWorksheet(wb, "Best_By_Crop_MAPE")
writeData(wb, "Best_By_Crop_MAPE", best_by_crop_mape)

addWorksheet(wb, "Best_By_Crop_By_Rate")
writeData(wb, "Best_By_Crop_By_Rate", best_by_crop_by_rate)

addWorksheet(wb, "All_Model_Metrics")
writeData(wb, "All_Model_Metrics", all_results)

addWorksheet(wb, "Rate_Summary")
writeData(wb, "Rate_Summary", rate_summary)

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
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:60, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_name, cols = 1:60, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("All fuzzy sections crop-wise comparison completed.\n")
cat("Outputs written to:", comparison_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n\n")
cat("Best crop-wise models by RMSE:\n")
print(best_by_crop_rmse[, c(
  "crop",
  "section",
  "model_family",
  "model",
  "fuzzification_percent",
  "MAE",
  "RMSE",
  "MAPE",
  "R2"
)])

cat("\nWinner counts:\n")
print(winner_counts)
