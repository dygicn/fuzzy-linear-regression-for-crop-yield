rm(list = ls())

library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")

comparison_dir <- file.path(output_dir, "fuzzy_sections_vs_tuned_ml")
if (!dir.exists(comparison_dir)) dir.create(comparison_dir, recursive = TRUE)

metric_sources <- data.frame(
  family = c(
    "Fuzzy",
    "Fuzzy",
    "Fuzzy",
    "Machine Learning"
  ),
  section = c(
    "Section 1",
    "Section 2",
    "Section 2.2",
    "Section 3"
  ),
  model_family = c(
    "TFN fuzzyreg package",
    "TFN Monte Carlo linear",
    "GFN Monte Carlo coefficient search",
    "Tuned machine learning"
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
    ),
    file.path(
      output_dir,
      "ml_models_tuned",
      "ml_tuned_expanding_window_metrics.csv"
    )
  ),
  stringsAsFactors = FALSE
)

missing_files <- metric_sources$path[!file.exists(metric_sources$path)]
if (length(missing_files) > 0) {
  stop("Missing metric files:\n", paste(missing_files, collapse = "\n"))
}

standardize_metrics <- function(path, family, section, model_family) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$family <- family
  df$section <- section
  df$model_family <- model_family

  if (!"fuzzification_rate" %in% names(df)) {
    df$fuzzification_rate <- NA_real_
  }
  if (!"fuzzification_percent" %in% names(df)) {
    df$fuzzification_percent <- ifelse(
      is.na(df$fuzzification_rate),
      NA_character_,
      paste0(df$fuzzification_rate * 100, "%")
    )
  }

  keep_cols <- c(
    "family",
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

  df[, keep_cols]
}

reset_rownames <- function(df) {
  row.names(df) <- NULL
  df
}

all_results <- do.call(
  rbind,
  Map(
    standardize_metrics,
    metric_sources$path,
    metric_sources$family,
    metric_sources$section,
    metric_sources$model_family
  )
)

# Tie-breaking is deterministic: RMSE is the primary criterion; if two rows
# are exactly tied, the lower MAE and then the lower MAPE are preferred.
all_results <- all_results[order(all_results$crop, all_results$RMSE, all_results$MAE, all_results$MAPE), ]
all_results <- reset_rownames(all_results)

best_overall_by_crop <- all_results[!duplicated(all_results$crop), ]
best_overall_by_crop <- reset_rownames(best_overall_by_crop)

fuzzy_results <- all_results[all_results$family == "Fuzzy", ]
ml_results <- all_results[all_results$family == "Machine Learning", ]

best_fuzzy_by_crop <- fuzzy_results[
  order(fuzzy_results$crop, fuzzy_results$RMSE, fuzzy_results$MAE, fuzzy_results$MAPE),
]
best_fuzzy_by_crop <- best_fuzzy_by_crop[!duplicated(best_fuzzy_by_crop$crop), ]
best_fuzzy_by_crop <- reset_rownames(best_fuzzy_by_crop)

best_ml_by_crop <- ml_results[
  order(ml_results$crop, ml_results$RMSE, ml_results$MAE, ml_results$MAPE),
]
best_ml_by_crop <- best_ml_by_crop[!duplicated(best_ml_by_crop$crop), ]
best_ml_by_crop <- reset_rownames(best_ml_by_crop)

fuzzy_vs_ml <- merge(
  best_fuzzy_by_crop,
  best_ml_by_crop,
  by = "crop",
  suffixes = c("_fuzzy", "_ml")
)

fuzzy_vs_ml$winner <- ifelse(
  fuzzy_vs_ml$RMSE_fuzzy <= fuzzy_vs_ml$RMSE_ml,
  "Fuzzy",
  "Machine Learning"
)
fuzzy_vs_ml$RMSE_difference_fuzzy_minus_ml <- fuzzy_vs_ml$RMSE_fuzzy - fuzzy_vs_ml$RMSE_ml
fuzzy_vs_ml$RMSE_percent_difference <- 100 *
  (fuzzy_vs_ml$RMSE_fuzzy - fuzzy_vs_ml$RMSE_ml) / fuzzy_vs_ml$RMSE_ml
fuzzy_vs_ml <- reset_rownames(fuzzy_vs_ml)

family_win_counts <- as.data.frame(table(best_overall_by_crop$family), stringsAsFactors = FALSE)
names(family_win_counts) <- c("family", "n_crop_wins")
family_win_counts <- family_win_counts[order(-family_win_counts$n_crop_wins), ]
family_win_counts <- reset_rownames(family_win_counts)

fuzzy_vs_ml_win_counts <- as.data.frame(table(fuzzy_vs_ml$winner), stringsAsFactors = FALSE)
names(fuzzy_vs_ml_win_counts) <- c("winner", "n_crop_wins")
fuzzy_vs_ml_win_counts <- fuzzy_vs_ml_win_counts[order(-fuzzy_vs_ml_win_counts$n_crop_wins), ]
fuzzy_vs_ml_win_counts <- reset_rownames(fuzzy_vs_ml_win_counts)

model_summary <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~ family + section + model_family + model + fuzzification_percent,
  all_results,
  mean,
  na.rm = TRUE
)
model_summary <- model_summary[order(model_summary$RMSE), ]
model_summary <- reset_rownames(model_summary)

section_summary <- aggregate(
  cbind(MAE, RMSE, MAPE, R2) ~ family + section + model_family + fuzzification_percent,
  all_results,
  mean,
  na.rm = TRUE
)
section_summary <- section_summary[order(section_summary$RMSE), ]
section_summary <- reset_rownames(section_summary)

# -------------------------------------------------------------------------
# Relative RMSE analysis for heatmap visualization
# -------------------------------------------------------------------------

all_results$minimum_crop_RMSE <- ave(
  all_results$RMSE,
  all_results$crop,
  FUN = function(x) min(x, na.rm = TRUE)
)
all_results$relative_RMSE <- all_results$RMSE / all_results$minimum_crop_RMSE
all_results$relative_RMSE_percent_above_best <- (all_results$relative_RMSE - 1) * 100

all_results$model_label <- ifelse(
  is.na(all_results$fuzzification_percent) | all_results$fuzzification_percent == "",
  all_results$model,
  paste0(all_results$model, " (", all_results$fuzzification_percent, ")")
)

relative_rmse <- all_results[
  order(all_results$crop, all_results$relative_RMSE, all_results$MAE, all_results$MAPE),
]
relative_rmse <- reset_rownames(relative_rmse)

top3_relative_rmse_by_crop <- do.call(
  rbind,
  lapply(split(relative_rmse, relative_rmse$crop), function(df) {
    df <- df[order(df$relative_RMSE, df$MAE, df$MAPE), ]
    head(df, 3)
  })
)
top3_relative_rmse_by_crop <- reset_rownames(top3_relative_rmse_by_crop)

write.csv(
  all_results,
  file.path(comparison_dir, "all_fuzzy_and_tuned_ml_metrics.csv"),
  row.names = FALSE
)
write.csv(
  best_overall_by_crop,
  file.path(comparison_dir, "best_overall_model_by_crop_rmse.csv"),
  row.names = FALSE
)
write.csv(
  fuzzy_vs_ml,
  file.path(comparison_dir, "best_fuzzy_vs_best_tuned_ml_by_crop.csv"),
  row.names = FALSE
)
write.csv(
  model_summary,
  file.path(comparison_dir, "model_average_performance.csv"),
  row.names = FALSE
)
write.csv(
  relative_rmse,
  file.path(comparison_dir, "relative_rmse_all_models.csv"),
  row.names = FALSE
)
write.csv(
  top3_relative_rmse_by_crop,
  file.path(comparison_dir, "top3_relative_rmse_by_crop.csv"),
  row.names = FALSE
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  crop_order <- unique(best_overall_by_crop$crop[order(best_overall_by_crop$crop)])
  model_order <- unique(relative_rmse$model_label[order(relative_rmse$family, relative_rmse$model_family, relative_rmse$model)])

  heatmap_data <- relative_rmse
  heatmap_data$crop <- factor(heatmap_data$crop, levels = crop_order)
  heatmap_data$model_label <- factor(heatmap_data$model_label, levels = rev(model_order))
  heatmap_data$relative_RMSE_capped <- pmin(heatmap_data$relative_RMSE, 3)

  heatmap_plot <- ggplot(
    heatmap_data,
    aes(x = crop, y = model_label, fill = relative_RMSE_capped)
  ) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_text(aes(label = sprintf("%.2f", relative_RMSE)), size = 2.4) +
    scale_fill_gradient(
      low = "#F7FCF5",
      high = "#006D2C",
      name = "Relative\nRMSE",
      limits = c(1, 3),
      breaks = c(1, 1.5, 2, 2.5, 3),
      labels = c("1.0", "1.5", "2.0", "2.5", ">=3.0")
    ) +
    labs(x = "Crop", y = "Model") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )

  ggsave(
    filename = file.path(comparison_dir, "relative_rmse_heatmap.png"),
    plot = heatmap_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
  ggsave(
    filename = file.path(comparison_dir, "relative_rmse_heatmap.pdf"),
    plot = heatmap_plot,
    width = 11,
    height = 7
  )
} else {
  warning("Package ggplot2 is not installed. Relative RMSE CSV files were written, but heatmap files were not created.")
}

excel_path <- file.path(comparison_dir, "fuzzy_sections_vs_tuned_ml_results.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb,
  "README",
  data.frame(
    Item = c(
      "Purpose",
      "Fuzzy sections",
      "ML section",
      "Primary criterion",
      "Validation",
      "Fairness note"
    ),
    Description = c(
      "Comparison of crop-wise fuzzy linear regression models with tuned machine-learning benchmarks.",
      "Section 1: TFN fuzzyreg package; Section 2: TFN-MC linear; Section 2.2: GFN-MC coefficient search.",
      "Section 3: MLR, Random Forest, SVR, XGBoost, kNN, and MLP with inner temporal hyperparameter tuning.",
      "Minimum out-of-sample RMSE for each crop.",
      "All methods use expanding-window test years 2015-2024.",
      "ML hyperparameters are selected only from training data using the last three training years as inner validation."
    ),
    stringsAsFactors = FALSE
  )
)

addWorksheet(wb, "Best_Overall_By_Crop")
writeData(wb, "Best_Overall_By_Crop", best_overall_by_crop)

addWorksheet(wb, "Best_Fuzzy_vs_Best_ML")
writeData(wb, "Best_Fuzzy_vs_Best_ML", fuzzy_vs_ml)

addWorksheet(wb, "Best_Fuzzy_By_Crop")
writeData(wb, "Best_Fuzzy_By_Crop", best_fuzzy_by_crop)

addWorksheet(wb, "Best_ML_By_Crop")
writeData(wb, "Best_ML_By_Crop", best_ml_by_crop)

addWorksheet(wb, "All_Model_Metrics")
writeData(wb, "All_Model_Metrics", all_results)

addWorksheet(wb, "Model_Summary")
writeData(wb, "Model_Summary", model_summary)

addWorksheet(wb, "Section_Summary")
writeData(wb, "Section_Summary", section_summary)

addWorksheet(wb, "Relative_RMSE")
writeData(wb, "Relative_RMSE", relative_rmse)

addWorksheet(wb, "Top3_Relative_RMSE")
writeData(wb, "Top3_Relative_RMSE", top3_relative_rmse_by_crop)

addWorksheet(wb, "Winner_Counts")
writeData(wb, "Winner_Counts", family_win_counts)
writeData(wb, "Winner_Counts", fuzzy_vs_ml_win_counts, startCol = 4)

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
    cols = 1:80,
    gridExpand = TRUE,
    stack = TRUE
  )
  setColWidths(wb, sheet_name, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("Fuzzy sections vs tuned ML comparison completed.\n")
cat("Outputs written to:", comparison_dir, "\n")
cat("Excel workbook written to:", excel_path, "\n\n")
cat("Best overall model by crop:\n")
print(best_overall_by_crop[, c(
  "crop",
  "family",
  "section",
  "model_family",
  "model",
  "fuzzification_percent",
  "MAE",
  "RMSE",
  "MAPE",
  "R2"
)])

cat("\nBest fuzzy vs best tuned ML by crop:\n")
print(fuzzy_vs_ml[, c(
  "crop",
  "section_fuzzy",
  "model_family_fuzzy",
  "model_fuzzy",
  "fuzzification_percent_fuzzy",
  "RMSE_fuzzy",
  "model_ml",
  "RMSE_ml",
  "winner",
  "RMSE_difference_fuzzy_minus_ml",
  "RMSE_percent_difference"
)])

cat("\nWinner counts:\n")
print(family_win_counts)
print(fuzzy_vs_ml_win_counts)
