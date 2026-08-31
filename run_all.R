scripts <- c(
  "00_build_panel_dataset.R",
  "01_data_quality_summary.R",
  "13_explained_cropwise_fuzzyreg_expanding_window.R",
  "14_TFN_MC_linear_section2.R",
  "17_GFN_MC_coefficient_search_section2_2.R",
  "08_ml_benchmark_models_tuned.R",
  "18_compare_all_fuzzy_sections_cropwise_best.R",
  "20_TFN_based_FLR_WES_comparison.R",
  "19_compare_fuzzy_sections_with_tuned_ml.R",
  "21_plot_best_model_predictions.R",
  "22_forecast_2025_2030_winning_models.R",
  "23_projection_tables_for_paper.R",
  "24_plot_future_projection_best_models.R",
  "25_plot_projection_only_best_models.R",
  "26_plot_fuzzy_projection_number_representations.R",
  "27_plot_fuzzy_projection_labels_over_time.R",
  "28_plot_barley_tfn_by_year_rotated.R",
  "29_plot_apple_gfn_by_year_rotated.R",
  "30_plot_projection_only_ml_models.R"
)

for (script in scripts) {
  message("Running ", script)
  source(file.path("R_analysis", script), local = new.env(parent = globalenv()))
}

message("Analysis completed. Results are available in the output directory.")
