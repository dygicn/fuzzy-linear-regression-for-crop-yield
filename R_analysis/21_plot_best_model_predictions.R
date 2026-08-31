rm(list = ls())

library(ggplot2)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
plot_dir <- file.path(output_dir, "fuzzy_sections_vs_tuned_ml")

best_path <- file.path(
  plot_dir,
  "best_overall_model_by_crop_rmse.csv"
)

ml_pred_path <- file.path(
  output_dir,
  "ml_models_tuned",
  "ml_tuned_expanding_window_predictions.csv"
)

fuzzyreg_pred_path <- file.path(
  output_dir,
  "explained_fuzzyreg_expanding_window",
  "explained_fuzzyreg_expanding_window_predictions.csv"
)

tfn_mc_pred_path <- file.path(
  output_dir,
  "TFN_MC_linear",
  "TFN_MC_linear_predictions.csv"
)

gfn_mc_pred_path <- file.path(
  output_dir,
  "GFN_MC_coefficient_search",
  "GFN_MC_coefficient_search_predictions.csv"
)

required_files <- c(
  best_path,
  ml_pred_path,
  fuzzyreg_pred_path,
  tfn_mc_pred_path,
  gfn_mc_pred_path
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

best_models <- read.csv(best_path, stringsAsFactors = FALSE)

crop_order <- c(
  "Apple",
  "Barley",
  "Bean",
  "Grape",
  "Maize",
  "Olive",
  "Potatoes",
  "Rice",
  "Sugar_Beet",
  "Tomatoes",
  "Wheat"
)

pretty_crop <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("Sugar Beet", "Sugar beet", x)
  x
}

short_model_label <- function(model) {
  ifelse(
    model == "GFN_MC_coefficient_search",
    "GFN-MC",
    ifelse(
      model == "TFN_MC_linear",
      "TFN-MC",
      ifelse(model == "RandomForest", "RF", model)
    )
  )
}

make_panel_label <- function(crop, model) {
  paste0(pretty_crop(crop), " (", short_model_label(model), ")")
}

standardize_ml_predictions <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    family = "Machine Learning",
    model_family = "Tuned machine learning",
    fuzzification_rate = NA_real_,
    fuzzification_percent = NA_character_,
    crop = df$crop,
    model = df$model,
    year = df$year,
    observed = df$observed,
    predicted = df$predicted,
    pred_lower = NA_real_,
    pred_upper = NA_real_,
    fit_status = df$fit_status,
    stringsAsFactors = FALSE
  )
}

standardize_fuzzyreg_predictions <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    family = "Fuzzy",
    model_family = "TFN fuzzyreg package",
    fuzzification_rate = df$fuzzification_rate,
    fuzzification_percent = df$fuzzification_percent,
    crop = df$crop,
    model = df$model,
    year = df$test_year,
    observed = df$observed,
    predicted = df$pred_defuzz_cog,
    pred_lower = df$pred_lower,
    pred_upper = df$pred_upper,
    fit_status = df$fit_status,
    stringsAsFactors = FALSE
  )
}

standardize_tfn_mc_predictions <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    family = "Fuzzy",
    model_family = "TFN Monte Carlo linear",
    fuzzification_rate = df$fuzzification_rate,
    fuzzification_percent = df$fuzzification_percent,
    crop = df$crop,
    model = df$model,
    year = df$year,
    observed = df$observed,
    predicted = df$pred_defuzz_cog,
    pred_lower = df$pred_lower,
    pred_upper = df$pred_upper,
    fit_status = df$fit_status,
    stringsAsFactors = FALSE
  )
}

standardize_gfn_mc_predictions <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    family = "Fuzzy",
    model_family = "GFN Monte Carlo coefficient search",
    fuzzification_rate = df$fuzzification_rate,
    fuzzification_percent = df$fuzzification_percent,
    crop = df$crop,
    model = df$model,
    year = df$year,
    observed = df$observed,
    predicted = df$pred_defuzz_gfn,
    pred_lower = df$pred_lower_95,
    pred_upper = df$pred_upper_95,
    fit_status = df$fit_status,
    stringsAsFactors = FALSE
  )
}

all_predictions <- rbind(
  standardize_ml_predictions(ml_pred_path),
  standardize_fuzzyreg_predictions(fuzzyreg_pred_path),
  standardize_tfn_mc_predictions(tfn_mc_pred_path),
  standardize_gfn_mc_predictions(gfn_mc_pred_path)
)

make_match_key <- function(df) {
  paste(
    ifelse(is.na(df$family), "NA", df$family),
    ifelse(is.na(df$model_family), "NA", df$model_family),
    ifelse(is.na(df$fuzzification_rate), "NA", as.character(df$fuzzification_rate)),
    ifelse(is.na(df$fuzzification_percent), "NA", df$fuzzification_percent),
    ifelse(is.na(df$crop), "NA", df$crop),
    ifelse(is.na(df$model), "NA", df$model),
    sep = "||"
  )
}

best_models$match_key <- make_match_key(best_models)
all_predictions$match_key <- make_match_key(all_predictions)

best_predictions <- all_predictions[
  all_predictions$match_key %in% best_models$match_key,
]

best_predictions <- best_predictions[best_predictions$fit_status == "ok", ]
best_predictions$crop <- factor(best_predictions$crop, levels = crop_order)
best_predictions$panel_label <- make_panel_label(
  as.character(best_predictions$crop),
  best_predictions$model
)
panel_levels <- unique(best_predictions[order(best_predictions$crop), "panel_label"])
best_predictions$panel_label <- factor(best_predictions$panel_label, levels = panel_levels)

write.csv(
  best_predictions,
  file.path(plot_dir, "best_model_observed_predicted_plot_data.csv"),
  row.names = FALSE
)

observed_predicted_plot <- ggplot(best_predictions, aes(x = year)) +
  geom_line(aes(y = observed, colour = "Observed"), linewidth = 0.7, linetype = "dashed") +
  geom_point(aes(y = observed, colour = "Observed"), size = 1.3) +
  geom_line(aes(y = predicted, colour = "Predicted"), linewidth = 0.8) +
  geom_point(aes(y = predicted, colour = "Predicted"), size = 1.3) +
  facet_wrap(~ panel_label, scales = "free_y", ncol = 4) +
  scale_colour_manual(
    values = c("Observed" = "black", "Predicted" = "#0072B2"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 2)) +
  labs(
    x = "Year",
    y = "Crop yield (kg/ha)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(plot_dir, "observed_predicted_best_models.png"),
  observed_predicted_plot,
  width = 11,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(plot_dir, "observed_predicted_best_models.pdf"),
  observed_predicted_plot,
  width = 11,
  height = 8
)

fuzzy_bounds <- best_predictions[
  best_predictions$family == "Fuzzy" &
    best_predictions$crop %in% c("Apple", "Barley") &
    !is.na(best_predictions$pred_lower) &
    !is.na(best_predictions$pred_upper),
]

fuzzy_bounds$crop_label <- pretty_crop(as.character(fuzzy_bounds$crop))
fuzzy_bounds$crop_label <- factor(fuzzy_bounds$crop_label, levels = c("Apple", "Barley"))

write.csv(
  fuzzy_bounds,
  file.path(plot_dir, "fuzzy_prediction_bounds_apple_barley_plot_data.csv"),
  row.names = FALSE
)

fuzzy_bounds_plot <- ggplot(fuzzy_bounds, aes(x = year)) +
  geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "#56B4E9", alpha = 0.22) +
  geom_line(aes(y = observed, colour = "Observed"), linewidth = 0.75, linetype = "dashed") +
  geom_point(aes(y = observed, colour = "Observed"), size = 1.5) +
  geom_line(aes(y = predicted, colour = "Defuzzified prediction"), linewidth = 0.85) +
  geom_point(aes(y = predicted, colour = "Defuzzified prediction"), size = 1.5) +
  facet_wrap(~ crop_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c(
      "Observed" = "black",
      "Defuzzified prediction" = "#0072B2"
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 2)) +
  labs(
    x = "Year",
    y = "Crop yield (kg/ha)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plot_dir, "fuzzy_prediction_bounds_apple_barley.png"),
  fuzzy_bounds_plot,
  width = 8,
  height = 4.6,
  dpi = 300
)

ggsave(
  file.path(plot_dir, "fuzzy_prediction_bounds_apple_barley.pdf"),
  fuzzy_bounds_plot,
  width = 8,
  height = 4.6
)

cat("Best-model prediction plots completed.\n")
cat("Outputs written to:", plot_dir, "\n")
cat("Created files:\n")
cat("- observed_predicted_best_models.png/.pdf\n")
cat("- fuzzy_prediction_bounds_apple_barley.png/.pdf\n")
cat("- best_model_observed_predicted_plot_data.csv\n")
cat("- fuzzy_prediction_bounds_apple_barley_plot_data.csv\n")
