rm(list = ls())

library(dplyr)
library(openxlsx)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
forecast_dir <- file.path(output_dir, "forecast_2025_2030_winning_models")
forecast_path <- file.path(forecast_dir, "winning_model_yield_forecasts_2025_2030.csv")

if (!file.exists(forecast_path)) {
  stop(
    "Forecast file not found. Run 22_forecast_2025_2030_winning_models_current.R first: ",
    forecast_path
  )
}

forecasts <- read.csv(forecast_path, stringsAsFactors = FALSE)

projection_crisp <- forecasts %>%
  transmute(
    Crop = crop,
    Year = year,
    Model_family = selected_family,
    Model = selected_model,
    Fuzzification = fuzzification_percent,
    Projected_yield_kg_ha = round(forecast_defuzz, 2)
  )

fuzzy_projection_components <- forecasts %>%
  filter(crop %in% c("Apple", "Barley")) %>%
  mutate(
    GFN_mu = ifelse(crop == "Apple", forecast_center, NA_real_),
    GFN_sigma2 = ifelse(crop == "Apple", forecast_sd^2, NA_real_),
    TFN_lower = ifelse(crop == "Barley", forecast_lower, NA_real_),
    TFN_center = ifelse(crop == "Barley", forecast_center, NA_real_),
    TFN_upper = ifelse(crop == "Barley", forecast_upper, NA_real_),
    Fuzzy_representation = case_when(
      crop == "Apple" ~ paste0(
        "GFN = (mu=", round(GFN_mu, 2),
        ", sigma^2=", round(GFN_sigma2, 2), ")"
      ),
      crop == "Barley" ~ paste0(
        "TFN = (", round(TFN_lower, 2), ", ",
        round(TFN_center, 2), ", ",
        round(TFN_upper, 2), ")"
      ),
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    Crop = crop,
    Year = year,
    Model = selected_model,
    Fuzzification = fuzzification_percent,
    Fuzzy_representation = Fuzzy_representation,
    GFN_mu = round(GFN_mu, 2),
    GFN_sigma2 = round(GFN_sigma2, 2),
    TFN_lower = round(TFN_lower, 2),
    TFN_center = round(TFN_center, 2),
    TFN_upper = round(TFN_upper, 2),
    Defuzzified_projection_kg_ha = round(forecast_defuzz, 2)
  )

write.csv(
  projection_crisp,
  file.path(forecast_dir, "projection_crisp_all_crops_2025_2030.csv"),
  row.names = FALSE
)

write.csv(
  fuzzy_projection_components,
  file.path(forecast_dir, "fuzzy_projection_components_apple_barley_2025_2030.csv"),
  row.names = FALSE
)

write.xlsx(
  list(
    Crisp_projection_all_crops = projection_crisp,
    Fuzzy_components_FLR_crops = fuzzy_projection_components
  ),
  file.path(forecast_dir, "projection_tables_for_paper.xlsx"),
  overwrite = TRUE
)

cat("Projection tables for paper created in:\n")
cat(forecast_dir, "\n\n")
cat("Crisp projection preview:\n")
print(head(projection_crisp, 12), row.names = FALSE)
cat("\nFuzzy component preview:\n")
print(fuzzy_projection_components, row.names = FALSE)
