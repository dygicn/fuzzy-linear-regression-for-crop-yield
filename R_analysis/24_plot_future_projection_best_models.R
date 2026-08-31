rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
forecast_dir <- file.path(output_dir, "forecast_2025_2030_winning_models")

panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")
projection_path <- file.path(forecast_dir, "projection_crisp_all_crops_2025_2030.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found: ", panel_path)
}
if (!file.exists(projection_path)) {
  stop("Projection table not found. Run 23_projection_tables_for_paper.R first: ", projection_path)
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE)
projection <- read.csv(projection_path, stringsAsFactors = FALSE)

model_labels <- projection %>%
  distinct(Crop, Model) %>%
  mutate(
    Model_short = case_when(
      Model == "GFN_MC_coefficient_search" ~ "GFN-MC",
      Model == "RandomForest" ~ "RF",
      TRUE ~ Model
    ),
    facet_label = paste0(Crop, " (", Model_short, ")")
  )

observed_plot <- panel %>%
  transmute(
    Crop = crop,
    Year = year,
    Yield = yield_kg_ha,
    Series = "Observed"
  ) %>%
  left_join(model_labels, by = "Crop")

projection_plot <- projection %>%
  transmute(
    Crop,
    Year,
    Yield = Projected_yield_kg_ha,
    Series = "Projected"
  ) %>%
  left_join(model_labels, by = "Crop")

plot_data <- bind_rows(observed_plot, projection_plot) %>%
  mutate(
    Series = factor(Series, levels = c("Observed", "Projected")),
    facet_label = factor(facet_label, levels = model_labels$facet_label)
  ) %>%
  arrange(Crop, Year, Series)

write.csv(
  plot_data,
  file.path(forecast_dir, "future_projection_plot_data.csv"),
  row.names = FALSE
)

p <- ggplot(plot_data, aes(x = Year, y = Yield, color = Series, linetype = Series)) +
  geom_vline(xintercept = 2024.5, color = "grey45", linewidth = 0.35, linetype = "dashed") +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.35) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c("Observed" = "black", "Projected" = "#0072B2")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "solid")) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2025, 2030),
    limits = c(2000, 2030)
  ) +
  labs(
    x = "Year",
    y = "Crop yield (kg/ha)",
    color = NULL,
    linetype = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey70"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

png_path <- file.path(forecast_dir, "future_projection_best_models.png")
pdf_path <- file.path(forecast_dir, "future_projection_best_models.pdf")

ggsave(png_path, p, width = 14, height = 10, dpi = 300)
ggsave(pdf_path, p, width = 14, height = 10)

cat("Future projection figure created:\n")
cat(png_path, "\n")
cat(pdf_path, "\n")
