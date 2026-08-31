rm(list = ls())

library(dplyr)
library(ggplot2)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
forecast_dir <- file.path(output_dir, "forecast_2025_2030_winning_models")
projection_path <- file.path(forecast_dir, "projection_crisp_all_crops_2025_2030.csv")

if (!file.exists(projection_path)) {
  stop("Projection table not found. Run 23_projection_tables_for_paper.R first: ", projection_path)
}

projection <- read.csv(projection_path, stringsAsFactors = FALSE)

plot_data <- projection %>%
  mutate(
    Model_short = case_when(
      Model == "GFN_MC_coefficient_search" ~ "GFN-MC",
      Model == "RandomForest" ~ "RF",
      TRUE ~ Model
    ),
    Crop_label = paste0(Crop, " (", Model_short, ")")
  ) %>%
  arrange(Crop, Year) %>%
  group_by(Crop) %>%
  mutate(
    Projection_index_2025 = Projected_yield_kg_ha / Projected_yield_kg_ha[Year == 2025][1] * 100
  ) %>%
  ungroup()

write.csv(
  plot_data,
  file.path(forecast_dir, "projection_only_plot_data.csv"),
  row.names = FALSE
)

crop_colors <- setNames(
  scales::hue_pal()(length(unique(plot_data$Crop_label))),
  sort(unique(plot_data$Crop_label))
)

p_faceted <- ggplot(
  plot_data,
  aes(x = Year, y = Projected_yield_kg_ha, color = Crop_label, group = Crop_label)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.8) +
  facet_wrap(~ Crop_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = crop_colors, guide = "none") +
  scale_x_continuous(breaks = 2025:2030) +
  labs(
    x = "Year",
    y = "Projected crop yield (kg/ha)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey70"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_indexed <- ggplot(
  plot_data,
  aes(x = Year, y = Projection_index_2025, color = Crop_label, group = Crop_label)
) +
  geom_hline(yintercept = 100, color = "grey55", linewidth = 0.35, linetype = "dashed") +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.8) +
  scale_color_manual(values = crop_colors) +
  scale_x_continuous(breaks = 2025:2030) +
  labs(
    x = "Year",
    y = "Projected yield index (2025 = 100)",
    color = "Crop (model)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.1, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE))

faceted_png <- file.path(forecast_dir, "projection_only_best_models_faceted.png")
faceted_pdf <- file.path(forecast_dir, "projection_only_best_models_faceted.pdf")
indexed_png <- file.path(forecast_dir, "projection_only_best_models_indexed.png")
indexed_pdf <- file.path(forecast_dir, "projection_only_best_models_indexed.pdf")

ggsave(faceted_png, p_faceted, width = 14, height = 9, dpi = 300)
ggsave(faceted_pdf, p_faceted, width = 14, height = 9)
ggsave(indexed_png, p_indexed, width = 11, height = 7.5, dpi = 300)
ggsave(indexed_pdf, p_indexed, width = 11, height = 7.5)

cat("Projection-only figures created:\n")
cat(faceted_png, "\n")
cat(faceted_pdf, "\n")
cat(indexed_png, "\n")
cat(indexed_pdf, "\n")
