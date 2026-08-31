rm(list = ls())

library(dplyr)
library(ggplot2)

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
forecast_dir <- file.path(output_dir, "forecast_2025_2030_winning_models")
fuzzy_path <- file.path(forecast_dir, "fuzzy_projection_components_apple_barley_2025_2030.csv")

if (!file.exists(fuzzy_path)) {
  stop("Fuzzy projection component file not found. Run 23_projection_tables_for_paper.R first: ", fuzzy_path)
}

fuzzy_components <- read.csv(fuzzy_path, stringsAsFactors = FALSE)

plot_data <- fuzzy_components %>%
  mutate(
    Model_short = case_when(
      Model == "GFN_MC_coefficient_search" ~ "GFN-MC",
      TRUE ~ Model
    ),
    Crop_label = paste0(Crop, " (", Model_short, ")"),
    Fuzzy_label = case_when(
      Crop == "Apple" ~ paste0(
        "(",
        round(GFN_mu, 1),
        ", ",
        formatC(GFN_sigma2, format = "e", digits = 2),
        ")"
      ),
      Crop == "Barley" ~ paste0(
        "(",
        round(TFN_lower, 1),
        ", ",
        round(TFN_center, 1),
        ", ",
        round(TFN_upper, 1),
        ")"
      ),
      TRUE ~ NA_character_
    ),
    Representation = case_when(
      Crop == "Apple" ~ "GFN = (mu, sigma^2)",
      Crop == "Barley" ~ "TFN = (lower, center, upper)",
      TRUE ~ NA_character_
    )
  )

write.csv(
  plot_data,
  file.path(forecast_dir, "fuzzy_projection_labels_over_time_plot_data.csv"),
  row.names = FALSE
)

p <- ggplot(
  plot_data,
  aes(x = Year, y = Defuzzified_projection_kg_ha, color = Crop)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.1) +
  geom_text(
    aes(label = Fuzzy_label),
    angle = 90,
    hjust = -0.05,
    vjust = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ Crop_label, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 2025:2030) +
  scale_color_manual(values = c("Apple" = "#0072B2", "Barley" = "#D55E00"), guide = "none") +
  labs(
    x = "Year",
    y = "Defuzzified projected yield (kg/ha)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey70"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_cartesian(clip = "off")

png_path <- file.path(forecast_dir, "fuzzy_projection_labels_over_time.png")
pdf_path <- file.path(forecast_dir, "fuzzy_projection_labels_over_time.pdf")

ggsave(png_path, p, width = 12, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 12, height = 6.5)

cat("Fuzzy projection label-over-time figure created:\n")
cat(png_path, "\n")
cat(pdf_path, "\n")
