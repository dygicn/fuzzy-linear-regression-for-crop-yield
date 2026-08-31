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

barley <- read.csv(fuzzy_path, stringsAsFactors = FALSE) %>%
  filter(Crop == "Barley") %>%
  arrange(Year)

triangle_width <- 0.34

polygon_data <- lapply(seq_len(nrow(barley)), function(i) {
  row <- barley[i, ]
  data.frame(
    Crop = row$Crop,
    Year = row$Year,
    x = c(row$Year, row$Year - triangle_width, row$Year),
    y = c(row$TFN_lower, row$TFN_center, row$TFN_upper),
    point_type = c("lower", "center", "upper"),
    value = c(row$TFN_lower, row$TFN_center, row$TFN_upper)
  )
}) %>%
  bind_rows()

label_data <- polygon_data %>%
  mutate(
    label = round(value, 2),
    label_x = case_when(
      point_type == "center" ~ x - 0.05,
      TRUE ~ x + 0.05
    ),
    label_hjust = case_when(
      point_type == "center" ~ 1,
      TRUE ~ 0
    )
  )

write.csv(
  polygon_data,
  file.path(forecast_dir, "barley_tfn_by_year_rotated_plot_data.csv"),
  row.names = FALSE
)

p <- ggplot() +
  geom_polygon(
    data = polygon_data,
    aes(x = x, y = y, group = Year, fill = factor(Year), color = factor(Year)),
    alpha = 0.16,
    linewidth = 0.85
  ) +
  geom_point(
    data = polygon_data,
    aes(x = x, y = y, color = factor(Year)),
    size = 1.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = label_data,
    aes(
      x = label_x,
      y = y,
      label = label,
      color = factor(Year),
      hjust = label_hjust
    ),
    angle = 45,
    vjust = -0.15,
    size = 2.8,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = barley$Year,
    limits = c(min(barley$Year) - 0.95, max(barley$Year) + 1.10)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.22))) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Year",
    y = "Projected barley yield (kg/ha)",
    fill = "Year",
    color = "Year"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(14, 52, 14, 14)
  )

png_path <- file.path(forecast_dir, "barley_tfn_by_year_rotated.png")
pdf_path <- file.path(forecast_dir, "barley_tfn_by_year_rotated.pdf")

ggsave(png_path, p, width = 9, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 9, height = 5.5)

cat("Barley rotated TFN-by-year figure created:\n")
cat(png_path, "\n")
cat(pdf_path, "\n")
