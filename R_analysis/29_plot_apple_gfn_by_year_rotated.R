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

apple <- read.csv(fuzzy_path, stringsAsFactors = FALSE) %>%
  filter(Crop == "Apple") %>%
  arrange(Year) %>%
  mutate(
    GFN_sigma = sqrt(GFN_sigma2),
    label = sprintf("%.2f", GFN_mu)
  )

curve_width <- 0.34

curve_data <- lapply(seq_len(nrow(apple)), function(i) {
  row <- apple[i, ]
  y_grid <- seq(row$GFN_mu - 2.5 * row$GFN_sigma,
                row$GFN_mu + 2.5 * row$GFN_sigma,
                length.out = 220)
  membership <- exp(-((y_grid - row$GFN_mu)^2) / (2 * row$GFN_sigma2))
  data.frame(
    Crop = row$Crop,
    Year = row$Year,
    y = y_grid,
    membership = membership,
    x = row$Year - curve_width * membership
  )
}) %>%
  bind_rows()

baseline_data <- curve_data %>%
  group_by(Year) %>%
  summarize(
    x = first(Year),
    y_min = min(y),
    y_max = max(y),
    .groups = "drop"
  )

label_data <- apple %>%
  mutate(
    x = Year - curve_width - 0.05,
    y = GFN_mu
  )

write.csv(
  curve_data,
  file.path(forecast_dir, "apple_gfn_by_year_rotated_plot_data.csv"),
  row.names = FALSE
)

p <- ggplot() +
  geom_segment(
    data = baseline_data,
    aes(x = x, xend = x, y = y_min, yend = y_max, color = factor(Year)),
    linewidth = 0.65,
    alpha = 0.75
  ) +
  geom_ribbon(
    data = curve_data,
    aes(xmin = x, xmax = Year, y = y, group = Year, fill = factor(Year)),
    alpha = 0.12,
    show.legend = FALSE
  ) +
  geom_path(
    data = curve_data,
    aes(x = x, y = y, group = Year, color = factor(Year)),
    linewidth = 0.95,
    show.legend = FALSE
  ) +
  geom_point(
    data = apple,
    aes(x = Year - curve_width, y = GFN_mu, color = factor(Year)),
    size = 1.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label, color = factor(Year)),
    angle = 45,
    hjust = 1,
    vjust = -0.1,
    size = 2.65,
    lineheight = 0.88,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = apple$Year,
    limits = c(min(apple$Year) - 1.10, max(apple$Year) + 0.75)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Year",
    y = "Projected apple yield (kg/ha)",
    fill = "Year",
    color = "Year"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(14, 58, 14, 14)
  )

png_path <- file.path(forecast_dir, "apple_gfn_by_year_rotated.png")
pdf_path <- file.path(forecast_dir, "apple_gfn_by_year_rotated.pdf")

ggsave(png_path, p, width = 9, height = 5.5, dpi = 300)
ggsave(pdf_path, p, width = 9, height = 5.5)

cat("Apple rotated GFN-by-year figure created:\n")
cat(png_path, "\n")
cat(pdf_path, "\n")
