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

apple <- fuzzy_components %>%
  filter(Crop == "Apple") %>%
  mutate(
    sigma = sqrt(GFN_sigma2)
  )

barley <- fuzzy_components %>%
  filter(Crop == "Barley")

gfn_membership_rows <- lapply(seq_len(nrow(apple)), function(i) {
  row <- apple[i, ]
  x_grid <- seq(
    max(0, row$GFN_mu - 3 * row$sigma),
    row$GFN_mu + 3 * row$sigma,
    length.out = 400
  )
  data.frame(
    Crop = row$Crop,
    Year = row$Year,
    x = x_grid,
    membership = exp(-((x_grid - row$GFN_mu)^2) / (2 * row$GFN_sigma2)),
    mu = row$GFN_mu,
    sigma2 = row$GFN_sigma2
  )
})

gfn_membership <- do.call(rbind, gfn_membership_rows)

tfn_membership_rows <- lapply(seq_len(nrow(barley)), function(i) {
  row <- barley[i, ]
  data.frame(
    Crop = row$Crop,
    Year = row$Year,
    x = c(row$TFN_lower, row$TFN_center, row$TFN_upper),
    membership = c(0, 1, 0),
    lower = row$TFN_lower,
    center = row$TFN_center,
    upper = row$TFN_upper
  )
})

tfn_membership <- do.call(rbind, tfn_membership_rows)

tfn_labels <- tfn_membership %>%
  mutate(
    label = round(x, 2),
    label_y = case_when(
      membership == 1 ~ 1.04,
      TRUE ~ -0.035
    )
  )

write.csv(
  gfn_membership,
  file.path(forecast_dir, "apple_gfn_projection_membership_plot_data.csv"),
  row.names = FALSE
)

write.csv(
  tfn_membership,
  file.path(forecast_dir, "barley_tfn_projection_membership_plot_data.csv"),
  row.names = FALSE
)

year_palette <- setNames(
  scales::hue_pal()(length(unique(fuzzy_components$Year))),
  sort(unique(fuzzy_components$Year))
)

p_gfn <- ggplot(
  gfn_membership,
  aes(x = x, y = membership, color = factor(Year), group = Year)
) +
  geom_line(linewidth = 0.85) +
  geom_vline(
    data = apple,
    aes(xintercept = GFN_mu, color = factor(Year)),
    linewidth = 0.35,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  geom_label(
    data = apple,
    aes(
      x = GFN_mu,
      y = 1.04,
      label = round(GFN_mu, 0),
      color = factor(Year)
    ),
    inherit.aes = FALSE,
    size = 2.8,
    label.size = 0.15,
    label.padding = unit(0.12, "lines"),
    show.legend = FALSE
  ) +
  scale_color_manual(values = year_palette) +
  coord_cartesian(ylim = c(0, 1.12), clip = "off") +
  labs(
    x = "Projected apple yield (kg/ha)",
    y = "Membership degree",
    color = "Year"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1, byrow = TRUE))

p_tfn <- ggplot(
  tfn_membership,
  aes(x = x, y = membership, color = factor(Year), group = Year)
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.5) +
  geom_text(
    data = tfn_labels,
    aes(
      x = x,
      y = label_y,
      label = label,
      color = factor(Year)
    ),
    inherit.aes = FALSE,
    size = 2.8,
    angle = 90,
    vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = year_palette) +
  coord_cartesian(ylim = c(-0.08, 1.12), clip = "off") +
  labs(
    title = "Fuzzy Projection for Barley",
    x = "Projected barley yield (kg/ha)",
    y = "Membership degree",
    color = "Year"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

apple_png <- file.path(forecast_dir, "apple_gfn_projection_membership.png")
apple_pdf <- file.path(forecast_dir, "apple_gfn_projection_membership.pdf")
barley_png <- file.path(forecast_dir, "barley_tfn_projection_membership.png")
barley_pdf <- file.path(forecast_dir, "barley_tfn_projection_membership.pdf")

ggsave(apple_png, p_gfn, width = 8.5, height = 5.5, dpi = 300)
ggsave(apple_pdf, p_gfn, width = 8.5, height = 5.5)
ggsave(barley_png, p_tfn, width = 8.5, height = 5.5, dpi = 300)
ggsave(barley_pdf, p_tfn, width = 8.5, height = 5.5)

cat("Fuzzy projection number representation figures created:\n")
cat(apple_png, "\n")
cat(apple_pdf, "\n")
cat(barley_png, "\n")
cat(barley_pdf, "\n")
