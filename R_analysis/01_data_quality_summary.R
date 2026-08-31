# Data quality summary for the Türkiye multi-crop yield panel

rm(list = ls())

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
output_dir <- file.path(analysis_dir, "output")
panel_path <- file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv")

if (!file.exists(panel_path)) {
  stop("Panel dataset not found. Run 00_build_panel_dataset.R first.")
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE, check.names = FALSE)

summary_dir <- file.path(output_dir, "summary")
if (!dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE)

basic_summary <- data.frame(
  item = c(
    "rows",
    "columns",
    "number_of_crops",
    "first_year",
    "last_year",
    "missing_values"
  ),
  value = c(
    nrow(panel),
    ncol(panel),
    length(unique(panel$crop)),
    min(panel$year),
    max(panel$year),
    sum(is.na(panel))
  )
)

numeric_cols <- names(panel)[sapply(panel, is.numeric)]
numeric_summary <- do.call(
  rbind,
  lapply(numeric_cols, function(v) {
    x <- panel[[v]]
    data.frame(
      variable = v,
      min = min(x, na.rm = TRUE),
      q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      max = max(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
)

crop_yield_summary <- do.call(
  rbind,
  lapply(split(panel, panel$crop), function(df) {
    data.frame(
      crop = df$crop[1],
      years = length(unique(df$year)),
      yield_min = min(df$yield_kg_ha),
      yield_mean = mean(df$yield_kg_ha),
      yield_max = max(df$yield_kg_ha),
      yield_sd = sd(df$yield_kg_ha),
      stringsAsFactors = FALSE
    )
  })
)

skewness_value <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  mean((x - mean(x))^3) / s^3
}

yield_summary_statistics <- do.call(
  rbind,
  lapply(split(panel, panel$crop), function(df) {
    y <- df$yield_kg_ha
    data.frame(
      Crop = df$crop[1],
      Mean = round(mean(y, na.rm = TRUE), 2),
      Median = round(median(y, na.rm = TRUE), 2),
      SD = round(sd(y, na.rm = TRUE), 2),
      IQR = round(IQR(y, na.rm = TRUE), 2),
      Min = round(min(y, na.rm = TRUE), 2),
      Max = round(max(y, na.rm = TRUE), 2),
      Skewness = round(skewness_value(y), 2),
      CV_percent = round(sd(y, na.rm = TRUE) / mean(y, na.rm = TRUE) * 100, 2),
      stringsAsFactors = FALSE
    )
  })
)

write.csv(basic_summary, file.path(summary_dir, "basic_summary.csv"), row.names = FALSE)
write.csv(numeric_summary, file.path(summary_dir, "numeric_summary.csv"), row.names = FALSE)
write.csv(crop_yield_summary, file.path(summary_dir, "crop_yield_summary.csv"), row.names = FALSE)
write.csv(
  yield_summary_statistics,
  file.path(output_dir, "yield_summary_statistics_2000_2024.csv"),
  row.names = FALSE
)
write.csv(
  yield_summary_statistics,
  file.path(summary_dir, "yield_summary_statistics_2000_2024.csv"),
  row.names = FALSE
)

cat("Data quality summaries written to:", summary_dir, "\n")
