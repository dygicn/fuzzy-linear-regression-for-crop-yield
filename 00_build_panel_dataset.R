# Build panel dataset for the Türkiye multi-crop yield study
# Input folder:  data/raw
# Output folder: output

rm(list = ls())

analysis_dir <- normalizePath(getwd(), mustWork = TRUE)
input_dir <- file.path(analysis_dir, "data", "raw")
output_dir <- file.path(analysis_dir, "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

read_csv_base <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

drop_gee_metadata <- function(df) {
  df[, setdiff(names(df), c("system:index", ".geo")), drop = FALSE]
}

standardize_year <- function(df) {
  if ("year" %in% names(df)) {
    df$year <- as.integer(df$year)
  }
  df
}

# -------------------------------------------------------------------------
# 1. Read annual national environmental predictors
# -------------------------------------------------------------------------

evi <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_EVI_2000_2024.csv"))
ndvi <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_NDVI_2000_2024.csv"))
lst <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_LST_Day_1km_2000_2024.csv"))
prec <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_Precipitation_CHIRPS_2000_2024.csv"))
et <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_ET_MOD16A2GF_2000_2024.csv"))
spei <- read_csv_base(file.path(input_dir, "NEW_Turkey_Annual_SPEI12_CSIC_2000_2024.csv"))

env_list <- list(evi, ndvi, lst, prec, et, spei)
env_list <- lapply(env_list, function(x) standardize_year(drop_gee_metadata(x)))

env <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE), env_list)
env <- env[order(env$year), ]

# -------------------------------------------------------------------------
# 2. Read crop-specific GDD, HeatStress, and ColdStress files
# -------------------------------------------------------------------------

gdd <- read_csv_base(file.path(input_dir, "NEW_GDD_Turkey_All_Crops_Yearly_GDD_2000_2024.csv"))
stress <- read_csv_base(file.path(input_dir, "NEW_Turkey_All_Crops_Annual_HeatColdStress_2000_2024.csv"))

gdd <- standardize_year(drop_gee_metadata(gdd))
stress <- standardize_year(drop_gee_metadata(stress))

# -------------------------------------------------------------------------
# 3. Read FAOSTAT national yield data
# -------------------------------------------------------------------------

faostat <- read_csv_base(file.path(input_dir, "NEW_FAOSTAT_data_en_6-29-2026.csv"))

yield <- faostat[faostat$Element == "Yield", c("Item", "Year", "Unit", "Value")]
names(yield) <- c("crop_faostat", "year", "yield_unit", "yield_kg_ha")
yield$year <- as.integer(yield$year)

crop_map <- data.frame(
  crop_faostat = c(
    "Apples",
    "Barley",
    "Beans, dry",
    "Grapes",
    "Maize (corn)",
    "Olives",
    "Potatoes",
    "Rice",
    "Sugar beet",
    "Tomatoes",
    "Wheat"
  ),
  crop = c(
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
  ),
  stringsAsFactors = FALSE
)

yield <- merge(yield, crop_map, by = "crop_faostat", all.x = TRUE)

if (any(is.na(yield$crop))) {
  stop("Some FAOSTAT crop names were not mapped. Check crop_map.")
}

# -------------------------------------------------------------------------
# 4. Add threshold metadata used for temperature-derived indices
# -------------------------------------------------------------------------

thresholds <- data.frame(
  crop = c("Wheat", "Barley", "Maize", "Rice", "Tomatoes", "Bean",
           "Grape", "Potatoes", "Apple", "Sugar_Beet", "Olive"),
  Tbase_C = c(0, 0, 10, 8, 10, 10, 10, 5, 4, 5, 7),
  Tupper_C = c(30, 30, 30, 35, 30, 30, 35, 30, 36, 30, 35),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------------
# 5. Build long panel: 11 crops x 25 years
# -------------------------------------------------------------------------

panel_rows <- list()
row_id <- 1

for (i in seq_len(nrow(yield))) {
  crop <- yield$crop[i]
  year <- yield$year[i]

  gdd_col <- paste0("GDD_", crop)
  heat_col <- paste0("HeatStress_", crop)
  cold_col <- paste0("ColdStress_", crop)

  if (!gdd_col %in% names(gdd)) stop(paste("Missing column in GDD file:", gdd_col))
  if (!heat_col %in% names(stress)) stop(paste("Missing column in stress file:", heat_col))
  if (!cold_col %in% names(stress)) stop(paste("Missing column in stress file:", cold_col))

  env_row <- env[env$year == year, , drop = FALSE]
  gdd_row <- gdd[gdd$year == year, , drop = FALSE]
  stress_row <- stress[stress$year == year, , drop = FALSE]
  threshold_row <- thresholds[thresholds$crop == crop, , drop = FALSE]

  if (nrow(env_row) != 1) stop(paste("Missing or duplicated environmental row for year:", year))
  if (nrow(gdd_row) != 1) stop(paste("Missing or duplicated GDD row for year:", year))
  if (nrow(stress_row) != 1) stop(paste("Missing or duplicated stress row for year:", year))
  if (nrow(threshold_row) != 1) stop(paste("Missing threshold row for crop:", crop))

  panel_rows[[row_id]] <- data.frame(
    crop = crop,
    crop_faostat = yield$crop_faostat[i],
    year = year,
    yield_kg_ha = yield$yield_kg_ha[i],
    Mean_NDVI = env_row$Mean_NDVI,
    Mean_EVI = env_row$Mean_EVI,
    Mean_LST_Day_1km = env_row$Mean_LST_Day_1km,
    Precipitation_mm = env_row$Precipitation_mm,
    ET_mm = env_row$ET_mm,
    SPEI = env_row$SPEI,
    GDD = gdd_row[[gdd_col]],
    HeatStress = stress_row[[heat_col]],
    ColdStress = stress_row[[cold_col]],
    Tbase_C = threshold_row$Tbase_C,
    Tupper_C = threshold_row$Tupper_C,
    stringsAsFactors = FALSE
  )

  row_id <- row_id + 1
}

panel <- do.call(rbind, panel_rows)
panel <- panel[order(panel$crop, panel$year), ]

# -------------------------------------------------------------------------
# 6. Quality checks
# -------------------------------------------------------------------------

expected_rows <- length(unique(panel$crop)) * length(unique(panel$year))
if (nrow(panel) != expected_rows) {
  stop(paste("Unexpected panel size:", nrow(panel), "expected:", expected_rows))
}

if (anyNA(panel)) {
  missing_counts <- colSums(is.na(panel))
  print(missing_counts[missing_counts > 0])
  stop("Panel contains missing values.")
}

cat("Panel dataset created successfully.\n")
cat("Rows:", nrow(panel), "\n")
cat("Crops:", paste(sort(unique(panel$crop)), collapse = ", "), "\n")
cat("Years:", min(panel$year), "-", max(panel$year), "\n")

# -------------------------------------------------------------------------
# 7. Export outputs
# -------------------------------------------------------------------------

write.csv(
  panel,
  file.path(output_dir, "NEW_crop_yield_environment_panel_2000_2024.csv"),
  row.names = FALSE
)

write.csv(
  env,
  file.path(output_dir, "NEW_environmental_predictors_annual_2000_2024.csv"),
  row.names = FALSE
)

write.csv(
  thresholds,
  file.path(output_dir, "NEW_crop_temperature_thresholds.csv"),
  row.names = FALSE
)

cat("Output written to:", output_dir, "\n")
