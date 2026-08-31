packages <- c(
  "FNN", "dplyr", "e1071", "fuzzyreg", "ggplot2", "glmnet",
  "nnet", "openxlsx", "randomForest", "readr", "xgboost"
)

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required R packages are already installed.")
}
