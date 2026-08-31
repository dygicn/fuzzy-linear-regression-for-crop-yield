## Repository structure

- `GEE_data/`: Google Earth Engine JavaScript codes used to obtain the environmental variables for Türkiye for 2000–2024.
- `FAOSTAT_data`: Source information and filters used to obtain the crop-yield data.
- `R_analysis/`: R codes used for data preparation, machine-learning benchmarks, fuzzy linear regression analyses, model comparisons, projections, and figures.
- `install_packages.R`: Installs the required R packages.
- `run_all.R`: Runs the complete R analysis in the required order.

## Data

Crop-yield data were obtained from FAOSTAT:

https://www.fao.org/faostat/

Environmental data can be reproduced using the JavaScript codes in the `GEE_data` folder. 
The exported CSV files should be placed in:

`data/raw/`

## Running the analysis

After placing the required CSV files in `data/raw/`, open R in the main repository directory and run:

```r
source("install_packages.R")
source("run_all.R")
