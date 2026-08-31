// =======================================================
// Annual crop-specific Heat and Cold Stress Indices
// Türkiye, 2000-2024
// Data: ERA5-Land Daily Aggregated 2m air temperature
// Heat stress: cumulative Tmax excess above Tupper
// Cold stress: cumulative Tmin deficit below Tbase
// =======================================================

// 1. Define Study Area: Türkiye
var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

// 2. Define time range
var startYear = 2000;
var endYear = 2024;
var years = ee.List.sequence(startYear, endYear);

// 3. Crop-specific temperature thresholds, °C
// Tbase and Tupper should be reported in the manuscript table.
// Main threshold source: Paredes et al. (2025)
var thresholds = {
  "Wheat":      {Tbase: 0,  Tupper: 30},
  "Barley":     {Tbase: 0,  Tupper: 30},
  "Maize":      {Tbase: 10, Tupper: 30},
  "Rice":       {Tbase: 8,  Tupper: 35},
  "Tomatoes":   {Tbase: 10, Tupper: 30},
  "Bean":       {Tbase: 10, Tupper: 30},
  "Grape":      {Tbase: 10, Tupper: 35},
  "Potatoes":   {Tbase: 5,  Tupper: 30},
  "Apple":      {Tbase: 4,  Tupper: 36},
  "Sugar_Beet": {Tbase: 5,  Tupper: 30},
  "Olive":      {Tbase: 7,  Tupper: 35}
};

var cropNames = [
  "Wheat",
  "Barley",
  "Maize",
  "Rice",
  "Tomatoes",
  "Bean",
  "Grape",
  "Potatoes",
  "Apple",
  "Sugar_Beet",
  "Olive"
];

// 4. Load ERA5-Land Daily Temperature Data
var dataset = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterBounds(turkey)
  .filterDate("2000-01-01", "2025-01-01")
  .select(["temperature_2m_max", "temperature_2m_min"]);

// 5. Compute annual heat and cold stress for all crops
var annualStress = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var start = ee.Date.fromYMD(year, 1, 1);
    var end = start.advance(1, "year");

    var yearlyTemperature = dataset.filterDate(start, end);

    var featureDict = ee.Dictionary({
      "year": year
    });

    cropNames.forEach(function(cropName) {
      var Tbase = thresholds[cropName].Tbase;
      var Tupper = thresholds[cropName].Tupper;

      var yearlyStressImage = yearlyTemperature.map(function(image) {
        var TmaxC = image.select("temperature_2m_max").subtract(273.15);
        var TminC = image.select("temperature_2m_min").subtract(273.15);

        var heatStress = TmaxC
          .subtract(Tupper)
          .max(0)
          .rename("HeatStress");

        var coldStress = ee.Image.constant(Tbase)
          .subtract(TminC)
          .max(0)
          .rename("ColdStress");

        return heatStress.addBands(coldStress);
      }).sum();

      var stats = yearlyStressImage.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: turkey.geometry(),
        scale: 11000,
        maxPixels: 1e13
      });

      featureDict = featureDict
        .set("HeatStress_" + cropName, stats.get("HeatStress"))
        .set("ColdStress_" + cropName, stats.get("ColdStress"));
    });

    return ee.Feature(null, featureDict);
  })
);

// 6. Print table
print("Annual crop-specific heat and cold stress indices for Türkiye", annualStress);

// 7. Export all crops in one CSV file
Export.table.toDrive({
  collection: annualStress,
  description: "Turkey_All_Crops_Annual_HeatColdStress_2000_2024",
  fileNamePrefix: "Turkey_All_Crops_Annual_HeatColdStress_2000_2024",
  fileFormat: "CSV"
});
