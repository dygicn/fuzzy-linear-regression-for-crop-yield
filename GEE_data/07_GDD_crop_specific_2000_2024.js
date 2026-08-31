// 1. Define Study Area: Türkiye
var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

// 2. Define time range
var startYear = 2000;
var endYear = 2024;
var years = ee.List.sequence(startYear, endYear);

// 3. Crop-specific base temperatures, °C
var baseTemps = {
  "Wheat": 0,
  "Tomatoes": 10,
  "Sugar Beet": 5,
  "Rice": 8,
  "Barley": 0,
  "Apple": 4,
  "Olive": 7,
  "Maize": 10,
  "Grape": 10,
  "Bean": 10,
  "Potatoes": 5
};

var cropNames = [
  "Wheat",
  "Tomatoes",
  "Sugar Beet",
  "Rice",
  "Barley",
  "Apple",
  "Olive",
  "Maize",
  "Grape",
  "Bean",
  "Potatoes"
];

// 4. Load ERA5-Land Daily Temperature Data
var dataset = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
  .filterBounds(turkey)
  .filterDate("2000-01-01", "2025-01-01")
  .select(["temperature_2m_max", "temperature_2m_min"]);

// 5. Compute yearly GDD for all crops
var yearlyGDDSeries = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var start = ee.Date.fromYMD(year, 1, 1);
    var end = start.advance(1, "year");

    var yearlyTemperature = dataset.filterDate(start, end);

    var featureDict = ee.Dictionary({
      "year": year
    });

    cropNames.forEach(function(cropName) {
      var T_base = baseTemps[cropName];

      var gddYearly = yearlyTemperature
        .map(function(image) {
          var T_max = image.select("temperature_2m_max").subtract(273.15);
          var T_min = image.select("temperature_2m_min").subtract(273.15);

          var T_avg = T_max.add(T_min).divide(2);

          return T_avg
            .subtract(T_base)
            .max(0)
            .rename("GDD");
        })
        .sum();

      var stats = gddYearly.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: turkey.geometry(),
        scale: 11000,
        maxPixels: 1e13
      });

      featureDict = featureDict.set(
        "GDD_" + cropName.replace(" ", "_"),
        stats.get("GDD")
      );
    });

    return ee.Feature(null, featureDict);
  })
);

// 6. Print table
print("Yearly crop-specific GDD values for Türkiye", yearlyGDDSeries);

// 7. Export all crops in one CSV file
Export.table.toDrive({
  collection: yearlyGDDSeries,
  description: "Turkey_All_Crops_Yearly_GDD_2000_2024",
  fileNamePrefix: "Turkey_All_Crops_Yearly_GDD_2000_2024",
  fileFormat: "CSV"
});
