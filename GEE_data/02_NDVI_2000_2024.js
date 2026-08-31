// Annual NDVI for Türkiye from MODIS MOD13Q1

var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

var startYear = 2000;
var endYear = 2024;

var ndviCollection = ee.ImageCollection("MODIS/061/MOD13Q1")
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .select("NDVI")
  .map(function(image) {
    return image.multiply(0.0001)
      .copyProperties(image, ["system:time_start"]);
  });

var years = ee.List.sequence(startYear, endYear);

var annualNDVI = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var annualImage = ndviCollection
      .filter(ee.Filter.calendarRange(year, year, "year"))
      .mean();

    var value = annualImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: turkey.geometry(),
      scale: 5000,
      maxPixels: 1e13
    });

    return ee.Feature(null, {
      year: year,
      Mean_NDVI: value.get("NDVI")
    });
  })
);

print("Annual NDVI", annualNDVI);

Export.table.toDrive({
  collection: annualNDVI,
  description: "Turkey_Annual_NDVI_2000_2024",
  fileNamePrefix: "Turkey_Annual_NDVI_2000_2024",
  fileFormat: "CSV"
});
