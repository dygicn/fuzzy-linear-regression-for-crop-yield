// LST for Türkiye from MODIS MOD11A2

var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

var startYear = 2000;
var endYear = 2024;

var lstCollection = ee.ImageCollection("MODIS/061/MOD11A2")
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .map(function(image) {
    var lstDay = image.select("LST_Day_1km")
      .multiply(0.02)
      .subtract(273.15)
      .rename("Mean_LST_Day_1km");

    return lstDay
      .clip(turkey)
      .copyProperties(image, ["system:time_start"]);
  });

var years = ee.List.sequence(startYear, endYear);

var annualLST = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var annualImage = lstCollection
      .filter(ee.Filter.calendarRange(year, year, "year"))
      .mean();

    var value = annualImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: turkey.geometry(),
      scale: 1000,
      maxPixels: 1e13
    });

    return ee.Feature(null, {
      year: year,
      Mean_LST_Day_1km: value.get("Mean_LST_Day_1km")
    });
  })
);

print("Annual daytime LST", annualLST);

Export.table.toDrive({
  collection: annualLST,
  description: "Turkey_Annual_LST_Day_1km_2000_2024",
  fileNamePrefix: "Turkey_Annual_LST_Day_1km_2000_2024",
  fileFormat: "CSV"
});
