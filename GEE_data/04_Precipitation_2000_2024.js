// Annual precipitation for Türkiye from CHIRPS Daily

var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

var startYear = 2000;
var endYear = 2024;

var precipitationCollection = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY")
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .select("precipitation");

var years = ee.List.sequence(startYear, endYear);

var annualPrecipitation = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var annualImage = precipitationCollection
      .filter(ee.Filter.calendarRange(year, year, "year"))
      .sum()
      .rename("Precipitation_mm");

    var value = annualImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: turkey.geometry(),
      scale: 5000,
      maxPixels: 1e13
    });

    return ee.Feature(null, {
      year: year,
      Precipitation_mm: value.get("Precipitation_mm")
    });
  })
);

print("Annual precipitation", annualPrecipitation);

Export.table.toDrive({
  collection: annualPrecipitation,
  description: "Turkey_Annual_Precipitation_CHIRPS_2000_2024",
  fileNamePrefix: "Turkey_Annual_Precipitation_CHIRPS_2000_2024",
  fileFormat: "CSV"
});
