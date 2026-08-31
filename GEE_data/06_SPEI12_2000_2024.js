// SPEI-12 for Türkiye from CSIC SPEIbase v2.11

var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

var startYear = 2000;
var endYear = 2024;

var speiCollection = ee.ImageCollection("CSIC/SPEI/2_11")
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .select("SPEI_12_month")
  .map(function(image) {
    return image.rename("SPEI")
      .clip(turkey)
      .copyProperties(image, ["system:time_start"]);
  });

var years = ee.List.sequence(startYear, endYear);

var annualSPEI = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var annualImage = speiCollection
      .filter(ee.Filter.calendarRange(year, year, "year"))
      .mean()
      .rename("SPEI");

    var value = annualImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: turkey.geometry(),
      scale: 50000,
      maxPixels: 1e13
    });

    return ee.Feature(null, {
      year: year,
      SPEI: value.get("SPEI")
    });
  })
);

print("Annual SPEI-12", annualSPEI);

Export.table.toDrive({
  collection: annualSPEI,
  description: "Turkey_Annual_SPEI12_CSIC_2000_2024",
  fileNamePrefix: "Turkey_Annual_SPEI12_CSIC_2000_2024",
  fileFormat: "CSV"
});
