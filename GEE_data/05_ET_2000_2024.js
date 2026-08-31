// Evapotranspiration for Türkiye from MODIS MOD16A2GF

var turkey = ee.FeatureCollection("FAO/GAUL/2015/level0")
  .filter(ee.Filter.eq("ADM0_NAME", "Turkey"));

var startYear = 2000;
var endYear = 2024;

// MOD16A2GF provides gap-filled 8-day ET, 500 m.
// ET scale factor = 0.1.
var etCollection = ee.ImageCollection("MODIS/061/MOD16A2GF")
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .select("ET")
  .map(function(image) {
    return image.multiply(0.1)
      .rename("ET_mm")
      .clip(turkey)
      .copyProperties(image, ["system:time_start"]);
  });

var years = ee.List.sequence(startYear, endYear);

var annualET = ee.FeatureCollection(
  years.map(function(year) {
    year = ee.Number(year);

    var annualImage = etCollection
      .filter(ee.Filter.calendarRange(year, year, "year"))
      .sum()
      .rename("ET_mm");

    var value = annualImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: turkey.geometry(),
      scale: 500,
      maxPixels: 1e13
    });

    return ee.Feature(null, {
      year: year,
      ET_mm: value.get("ET_mm")
    });
  })
);

print("Annual ET", annualET);

Export.table.toDrive({
  collection: annualET,
  description: "Turkey_Annual_ET_MOD16A2GF_2000_2024",
  fileNamePrefix: "Turkey_Annual_ET_MOD16A2GF_2000_2024",
  fileFormat: "CSV"
});
