// =====================================================
// FIRE RECOVERY DATA - GOOGLE EARTH ENGINE SCRIPT
// Cape of Good Hope | TMNP
// =====================================================
// NOTE - Time Since Fire data seems not to have worked as desired. See veldage_terra.R for calculating vegetation age from SANParks fire polygons.
// =====================================================
// FIRE RECOVERY DATA CUBE
// Cape of Good Hope | TMNP
// =====================================================
// ==============================
// 1. AOI
// ==============================
var study_area = ee.Geometry.Polygon([
  [[18.35, -34.36],
   [18.35, -34.20],
   [18.5, -34.20],
   [18.5, -34.36]]
]);

Map.centerObject(study_area, 11);

var start_date = '2001-01-01';
var end_date   = '2026-03-31';


// -----------------------------
// LOAD MODIS NDVI
// -----------------------------
var modis = ee.ImageCollection('MODIS/061/MOD13Q1')
  .filterDate(start_date, end_date)
  .select('NDVI');

var scaleFactor = 0.0001;

// -----------------------------
// LOAD DYNAMIC WORLD
// -----------------------------
var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filterDate(start_date, end_date)
  .select('label');

// Mask out Water (0) and Built (6)
var dw_mask = dw.map(function(img) {
  var label = img.select('label');
  return label.neq(0).and(label.neq(6));
}).reduce(ee.Reducer.mode());

// OPTIONAL: match MODIS projection
dw_mask = dw_mask.reproject({
  crs: modis.first().projection(),
  scale: 250
});

// -----------------------------
// NDVI COLLECTION
// -----------------------------
var NDVI_collection = modis.map(function(img) {
  return img.multiply(scaleFactor)
    .updateMask(dw_mask)
    .toFloat()
    .unmask(-9999)
    .copyProperties(img, img.propertyNames());
});

// -----------------------------
// NDVI STACK
// -----------------------------
var NDVI_stack = NDVI_collection
  .toBands()
  .setDefaultProjection(modis.first().projection());

// -----------------------------
// LOAD BURNED AREA
// -----------------------------
var burned = ee.ImageCollection('MODIS/061/MCD64A1')
  .filterDate(start_date, end_date)
  .select('BurnDate');

// Convert to binary burn (1 = burned)
var burnedBinary = burned.map(function(img) {
  return img.gt(0)
    .rename('burned')
    .copyProperties(img, ['system:time_start']);
});

// -----------------------------
// COMPUTE MONTHLY TSF
// -----------------------------
var addTSF = function(collection) {

  var first = ee.Image(0).rename('TSF')
    .toFloat()
    .set('system:time_start', ee.Image(collection.first()).get('system:time_start'));

  var tsfList = ee.List(collection.iterate(function(img, prev) {
    prev = ee.List(prev);
    var last = ee.Image(prev.get(-1));
    img = ee.Image(img);

    var tsf = last.add(1)
      .where(img.eq(1), 0)
      .rename('TSF')
      .toFloat()
      .copyProperties(img, ['system:time_start']);

    return prev.add(tsf);

  }, ee.List([first])));

  return ee.ImageCollection(tsfList);
};

var TSF_monthly = addTSF(burnedBinary);

// -----------------------------
// SAMPLE TSF AT NDVI DATES + MATCH GRID
// -----------------------------
var TSF_collection = NDVI_collection.map(function(ndvi_img) {

  var date = ee.Date(ndvi_img.get('system:time_start'));

  // Get most recent TSF before or at NDVI date
  var tsf = TSF_monthly
    .filterDate(start_date, date.advance(1, 'day'))
    .sort('system:time_start', false)
    .first();

  // Use NDVI image as template
  var ndvi_proj = ndvi_img.projection();

  return ee.Image(tsf)
    // resample TSF to continuous surface
    .resample('bilinear')
    
    // force alignment to NDVI grid
    .reproject({
      crs: ndvi_proj.crs(),
      scale: ndvi_proj.nominalScale()
    })
    
    // apply same mask + export fixes
    .updateMask(dw_mask)
    .toFloat()
    .unmask(-9999)
    .rename('TSF')
    .set('system:time_start', date);
});

// -----------------------------
// TSF STACK (ALIGNED WITH NDVI)
// -----------------------------
var TSF_stack = TSF_collection
  .toBands()
  .setDefaultProjection(burned.first().projection());

// =====================================================
// STATIC COVARIATES (CHELSA + TERRAIN, NDVI-ALIGNED)
// =====================================================

// Use NDVI projection as master grid
var proj = NDVI_collection.first().projection();

// -----------------------------
// HELPER FUNCTION
// -----------------------------
function prepStatic(img, name) {
  return img
    .resample('bilinear')
    .reproject({
      crs: proj.crs(),
      scale: proj.nominalScale()
    })
    .updateMask(dw_mask)
    .toFloat()
    .unmask(-9999)
    .rename(name);
}

// =====================================================
// CHELSA CLIMATE (uploaded assets)
// =====================================================
var climate = ee.Image.cat([

  prepStatic(ee.Image('projects/emma-360410/assets/chelsa_pr01_1980_2010'), 'prec_jan'),
  prepStatic(ee.Image('projects/emma-360410/assets/chelsa_tmax_1981_2010'), 'tmax_jan'),

  prepStatic(ee.Image('projects/emma-360410/assets/chelsa_pr07_1980_2010'), 'prec_jul'),
  prepStatic(ee.Image('projects/emma-360410/assets/chelsa_tmin_1980_2010'), 'tmin_jul')

]).clip(study_area);


// =====================================================
// TERRAIN (SRTM)
// =====================================================
var dem = ee.Image('USGS/SRTMGL1_003');

// Terrain derivatives
var terrain = ee.Terrain.products(dem);

// Topographic Position Index (TPI)
var tpi = dem.subtract(
  dem.focal_mean({
    kernel: ee.Kernel.circle(3)
  })
).rename('tpi');

// Prepare terrain layers
var topo = ee.Image.cat([

  prepStatic(dem, 'elevation'),
  prepStatic(terrain.select('slope'), 'slope'),
  prepStatic(terrain.select('aspect'), 'aspect'),
  prepStatic(tpi, 'tpi')

]).clip(study_area);


// =====================================================
// FINAL STATIC STACK
// =====================================================
var STATIC_stack = ee.Image.cat([
  climate,
  topo
]).setDefaultProjection(proj);
  
// -----------------------------
// DEBUG
// -----------------------------
print('NDVI size:', NDVI_collection.size());
print('TSF size:', TSF_collection.size());

Map.centerObject(study_area);
Map.addLayer(NDVI_collection.first(), {min: 0, max: 1}, 'NDVI first');
Map.addLayer(TSF_collection.first(), {min: 0, max: 50}, 'TSF first');

// -----------------------------
// EXPORT NDVI
// -----------------------------
Export.image.toDrive({
  image: NDVI_stack,
  description: 'NDVI_stack_2001_2026',
  folder: 'EarthEngine/CapePoint',
  region: study_area,
  scale: 250,
  maxPixels: 1e13
});

// -----------------------------
// EXPORT NDVI DATES
// -----------------------------
var ndvi_dates = NDVI_collection.aggregate_array('system:time_start');

var ndvi_dates_fc = ee.FeatureCollection(
  ndvi_dates.map(function(d) {
    return ee.Feature(null, {
      date: ee.Date(d).format('YYYY-MM-dd')
    });
  })
);

Export.table.toDrive({
  collection: ndvi_dates_fc,
  description: 'NDVI_dates',
  folder: 'EarthEngine/CapePoint',
  fileFormat: 'CSV'
});

// -----------------------------
// EXPORT TSF
// -----------------------------
Export.image.toDrive({
  image: TSF_stack,
  description: 'TSF_stack_2001_2026',
  folder: 'EarthEngine/CapePoint',
  region: study_area,
  scale: 250,
  maxPixels: 1e13
});

// -----------------------------
// EXPORT STATIC STACK
// -----------------------------
Export.image.toDrive({
  image: STATIC_stack,
  description: 'STATIC_stack_terrain_climate',
  folder: 'EarthEngine/CapePoint',        // optional: change/remove as needed
  fileNamePrefix: 'STATIC_stack',
  region: study_area,           // must be valid geometry
  scale: 250,
  maxPixels: 1e13
});