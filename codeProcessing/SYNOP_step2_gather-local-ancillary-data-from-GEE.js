// import data
var hansen = ee.Image("UMD/hansen/global_forest_change_2017_v1_5"),
    pekel = ee.Image("JRC/GSW1_0/GlobalSurfaceWater");

/// Prepare points where to do the analysis

// SYNOP points
var pts = ee.FeatureCollection("users/gduveiller/boreal_cleaned_SYNOP_points");

// Function to get buffers around those points
function bufferFeature(ft){
  ft = ft.buffer(15000);
    return ft.set({ncoords: ft.geometry().coordinates().length()});
}

var pts_buffer = pts.map(bufferFeature);



/// Get tree cover fraction per point /// 

// Define year of interest/reference for the forest cover
var ref_year = 2014;

// Load necessary forest cover files: Tree cover, loss, and gain
var treecover = hansen.select(['treecover2000'])//.clip(geometry);
var lossyear = hansen.select(['lossyear'])//.clip(geometry);
var loss = hansen.select(['loss'])//.clip(geometry);
var gain = hansen.select(['gain'])//.clip(geometry);

// Functions to get forest LOSS in 2001 -2017
function current_loss(year) { 
  return lossyear.eq(ee.Number(year).subtract(2000)) 
}
function current_treecover(year) {
  return ee.List.sequence(2001, year).iterate(function (year, img) { 
    return ee.Image(img).where(current_loss(year).eq(1), 0) 
  }, treecover)
}

// get TC and FC for ref year
var TC_refYear = ee.Image(current_treecover(ref_year)); // 
var TC_refYear_gain = TC_refYear.where(gain.eq(1),100); // to include gain ? 
var forest_cover = TC_refYear_gain.gte(20); // ?

/// Get water cover fraction per point /// 
var water_cover = pekel.select('occurrence').unmask().gte(80);

// Combine all together
var BBB = ee.ImageCollection.fromImages([water_cover,forest_cover])
BBB = BBB.toBands()

// reduce regions
var mean_local_cover = BBB.reduceRegions({
  reducer: ee.Reducer.mean(),
  collection: pts_buffer,
  scale: 30,
  tileScale:4
});

//print('pts_updated:',pts_updated)
print('data:',mean_local_cover)


Export.table.toDrive({
  collection: mean_local_cover, 
  description: 'SYNOP_points_with_local_cover', 
  // selectors: ['member','ncoords','pair','treecover2000'],
  fileFormat:'SHP'
  })
  
  
/// Visualization
var water_cover_clip = pekel.select('occurrence').gte(80).clip(pts_buffer);
var forest_cover_clip = forest_cover.clip(pts_buffer);

Map.addLayer(forest_cover_clip,{min:0,max:1, palette: ['wheat','darkgreen']},'forest cover');
Map.addLayer(water_cover_clip,{min:0,max:1, palette:  ['wheat','blue']},'water bodies');
Map.addLayer(pts,{color:'red'},'SYNOP points');
Map.centerObject(pts,10);



