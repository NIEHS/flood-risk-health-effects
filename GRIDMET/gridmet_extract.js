// JavaScript to extract and summarize GRIDMET data 
// (https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET)
// on Google Earth Engine

// Create a geometry representing an export region.
// var geometry = ee.Geometry.Rectangle([-131, -65, 22, 51]);
// var geometry = ee.Geometry.Rectangle([-109, -91, 40, 45]);
var geometry = 
    /* color: #98ff00 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-134.86771875, 51.34473095759078],
          [-134.86771875, 23.645099515205715],
          [-65.08256250000001, 23.645099515205715],
          [-65.08256250000001, 51.34473095759078]]], null, false);
          
          
          
// When running through the script on Google Earth Engine, click on the 
// button "Run" for each task, then send it to the desired Google Drive folder,
// e.g. NIEHS work/GRIDMET

// Takes about 10 minutes for each task



var dataset = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')
                  .filter(ee.Filter.date('2005-01-01', '2021-04-01'));

Map.setCenter(-115.356, 38.686, 4);
                  
                  
                  
var maximumTemperature = dataset.select('tmmx');

var tempVis = {
  min: 250.0,
  max: 320.0,
  palette: ['d8d8d8', '4addff', '5affa3', 'f2ff89', 'ff725c'],
};



var tmmx_2020 = maximumTemperature.filter(ee.Filter.date('2020-01-01', '2020-12-31'));

var mean = tmmx_2020.mean()

Map.addLayer(mean, tempVis, 'tmmx_2020');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2020', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2019 = maximumTemperature.filter(ee.Filter.date('2019-01-01', '2019-12-31'));

var mean = tmmx_2019.mean()

Map.addLayer(mean, tempVis, 'tmmx_2019');

Export.image.toDrive({
    image: mean,
    description: 'tmmx_2019', 
    crs: 'EPSG:3857', 
    scale: 4638.3, 
    region: geometry
  });
  
  
  
var tmmx_2018 = maximumTemperature.filter(ee.Filter.date('2018-01-01', '2018-12-31'));

var mean = tmmx_2018.mean()

Map.addLayer(mean, tempVis, 'tmmx_2018');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2018', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2017 = maximumTemperature.filter(ee.Filter.date('2017-01-01', '2017-12-31'));

var mean = tmmx_2017.mean()

Map.addLayer(mean, tempVis, 'tmmx_2017');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2017', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2016 = maximumTemperature.filter(ee.Filter.date('2016-01-01', '2016-12-31'));

var mean = tmmx_2016.mean()

Map.addLayer(mean, tempVis, 'tmmx_2016');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2016', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2015 = maximumTemperature.filter(ee.Filter.date('2015-01-01', '2015-12-31'));

var mean = tmmx_2015.mean()

Map.addLayer(mean, tempVis, 'tmmx_2015');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2015', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2014 = maximumTemperature.filter(ee.Filter.date('2014-01-01', '2014-12-31'));

var mean = tmmx_2014.mean()

Map.addLayer(mean, tempVis, 'tmmx_2014');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2014', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2013 = maximumTemperature.filter(ee.Filter.date('2013-01-01', '2013-12-31'));

var mean = tmmx_2013.mean()

Map.addLayer(mean, tempVis, 'tmmx_2013');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2013', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2012 = maximumTemperature.filter(ee.Filter.date('2012-01-01', '2012-12-31'));

var mean = tmmx_2012.mean()

Map.addLayer(mean, tempVis, 'tmmx_2012');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2012', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2011 = maximumTemperature.filter(ee.Filter.date('2011-01-01', '2011-12-31'));

var mean = tmmx_2011.mean()

Map.addLayer(mean, tempVis, 'tmmx_2011');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2011', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2010 = maximumTemperature.filter(ee.Filter.date('2010-01-01', '2010-12-31'));

var mean = tmmx_2010.mean()

Map.addLayer(mean, tempVis, 'tmmx_2010');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2010', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2009 = maximumTemperature.filter(ee.Filter.date('2009-01-01', '2009-12-31'));

var mean = tmmx_2009.mean()

Map.addLayer(mean, tempVis, 'tmmx_2009');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2009', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2008 = maximumTemperature.filter(ee.Filter.date('2008-01-01', '2008-12-31'));

var mean = tmmx_2008.mean()

Map.addLayer(mean, tempVis, 'tmmx_2008');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2008', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2007 = maximumTemperature.filter(ee.Filter.date('2007-01-01', '2007-12-31'));

var mean = tmmx_2007.mean()

Map.addLayer(mean, tempVis, 'tmmx_2007');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2007', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2006 = maximumTemperature.filter(ee.Filter.date('2006-01-01', '2006-12-31'));

var mean = tmmx_2006.mean()

Map.addLayer(mean, tempVis, 'tmmx_2006');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2006', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var tmmx_2005 = maximumTemperature.filter(ee.Filter.date('2005-01-01', '2005-12-31'));

var mean = tmmx_2005.mean()

Map.addLayer(mean, tempVis, 'tmmx_2005');

Export.image.toDrive({
  image: mean,
  description: 'tmmx_2005', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



////// Maximum relative humidity

var maximumHumidity = dataset.select('rmax');

var humidVis = {
  min: 0,
  max: 100,
  palette: ['d8d8d8', '4addff', '5affa3', 'f2ff89', 'ff725c'],
};



var rmax_2020 = maximumHumidity.filter(ee.Filter.date('2020-01-01', '2020-12-31'));

var mean = rmax_2020.mean()

Map.addLayer(mean, humidVis, 'rmax_2020');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2020', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2019 = maximumHumidity.filter(ee.Filter.date('2019-01-01', '2019-12-31'));

var mean = rmax_2019.mean()

Map.addLayer(mean, humidVis, 'rmax_2019');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2019', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2018 = maximumHumidity.filter(ee.Filter.date('2018-01-01', '2018-12-31'));

var mean = rmax_2018.mean()

Map.addLayer(mean, humidVis, 'rmax_2018');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2018', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2017 = maximumHumidity.filter(ee.Filter.date('2017-01-01', '2017-12-31'));

var mean = rmax_2017.mean()

Map.addLayer(mean, humidVis, 'rmax_2017');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2017', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2016 = maximumHumidity.filter(ee.Filter.date('2016-01-01', '2016-12-31'));

var mean = rmax_2016.mean()

Map.addLayer(mean, humidVis, 'rmax_2016');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2016', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2015 = maximumHumidity.filter(ee.Filter.date('2015-01-01', '2015-12-31'));

var mean = rmax_2015.mean()

Map.addLayer(mean, humidVis, 'rmax_2015');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2015', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2014 = maximumHumidity.filter(ee.Filter.date('2014-01-01', '2014-12-31'));

var mean = rmax_2014.mean()

Map.addLayer(mean, humidVis, 'rmax_2014');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2014', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2013 = maximumHumidity.filter(ee.Filter.date('2013-01-01', '2013-12-31'));

var mean = rmax_2013.mean()

Map.addLayer(mean, humidVis, 'rmax_2013');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2013', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2012 = maximumHumidity.filter(ee.Filter.date('2012-01-01', '2012-12-31'));

var mean = rmax_2012.mean()

Map.addLayer(mean, humidVis, 'rmax_2012');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2012', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2011 = maximumHumidity.filter(ee.Filter.date('2011-01-01', '2011-12-31'));

var mean = rmax_2011.mean()

Map.addLayer(mean, humidVis, 'rmax_2011');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2011', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2010 = maximumHumidity.filter(ee.Filter.date('2010-01-01', '2010-12-31'));

var mean = rmax_2010.mean()

Map.addLayer(mean, humidVis, 'rmax_2010');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2010', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2009 = maximumHumidity.filter(ee.Filter.date('2009-01-01', '2009-12-31'));

var mean = rmax_2009.mean()

Map.addLayer(mean, humidVis, 'rmax_2009');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2009', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2008 = maximumHumidity.filter(ee.Filter.date('2008-01-01', '2008-12-31'));

var mean = rmax_2008.mean()

Map.addLayer(mean, humidVis, 'rmax_2008');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2008', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2007 = maximumHumidity.filter(ee.Filter.date('2007-01-01', '2007-12-31'));

var mean = rmax_2007.mean()

Map.addLayer(mean, humidVis, 'rmax_2007');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2007', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2006 = maximumHumidity.filter(ee.Filter.date('2006-01-01', '2006-12-31'));

var mean = rmax_2006.mean()

Map.addLayer(mean, humidVis, 'rmax_2006');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2006', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var rmax_2005 = maximumHumidity.filter(ee.Filter.date('2005-01-01', '2005-12-31'));

var mean = rmax_2005.mean()

Map.addLayer(mean, humidVis, 'rmax_2005');

Export.image.toDrive({
  image: mean,
  description: 'rmax_2005', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});




