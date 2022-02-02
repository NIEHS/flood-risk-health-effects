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



var dataset = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')
                  .filter(ee.Filter.date('2005-01-01', '2021-04-01'));

Map.setCenter(-115.356, 38.686, 4);
                  
                  
                  
var maximumTemperature = dataset.select('tmmx');

var tempVis = {
  min: 250.0,
  max: 320.0,
  palette: ['d8d8d8', '4addff', '5affa3', 'f2ff89', 'ff725c'],
};



var summer_tmmx_2020 = maximumTemperature.filter(ee.Filter.date('2020-06-20', '2020-09-23'));

var mean = summer_tmmx_2020.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2020');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2020', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2020 = maximumTemperature.filter(ee.Filter.date('2020-12-21', '2021-03-21'));

var mean = winter_tmmx_2020.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2020');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2020', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2019 = maximumTemperature.filter(ee.Filter.date('2019-06-21', '2019-09-24'));

var mean = summer_tmmx_2019.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2019');

// Export.image.toDrive({
//     image: mean,
//     description: 'summer_tmmx_2019', 
//     crs: 'EPSG:3857', 
//     scale: 4638.3, 
//     region: geometry
//   });



var winter_tmmx_2019 = maximumTemperature.filter(ee.Filter.date('2019-12-21', '2020-03-20'));

var mean = winter_tmmx_2019.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2019');


// Export.image.toDrive({
//     image: mean,
//     description: 'winter_tmmx_2019', 
//     crs: 'EPSG:3857', 
//     scale: 4638.3, 
//     region: geometry
//   });
  
  
  
var summer_tmmx_2018 = maximumTemperature.filter(ee.Filter.date('2018-06-21', '2018-09-23'));

var mean = summer_tmmx_2018.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2018');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2018', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2018 = maximumTemperature.filter(ee.Filter.date('2018-12-21', '2019-03-21'));

var mean = winter_tmmx_2018.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2018');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2018', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2017 = maximumTemperature.filter(ee.Filter.date('2017-06-21', '2017-09-23'));

var mean = summer_tmmx_2017.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2017');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2017', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2017 = maximumTemperature.filter(ee.Filter.date('2017-12-21', '2018-03-21'));

var mean = winter_tmmx_2017.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2017');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2017', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2016 = maximumTemperature.filter(ee.Filter.date('2016-06-20', '2016-09-23'));

var mean = summer_tmmx_2016.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2016');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2016', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2016 = maximumTemperature.filter(ee.Filter.date('2016-12-21', '2017-03-21'));

var mean = winter_tmmx_2016.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2016');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2016', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2015 = maximumTemperature.filter(ee.Filter.date('2015-06-21', '2015-09-24'));

var mean = summer_tmmx_2015.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2015');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2015', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2015 = maximumTemperature.filter(ee.Filter.date('2015-12-21', '2016-03-21'));

var mean = winter_tmmx_2015.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2015');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2015', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2014 = maximumTemperature.filter(ee.Filter.date('2014-06-21', '2014-09-23'));

var mean = summer_tmmx_2014.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2014');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2014', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2014 = maximumTemperature.filter(ee.Filter.date('2014-12-21', '2015-03-21'));

var mean = winter_tmmx_2014.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2014');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2014', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2013 = maximumTemperature.filter(ee.Filter.date('2013-06-21', '2013-09-23'));

var mean = summer_tmmx_2013.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2013');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2013', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2013 = maximumTemperature.filter(ee.Filter.date('2013-12-21', '2014-03-21'));

var mean = winter_tmmx_2013.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2013');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2013', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2012 = maximumTemperature.filter(ee.Filter.date('2012-06-20', '2012-09-23'));

var mean = summer_tmmx_2012.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2012');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2012', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2012 = maximumTemperature.filter(ee.Filter.date('2012-12-21', '2013-03-21'));

var mean = winter_tmmx_2012.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2012');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2012', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2011 = maximumTemperature.filter(ee.Filter.date('2011-06-21', '2011-09-24'));

var mean = summer_tmmx_2011.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2011');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2011', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2011 = maximumTemperature.filter(ee.Filter.date('2011-12-22', '2012-03-21'));

var mean = winter_tmmx_2011.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2011');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2011', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2010 = maximumTemperature.filter(ee.Filter.date('2010-06-21', '2010-09-23'));

var mean = summer_tmmx_2010.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2010');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2010', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2010 = maximumTemperature.filter(ee.Filter.date('2010-12-21', '2011-03-21'));

var mean = winter_tmmx_2010.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2010');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2010', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2009 = maximumTemperature.filter(ee.Filter.date('2009-06-21', '2009-09-23'));

var mean = summer_tmmx_2009.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2009');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2009', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2009 = maximumTemperature.filter(ee.Filter.date('2009-12-21', '2010-03-21'));

var mean = winter_tmmx_2009.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2009');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2009', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2008 = maximumTemperature.filter(ee.Filter.date('2008-06-20', '2008-09-23'));

var mean = summer_tmmx_2008.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2008');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2008', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2008 = maximumTemperature.filter(ee.Filter.date('2008-12-21', '2009-03-21'));

var mean = winter_tmmx_2008.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2008');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2008', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2007 = maximumTemperature.filter(ee.Filter.date('2007-06-21', '2007-09-24'));

var mean = summer_tmmx_2007.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2007');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2007', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2007 = maximumTemperature.filter(ee.Filter.date('2007-12-22', '2008-03-21'));

var mean = winter_tmmx_2007.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2007');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2007', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2006 = maximumTemperature.filter(ee.Filter.date('2006-06-21', '2006-09-24'));

var mean = summer_tmmx_2006.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2006');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2006', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2006 = maximumTemperature.filter(ee.Filter.date('2006-12-21', '2007-03-21'));

var mean = winter_tmmx_2006.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2006');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2006', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_tmmx_2005 = maximumTemperature.filter(ee.Filter.date('2005-06-21', '2005-09-24'));

var mean = summer_tmmx_2005.mean()

Map.addLayer(mean, tempVis, 'summer_tmmx_2005');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_tmmx_2005', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_tmmx_2005 = maximumTemperature.filter(ee.Filter.date('2005-12-21', '2006-03-21'));

var mean = winter_tmmx_2005.mean()

Map.addLayer(mean, tempVis, 'winter_tmmx_2005');


// Export.image.toDrive({
//   image: mean,
//   description: 'winter_tmmx_2005', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



////// Maximum relative humidity

var maximumHumidity = dataset.select('rmax');

var humidVis = {
  min: 0,
  max: 100,
  palette: ['d8d8d8', '4addff', '5affa3', 'f2ff89', 'ff725c'],
};



var summer_rmax_2020 = maximumHumidity.filter(ee.Filter.date('2020-06-20', '2020-09-23'));

var mean = summer_rmax_2020.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2020');

// Export.image.toDrive({
//   image: mean,
//   description: 'summer_rmax_2020', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var winter_rmax_2020 = maximumHumidity.filter(ee.Filter.date('2020-12-21', '2021-03-21'));

var mean = winter_rmax_2020.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2020');

// Export.image.toDrive({
//   image: mean,
//   description: 'winter_rmax_2020', 
//   crs: 'EPSG:3857', 
//   scale: 4638.3, 
//   region: geometry
// });



var summer_rmax_2019 = maximumHumidity.filter(ee.Filter.date('2019-06-21', '2019-09-24'));

var mean = summer_rmax_2019.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2019');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2019', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2019 = maximumHumidity.filter(ee.Filter.date('2019-12-21', '2020-03-20'));

var mean = winter_rmax_2019.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2019');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2019', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2018 = maximumHumidity.filter(ee.Filter.date('2018-06-21', '2018-09-23'));

var mean = summer_rmax_2018.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2018');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2018', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2018 = maximumHumidity.filter(ee.Filter.date('2018-12-21', '2019-03-21'));

var mean = winter_rmax_2018.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2018');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2018', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2017 = maximumHumidity.filter(ee.Filter.date('2017-06-21', '2017-09-23'));

var mean = summer_rmax_2017.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2017');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2017', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2017 = maximumHumidity.filter(ee.Filter.date('2017-12-21', '2018-03-21'));

var mean = winter_rmax_2017.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2017');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2017', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2016 = maximumHumidity.filter(ee.Filter.date('2016-06-20', '2016-09-23'));

var mean = summer_rmax_2016.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2016');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2016', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2016 = maximumHumidity.filter(ee.Filter.date('2016-12-21', '2017-03-21'));

var mean = winter_rmax_2016.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2016');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2016', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2015 = maximumHumidity.filter(ee.Filter.date('2015-06-21', '2015-09-24'));

var mean = summer_rmax_2015.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2015');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2015', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2015 = maximumHumidity.filter(ee.Filter.date('2015-12-21', '2016-03-21'));

var mean = winter_rmax_2015.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2015');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2015', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2014 = maximumHumidity.filter(ee.Filter.date('2014-06-21', '2014-09-23'));

var mean = summer_rmax_2014.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2014');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2014', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2014 = maximumHumidity.filter(ee.Filter.date('2014-12-21', '2015-03-21'));

var mean = winter_rmax_2014.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2014');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2014', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2013 = maximumHumidity.filter(ee.Filter.date('2013-06-21', '2013-09-23'));

var mean = summer_rmax_2013.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2013');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2013', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2013 = maximumHumidity.filter(ee.Filter.date('2013-12-21', '2014-03-21'));

var mean = winter_rmax_2013.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2013');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2013', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2012 = maximumHumidity.filter(ee.Filter.date('2012-06-20', '2012-09-23'));

var mean = summer_rmax_2012.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2012');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2012', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2012 = maximumHumidity.filter(ee.Filter.date('2012-12-21', '2013-03-21'));

var mean = winter_rmax_2012.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2012');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2012', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2011 = maximumHumidity.filter(ee.Filter.date('2011-06-21', '2011-09-24'));

var mean = summer_rmax_2011.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2011');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2011', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2011 = maximumHumidity.filter(ee.Filter.date('2011-12-22', '2012-03-21'));

var mean = winter_rmax_2011.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2011');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2011', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2010 = maximumHumidity.filter(ee.Filter.date('2010-06-21', '2010-09-23'));

var mean = summer_rmax_2010.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2010');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2010', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2010 = maximumHumidity.filter(ee.Filter.date('2010-12-21', '2011-03-21'));

var mean = winter_rmax_2010.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2010');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2010', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2009 = maximumHumidity.filter(ee.Filter.date('2009-06-21', '2009-09-23'));

var mean = summer_rmax_2009.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2009');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2009', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2009 = maximumHumidity.filter(ee.Filter.date('2009-12-21', '2010-03-21'));

var mean = winter_rmax_2009.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2009');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2009', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2008 = maximumHumidity.filter(ee.Filter.date('2008-06-20', '2008-09-23'));

var mean = summer_rmax_2008.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2008');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2008', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2008 = maximumHumidity.filter(ee.Filter.date('2008-12-21', '2009-03-21'));

var mean = winter_rmax_2008.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2008');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2008', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2007 = maximumHumidity.filter(ee.Filter.date('2007-06-21', '2007-09-24'));

var mean = summer_rmax_2007.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2007');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2007', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2007 = maximumHumidity.filter(ee.Filter.date('2007-12-22', '2008-03-21'));

var mean = winter_rmax_2007.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2007');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2007', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2006 = maximumHumidity.filter(ee.Filter.date('2006-06-21', '2006-09-24'));

var mean = summer_rmax_2006.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2006');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2006', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2006 = maximumHumidity.filter(ee.Filter.date('2006-12-21', '2007-03-21'));

var mean = winter_rmax_2006.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2006');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2006', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var summer_rmax_2005 = maximumHumidity.filter(ee.Filter.date('2005-06-21', '2005-09-24'));

var mean = summer_rmax_2005.mean()

Map.addLayer(mean, humidVis, 'summer_rmax_2005');

Export.image.toDrive({
  image: mean,
  description: 'summer_rmax_2005', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});



var winter_rmax_2005 = maximumHumidity.filter(ee.Filter.date('2005-12-21', '2006-03-21'));

var mean = winter_rmax_2005.mean()

Map.addLayer(mean, humidVis, 'winter_rmax_2005');


Export.image.toDrive({
  image: mean,
  description: 'winter_rmax_2005', 
  crs: 'EPSG:3857', 
  scale: 4638.3, 
  region: geometry
});





