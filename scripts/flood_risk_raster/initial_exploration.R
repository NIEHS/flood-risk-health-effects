library(raster)
library(here)

r <- raster(here("imported_data/flood_risk_raster/n48w124_2050_0p50_1in500.tif"))

plot(r)


