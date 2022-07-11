
library(here)
library(rgdal)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(viridis)
library(stringr)

# first test if you can get blank plot, with reduced dpi



# using 2010 census tract boundaries

ct_files <- list.files(here("imported_data/census_tract_shapefiles/"))

shp_list <- vector("list", length = length(ct_files))

for (i in 1:length(ct_files)) {
  shp_list[[i]] <- readOGR(dsn = here("imported_data/census_tract_shapefiles", ct_files[i], paste0(ct_files[i], ".shp")), verbose = F)
}

# I use 2019 census tract boundaries for Virginia and South Dakota, because some of their 
# fips codes changed in the 2010s

# making names consistent between 2019 versions and 2010 versions

names(shp_list[[which(ct_files == "tl_2019_46_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])
names(shp_list[[which(ct_files == "tl_2019_51_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])

all_ct_df <- do.call("rbind", shp_list)





# plotting the choropleth for smoothed CHD prevalence

load(here("modeling_files/all_census_tract_intrinsic.RData"))

mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3



fhs_model_df <- readRDS("intermediary_data/fhs_model_df_all_census_tract_pc.rds")

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

outcome_df <- data.frame(GEOID10 = fips_leading_zero, Prevalence = mean.fitted)



all_ct_df@data <- left_join(all_ct_df@data, outcome_df, by = "GEOID10")

all_ct_df2 <- broom::tidy(all_ct_df, region = "GEOID10")



all_ct_df2 <- all_ct_df2 %>% left_join(all_ct_df@data, by = c("id" = "GEOID10"))

map_all_ct_CHD <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = Prevalence)) + theme_void() + 
  scale_fill_viridis()



# map_all_ct_CHD


