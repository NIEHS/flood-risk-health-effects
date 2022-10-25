
library(here)
library(rgdal)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(viridis)
library(stringr)
library(geomerge)
library(tmap)

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
CHD.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3

load(here("modeling_files/all_census_tract_BPHIGH.RData"))
BPHIGH.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3

load(here("modeling_files/all_census_tract_CASTHMA.RData"))
CASTHMA.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3

load(here("modeling_files/all_census_tract_MHLTH.RData"))
MHLTH.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3



fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_fr_and_pollute_pc.rds"))

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

# Flipped the flood_risk_pc1 variable to make it a "risk" variable

outcome_df <- data.frame(GEOID10 = fips_leading_zero, 
                         CHD_prevalence_smoothed = CHD.mean.fitted, 
                         BPHIGH_prevalence_smoothed = BPHIGH.mean.fitted, 
                         CASTHMA_prevalence_smoothed = CASTHMA.mean.fitted, 
                         MHLTH_prevalence_smoothed = MHLTH.mean.fitted, 
                         flood_risk_pc1 = -fhs_model_df$flood_risk_pc1)

# truncating the occasional negative prevalence at zero
outcome_df$CHD_prevalence_smoothed <- pmax(outcome_df$CHD_prevalence_smoothed, 0)
outcome_df$BPHIGH_prevalence_smoothed <- pmax(outcome_df$BPHIGH_prevalence_smoothed, 0)
outcome_df$CASTHMA_prevalence_smoothed <- pmax(outcome_df$CASTHMA_prevalence_smoothed, 0)
outcome_df$MHLTH_prevalence_smoothed <- pmax(outcome_df$MHLTH_prevalence_smoothed, 0)

# merge outcome_df to all_ct_df's dataframe (@data)

all_ct_df@data <- left_join(all_ct_df@data, outcome_df)





# # plotting just for NC to test out code
# 
all_ct_df_NC <- all_ct_df[all_ct_df$STATEFP10 == 37, ]
# 
# p <- tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", palette = "viridis", title = "Coronary Heart Disease\nPrevalence", style = "cont")
# tmap_save(p, here("figures/final_figures/NC_test_tmap.pdf"))
# 
# # turn off boundaries and legend, use png with low res
# png(file = here("figures/final_figures/NC_test_tmap.png"), res = 100)
# tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", legend.show = F, palette = "viridis")
# dev.off()
# 
# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/NC_test_tmap.jpeg"), width = 700)
tm_shape(all_ct_df_NC) +
  tm_fill("CHD_prevalence_smoothed", palette = "viridis",
          title = "Coronary Heart Disease\nPrevalence", style = "cont")
dev.off()





# for whole continental U.S.
# Pdf seems better, png is too blurry



p <- tm_shape(all_ct_df) +
  tm_fill("CHD_prevalence_smoothed", palette = "viridis", title = "Coronary Heart Disease\nPrevalence", style = "cont", breaks = c(0, 5, 10, 15, 20, 25))
tmap_save(p, here("figures/final_figures/CHD_mean_fitted.pdf"))

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/CHD_mean_fitted.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CHD_prevalence_smoothed", palette = "viridis", 
          title = "Coronary Heart Disease\nPrevalence", style = "cont", breaks = c(0, 5, 10, 15, 20, 25))
dev.off()



p <- tm_shape(all_ct_df) +
  tm_fill("BPHIGH_prevalence_smoothed", palette = "viridis", title = "High Blood Pressure\nPrevalence", style = "cont", breaks = c(0, 16, 32, 48, 64, 80))
tmap_save(p, here("figures/final_figures/BPHIGH_mean_fitted.pdf"))

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/BPHIGH_mean_fitted.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("BPHIGH_prevalence_smoothed", palette = "viridis", 
          title = "High Blood Pressure\nPrevalence", style = "cont", breaks = c(0, 16, 32, 48, 64, 80))
dev.off()



p <- tm_shape(all_ct_df) +
  tm_fill("CASTHMA_prevalence_smoothed", palette = "viridis", title = "Asthma\nPrevalence", style = "cont", breaks = c(0, 4, 8, 12, 16, 20))
tmap_save(p, here("figures/final_figures/CASTHMA_mean_fitted.pdf"))

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/CASTHMA_mean_fitted.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CASTHMA_prevalence_smoothed", palette = "viridis", 
          title = "Asthma\nPrevalence", style = "cont", breaks = c(0, 4, 8, 12, 16, 20))
dev.off()



p <- tm_shape(all_ct_df) +
  tm_fill("MHLTH_prevalence_smoothed", palette = "viridis", title = "Poor Mental Health\nPrevalence", style = "cont", breaks = c(0, 7, 14, 21, 28, 35))
tmap_save(p, here("figures/final_figures/MHLTH_mean_fitted.pdf"))

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/MHLTH_mean_fitted.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("MHLTH_prevalence_smoothed", palette = "viridis", 
          title = "Poor Mental Health\nPrevalence", style = "cont", breaks = c(0, 7, 14, 21, 28, 35))
dev.off()



# flood risk PC1, or "consistent flood risk"
p <- tm_shape(all_ct_df) +
  tm_fill("flood_risk_pc1", palette = "viridis", title = "Consistent Flood Risk Score", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26))
tmap_save(p, here("figures/final_figures/consistent_flood_risk_fitted.pdf"))

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/consistent_flood_risk_fitted.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("flood_risk_pc1", palette = "viridis", 
          title = "Consistent Flood Risk Score", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26))
dev.off()





# # below chunk of code takes too long
# 
# all_ct_df@data <- left_join(all_ct_df@data, outcome_df, by = "GEOID10")
# 
# all_ct_df2 <- broom::tidy(all_ct_df, region = "GEOID10")
# 
# 
# 
# all_ct_df2 <- all_ct_df2 %>% left_join(all_ct_df@data, by = c("id" = "GEOID10"))
# 
# map_all_ct_CHD <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = CHD_prevalence_smoothed)) + theme_void() + 
#   scale_fill_viridis()
# 
# map_all_ct_CHD




