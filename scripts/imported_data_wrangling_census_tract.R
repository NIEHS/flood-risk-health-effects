
library(here)
library(readxl)
library(stringr)
library(tidyverse)
library(tidyr)
library(Matrix)

library(sf)

select <- dplyr::select

i_am("scripts/imported_data_wrangling.R")



# using 2010 census tract boundaries

ct_files <- list.files(here("imported_data/census_tract_shapefiles/"))

shp_list <- vector("list", length = length(ct_files))

for (i in 1:length(ct_files)) {
  shp_list[[i]] <- st_read(dsn = here("imported_data/census_tract_shapefiles", ct_files[i], paste0(ct_files[i], ".shp")), quiet = T)
}

# I use 2019 census tract boundaries for Virginia and South Dakota, because some of their 
# fips codes changed in the 2010s

# making names consistent between 2019 versions and 2010 versions

names(shp_list[[which(ct_files == "tl_2019_46_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])
names(shp_list[[which(ct_files == "tl_2019_51_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])

all_ct_df <- do.call("rbind", shp_list)





# work with the PLACES dataset
# https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh 

places_dat <- read.csv(here("imported_data", 
                            "PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2020_release.csv"))

# don't need the year, state_abbr, lat or lon

places_subset <- dplyr::select(places_dat, -c(Year, StateAbbr, StateDesc, LocationName, 
                                              DataSource, Category, Data_Value_Unit, Data_Value_Type, 
                                              Data_Value_Footnote_Symbol, Data_Value_Footnote, 
                                              Geolocation, DataValueTypeID))

# convert from long to wide format

places_dat_wide <- pivot_wider(places_subset, id_cols = c(LocationID, CountyFIPS, TotalPopulation),
                               names_from = MeasureId, 
                               values_from = c(Data_Value, Low_Confidence_Limit, High_Confidence_Limit))

places_dat_wide <- rename(places_dat_wide, fips = LocationID)



# reading in the CDC SVI data
cdc_svi <- read.csv(here("imported_data", "CDC_SVI", "SVI2018_US.csv"))

# Lowercase the FIPS
cdc_svi <- rename(cdc_svi, fips = FIPS)

# take care of the -999 missing value indicators
cdc_svi[cdc_svi == -999] <- NA





# CACES LUR air pollution data

# Since CACES doesn't like files with more than 5k fips, 
# I'm using data provided by Melissa Lowe

caces_lur <- read.csv(here("imported_data/caces_lur_air_pollution/caces_lur_air_pollution_census_tract.csv"))

# don't need the state_abbr, lat or lon

caces_lur_subset <- dplyr::select(caces_lur, -c(state_abbr, lat, lon))

# convert from long to wide format

caces_lur_wide <- spread(caces_lur_subset, pollutant, pred_wght)

caces_lur_summ <- caces_lur_wide %>% group_by(fips) %>% summarise(co = mean(co), no2 = mean(no2), o3 = mean(o3), 
                                                                  pm10 = mean(pm10), pm25 = mean(pm25), 
                                                                  so2 = mean(so2))

# correct the fips for Virginia and South Dakota

caces_lur_summ$fips[caces_lur_summ$fips %/% 1e6 == 46113] <- caces_lur_summ$fips[caces_lur_summ$fips %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
caces_lur_summ$fips[caces_lur_summ$fips %/% 1e6 == 51515] <- caces_lur_summ$fips[caces_lur_summ$fips %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)

saveRDS(caces_lur_summ, file = here("intermediary_data/caces_lur_summ_census_tract.rds"))



caces_lur_summ <- readRDS(file = here("intermediary_data/caces_lur_summ_census_tract.rds"))



#####

# merge all three datasets together by their fips

ct_fips <- data.frame(fips = as.numeric(all_ct_df$GEOID10))

ct_health <- left_join(ct_fips, places_dat_wide)



extracted_fr <- readRDS(file = here("intermediary_data/extracted_fr.rds"))

flood_health <- left_join(ct_health, extracted_fr, by = c("fips" = "tract_fips"))



# then merging with SVI 
flood_health_svi <- left_join(flood_health, cdc_svi, by = "fips")

# then merging with air pollution
flood_health_svi <- left_join(flood_health_svi, caces_lur_summ, by = "fips")



# remove census tracts in Alaska and Hawaii

flood_health_svi <- flood_health_svi[!(flood_health_svi$STATE %in% c("ALASKA", "HAWAII")), ]



# merging with GRIDMET data

mean_df_GRIDMET <- readRDS(file = here("intermediary_data/mean_df_GRIDMET.rds"))

mean_df_GRIDMET$fips <- as.numeric(mean_df_GRIDMET$fips)

# correction of SD and VA fips 
mean_df_GRIDMET$fips[mean_df_GRIDMET$fips %/% 1e6 == 46113] <- mean_df_GRIDMET$fips[mean_df_GRIDMET$fips %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
mean_df_GRIDMET$fips[mean_df_GRIDMET$fips %/% 1e6 == 51515] <- mean_df_GRIDMET$fips[mean_df_GRIDMET$fips %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)

flood_health_svi <- left_join(flood_health_svi, mean_df_GRIDMET, by = "fips")



# save the dataset

saveRDS(flood_health_svi, file = here("intermediary_data/flood_health_svi_all_census_tract.rds")) 





# Removing redundant columns, moving id columns to the left

flood_health_svi <- readRDS(file = here("intermediary_data/flood_health_svi_all_census_tract.rds"))

# remove places_dat variables other than Data_Value_CHD
# this also puts the outcome variable as the last variable
fhs_outcome_subset <- flood_health_svi %>% dplyr::select(!(starts_with("Data_Value") | starts_with("Low_Confidence_Limit") | starts_with("High_Confidence_Limit")) |
                                                           Data_Value_CSMOKING |
                                                           Data_Value_CHD | 
                                                           Data_Value_CASTHMA | 
                                                           Data_Value_BPHIGH | 
                                                           Data_Value_MHLTH)



# Reorganizing the CDC SVI variables
# removing the margins of errors for now
# focusing on the EP_ variables for now
fhs_svi_subset <- fhs_outcome_subset %>% 
  relocate(ST, STATE, ST_ABBR, STCNTY, COUNTY,
           LOCATION, AREA_SQMI, E_TOTPOP, 
           E_HU, E_HH,
           RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES,
           .after = TotalPopulation) %>%
  select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "F_")))



fhs_model_df <- fhs_svi_subset

saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_all_census_tract.rds"))



####################

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract.rds"))



# Constructing an edge list (i.e. matrix of triplets) using sparseMatrix in R



# making matrix using sparseMatrix rather than making dense numeric matrix, 

census_tract_adjacency <- read.csv(here("imported_data/tract10co/nlist_2010.csv"))

# correction of SD and VA fips 
census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 46113] <- census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 51515] <- census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)

census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 46113] <- census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 51515] <- census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)



census_tract_fips <- unique(census_tract_adjacency$SOURCE_TRACTID)



# Next seven lines take quite a while. 
# Can skip the construction of adjacency matrix and read it in instead

row_idx_vec <- sapply(census_tract_adjacency$SOURCE_TRACTID, function(fip) {which(census_tract_fips == fip)})
col_idx_vec <- sapply(census_tract_adjacency$NEIGHBOR_TRACTID, function(fip) {which(census_tract_fips == fip)})

census_tract_adj <- sparseMatrix(row_idx_vec, col_idx_vec)

saveRDS(census_tract_adj, file = here("intermediary_data", "census_tract_adj_all.rds"))



# final processing of census_tract_adj and fhs_model_df

census_tract_adj <- readRDS(here("intermediary_data", "census_tract_adj_all.rds"))



# There are 2 fips in fhs_model_df not present in the Diversity and Disparities adjacency file.
missing_fips <- fhs_model_df$fips[!(fhs_model_df$fips %in% census_tract_fips)]

# Let's omit those fips from fhs_model_df.
fhs_model_df <- fhs_model_df[!(fhs_model_df$fips %in% missing_fips), ]



reorganize_idx <- match(fhs_model_df$fips, census_tract_fips) 



census_tract_adj_reorganize <- census_tract_adj[, reorganize_idx]

census_tract_adj_reorganize <- census_tract_adj_reorganize[reorganize_idx, ]

census_tract_fips <- census_tract_fips[reorganize_idx]



saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_all_census_tract_reorg.rds"))



saveRDS(census_tract_adj_reorganize, file = here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))





# Replacing flood risk variables with the PCs

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_reorg.rds"))

first_var <- 19

fr_index <- first_var:(first_var + 21)

flood_risk <- fhs_model_df[, fr_index] 

fr_pca <- prcomp(flood_risk[complete.cases(flood_risk),], center = T, scale. = T)



summ_pca <- summary(fr_pca)

summ_pca$importance[,1:10] # The first 5 PCs cover 80% of the variance. 

num_pc <- 5

flood_pcs <- matrix(NA, nrow = nrow(fhs_model_df), ncol = num_pc)

flood_pcs[complete.cases(flood_risk), ] <- fr_pca$x[, 1:num_pc]

flood_pcs <- data.frame(flood_pcs)

names(flood_pcs) <- paste0("flood_risk_pc", 1:ncol(flood_pcs))



# dimensionality reduction
fhs_model_df <- fhs_model_df[, -fr_index]

fhs_model_df <- data.frame(fhs_model_df, flood_pcs)

fhs_model_df <- fhs_model_df %>% relocate(starts_with("flood_risk_pc"), .after = RPL_THEMES)


saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_all_census_tract_pc.rds"))


