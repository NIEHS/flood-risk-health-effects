
library(here)
library(readxl)
library(stringr)
library(tidyverse)
library(tidyr)
library(Matrix)
library(stringr)

select <- dplyr::select

i_am("scripts/imported_data_wrangling.R")



library(sf)

# using 2010 census tract boundaries

ct_files <- list.files(here("imported_data/census_tract_shapefiles/"))

shp_list <- vector("list", length = length(ct_files))

for (i in 1:length(ct_files)) {
  shp_list[[i]] <- st_read(dsn = here("imported_data/census_tract_shapefiles", ct_files[i], paste0(ct_files[i], ".shp")), quiet = T)
}

# I use 2019 census tract boundaries for Virginia and South Dakota, because some of their 
# fips codes changed in the 2010s

names(shp_list[[48]]) <- names(shp_list[[1]])
names(shp_list[[49]]) <- names(shp_list[[1]])

all_ct_df <- do.call("rbind", shp_list)





# reading in the zip code flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "Zip_level_risk_FEMA_FSF_v1.3.csv"), 
                       colClasses = c("character", rep(NA, 33)))

# adds leading zero to the front of less-than-5-digits zip codes, to make all zip codes length 5
flood_risk$zipcode <- sprintf("%05s", flood_risk$zipcode)

flood_risk_prev <- flood_risk

count_ff_mat <- diag(1 / flood_risk$count_property) %*% as.matrix(select(flood_risk, starts_with("count_floodfactor"))) * 100

colnames(count_ff_mat) <- str_replace(colnames(count_ff_mat), "count", "pct")

flood_risk <- data.frame(flood_risk_prev, count_ff_mat)

# # make percent change variables
# flood_risk <- flood_risk %>% 
#   mutate(pct_change_5 = (pct_fs_risk_2050_5 - pct_fs_risk_2020_5) / pct_fs_risk_2020_5, 
#          pct_change_100 = (pct_fs_risk_2050_100 - pct_fs_risk_2020_100) / pct_fs_risk_2020_100,
#          pct_change_500 = (pct_fs_risk_2050_500 - pct_fs_risk_2020_500) / pct_fs_risk_2020_500)

saveRDS(flood_risk, file = here("intermediary_data/flood_risk_pct_ff.rds")) 



flood_risk <- readRDS(file = here("intermediary_data/flood_risk_pct_ff.rds"))



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





# reading in the ZCTA crosswalk

# use colClasses to read identifiers with leading zeros

zcta_crosswalk <- read.csv(here("imported_data", "zcta_crosswalk", "zcta_tract_rel_10.txt"), 
                           colClasses = c(rep("character", 5), rep("numeric", 20)))

zcta_crosswalk$GEOID <- as.numeric(zcta_crosswalk$GEOID)

# correct the fips for Virginia and South Dakota

zcta_crosswalk$GEOID[zcta_crosswalk$GEOID %/% 1e6 == 46113] <- zcta_crosswalk$GEOID[zcta_crosswalk$GEOID %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
zcta_crosswalk$GEOID[zcta_crosswalk$GEOID %/% 1e6 == 51515] <- zcta_crosswalk$GEOID[zcta_crosswalk$GEOID %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)



# I focus on TRHUPCT, "The Percentage of Total Housing Unit Count of the 2010 Census Tract represented by the record" 
# to merge the flood risk zip code data with the rest of the data in terms of census tracts

# mini EDA
trhupct_summary<- zcta_crosswalk %>% group_by(GEOID) %>% summarise(trhupct_sum = sum(TRHUPCT), trpoppct_sum = sum(TRPOPPCT), 
                                                                   trareapct_sum = sum(TRAREAPCT))

# most census tracts are wholly accounted for by the zip codes.
mean(trhupct_summary$trhupct_sum >= 99)

# all the flood risk zip codes are accounted for within the crosswalk.
all(flood_risk$zipcode %in% zcta_crosswalk$ZCTA5)

# There are some fips in the census tracts df not present in the ZCTA crosswalk. This will lead to some missing
# flood risk variables. 

mean(ct_health$fips %in% unique(zcta_crosswalk$GEOID))

# approach: take a weighted mean of the non-missing flood risk values of the ZCTAs within each tract.

flood_risk_colnames_subset <- colnames(flood_risk)[(startsWith(colnames(flood_risk), "pct_") | 
                                                      startsWith(colnames(flood_risk), "avg_risk_")) & 
                                                     !endsWith(colnames(flood_risk), "fs_fema_difference_2020") & 
                                                     !endsWith(colnames(flood_risk), "fema_sfha")]

merged_flood_risk_mat <- matrix(NA, nrow = length(all_ct_df$GEOID10), ncol = length(flood_risk_colnames_subset))

no_f_dat <- 0

merged_mat_idx <- 1

for (fip in ct_health$fips) {
  
  one_tract_mult_zip <- zcta_crosswalk[zcta_crosswalk$GEOID == fip, names(zcta_crosswalk) %in% c("ZCTA5", "TRHUPCT")]
  
  one_tract_mult_zip <- rename(one_tract_mult_zip, zipcode = ZCTA5)
  
  one_tract_mult_zip_flood_risk <- merge(one_tract_mult_zip, flood_risk, by = "zipcode")
  
  # # zip codes making up the tract
  # zips <- one_tract_mult_zip$ZCTA5
  
  # # corresponding TRHUPCT, i.e. percentage of housing units covered by zip code within the tract,
  # # turned back into decimal
  # trhupcts <- one_tract_mult_zip$TRHUPCT[one_tract_mult_zip$ZCTA5 %in% zip_flood_risk_mat$zipcode] / 100
  
  
  
  col_idx <- 1
  
  for (coln in flood_risk_colnames_subset) {
    
    if (nrow(one_tract_mult_zip_flood_risk) == 0) {
      
      merged_flood_risk_mat[merged_mat_idx, col_idx] <- NaN
      
      no_f_dat <- no_f_dat + 1
      
    } else {
      
      merged_flood_risk_mat[merged_mat_idx, col_idx] <- sum(one_tract_mult_zip_flood_risk[coln] * 
                                                              one_tract_mult_zip_flood_risk$TRHUPCT, na.rm = T) / 
        sum(one_tract_mult_zip_flood_risk$TRHUPCT[!is.na(one_tract_mult_zip_flood_risk[coln])])
      
    }
    
    col_idx <- col_idx + 1
    
  }
  
  merged_mat_idx <- merged_mat_idx + 1
  
}

colnames(merged_flood_risk_mat) <- flood_risk_colnames_subset

saveRDS(merged_flood_risk_mat, file = here("intermediary_data/merged_flood_risk_mat_all_census_tract.rds")) 



merged_flood_risk_mat <- readRDS(here("intermediary_data/merged_flood_risk_mat_all_census_tract.rds"))

  

flood_health <- data.frame(ct_health, merged_flood_risk_mat)



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
# TBC: selecting only cardiovascular outcomes
fhs_outcome_subset <- flood_health_svi %>% dplyr::select(!(starts_with("Data_Value") | starts_with("Low_Confidence_Limit") | starts_with("High_Confidence_Limit")) |
                                                           Data_Value_CSMOKING |
                                                           Data_Value_CHD)



# Reorganizing the CDC SVI variables
# removing the margins of errors for now
# focusing on the EP_ variables for now
fhs_svi_subset <- fhs_outcome_subset %>% 
  relocate(ST, STATE, ST_ABBR, STCNTY, COUNTY,
           LOCATION, AREA_SQMI, E_TOTPOP, 
           E_HU, E_HH, .after = TotalPopulation) %>%
  select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "RPL_", "F_")))



fhs_model_df <- fhs_svi_subset

saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_all_census_tract.rds"))



####################

# Constructing an edge list (i.e. matrix of triplets) using sparseMatrix in R



# making matrix using sparseMatrix rather than making dense numeric matrix, 

census_tract_adjacency <- read.csv(here("imported_data/tract10co/nlist_2010.csv"))

# correction of SD and VA fips 
census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 46113] <- census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 51515] <- census_tract_adjacency$SOURCE_TRACTID[census_tract_adjacency$SOURCE_TRACTID %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)

census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 46113] <- census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 46113] %% (46113 * 1e6) + (46102 * 1e6)
census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 51515] <- census_tract_adjacency$NEIGHBOR_TRACTID[census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e6 == 51515] %% (51515 * 1e6) + (51019 * 1e6)



census_tract_fips <- unique(census_tract_adjacency$SOURCE_TRACTID)



row_idx_vec <- sapply(census_tract_adjacency$SOURCE_TRACTID, function(fip) {which(census_tract_fips == fip)})
col_idx_vec <- sapply(census_tract_adjacency$NEIGHBOR_TRACTID, function(fip) {which(census_tract_fips == fip)})



census_tract_adj <- sparseMatrix(row_idx_vec, col_idx_vec)



saveRDS(census_tract_adj, file = here("intermediary_data", "census_tract_adj_all.rds"))



# final processing of census_tract_adj and fhs_model_df

census_tract_adj <- readRDS(here("intermediary_data", "census_tract_adj_all.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract.rds"))



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

flood_risk <- fhs_model_df[, c(14:23, 25:35)] # omitting the 24th variable, avg_risk_score_sfha, because of too many NAs

fr_pca <- prcomp(flood_risk[complete.cases(flood_risk),], center = T, scale. = T)



summ_pca <- summary(fr_pca)

summ_pca$importance[,1:10] # The first 4 PCs cover 80% of the variance. 

flood_pcs <- matrix(NA, nrow = nrow(fhs_model_df), ncol = 4)

flood_pcs[complete.cases(flood_risk), ] <- fr_pca$x[, 1:4]

flood_pcs <- data.frame(flood_pcs)

names(flood_pcs) <- paste0("flood_risk_pc", 1:ncol(flood_pcs))



# dimensionality reduction
fhs_model_df <- fhs_model_df[, -c(14:35)]

fhs_model_df <- data.frame(fhs_model_df, flood_pcs)

fhs_model_df <- fhs_model_df %>% relocate(flood_risk_pc1, flood_risk_pc2, flood_risk_pc3, flood_risk_pc4, .after = E_HH)


saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_all_census_tract_pc.rds"))



####################

# Partitioning the U.S. into several regions, to estimate rho parameter via divide-and-conquer

# # Setting up for bigDM package
# install.packages("devtools")
# library(devtools)
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# install_github("spatialstatisticsupna/bigDM")
# install.packages("lwgeom")

library(bigDM)
library(tmap)
library(lwgeom)

rand_carto <- random_partition(all_ct_df, max.size = NULL) 

tm_shape(rand_carto) +
  tm_polygons(col="ID.group") +
  tm_layout(legend.show=FALSE)






fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_pc.rds"))

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))



ne_states <- c(23, 50, 33, 25, 9, 44, 36, 34)

names(ne_states) <- c("ME", "VT", "NH", "MA", "CT", "RI", "NY", "NJ")

fhs_ne <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% ne_states, ]

W_ne <- W[(fhs_model_df$fips %/% 1e9) %in% ne_states, (fhs_model_df$fips %/% 1e9) %in% ne_states]



se_states <- c(37, 45, 47, 13, 1, 28, 12)

names(se_states) <- c("NC", "SC", "TN", "GA", "AL", "MS", "FL")

fhs_se <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% se_states, ]

W_se <- W[(fhs_model_df$fips %/% 1e9) %in% se_states, (fhs_model_df$fips %/% 1e9) %in% se_states]



at_states <- c(42, 10, 24, 11, 51, 54, 39, 21)

names(at_states) <- c("PA", "DE", "MD", "DC", "VA", "WV", "OH", "KY")

fhs_at <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% at_states, ]

W_at <- W[(fhs_model_df$fips %/% 1e9) %in% at_states, (fhs_model_df$fips %/% 1e9) %in% at_states]



mw_states <- c(26, 18, 17, 55, 29, 5)

names(mw_states) <- c("MI", "IN", "IL", "WI", "MO", "AR")

fhs_mw <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% mw_states, ]

W_mw <- W[(fhs_model_df$fips %/% 1e9) %in% mw_states, (fhs_model_df$fips %/% 1e9) %in% mw_states]



nw_states <- c(27, 19, 38, 46, 31, 20, 30, 56, 16, 53, 41)

names(nw_states) <- c("MN", "IA", "ND", "SD", "NE", "KS", "MT", "WY", "ID", "WA", "OR")

fhs_nw <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% nw_states, ]

W_nw <- W[(fhs_model_df$fips %/% 1e9) %in% nw_states, (fhs_model_df$fips %/% 1e9) %in% nw_states]



sw_states <- c(22, 48, 40, 35, 4, 8, 49)

names(sw_states) <- c("LA", "TX", "OK", "NM", "AZ", "CO", "UT")

fhs_sw <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% sw_states, ]

W_sw <- W[(fhs_model_df$fips %/% 1e9) %in% sw_states, (fhs_model_df$fips %/% 1e9) %in% sw_states]



we_states <- c(6, 32)

names(we_states) <- c("CA", "NV")

fhs_we <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% we_states, ]

W_we <- W[(fhs_model_df$fips %/% 1e9) %in% we_states, (fhs_model_df$fips %/% 1e9) %in% we_states]




