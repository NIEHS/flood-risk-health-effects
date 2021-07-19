
library(here)
library(readxl)
library(stringr)
library(tidyverse)
library(tidyr)
library(Matrix)
library(stringr)

select <- dplyr::select

i_am("scripts/imported_data_wrangling.R")





# reading in the zip code flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "Zip_level_risk_FEMA_FSF_v1.3.csv"), 
                       colClasses = c("character", rep(NA, 33)))

flood_risk$zipcode <- sprintf("%05s", flood_risk$zipcode)

flood_risk_prev <- flood_risk

count_ff_mat <- diag(1 / flood_risk$count_property) %*% as.matrix(select(flood_risk, starts_with("count_floodfactor"))) * 100

colnames(count_ff_mat) <- str_replace(colnames(count_ff_mat), "count", "pct")

flood_risk <- data.frame(flood_risk_prev, count_ff_mat)

saveRDS(flood_risk, file = here("intermediary_data/flood_risk_pct_ff.rds")) 



flood_risk <- readRDS(file = here("intermediary_data/flood_risk_pct_ff.rds"))



# work with the PLACES dataset

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

# TBC: focusing on NC census tracts for now

places_dat_wide <- places_dat_wide[places_dat_wide$fips %/% 1e9 == 37, ]





# reading in the CDC SVI data
cdc_svi <- read.csv(here("imported_data", "CDC_SVI", "SVI2018_US.csv"))

# Lowercase the FIPS
cdc_svi <- rename(cdc_svi, fips = FIPS)

# take care of the -999 missing value indicators
cdc_svi[cdc_svi == -999] <- NA

# TBC: focusing on NC census tracts for now

cdc_svi <- cdc_svi[cdc_svi$fips %/% 1e9 == 37, ]



# can grab smoking prevalence data from PLACES dataset





# CACES LUR air pollution data

# # Extracting the list of county fips in the dataset, for CACES data extraction
# 
# fips <- as.character(places_dat_wide$fips)
# 
# # TBC: fips just for North Carolina
# fips <- as.character(places_dat_wide$fips[places_dat_wide$fips %/% 1e9 == 37])
# 
# # TBC: figure out what happens to census tracts in Oglala County
# # # switch fip for Oglala County, since CACES uses outdated fips
# # fips[fips == 46102] <- 46113
# 
# fips_leading_zero <- sapply(fips, FUN = function(fip) {
#   if (str_length(fip) == 10) {paste0("0", fip)}
#   else {fip}
# })
# 
# # TBC: list of fips just for North Carolina
# write.csv(fips_leading_zero, file = here("intermediary_data/census_tract_fips_NC.txt"),
#           row.names = FALSE)
# 
# # CACES doesn't like files with more than 5k fips
# write.csv(fips_leading_zero, file = here("intermediary_data/census_tract_fips.txt"),
#           row.names = FALSE)

# reading in the downloaded data from
# https://s3.amazonaws.com/files.airclimateenergy.org/caces/uwc162557682462152e2a39746642243d77e984d07dd562d.zip



# Since CACES doesn't like files with more than 5k fips, 
# I'm using data provided by Melissa Lowe

caces_lur <- read.csv(here("imported_data/caces_lur_air_pollution/caces_lur_air_pollution_census_tract.csv"))

# extract 2015 data

caces_lur_2015 <- caces_lur[caces_lur$year == 2015,]

# don't need the year, state_abbr, lat or lon

caces_lur_subset <- dplyr::select(caces_lur_2015, -c(year, state_abbr, lat, lon))

# convert from long to wide format

caces_lur_wide <- spread(caces_lur_subset, pollutant, pred_wght)

saveRDS(caces_lur_wide, file = here("intermediary_data/caces_lur_wide_census_tract.rds")) 



caces_lur_wide <- readRDS(file = here("intermediary_data/caces_lur_wide_census_tract.rds"))



#####

# merge all three datasets together by their fips

# reading in the ZCTA crosswalk

# use colClasses to read identifiers with leading zeros

zcta_crosswalk <- read.csv(here("imported_data", "zcta_crosswalk", "zcta_tract_rel_10.txt"), 
                           colClasses = c(rep("character", 5), rep("numeric", 20)))

# I focus on TRHUPCT, "The Percentage of Total Housing Unit Count of the 2010 Census Tract represented by the record" 
# to merge the flood risk zip code data with the rest of the data in terms of census tracts

# mini EDA
trhupct_summary<- zcta_crosswalk %>% group_by(GEOID) %>% summarise(trhupct_sum = sum(TRHUPCT), trpoppct_sum = sum(TRPOPPCT), 
                                                                   trareapct_sum = sum(TRAREAPCT))

# most census tracts are wholly accounted for by the zip codes.
mean(trhupct_summary$trhupct_sum >= 99)

# all the flood risk zip codes are accounted for within the crosswalk.
all(flood_risk$zipcode %in% zcta_crosswalk$ZCTA5)

# approach: take a weighted mean of the non-missing flood risk values of the ZCTAs within each tract.

flood_risk_colnames_subset <- colnames(flood_risk)[(startsWith(colnames(flood_risk), "pct_") | 
                                                      startsWith(colnames(flood_risk), "avg_risk_")) & 
                                                     !endsWith(colnames(flood_risk), "fs_fema_difference_2020") & 
                                                     !endsWith(colnames(flood_risk), "fema_sfha")]

merged_flood_risk_mat <- matrix(NA, nrow = nrow(places_dat_wide), ncol = length(flood_risk_colnames_subset))

no_f_dat <- 0

merged_mat_idx <- 1

for (fip in places_dat_wide$fips) {
  
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

saveRDS(merged_flood_risk_mat, file = here("intermediary_data/merged_flood_risk_mat_NC_census_tract.rds")) 



flood_health <- data.frame(places_dat_wide, merged_flood_risk_mat)



# then merging with SVI 
flood_health_svi <- merge(flood_health, cdc_svi, all.x = T, by = "fips")

# then merging with air pollution
flood_health_svi <- merge(flood_health_svi, caces_lur_wide, all.x = T, by = "fips")



# remove counties in Alaska and Hawaii

flood_health_svi <- flood_health_svi[!(flood_health_svi$STATE %in% c("ALASKA", "HAWAII")), ]



# save the dataset

# TBC: NC census tracts for now

saveRDS(flood_health_svi, file = here("intermediary_data/flood_health_svi_NC_census_tract.rds")) 





# Removing redundant columns, moving id columns to the left

flood_health_svi <- readRDS(file = here("intermediary_data/flood_health_svi_NC_census_tracts.rds"))

# remove places_dat variables other than Data_Value_CHD
# this also puts the outcome variable as the last variable
# TBC: selecting only cardiovascular outcomes
fhs_outcome_subset <- flood_health_svi %>% dplyr::select(!(starts_with("Data_Value") | starts_with("Low_Confidence_Limit") | starts_with("High_Confidence_Limit")) | Data_Value_CSMOKING | Data_Value_CHD)

# TBC: also delete the count_floodfactor* variables, dplyr::select(!(starts_with("count_fs") | starts_with("count_floodfactor")))
# Deleting and reorganizing some flood risk variables
fhs_flood_risk_subset <- fhs_outcome_subset %>% dplyr::select(!starts_with("count_fs")) %>% 
  relocate(pct_fs_fema_difference_2020, .before = pct_fs_risk_2020_5)

# Reorganizing the CDC SVI variables
# removing the margins of errors for now
# focusing on the EP_ variables for now
fhs_svi_subset <- fhs_flood_risk_subset %>% 
  relocate(ST, STATE, ST_ABBR, STCNTY, COUNTY,
           LOCATION, AREA_SQMI, E_TOTPOP, 
           E_HU, E_HH, .after = pct_fs_fema_difference_2020) %>%
  select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "RPL_", "F_")))



fhs_model_df <- fhs_svi_subset

# TBC: NC census tract version
saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df_NC_census_tract.rds"))

saveRDS(fhs_model_df, file = here("intermediary_data/fhs_model_df.rds"))



####################

# TBC: make an edge list instead of adjacency matrix

# making the census tract adjacency matrix from the census tract adjacency file provided by 
# the Diversity and Disparities website
# (https://s4.ad.brown.edu/Projects/Diversity/Researcher/Pooling.htm)

census_tract_adjacency <- read.csv(here("imported_data/tract10co/nlist_2010.csv"))

census_tract_fips <- unique(census_tract_adjacency$SOURCE_TRACTID)

# TBC: focusing on North Carolina for now
census_tract_adjacency <- census_tract_adjacency[(census_tract_adjacency$SOURCE_TRACTID %/% 1e9) == 37,]
census_tract_adjacency <- census_tract_adjacency[(census_tract_adjacency$NEIGHBOR_TRACTID %/% 1e9) == 37,]
census_tract_fips <- census_tract_fips[(census_tract_fips %/% 1e9) == 37]

census_tract_adj <- matrix(0, nrow = length(census_tract_fips), ncol = length(census_tract_fips))

row.names(census_tract_adj) <- census_tract_fips
colnames(census_tract_adj) <- census_tract_fips

start_idx <- 1

for (k in 1:length(census_tract_fips)) {
  
  start_idx <- which(census_tract_adjacency$SOURCE_TRACTID == census_tract_fips[k])[1]
  
  end_idx <- start_idx
  
  while (census_tract_adjacency$SOURCE_TRACTID[end_idx + 1] == census_tract_fips[k]) {
    
    end_idx <- end_idx + 1
    
    if (end_idx == nrow(census_tract_adjacency)) {
      break
    }
    
  }
  
  nbr_census_tracts <- census_tract_adjacency$NEIGHBOR_TRACTID[start_idx:end_idx]
  
  nbr_idx <- which(census_tract_fips %in% nbr_census_tracts)
  
  # not necessary, census tract isn't listed as adjacent to itself
  # nbr_idx <- nbr_idx[nbr_idx != k]
  
  census_tract_adj[k, nbr_idx] <- 1
  
}

# TBC: adjacency matrix just for North Carolina census tracts
saveRDS(census_tract_adj, file = here("intermediary_data", "census_tract_adj_NC.rds"))



# TBC
# changing the Oglala county FIPS in the row name/column name from 46113 to 46102
row.names(census_tract_adj)[row.names(census_tract_adj) == 46113] <- 46102
colnames(census_tract_adj)[colnames(census_tract_adj) == 46113] <- 46102

# saving the full, unprocessed adjacency matrix for all counties

saveRDS(census_tract_adj, file = here("intermediary_data", "census_tract_adj.rds"))





# omit (and reorder) the fips to match the flood risk fips

# TBC: NC census tract version

census_tract_adj <- readRDS(here("intermediary_data", "census_tract_adj_NC.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_NC_census_tract.rds"))



reorganize_idx <- match(fhs_model_df$fips, colnames(census_tract_adj)) 

census_tract_adj_reorganize <- census_tract_adj[, reorganize_idx]

census_tract_adj_reorganize <- census_tract_adj_reorganize[reorganize_idx, ]



saveRDS(census_tract_adj_reorganize, file = here("intermediary_data", "census_tract_adj_reorganize_NC_census_tract.rds"))


