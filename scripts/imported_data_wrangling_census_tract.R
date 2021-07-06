
library(here)
library(readxl)
library(stringr)
library(tidyverse)
library(tidyr)
library(Matrix)

select <- dplyr::select

i_am("scripts/imported_data_wrangling.R")





# reading in the county flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "Zip_level_risk_FEMA_FSF_v1.3.csv"))

# TBC: focusing on county-level flood risk for now. I can map zip code to census tracts later, or just wait till we can process the 3m raster data. 
# reading in the county flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "County_level_risk_FEMA_FSF_v1.3.csv"))
# rename fips into CountyFIPS
flood_risk <- rename(flood_risk, CountyFIPS = fips)



# work with the PLACES dataset

places_dat <- read.csv(here("imported_data", 
                            "PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2020_release.csv"))

# don't need the year, state_abbr, lat or lon

places_subset <- select(places_dat, -c(Year, StateAbbr, StateDesc, LocationName, 
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

caces_lur <- read.csv(here("imported_data/caces_lur_air_pollution/caces_lur_air_pollution_NC_census_tract.csv"))

# don't need the year, state_abbr, lat or lon

caces_lur_subset <- dplyr::select(caces_lur, -c(year, state_abbr, lat, lon))

# convert from long to wide format

caces_lur_wide <- spread(caces_lur_subset, pollutant, pred_wght)





# merge all three datasets together by their fips

# merging health outcomes with flood risk
flood_health <- merge(places_dat_wide, flood_risk, all.x = T, by = "CountyFIPS")

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


