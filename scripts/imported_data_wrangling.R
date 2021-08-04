
library(here)
library(readxl)
library(stringr)
library(tidyverse)
library(tidyr)

select <- dplyr::select

i_am("scripts/imported_data_wrangling.R")



# reading in the county flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "County_level_risk_FEMA_FSF_v1.3.csv"))

flood_risk_prev <- flood_risk

count_ff_mat <- diag(1 / flood_risk$count_property) %*% as.matrix(select(flood_risk, starts_with("count_floodfactor"))) * 100

colnames(count_ff_mat) <- str_replace(colnames(count_ff_mat), "count", "pct")

flood_risk <- data.frame(flood_risk_prev, count_ff_mat)





# work with the PLACES dataset

places_dat <- read.csv(here("imported_data", 
                            "PLACES__Local_Data_for_Better_Health__County_Data_2020_release.csv"))

# don't need the year, state_abbr, lat or lon

places_subset <- dplyr::select(places_dat, -c(Year, StateAbbr, StateDesc, LocationName, 
                                              DataSource, Category, Data_Value_Unit, Data_Value_Type, 
                                              Data_Value_Footnote_Symbol, Data_Value_Footnote, 
                                              geolocation, DataValueTypeID))

# convert from long to wide format

places_dat_wide <- pivot_wider(places_subset, id_cols = c(LocationID, TotalPopulation),
                               names_from = MeasureId, 
                               values_from = c(Data_Value, Low_Confidence_Limit, High_Confidence_Limit))

places_dat_wide <- rename(places_dat_wide, fips = LocationID)





# reading in the CDC SVI data
cdc_svi <- read.csv(here("imported_data", "CDC_SVI", "SVI2018_US_COUNTY.csv"))

# Lowercase the FIPS
cdc_svi <- rename(cdc_svi, fips = FIPS)

# take care of the -999 missing value indicators

cdc_svi[cdc_svi == -999] <- NA



# FOR THE 2014 VERSION

# # remove empty variable FID
# cdc_svi <- subset(cdc_svi, select = -FID)
# 
# # Because the CDC SVI data was created in 2014, the data for Shannon County, South Dakota
# # is now data for Oglala Lakota County, South Dakota.
# # Thus, the FIPS for the county needs to be changed from 46113 to 46102.
# 
# cdc_svi$FIPS[cdc_svi$FIPS == 46113] <- 46102



# reading in the smoking prevalence data

smoke_prevalence <- read.csv(here("imported_data", 
                                  "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012", 
                                  "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv"), 
                             col.names = c("state", "county", "sex", "year", "total_mean", 
                                           "total_lb", "total_ub", "daily_mean", "daily_lb", 
                                           "daily_ub"))

# extract data for the year 2012
smoke_prevalence_2012 <- smoke_prevalence[smoke_prevalence$year == 2012, ]

# extract for both sexes
smoke_prevalence_both <- smoke_prevalence_2012[smoke_prevalence_2012$sex == "Both",]

# extract for county-level (rather than state-level data)
smoke_prevalence_county <- smoke_prevalence_both[smoke_prevalence_both$county != "",]

# connecting county name and the fips
# a good approach is to connect with the CDC SVI "Location"
# pay attention to capitalization

svi_location <- cdc_svi$LOCATION

svi_loc_list <- strsplit(svi_location, split = ", ")

svi_county <- tolower(lapply(svi_loc_list, `[[`, 1))

state <- str_to_title(unlist(lapply(svi_loc_list, `[[`, 2)))

# Dealing with the rest of the missing counties on a case by case basis
svi_county[svi_county == "doña ana county" & 
               state == "New Mexico"] <- "dona ana county"

fips_county_name <- data.frame(fips = cdc_svi$fips, county = svi_county, state = state)

# the above will lead to 87 counties missing, after merging.
# sum(smoke_fips$fips[is.na(smoke_fips$sex)] %in% cdc_svi$fips)
# Need to account for the edge cases.
# These are denoted by "/" in the smoking data, e.g. "Southampton County/Franklin City"
# These actually indicate merged counties. Need a duplicate row for each member in group.

smoke_county_list <- strsplit(smoke_prevalence_county$county, split = "/")

smoke_county <- sapply(smoke_county_list, function(item) {
  if (length(item) == 1) {
    item
  } else {
    item[str_detect(item, "County")][1] # there may be multiple "_ County" in here
  }
})

smoke_county <- tolower(smoke_county)

# Dealing with the rest of the missing counties on a case by case basis
# smoke_fips[!(smoke_fips$state %in% c("Alaska", "Hawaii")) & is.na(smoke_fips$sex),]$county
# View(cbind(smoke_prevalence_county$county, smoke_prevalence_county$state, smoke_county))

# Oglala County mixup 
smoke_county[smoke_county == "shannon county" & 
               smoke_prevalence_county$state == "South Dakota"] <- "oglala lakota county" 

smoke_prevalence_county_fixed <- smoke_prevalence_county
smoke_prevalence_county_fixed$county <- smoke_county

# accounting for counties grouped with other counties in smoking prevalence dataset

# Boulder, Broomfield, Jefferson, Weld grouped with Adams County, Colorado
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Colorado" & 
    smoke_prevalence_county_fixed$county == "adams county",]
addend$county <- "boulder county"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Colorado" & 
                                          smoke_prevalence_county_fixed$county == "adams county",]
addend$county <- "broomfield county"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Colorado" & 
                                          smoke_prevalence_county_fixed$county == "adams county",]
addend$county <- "jefferson county"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Colorado" & 
                                          smoke_prevalence_county_fixed$county == "adams county",]
addend$county <- "weld county"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

# Fairfax City grouped with Fairfax County, Virginia
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Virginia" & 
                                          smoke_prevalence_county_fixed$county == "fairfax county",]
addend$county <- "fairfax city"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

# Franklin City grouped with Southampton County, Virginia
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Virginia" & 
                                          smoke_prevalence_county_fixed$county == "southampton county",]
addend$county <- "franklin city"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

# I think LaSalle Parish, Louisiana was grouped together with Catahoula Parish, Louisiana
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Louisiana" & 
                                          smoke_prevalence_county_fixed$county == "catahoula parish",]
addend$county <- "lasalle parish"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

# Manassas Park City grouped with Prince William County, Virginia
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Virginia" & 
                                          smoke_prevalence_county_fixed$county == "prince william county",]
addend$county <- "manassas park city"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)

# Waynesboro City grouped with Augusta County, Virginia
addend <- smoke_prevalence_county_fixed[smoke_prevalence_county_fixed$state == "Virginia" & 
                                          smoke_prevalence_county_fixed$county == "augusta county",]
addend$county <- "waynesboro city"
smoke_prevalence_county_fixed <- rbind(smoke_prevalence_county_fixed, 
                                       addend)



smoke_fips <- merge(smoke_prevalence_county_fixed, fips_county_name, 
                    by = c("county", "state"), 
                    all.x = F, all.y = T)



saveRDS(smoke_fips, file = here("intermediary_data/smoke_fips.rds")) 



# # for reading in the other version, 
# # "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_ANNUALIZED_RATE_OF_CHANGE_1996_2012"
# smoke_change <- read.csv(here("imported_data",
#                               "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012",
#                               "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_ANNUALIZED_RATE_OF_CHANGE_1996_2012.csv"))





# CACES LUR air pollution data

# # Extracting the list of county fips in the dataset, for CACES data extraction
# 
# fips <- as.character(flood_le_svi$fips)
# 
# # switch fip for Oglala County, since CACES uses outdated fips
# 
# fips[fips == 46102] <- 46113
# 
# fips_leading_zero <- sapply(fips, FUN = function(fip) {
#   if (str_length(fip) == 4) {paste0("0", fip)} 
#   else {fip}
# })
# 
# write.csv(fips_leading_zero, file = here("intermediary_data/county_fips.txt"), 
#           row.names = FALSE)

# reading in the downloaded data from
# https://s3.amazonaws.com/files.airclimateenergy.org/caces/uwc162421434261410dee280fa0513698216d570056ed300.zip

caces_lur <- read.csv(here("imported_data/caces_lur_air_pollution/caces_lur_air_pollution.csv"))

# don't need the year, state_abbr, lat or lon

caces_lur_subset <- select(caces_lur, -c(year, state_abbr, lat, lon))

# convert from long to wide format

caces_lur_wide <- spread(caces_lur_subset, pollutant, pred_wght)



# merge all three datasets together by their fips

# merging life expectancy with flood risk
flood_le <- merge(life_expect_mort_no_ui, flood_risk, all.x = T, by = "fips")

# then merging with SVI 
flood_le_svi <- merge(flood_le, cdc_svi, all.x = T, by = "fips")

# then merging with air pollution
flood_le_svi <- merge(flood_le_svi, caces_lur_wide, all.x = T, by = "fips")

# then merging with smoke prevalence
flood_le_svi <- merge(flood_le_svi, smoke_fips, all.x = T, by = "fips")



# remove geographical units that are not counties

flood_le_svi <- flood_le_svi[!is.na(flood_le_svi$COUNTY), ] 

# remove counties in Alaska and Hawaii

flood_le_svi <- flood_le_svi[!(flood_le_svi$STATE %in% c("ALASKA", "HAWAII")), ]



# save the dataset

saveRDS(flood_le_svi, file = here("intermediary_data/flood_le_svi.rds"))





# Removing redundant columns, moving id columns to the left

flood_le_svi <- readRDS(file = here("intermediary_data/flood_le_svi.rds"))

# remove life expectancy variables other than "Life expectancy, 2014*"
# this also puts the outcome variable as the last variable
fls_outcome_subset <- flood_le_svi %>% dplyr::select(!starts_with("Life expectancy") | ends_with(", 2014*")) %>%
  dplyr::select(!`% Change in Life Expectancy, 1980-2014`)

# Deleting and reorganizing some flood risk variables
fls_flood_risk_subset <- fls_outcome_subset %>% dplyr::select(!starts_with("count_fs")) %>% 
  relocate(pct_fs_fema_difference_2020, .before = pct_fs_risk_2020_5)

# Reorganizing the CDC SVI variables
# removing the margins of errors for now
# focusing on the EP_ variables for now
fls_svi_subset <- fls_flood_risk_subset %>% 
  relocate(ST, STATE, ST_ABBR, COUNTY, 
           LOCATION, AREA_SQMI, E_TOTPOP, 
           E_HU, E_HH, .after = pct_fs_fema_difference_2020) %>%
  select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "RPL_", "F_")))

# cleaning up the smoking prevalence variables that are not needed
fls_smoke_subset <- fls_svi_subset %>% select(!c(county, state, sex, year, 
                                                 total_lb, total_ub, daily_lb, daily_ub))

fls_model_df <- fls_smoke_subset

saveRDS(fls_model_df, file = here("intermediary_data/fls_model_df.rds"))



####################

# making the county adjacency matrix from the County Adjacency File provided by the 
# Census Bureau 
# (https://www.census.gov/programs-surveys/geography/library/reference/county-adjacency-file.html)

county_adjacency <- read.delim(here("imported_data", "county_adjacency.txt"), 
                               header = F)

county_fips <- county_adjacency$V2[!is.na(county_adjacency$V2)]

countyadj <- matrix(0, nrow = length(county_fips), ncol = length(county_fips))

row.names(countyadj) <- county_fips
colnames(countyadj) <- county_fips

start_idx <- 1

for (k in 1:length(county_fips)) {
  
  start_idx <- which(county_adjacency$V2 == county_fips[k])
  
  end_idx <- start_idx
  
  while (is.na(county_adjacency$V2[end_idx + 1])) {
    
    end_idx <- end_idx + 1
    
    if (end_idx == nrow(county_adjacency)) {
      break
    }
    
  }
  
  nbr_counties <- county_adjacency[start_idx:end_idx, ]
  
  nbr_idx <- which(county_fips %in% nbr_counties$V4)
  
  nbr_idx <- nbr_idx[nbr_idx != k]
  
  countyadj[k, nbr_idx] <- 1
  
  # start_idx <- end_idx + 1
  # 
  # if (start_idx == nrow(county_adjacency) + 1) {
  #   break
  # }
  
}

# changing the Oglala county FIPS in the row name/column name from 46113 to 46102
row.names(countyadj)[row.names(countyadj) == 46113] <- 46102
colnames(countyadj)[colnames(countyadj) == 46113] <- 46102

# saving the full, unprocessed adjacency matrix for all counties

saveRDS(countyadj, file = here("intermediary_data", "countyadj.rds"))



# omit (and reorder) the fips to match the flood risk fips

countyadj <- readRDS(here("intermediary_data", "countyadj.rds"))

fls_model_df <- readRDS(here("intermediary_data/fls_model_df.rds"))



reorganize_idx <- match(fls_model_df$fips, colnames(countyadj)) 

countyadj_reorganize <- countyadj[, reorganize_idx]

countyadj_reorganize <- countyadj_reorganize[reorganize_idx, ]



saveRDS(countyadj_reorganize, file = here("intermediary_data", "countyadj_reorganize.rds"))


