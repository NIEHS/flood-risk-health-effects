
library(here)
library(readxl)
library(stringr)
library(tidyverse)

i_am("scripts/imported_data_wrangling.R")



# reading in the county flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "County_level_risk_FEMA_FSF_v1.3.csv"))



# reading in the Life Expectancy/Mortality Risk data
# omitting the last two rows, which don't have data
life_expect_mort <- read_excel(here("imported_data", "life_expectancy_mortality_risk", 
                                    "IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.XLSX"), 
                               sheet = "Life expectancy", skip = 1, n_max = 3194)

# Lowercase the FIPS
life_expect_mort <- rename(life_expect_mort, fips = FIPS)

# remove the confidence interval in the data columns

life_expect_mort_no_ui <- life_expect_mort

for (j in 3:11) {
  
  life_expect_mort_no_ui[[j]] <- as.numeric(str_replace(life_expect_mort[[j]], " .+", ""))
  
}

saveRDS(life_expect_mort_no_ui, file = here("imported_data", "life_expectancy_mortality_risk", 
                                            "life_expect_mort_no_ui.rds"))



# reading in the CDC SVI data
cdc_svi <- read.csv(here("imported_data", "CDC_SVI", "SVI2018_US_COUNTY.csv"))

# Lowercase the FIPS
cdc_svi <- rename(cdc_svi, fips = FIPS)



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

# TBC
# figure out how to connect county name and the fips
# a good approach is to connect with the CDC SVI "Location"
# pay attention to capitalization

svi_location <- cdc_svi$LOCATION

svi_loc_list <- strsplit(svi_location, split = ", ")

svi_county <- str_to_title(lapply(svi_loc_list, `[[`, 1))

state <- unlist(lapply(svi_loc_list, `[[`, 2))

fips_county_name <- data.frame(fips = cdc_svi$FIPS, county = svi_county, state = state)

# the above will lead to 87 counties missing, after merging. Need to account for the edge cases.
# These are denoted by "/" in the smoking data, e.g. "Southampton County/Franklin City"

smoke_county_list <- strsplit(smoke_prevalence_county$county, split = "/")

smoke_county <- sapply(smoke_county_list, function(item) {
  if (length(item) == 1) {
    item
  } else {
    item[str_detect(item, "County")][1] # there may be multiple "_ County" in here
  }
})

smoke_fips <- merge(smoke_prevalence_county, fips_county_name, 
                    by = c("county", "state"), 
                    all.x = F, all.y = T)



# # for reading in the other version, 
# # "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_ANNUALIZED_RATE_OF_CHANGE_1996_2012"
# smoke_change <- read.csv(here("imported_data",
#                               "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012",
#                               "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_ANNUALIZED_RATE_OF_CHANGE_1996_2012.csv"))





# CACES LUR air pollution data

# Extracting the list of county fips in the dataset, for CACES data extraction

fips <- as.character(flood_le_svi$fips)

# switch fip for Oglala County, since CACES uses outdated fips

fips[fips == 46102] <- 46113

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 4) {paste0("0", fip)} 
  else {fip}
})

write.csv(fips_leading_zero, file = here("intermediary_data/county_fips.txt"), 
          row.names = FALSE)

# data wrangling of the CACES data

caces_lur <- read.csv(here("imported_data/caces_lur_air_pollution/caces_lur_air_pollution.csv"))





# merge all three datasets together by their fips

flood_le <- merge(life_expect_mort_no_ui, flood_risk, all.x = T, by = "fips")

flood_le_svi <- merge(flood_le, cdc_svi, all.x = T, by = "fips")



# remove geographical units that are not counties

flood_le_svi <- flood_le_svi[!is.na(flood_le_svi$COUNTY), ] 

# remove counties in Alaska and Hawaii

flood_le_svi <- flood_le_svi[!(flood_le_svi$STATE %in% c("ALASKA", "HAWAII")), ]



# save the dataset

saveRDS(flood_le_svi, file = here("intermediary_data/flood_le_svi.rds"))





# TBC: remove redundant columns, move id columns to the left

flood_le_svi <- readRDS(file = here("intermediary_data/flood_le_svi.rds"))



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

saveRDS(countyadj, file = here("imported_data", "countyadj.rds"))






# to-do: omit and reorder the fips to match the flood risk fips

countyadj <- readRDS(here("imported_data", "countyadj.rds"))



