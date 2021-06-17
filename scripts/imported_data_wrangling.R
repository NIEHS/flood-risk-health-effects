
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

smoke_prev <- read.csv(here("imported_data", 
                            "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012", 
                            "IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv"), 
                       col.names = c("state", "county", "sex", "year", "total_mean", 
                                     "total_lb", "total_ub", "daily_mean", "daily_lb", 
                                     "daily_ub"))

# extract data for the year 2012

smoke_prev <- smoke_prev[smoke_prev$year == 2012, ]

smoke_prev_both <- smoke_prev[smoke_prev$sex == "Both",]

# TBC
# figure out how to connect county name and the fips



# merge all three datasets together by their fips

flood_le <- merge(life_expect_mort_no_ui, flood_risk, all.x = T, by = "fips")

flood_le_svi <- merge(flood_le, cdc_svi, all.x = T, by = "fips")



saveRDS(flood_le_svi, file = here("intermediary_data/flood_le_svi.rds"))



# remove redundant columns, move id columns to the left

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



