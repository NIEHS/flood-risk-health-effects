
library(here)
library(readxl)
library(stringr)

i_am("scripts/imported_data_wrangling.R")

# reading in the county flood risk data
flood_risk <- read.csv(here("imported_data", "flood_risk", "County_level_risk_FEMA_FSF_v1.3.csv"))

# reading in the Life Expectancy/Mortality Risk data
# omitting the last two rows, which don't have data
life_expect_mort <- read_excel(here("imported_data", "life_expectancy_mortality_risk", 
                            "IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.XLSX"), 
                            sheet = "Life expectancy", skip = 1, n_max = 3194)

# remove the confidence interval in the data columns

life_expect_mort_no_ui <- life_expect_mort

for (j in 3:11) {
  
  life_expect_mort_no_ui[[j]] <- as.numeric(str_replace(life_expect_mort[[j]], " .+", ""))
  
}

saveRDS(life_expect_mort_no_ui, file = here("imported_data", "life_expectancy_mortality_risk", 
                                         "life_expect_mort_no_ui.rds"))

# reading in the CDC SVI data
cdc_svi <- read.csv(here("imported_data", "CDC_SVI", "SVI2014_US_CNTY.csv"))



# merge all three datasets together by their fips






