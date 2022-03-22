
# Exploring new First Street parcel-level data

library(readxl)
library(here)
library(tidyverse)



flood_risk_DE <- read_excel(here("imported_data/flood_risk/FSF_Property_Data_State_10_Confidential_TradeSecrets_BusinessProprietary.xlsx"), 
                            col_types = c(rep("numeric", 75), "text", rep("numeric", 3), "text", rep("numeric", 3), "text", "numeric", "text"))



# comparing with the previous version of the cleaned dataset

fhs_model_fr <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_reorg.rds"))




# do summarise, take proportion of parcels with certain flood risk score in the census tract

pct_ff_df <- flood_risk_DE %>% group_by(tract_fips) %>% summarise(
  pct_fr_2020_5 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .20), 
  pct_fr_2050_5 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .20), 
  pct_fr_2020_100 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .01), 
  pct_fr_2050_100 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .01),
  pct_fr_2020_500 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .002), 
  pct_fr_2050_500 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .002),
  avg_ff_all = mean(floodfactor),
  sd_ff_all = sd(floodfactor),
  cv_ff_all = sd(floodfactor) / mean(floodfactor),
  avg_ff_2_10 = mean(floodfactor[floodfactor > 1]), 
  avg_ff_2020_100 = mean(floodfactor[!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .01]), 
  avg_ff_2020_500 = mean(floodfactor[!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .002]), 
  pct_ff1 = sum(floodfactor == 1) / n(), 
  pct_ff2 = sum(floodfactor == 2) / n(), 
  pct_ff3 = sum(floodfactor == 3) / n(), 
  pct_ff4 = sum(floodfactor == 4) / n(), 
  pct_ff5 = sum(floodfactor == 5) / n(), 
  pct_ff6 = sum(floodfactor == 6) / n(), 
  pct_ff7 = sum(floodfactor == 7) / n(), 
  pct_ff8 = sum(floodfactor == 8) / n(), 
  pct_ff9 = sum(floodfactor == 9) / n(), 
  pct_ff10 = sum(floodfactor == 10) / n()
)

pct_ff_compare <- left_join(pct_ff_df, fhs_model_fr, by = c("tract_fips" = "fips"))

mean(complete.cases(pct_ff_compare)) # 0.9953488

pct_ff_compare <- pct_ff_compare[complete.cases(pct_ff_compare),]



# correlations are pretty high, but not so high that updating them is useless

cor(pct_ff_compare$pct_fr_2020_5, pct_ff_compare$pct_fs_risk_2020_5)
cor(pct_ff_compare$pct_fr_2050_5, pct_ff_compare$pct_fs_risk_2050_5)
cor(pct_ff_compare$pct_fr_2020_100, pct_ff_compare$pct_fs_risk_2020_100)
cor(pct_ff_compare$pct_fr_2050_100, pct_ff_compare$pct_fs_risk_2050_100)
cor(pct_ff_compare$pct_fr_2020_500, pct_ff_compare$pct_fs_risk_2020_500)
cor(pct_ff_compare$pct_fr_2050_500, pct_ff_compare$pct_fs_risk_2050_500)

cor(pct_ff_compare$avg_ff_all, pct_ff_compare$avg_risk_score_all)
cor(pct_ff_compare$avg_ff_2_10, pct_ff_compare$avg_risk_score_2_10)
cor(pct_ff_compare$avg_ff_2020_100, pct_ff_compare$avg_risk_fsf_2020_100)
cor(pct_ff_compare$avg_ff_2020_500, pct_ff_compare$avg_risk_fsf_2020_500)

cor(pct_ff_compare$pct_ff1, pct_ff_compare$pct_floodfactor1)
cor(pct_ff_compare$pct_ff2, pct_ff_compare$pct_floodfactor2)
cor(pct_ff_compare$pct_ff3, pct_ff_compare$pct_floodfactor3)
cor(pct_ff_compare$pct_ff4, pct_ff_compare$pct_floodfactor4)
cor(pct_ff_compare$pct_ff5, pct_ff_compare$pct_floodfactor5)
cor(pct_ff_compare$pct_ff6, pct_ff_compare$pct_floodfactor6)
cor(pct_ff_compare$pct_ff7, pct_ff_compare$pct_floodfactor7)
cor(pct_ff_compare$pct_ff8, pct_ff_compare$pct_floodfactor8)
cor(pct_ff_compare$pct_ff9, pct_ff_compare$pct_floodfactor9)
cor(pct_ff_compare$pct_ff10, pct_ff_compare$pct_floodfactor10)






summary(fhs_model_fr[, 14:35])

View(head(flood_risk_DE, n = 100))

summary(flood_risk_DE[, str_detect(names(flood_risk_DE), "chance")])



summary(flood_risk_TX1[, str_detect(names(flood_risk_TX1), "chance")])

# Why does none of the cumulative probabilities go above 99%? Is it just for Delaware? Check out Florida, for instance.
# No, even for other states the maximum cumulative probability is 50%. I'm skeptical 
# as to whether it can be interpreted as cumulative probability.
# I think it's annual probability, rather

# Choose common-sense cut-offs to aggregate proportion of parcels above a certain risk
# based on DE, part of TX, and FL



# Proportion of properties with any risk 
mean(!is.na(flood_risk_DE2$mid_chance_0_2021) & flood_risk_DE2$mid_chance_0_2021 >= .002)

# Proportion of properties with substantial risk
mean(!is.na(flood_risk_DE2$mid_chance_0_2021) & flood_risk_DE2$mid_chance_0_2021 >= .01)

# Proportion of properties with almost certain risk
mean(!is.na(flood_risk_DE2$mid_chance_0_2021) & flood_risk_DE2$mid_chance_0_2021 >= .20)



# Proportion of properties with any risk 
mean(!is.na(flood_risk_DE$mid_chance_0_2051) & flood_risk_DE$mid_chance_0_2021 >= .002)

# Proportion of properties with substantial risk
mean(!is.na(flood_risk_DE$mid_chance_0_2051) & flood_risk_DE$mid_chance_0_2051 >= .01)

# Proportion of properties with almost certain risk
mean(!is.na(flood_risk_DE$mid_chance_0_2051) & flood_risk_DE$mid_chance_0_2051 >= .20)







# Proportion of properties with any risk 
mean(!is.na(flood_risk_FL$mid_chance_0_2021) & flood_risk_FL$mid_chance_0_2021 >= .002)

# Proportion of properties with substantial risk
mean(!is.na(flood_risk_FL$mid_chance_0_2021) & flood_risk_FL$mid_chance_0_2021 >= .01)

# Proportion of properties with almost certain risk
mean(!is.na(flood_risk_FL$mid_chance_0_2021) & flood_risk_FL$mid_chance_0_2021 >= .20)


