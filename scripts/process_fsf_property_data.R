
library(googledrive)
library(here)
library(tidyverse)

parcel_data_files <- drive_ls("NIEHS work/FirstStreetParcelData")



extracted_fr_list <- vector("list", length = nrow(parcel_data_files))

for (i in 1:nrow(parcel_data_files)) {
  
  drive_download(parcel_data_files[i, ])
  
  flood_risk_parcel <- read.csv(parcel_data_files[i,]$name)
  
  
  
  pct_ff_df <- flood_risk_parcel %>% group_by(tract_fips) %>% summarise(
    pct_fs_risk_2020_5 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .20), 
    pct_fs_risk_2050_5 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .20), 
    pct_fs_risk_2020_100 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .01), 
    pct_fs_risk_2050_100 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .01),
    pct_fs_risk_2020_500 = mean(!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .002), 
    pct_fs_risk_2050_500 = mean(!is.na(mid_chance_0_2051) & mid_chance_0_2051 >= .002),
    avg_risk_score_all = mean(floodfactor),
    avg_risk_score_2_10 = mean(floodfactor[floodfactor > 1]), 
    avg_risk_fsf_2020_100 = mean(floodfactor[!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .01]), 
    avg_risk_fsf_2020_500 = mean(floodfactor[!is.na(mid_chance_0_2021) & mid_chance_0_2021 >= .002]), 
    pct_floodfactor1 = sum(floodfactor == 1) / n(), 
    pct_floodfactor2 = sum(floodfactor == 2) / n(), 
    pct_floodfactor3 = sum(floodfactor == 3) / n(), 
    pct_floodfactor4 = sum(floodfactor == 4) / n(), 
    pct_floodfactor5 = sum(floodfactor == 5) / n(), 
    pct_floodfactor6 = sum(floodfactor == 6) / n(), 
    pct_floodfactor7 = sum(floodfactor == 7) / n(), 
    pct_floodfactor8 = sum(floodfactor == 8) / n(), 
    pct_floodfactor9 = sum(floodfactor == 9) / n(), 
    pct_floodfactor10 = sum(floodfactor == 10) / n()
  )
  
  extracted_fr_list[[i]] <- pct_ff_df
  
  # remove large file objects after you finish extracting variables from them
  rm(list = c("flood_risk_parcel", "pct_ff_df"))
  file.remove(parcel_data_files[i,]$name) 
  
}
  


extracted_fr <- do.call("rbind", extracted_fr_list)

saveRDS(extracted_fr, file = here("intermediary_data/extracted_fr.rds"))


