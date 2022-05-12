
tick <- proc.time()[3]

# testing whether packages can be loaded, 
# and whether it can access file from root directory 

library(here)

library(spdep)

library(dplyr)



library(Rcpp)
library(truncnorm)
library(coda)
library(Matrix)

spdep::mat2listw

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_reorg.rds"))

names(fhs_model_df)




