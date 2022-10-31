
library(tidyverse)
library(here)

# script to conduct sensitivity analysis where I omit certain subsets of variables

# Hard-coded parameters (e.g., n_burn_in, n_iter, has_intercept, etc.)

rho = 1
n_burn_in = 10000
n_iter = 100000
thin = 2
num_chains = 3



##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_fr_and_pollute_pc.rds"))

# extract the four response variables
responses <- fhs_model_df[, (ncol(fhs_model_df) - 3):ncol(fhs_model_df)]
# extract the other covariates
covariates <- fhs_model_df[, -((ncol(fhs_model_df) - 3):ncol(fhs_model_df))]

first_var <- which(names(covariates) == "flood_risk_pc1")

source(here("scripts/sensitivity_analysis/sensitan_helper_fns.R"))



# sampling 1000 census tract to keep all phi and fitted mcmc results for, 
# to reduce memory size

set.seed(346, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

keep_first <- sort(sample(1:nrow(fhs_model_df), size = 1000))



# CHD
covariates_CHD <- data.frame(covariates, Data_Value_CHD = responses$Data_Value_CHD)
# BPHIGH
covariates_BPHIGH <- data.frame(covariates, Data_Value_BPHIGH = responses$Data_Value_BPHIGH)
# CASTHMA
covariates_CASTHMA <- data.frame(covariates, Data_Value_CASTHMA = responses$Data_Value_CASTHMA)
# MHLTH
covariates_MHLTH <- data.frame(covariates, Data_Value_MHLTH = responses$Data_Value_MHLTH)

# Function to run the models where I omit 3 subsets of variables (SVI, pollution, weather), one by one.
# Will be repeated for four health outcomes
omit_subsets_run_model <- function(dat_frame, save_folder) {
  
  # SVI
  fhs_car_chains_subset_omit(dat_frame, first_var, W, rho, n_burn_in, n_iter, thin, 
                             keep_first, num_chains, 
                             save_dir = paste0(save_folder, "omit_SVI.RData"), 
                             subset_omit = c("EP_POV", "EP_UNEMP", "EP_PCI", "EP_NOHSDP", 
                                             "EP_AGE65", "EP_AGE17", "EP_DISABL", "EP_SNGPNT", 
                                             "EP_MINRTY", "EP_LIMENG", "EP_MUNIT", "EP_MOBILE", 
                                             "EP_CROWD", "EP_NOVEH", "EP_GROUPQ", "EP_UNINSUR")) 
  
  # weather
  fhs_car_chains_subset_omit(dat_frame, first_var, W, rho, n_burn_in, n_iter, thin, 
                             keep_first, num_chains, 
                             save_dir = paste0(save_folder, "omit_pollution.RData"), 
                             subset_omit = c("pollute_conc_pc1", "pollute_conc_pc2", "pollute_conc_pc3")) 
  
  # pollution
  fhs_car_chains_subset_omit(dat_frame, first_var, W, rho, n_burn_in, n_iter, thin, 
                             keep_first, num_chains, 
                             save_dir = paste0(save_folder, "omit_weather.RData"), 
                             subset_omit = c("tmmx", "rmax"))
  
}

omit_subsets_run_model(covariates_CHD, save_folder = here("modeling_files/sensitivity_analysis/omit_subsets_of_variables/CHD_"))

omit_subsets_run_model(covariates_BPHIGH, save_folder = here("modeling_files/sensitivity_analysis/omit_subsets_of_variables/BPHIGH_"))

omit_subsets_run_model(covariates_CASTHMA, save_folder = here("modeling_files/sensitivity_analysis/omit_subsets_of_variables/CASTHMA_"))

omit_subsets_run_model(covariates_MHLTH, save_folder = here("modeling_files/sensitivity_analysis/omit_subsets_of_variables/MHLTH_"))


