# Running CAR model for CHD, stratified

library(here)

library(shapefiles)
library(sp)
library(spdep)
library(parallel)

library(dplyr)

select <- dplyr::select

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_pc.rds"))

# remove 3 response variables that are not MHLTH
fhs_model_df <- fhs_model_df[, -(ncol(fhs_model_df) + c(-3, -2, -1))]

# TODO: remove SVIs that correspond to a given stratification variable
strat_covariate <- fhs_model_df$RPL_THEMES
fhs_model_df <- select(fhs_model_df, -EP_POV, -EP_UNEMP, -EP_PCI, -EP_NOHSDP, 
                       -EP_AGE65, -EP_AGE17, -EP_DISABL, -EP_SNGPNT, 
                       -EP_MINRTY, -EP_LIMENG,
                       -EP_MUNIT, -EP_MOBILE, -EP_CROWD, -EP_NOVEH, -EP_GROUPQ)

source(here("scripts/sensitan_helper_fns.R"))



# sampling 1000 census tract to keep all phi and fitted mcmc results for, 
# to reduce memory size

set.seed(346, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

keep_first <- sort(sample(1:nrow(fhs_model_df), size = 1000))





set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

model_res <- fhs_car_chains_stratif(fhs_model_df, first_var = 19, W = W, rho = 1, n_burn_in = 10000, n_iter = 100000, thin = 2,
                                    keep_first = keep_first, num_chains = 3, strat_covariate = strat_covariate, strat_fn = median)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



chain1 <- model_res$chain_list[[1]]

chain2 <- model_res$chain_list[[2]]

chain3 <- model_res$chain_list[[3]]

var_names <- model_res$var_names



save(chain1, chain2, chain3, var_names,
     file = here("modeling_files/model_stratif_rpls_MHLTH.RData"))





