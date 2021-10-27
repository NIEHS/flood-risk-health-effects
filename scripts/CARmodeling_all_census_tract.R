
library(CARBayes)
library(here)

library(rstan)

library(CARBayesdata)
library(shapefiles)
library(sp)
library(spdep)
library(parallel)

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_reorg.rds"))

source(here("scripts/my_gaussian_leroux_car.R"))



# extract the response variable

Y <- fhs_model_df$Data_Value_CHD

# extract the covariates matrix

X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]

X <- X[, names(X) != "pct_floodfactor1"]

# exclude some more variables selected by vifstep, to account for multicollinearity
# excluding all of the pct_fs_risk variables, as well as 3 of the avg_risk_score variables

collin_var_names <- c("pct_fs_risk_2050_500", "avg_risk_score_all", "pct_fs_risk_2050_100",
                      "pct_fs_risk_2020_500", "pct_fs_risk_2020_100", "avg_risk_fsf_2020_500",
                      "pct_fs_risk_2050_5", "avg_risk_score_2_10", "pct_fs_risk_2020_5")

X <- X[, !(names(X) %in% collin_var_names)]

# also removing avg_risk_score_sfha due to large numbers of NAs
X <- X[, names(X) != "avg_risk_score_sfha"]



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the counties 
# will have neighbors in W

X <- data.frame(X)



# ##### CARBayes part
# 
# # TBC: quick version



# sampling 1000 census tract to keep all phi and fitted mcmc results for, 
# to reduce memory size

set.seed(346, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

keep_first <- sort(sample(1:nrow(X), size = 1000))



# NOTE: using more cores

options(mc.cores = 6)



set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5, 
                         keep_first = keep_first)
chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5, 
                         keep_first = keep_first)
chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5, 
                         keep_first = keep_first)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



save(chain1, chain2, chain3, file = here("modeling_files/model_3chains_all_census_tract.RData"))




