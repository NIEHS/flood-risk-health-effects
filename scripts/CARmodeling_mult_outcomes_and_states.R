
library(CARBayes)
library(here)

library(rstan)

library(CARBayesdata)
library(shapefiles)
library(sp)
library(spdep)

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_NC_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_NC_census_tract.rds"))

# checking that the fips in W and fhs_model_df align
all.equal(as.numeric(colnames(W)), fhs_model_df$fips)



# extract the response variable

Y <- cbind(fhs_model_df$Data_Value_CHD, fhs_model_df$Data_Value_CASTHMA, 
           fhs_model_df$Data_Value_BPHIGH, fhs_model_df$Data_Value_MHLTH)

# extract the covariates matrix

X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 4)]

X <- X[, names(X) != "pct_floodfactor1"]

X <- as.matrix(X)



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the counties 
# will have neighbors in W



# ##### CARBayes part
# 
# # TBC: quick version
# set.seed(1044, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- MVS.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 1000,
#                        n.sample = 10000, thin = 5, verbose = TRUE)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# save(chain1, file = here("modeling_files/model_NC_census_tract_quick_version_mult_outcomes.RData"))





set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

chain1  <- MVS.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)
chain2  <- MVS.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)
chain3  <- MVS.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



save(chain1, chain2, chain3, file = here("modeling_files/model_NC_census_tract_fr_zip_mult_outcomes.RData"))

