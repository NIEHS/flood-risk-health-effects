
library(CARBayes)
library(here)

library(CARBayesdata)
library(shapefiles)
library(sp)
library(spdep)
library(parallel)

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_all_census_tract_pc.rds"))

source(here("scripts/my_gaussian_leroux_car.R"))



# sampling 1000 census tract to keep all phi and fitted mcmc results for, 
# to reduce memory size

set.seed(346, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

keep_first <- sort(sample(1:nrow(fhs_model_df), size = 1000))



#' Function to conveniently run several chains on the flood risk data
#'
#' @param dat_frame the data frame to analyze. Assumes that dependent variable is the last one in data frame.
#' @param first_var first variable in dat_frame to put into X
#' @param W adjacency matrix in sparse matrix format (class ngCMatrix)
#' @param rho option to fix the spatial smoothing parameter
#' @param n_burn_in number of burn-in iterations, for each chain
#' @param n_iter number of kept iterations, for each chain
#' @param thin level of thinning to apply to the MCMC samples, for each chain
#' @param keep_first vector of spatial units to keep the parameters for; for other subjects, 
#' keep track of the running posterior mean.
#' @param num_chains number of desired chains
#' @return MCMC chains
#' @export
fhs_car_chains <- function(dat_frame, first_var, W, rho = NULL, n_burn_in, n_iter, thin = 1, 
                          keep_first = 1:nrow(data), num_chains = 3) {

  # extract the response variable
  
  Y <- dat_frame[, ncol(dat_frame)]
  
  # extract the covariates matrix
  
  X <- dat_frame[, first_var:(ncol(dat_frame) - 1)]
  
  
  
  X           <- scale(X) # Scale covariates
  X[is.na(X)] <- 0        # Fill in missing values with the mean
  
  # if I do mean imputation (which may be problematic), all the counties
  # will have neighbors in W
  
  X <- data.frame(X)
  

  
  chain_list <- vector("list", length = num_chains)
  
  for (i in 1:num_chains) {
  
  chain_list[[i]]  <- met_gibbs_car(Y, data = X, W, rho = rho, n_burn_in = n_burn_in, n_iter = n_iter, thin = thin,
                           keep_first = keep_first)
  
  }
  
  
  
  return(chain_list)
  
}



# # Chunk that runs through entire U.S, without fixing rho. Don't actually run this
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# ### put in function here
# 
# chain_list <- fhs_car_chains(fhs_model_df, first_var = 14, W = W, rho = NULL, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                              keep_first = keep_first, num_chains = 3)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# chain1 <- chain_list[[1]]
# 
# chain2 <- chain_list[[2]]
# 
# chain3 <- chain_list[[3]]
# 
# save(chain1, chain2, chain3, file = here("modeling_files/all_census_tract_entire.RData"))




#########################

# Section that copies and pastes code inefficiently

# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, rho = 1, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, rho = 1, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, rho = 1, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/all_census_tract_intrinsic.RData"))





# # Northeast states
# 
# ne_states <- c(23, 50, 33, 25, 9, 44, 36, 34)
# 
# names(ne_states) <- c("ME", "VT", "NH", "MA", "CT", "RI", "NY", "NJ")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% ne_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% ne_states, (fhs_model_df$fips %/% 1e9) %in% ne_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% ne_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_ne.RData"))




# # Southeast states
# 
# se_states <- c(37, 45, 47, 13, 1, 28, 12)
# 
# names(se_states) <- c("NC", "SC", "TN", "GA", "AL", "MS", "FL")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% se_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% se_states, (fhs_model_df$fips %/% 1e9) %in% se_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% se_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_se.RData"))





# # Mid-Atlantic states
# 
# at_states <- c(42, 10, 24, 11, 51, 54, 39, 21)
# 
# names(at_states) <- c("PA", "DE", "MD", "DC", "VA", "WV", "OH", "KY")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% at_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% at_states, (fhs_model_df$fips %/% 1e9) %in% at_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% at_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_at.RData"))





# # Midwest states
# 
# mw_states <- c(26, 18, 17, 55, 29, 5)
# 
# names(mw_states) <- c("MI", "IN", "IL", "WI", "MO", "AR")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% mw_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% mw_states, (fhs_model_df$fips %/% 1e9) %in% mw_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% mw_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_mw.RData"))





# # Southwest states
# 
# sw_states <- c(22, 48, 40, 35, 4, 8, 49)
# 
# names(sw_states) <- c("LA", "TX", "OK", "NM", "AZ", "CO", "UT")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% sw_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% sw_states, (fhs_model_df$fips %/% 1e9) %in% sw_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% sw_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_sw.RData"))





# # West states
# 
# we_states <- c(6, 32)
# 
# names(we_states) <- c("CA", "NV")
# 
# keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% we_states) %in% keep_first)
# 
# W <- W[(fhs_model_df$fips %/% 1e9) %in% we_states, (fhs_model_df$fips %/% 1e9) %in% we_states]
# 
# fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% we_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_we.RData"))





# Northwest states

nw_states <- c(27, 19, 38, 46, 31, 20, 30, 56, 16, 53, 41)

names(nw_states) <- c("MN", "IA", "ND", "SD", "NE", "KS", "MT", "WY", "ID", "WA", "OR")

keep_first <- which(which((fhs_model_df$fips %/% 1e9) %in% nw_states) %in% keep_first)

W <- W[(fhs_model_df$fips %/% 1e9) %in% nw_states, (fhs_model_df$fips %/% 1e9) %in% nw_states]

fhs_model_df <- fhs_model_df[(fhs_model_df$fips %/% 1e9) %in% nw_states, ]
# 
# 
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, n_burn_in = 10000, n_iter = 100000, thin = 5,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/census_tract_nw.RData"))





set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

### put in function here

chain_list <- fhs_car_chains(fhs_model_df, first_var = 14, W = W, rho = NULL, n_burn_in = 10000, n_iter = 100000, thin = 2,
                             keep_first = keep_first, num_chains = 3)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes

chain1 <- chain_list[[1]]

chain2 <- chain_list[[2]]

chain3 <- chain_list[[3]]

save(chain1, chain2, chain3, file = here("modeling_files/census_tract_nw.RData"))






# # version using averaged rho from the 7 divide and conquer models
# 
# # extract the response variable
# 
# Y <- fhs_model_df$Data_Value_CHD
# 
# # extract the covariates matrix
# 
# X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]
# 
# 
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# X <- data.frame(X)
# 
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- met_gibbs_car(Y, data = X, W, rho = 0.9918268, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# chain2  <- met_gibbs_car(Y, data = X, W, rho = 0.9918268, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# chain3  <- met_gibbs_car(Y, data = X, W, rho = 0.9918268, n_burn_in = 10000, n_iter = 100000, thin = 2,
#                          keep_first = keep_first)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/all_census_tract_ave_rho.RData"))


