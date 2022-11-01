# Running CAR model for CASTHMA

library(CARBayes)
library(here)

library(CARBayesdata)
library(shapefiles)
library(sp)
library(spdep)
library(parallel)

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/sensitivity_analysis/fhs_model_df_high_ver.rds"))

# remove 3 response variables that are not CASTHMA
fhs_model_df <- fhs_model_df[, -(ncol(fhs_model_df) + c(-3, -1, 0))]

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
                           keep_first = 1:nrow(dat_frame), num_chains = 3) {
  
  # extract the response variable
  
  Y <- dat_frame[, ncol(dat_frame)]
  
  # extract the covariates matrix
  
  X <- dat_frame[, first_var:(ncol(dat_frame) - 1)]
  
  
  
  X           <- scale(X) # Scale covariates
  X[is.na(X)] <- 0        # Fill in missing values with the mean
  
  # if I do mean imputation (which may be problematic), all the spatial units
  # will have neighbors in W
  
  X <- data.frame(X)
  
  
  
  chain_list <- vector("list", length = num_chains)
  
  for (i in 1:num_chains) {
    
    chain_list[[i]]  <- met_gibbs_car(Y, data = X, W, rho = rho, n_burn_in = n_burn_in, n_iter = n_iter, thin = thin,
                                      keep_first = keep_first)
    
  }
  
  
  
  return(chain_list)
  
}



set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

### put in function here

chain_list <- fhs_car_chains(fhs_model_df, first_var = 19, W = W, rho = 1, n_burn_in = 10000, n_iter = 100000, thin = 2,
                             keep_first = keep_first, num_chains = 3)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes

chain1 <- chain_list[[1]]

chain2 <- chain_list[[2]]

chain3 <- chain_list[[3]]

save(chain1, chain2, chain3, file = here("modeling_files/sensitivity_analysis/high_ver/all_census_tract_CASTHMA.RData"))


