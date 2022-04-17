
source(here("scripts/my_gaussian_leroux_car.R"))

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
#' @param strat_covariate numeric covariate to stratify analysis on
#' @param strat_fn summary statistic of strat_covariate, to base the stratification on
#' @return MCMC chains
#' @export
fhs_car_chains_stratif <- function(dat_frame, first_var, W, rho = 1, n_burn_in, n_iter, thin = 1, 
                                   keep_first = 1:nrow(data), num_chains = 3, strat_covariate, strat_fn = median) {
  
  # extract the response variable
  
  Y <- dat_frame[, ncol(dat_frame)]
  
  # extract the covariates matrix
  
  X <- dat_frame[, first_var:(ncol(dat_frame) - 1)]
  
  
  
  X           <- scale(X) # Scale covariates
  X[is.na(X)] <- 0        # Fill in missing values with the mean
  
  # if I do mean imputation (which may be problematic), all the spatial units
  # will have neighbors in W
  
  X <- data.frame(X)
  
  # if the stratification variable has missing values, assume that they take the mean value, 
  # which may be below or above the median value that everything is stratified on.
  
  strat_covariate[is.na(strat_covariate)] <- mean(strat_covariate, na.rm = T)
  
  
  
  strat0 <- ifelse(strat_covariate <= strat_fn(strat_covariate), 1, 0)
  strat1 <- ifelse(strat_covariate <= strat_fn(strat_covariate), 0, 1)
  
  
  
  X_intx0 <- model.matrix(rep(1, nrow(X)) ~ strat0 + strat0:., data = X)[, -1]
  X_intx1 <- model.matrix(rep(1, nrow(X)) ~ strat1 + strat1:., data = X)[, -1]
  
  X_intx_cbind <- as.data.frame(cbind(X_intx0, X_intx1))
  
  
  
  # assuming there are two strata
  chain_list <- vector("list", length = num_chains)
  
  # for the first strata (values corresponding to strat_covariate being below the median)
  for (i in 1:num_chains) {
    
    chain_list[[i]]  <- met_gibbs_car(Y, data = X_intx_cbind, W, rho = rho, n_burn_in = n_burn_in, n_iter = n_iter, thin = thin,
                                      keep_first = keep_first, has_intercept = F)
    
  }
  
  
  
  return(list(chain_list = chain_list, var_names = colnames(X_intx_cbind)))
  
}


