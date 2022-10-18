
# Functions for the sensitivity analyses



#' Function to omit sets of variables and run the model
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
#' @param subset_omit subset of variables to omit from dat_frame
#' @return MCMC chains
#' @export
fhs_car_chains_subset_omit <- function(dat_frame, first_var, W, rho = NULL, n_burn_in, n_iter, thin = 1, 
                           keep_first = 1:nrow(dat_frame), num_chains = 3, subset_omit = c()) {
  
  # extract the response variable
  
  Y <- dat_frame[, ncol(dat_frame)]
  
  # extract the covariates matrix
  
  X <- dat_frame[, first_var:(ncol(dat_frame) - 1)]
  
  # remove variables in subset_omit from covariates matrix
  
  X <- X[, !(names(X) %in% subset_omit)]
  
  
  
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


