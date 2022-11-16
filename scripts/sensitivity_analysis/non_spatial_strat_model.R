
library(dplyr)

select <- dplyr::select

#' Function to fit the non-spatial model stratified on a given variable
#'
#' @param dat_frame the data frame to analyze. Assumes that dependent variable is the last one in data frame.
#' @param first_var first variable in dat_frame to put into X
#' @param strat_covariate numeric covariate to stratify analysis on
#' @param strat_fn summary statistic of strat_covariate, to base the stratification on
#' @return lm object
#' @export
non_spatial_strat_model <- function(dat_frame, first_var, strat_covariate, strat_fn) {
  
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
  
  
  
  lm_obj <- lm(Y ~ . - 1, data = X_intx_cbind)
  
  
  
  return(list(lm_obj = lm_obj, var_names = colnames(X_intx_cbind)))
  
}


