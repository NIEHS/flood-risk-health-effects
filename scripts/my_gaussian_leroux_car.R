


#' Function to compute the log posterior for the spatial markov model M-H steps
#'
#' @param y_mat response variables
#' @param parameters: Z_mat, W, num_nbr_mat, sigma2, rho, mu, tau2, cluster_labs
#' later on this will involve M, phi
#' @return log of the posterior density 
#' @export
log_post_agdsq <- function(y_mat, Z_mat, W, num_nbr_mat, sigma2, rho,
                           mu, tau2, cluster_labs) {
  
  num_gp <- dim(y_mat)[2]
  
  like <- sum(dnorm(y_mat, mu[cluster_labs] + Z_mat, tau2, log = TRUE))
  
  # spatial terms
  Sigma <- sigma2 * solve(num_nbr_mat - rho * W)
  spatial_like <- sum(dmvnorm(Z_mat, mean = rep(0, num_gp), sigma = Sigma, log = T))
  
  # cluster means
  cluster_mean_like <- sum(dnorm(mu, 0, tau2 * b, log = TRUE))
  
  
  
  # prior <- dnorm(alpha, 0, 100, log = TRUE) + # intercept
  #   dbinom(sum(gamma), length(gamma), q, log = TRUE) + # nonzero slope indicators
  #   sum(dnorm(delta, 0, sigma, log = TRUE)) + # slopes
  #   dhcauchy(x = sigma, 0.5, log = TRUE) # sigma
  
  
  
  return(like + spatial_like + cluster_mean_like)
  
}





#' One MCMC iteration of the Metropolis algorithm for the spatial markov model
#'
#' @param y_mat response variables
#' @param log_post the log posterior of the last iteration
#' @param parameters: Z_mat, W, num_nbr_mat, sigma2, rho, mu, tau2, cluster_labs
#' @param can_sd list containing the candidate standard deviations for 
#' @return the updated parameters for the MCMC iteration
#' @export
met_glm_iter <- function(y_mat,
                         log_post,
                         Z_mat, W, num_nbr_mat, sigma2, rho, mu, tau2, cluster_labs,
                         can_sd) {
  
  # Candidate sd's for alpha and delta
  
  can_alpha_sd <- can_sd$can_alpha_sd
  
  can_delta_sd <- can_sd$can_delta_sd
  
  can_sigma_sd <- can_sd$can_sigma_sd
  
  
  
  # Metropolis/Gibbs for Z_mat, sigma2, rho, mu, tau2
  
  # Z_mat
  
  for (i in 1:n) { # subject
    
    for (j in 1:num_gp) { # grid point
      
      can_Z_mat <- Z_mat
      can_Z_mat[i, j] <- rnorm(1, Z_mat[j], can_Z_mat_sd[j])
      cans <- Xb_beta_update(X, Xb, gamma, can_Z_mat, beta, j, can = "delta")
      can_Xb <- cans$can_Xb
      can_log_post <- log_post_agdsq(Y, can_Xb, alpha, gamma, can_Z_mat, sigma, q)
      logR <- can_log_post - log_post
      if (log(runif(1)) < logR) {
        Z_mat <- can_Z_mat
        beta <- cans$can_beta
        Xb <- can_Xb
        log_post <- can_log_post
      }
      
    }
    
  } 
  
  
  
  # alpha
  can_alpha <- rnorm(1, alpha, can_alpha_sd)
  can_log_post <- log_post_agdsq(Y, Xb, can_alpha, gamma, delta, sigma, q)
  logR <- can_log_post - log_post
  if (log(runif(1)) < logR) {
    alpha <- can_alpha
    log_post <- can_log_post
  }
  
  # gamma
  for (j in 1:num_gamma) {
    can_gamma <- gamma
    can_gamma[j] <- 1 - can_gamma[j]
    cans <- Xb_beta_update(X, Xb, can_gamma, delta, beta, j, can = "gamma")
    can_Xb <- cans$can_Xb
    can_log_post <- log_post_agdsq(Y, can_Xb, alpha, can_gamma, delta, sigma, q)
    logR <- can_log_post - log_post
    if (log(runif(1)) < logR) {
      gamma <- can_gamma
      beta <- cans$can_beta
      Xb <- can_Xb
      log_post <- can_log_post
    }
  }
  
  # delta
  for (j in 1:p) {
    can_delta <- delta
    can_delta[j] <- rnorm(1, delta[j], can_delta_sd[j])
    cans <- Xb_beta_update(X, Xb, gamma, can_delta, beta, j, can = "delta")
    can_Xb <- cans$can_Xb
    can_log_post <- log_post_agdsq(Y, can_Xb, alpha, gamma, can_delta, sigma, q)
    logR <- can_log_post - log_post
    if (log(runif(1)) < logR) {
      delta <- can_delta
      beta <- cans$can_beta
      Xb <- can_Xb
      log_post <- can_log_post
    }
  }
  
  # sigma
  can_sigma <- exp(rnorm(1, log(sigma), can_sigma_sd))
  can_log_post <- log_post_agdsq(Y, Xb, alpha, gamma, delta, can_sigma, q)
  logR <- can_log_post - log_post
  if (log(runif(1)) < logR) {
    sigma <- can_sigma
    log_post <- can_log_post
  }
  
  # Gibbs for q
  
  q <- rbeta(1, shape1 = sum(gamma) + 1, shape2 = p - sum(gamma) + 1)
  log_post <- log_post_agdsq(Y, Xb, alpha, gamma, delta, sigma, q)
  
  
  
  return(list(Xb = Xb, beta = beta, log_post = log_post, alpha = alpha, gamma = gamma,
              delta = delta, sigma = sigma, q = q))
  
}





#' Metropolis algorithm for logistic regression 
#'
#' @param Y response variable
#' @param X covariates matrix (without intercept)
#' @param inits initialize the parameters
#' @param can_sd list containing the candidate standard deviations for alpha, delta, sigma, q
#' @param n_burn_in number of burn-in iterations
#' @param n_iter number of kept iterations
#' @return MCMC chain
#' @export
met_glm <- function(Y, X, inits, can_sd, n_burn_in, n_iter) {
  
  n <- length(Y)
  
  p <- ncol(X)
  
  # Create empty matrix for MCMC samples
  
  S <- n_iter
  
  mcmc_ssvs <- matrix(NA, S, length(unlist(inits))) # tracking alpha, gamma, delta, and sigma
  
  colnames(mcmc_ssvs) <- c("alpha", rep("gamma", length(inits$gamma)), rep("delta", p), "sigma", "q")
  
  
  
  # Initial values
  
  alpha <- inits$alpha
  
  gamma <- inits$gamma
  
  delta <- inits$delta
  
  sigma <- inits$sigma
  
  q <- inits$q
  
  
  
  if (length(gamma) < length(delta)) { # grouped selection. Uses global variables
    
    beta <- c(rep(gamma[1:num_gamma_pc], each = 3), 
              gamma[(num_gamma_pc + 1):(num_gamma_pc + p2)]) * delta
    
  } else { # individual selection
    
    beta <- gamma * delta
    
  }
  
  Xb <- X %*% beta
  
  log_post <- log_post_agdsq(Y, Xb, alpha, gamma, delta, sigma, q)
  
  
  
  for (s in 1:n_burn_in) {
    
    mcmc_iter <- met_glm_iter(Y, X, Xb, beta, log_post, alpha, gamma, delta, sigma, q, can_sd)
    
    Xb <- mcmc_iter$Xb
    
    beta <- mcmc_iter$beta
    
    log_post <- mcmc_iter$log_post
    
    alpha <- mcmc_iter$alpha
    
    gamma <- mcmc_iter$gamma
    
    delta <- mcmc_iter$delta
    
    sigma <- mcmc_iter$sigma
    
    q <- mcmc_iter$q
    
  }
  
  for (s in 1:S) {
    
    mcmc_iter <- met_glm_iter(Y, X, Xb, beta, log_post, alpha, gamma, delta, sigma, q, can_sd)
    
    Xb <- mcmc_iter$Xb
    
    beta <- mcmc_iter$beta
    
    log_post <- mcmc_iter$log_post
    
    alpha <- mcmc_iter$alpha
    
    gamma <- mcmc_iter$gamma
    
    delta <- mcmc_iter$delta
    
    sigma <- mcmc_iter$sigma
    
    q <- mcmc_iter$q
    
    mcmc_ssvs[s, ] <- c(alpha, gamma, delta, sigma, q)
    
  }
  
  
  
  return(mcmc_ssvs)
  
}



