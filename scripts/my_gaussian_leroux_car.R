
library(Rcpp)

# Put in cpp helper functions from duncanplee's code below

cppFunction('double quadform(NumericMatrix Wtriplet, NumericVector Wtripletsum, const int n_triplet, const int nsites, 
                    NumericVector phi, NumericVector theta, double rho)
{
// Compute a quadratic form for the random effects
// Create new objects 
double tau2_posteriorscale;
double tau2_quadform = 0, tau2_phisq = 0;
int row, col;
   
   
// Compute the off diagonal elements of the quadratic form
     for(int l = 0; l < n_triplet; l++)
     {
     row = Wtriplet(l,0) - 1;
     col = Wtriplet(l,1) - 1;
     tau2_quadform = tau2_quadform + phi[(Wtriplet(l,0) - 1)] * theta[(Wtriplet(l,1) - 1)] * Wtriplet(l,2); 
     }
 
 
 // Compute the diagonal elements of the quadratic form          
     for(int l = 0; l < nsites; l++)
     {
     tau2_phisq = tau2_phisq + phi[l] * theta[l] * (rho * Wtripletsum[l] + 1 - rho);    
     }
           
     
// Compute the quadratic form
tau2_posteriorscale = 0.5 * (tau2_phisq - rho * tau2_quadform);

 
// Return the simulated value
return tau2_posteriorscale;
}')



cppFunction('// [[Rcpp::export]]
NumericVector gaussiancarupdate(NumericMatrix Wtriplet, NumericMatrix Wbegfin, 
     NumericVector Wtripletsum, const int nsites, NumericVector phi, double tau2, 
     double rho, double nu2, NumericVector offset)
{
// Update the spatially correlated random effects 
//Create new objects
int rowstart=0, rowend=0;
double sumphi;
double fcprecision, fcsd, fcmean;
double priorvardenom, priormean, priorvar;
NumericVector phinew(nsites);


//  Update each random effect in turn
phinew = phi;
     for(int j = 0; j < nsites; j++)
     {
     // Calculate prior variance
     priorvardenom = rho * Wtripletsum[j] + 1 - rho;
     priorvar = tau2 / priorvardenom;
     
     // Calculate the prior mean
     rowstart = Wbegfin(j,0) - 1;
     rowend = Wbegfin(j,1);
     sumphi = 0;
          for(int l = rowstart; l < rowend; l++) sumphi += Wtriplet(l, 2) * phinew[(Wtriplet(l,1) - 1)];
     priormean = rho * sumphi / priorvardenom; 
      
      // propose a value  
      fcprecision = (1/nu2) + (1/priorvar);
      fcsd = pow((1/fcprecision),0.5);
      fcmean = (priormean / priorvar + offset[j]) / fcprecision;
      phinew[j] = rnorm(1, fcmean, fcsd)[0];      
      }

return phinew;
}')







# Taken from duncanplee's code

common.Wcheckformat <- function(W)
{
  #### Check W is a matrix of the correct dimension
  if(!is.matrix(W)) stop("W is not a matrix.", call.=FALSE)
  n <- nrow(W)
  if(ncol(W)!= n) stop("W is not a square matrix.", call.=FALSE)    
  
  
  #### Check validity of inputed W matrix
  if(sum(is.na(W))>0) stop("W has missing 'NA' values.", call.=FALSE)
  if(!is.numeric(W)) stop("W has non-numeric values.", call.=FALSE)
  if(min(W)<0) stop("W has negative elements.", call.=FALSE)
  if(sum(W!=t(W))>0) stop("W is not symmetric.", call.=FALSE)
  if(min(apply(W, 1, sum))==0) stop("W has some areas with no neighbours (one of the row sums equals zero).", call.=FALSE)    
  
  
  #### Create the triplet form
  ids <- which(W > 0, arr.ind = T)
  W.triplet <- cbind(ids, W[ids])
  W.triplet <- W.triplet[ ,c(2,1,3)]
  
  #W.triplet <- c(NA, NA, NA)
  #for(i in 1:n)
  #{
  #    for(j in 1:n)
  #    {
  #        if(W[i,j]>0)
  #        {
  #            W.triplet <- rbind(W.triplet, c(i,j, W[i,j]))     
  #        }else{}
  #    }
  #}
  #W.triplet <- W.triplet[-1, ]     
  n.triplet <- nrow(W.triplet) 
  W.triplet.sum <- tapply(W.triplet[ ,3], W.triplet[ ,1], sum)
  n.neighbours <- tapply(W.triplet[ ,3], W.triplet[ ,1], length)
  
  
  #### Create the start and finish points for W updating
  W.begfin <- cbind(c(1, cumsum(n.neighbours[-n])+1), cumsum(n.neighbours))
  #W.begfin <- array(NA, c(n, 2))     
  #temp <- 1
  #for(i in 1:n)
  #{
  #    W.begfin[i, ] <- c(temp, (temp + n.neighbours[i]-1))
  #    temp <- temp + n.neighbours[i]
  #}
  
  
  #### Return the critical quantities
  results <- list(W=W, W.triplet=W.triplet, n.triplet=n.triplet, W.triplet.sum=W.triplet.sum, n.neighbours=n.neighbours, W.begfin=W.begfin, n=n)
  return(results)   
}



# Taken from duncanplee's code
# Acceptance rates - maximum limit on the proposal sd
common.accceptrates2 <- function(accept, sd, min, max, sd.max)
{
  #### Update the proposal standard deviations
  rate <- 100 * accept[1] / accept[2]
  
  if(rate > max)
  {
    sd <- sd + 0.1 * sd
    sd[which(sd>sd.max)] <- sd.max
  }else if(rate < min)              
  {
    sd <- sd - 0.1 * sd
  }else
  {
  }
  
  return(sd)
}



#' Function to compute the log posterior for the spatial markov model M-H steps
#' Just used for parameter rho
#'
#' @param y response variable
#' @param parameters: phi, W, num_nbr_mat, sigma2, rho, beta, nu2, X
#' later on this will involve M, phi~
#' @return log of the posterior density 
#' @export
log_post_agdsq <- function(Y, phi, W, num_nbr_mat, sigma2, rho,
                           beta, nu2, X) {
  
  K <- length(Y)
  
  like <- sum(dnorm(Y, X %*% beta + phi, sqrt(nu2), log = TRUE))
  
  # spatial terms
  Sigma <- sigma2 * solve((1 - rho) * diag(K) + rho * (num_nbr_mat - W))
  spatial_like <- sum(dmvnorm(phi, mean = rep(0, K), sigma = Sigma, log = T))
  
  # cluster means
  cluster_mean_like <- sum(dnorm(beta, 0, sqrt(b), log = TRUE))
  
  
  
  # prior <- dnorm(alpha, 0, 100, log = TRUE) + # intercept
  #   dbinom(sum(gamma), length(gamma), q, log = TRUE) + # nonzero slope indicators
  #   sum(dnorm(delta, 0, sigma, log = TRUE)) + # slopes
  #   dhcauchy(x = sigma, 0.5, log = TRUE) # sigma
  
  
  
  return(like + spatial_like + cluster_mean_like)
  
}





#' Metropolis-Gibbs algorithm for CAR model
#'
#' @param Y response variable
#' @param data covariates data frame (without intercept). Assumed to consist of factor variables or standardized continuous variables.
#' @param W adjacency matrix (assuming no additional islands)
#' @param inits initialize the parameters
#' @param can_sd list containing the candidate standard deviations for alpha, delta, sigma, q
#' @param n_burn_in number of burn-in iterations
#' @param n_iter number of kept iterations
#' @return MCMC chain
#' @export
met_gibbs_car <- function(Y, data, inits, can_sd, n_burn_in, n_iter) {
  
  X <- model.matrix(~., data = data)
  
  K <- length(Y)
  
  p <- ncol(X)
  
  # Create empty matrix for MCMC samples
  
  S <- n_iter
  
  mcmc_ssvs <- matrix(NA, S, length(unlist(inits))) # tracking alpha, gamma, delta, and sigma
  
  # colnames(mcmc_ssvs) <- c("alpha", rep("gamma", length(inits$gamma)), rep("delta", p - 1), "sigma", "q")
  
  
  
  # Initial values
  
  # alpha <- inits$alpha
  # 
  # gamma <- inits$gamma
  # 
  # delta <- inits$delta
  # 
  # sigma <- inits$sigma
  # 
  # q <- inits$q
  
  
  
  
  
  Xb <- X %*% beta
  
  log_post <- log_post_agdsq(Y, Xb, alpha, gamma, delta, sigma, q)
  
  
  
  fix.rho <- FALSE
  
  #### CAR quantities (make sparse)
  W.quants <- common.Wcheckformat(W)
  W <- W.quants$W
  W.triplet <- W.quants$W.triplet
  n.triplet <- W.quants$n.triplet
  W.triplet.sum <- W.quants$W.triplet.sum
  n.neighbours <- W.quants$n.neighbours 
  W.begfin <- W.quants$W.begfin
  
  
  
  # Prior quantities
  
  ## beta update quantities (Gibbs)
  prior.var.beta <- rep(100000, p)
  prior.precision.beta <- solve(diag(prior.var.beta))
  data.precision.beta <- data.precision.beta <- t(X) %*% X
  prior.mean.beta <- rep(0, p)
  
  ## nu2 update quantities (Gibbs)
  prior.nu2 <- c(1, 0.01) # prior shape and scale
  nu2.posterior.shape <- prior.nu2[1] + 0.5*K
  
  ## sigma2 update quantities (Gibbs)
  prior.sigma2 <- c(1, 0.01) # prior shape and scale
  sigma2.posterior.shape <- prior.sigma2[1] + 0.5*K
  
  ## rho update quantities (Metropolis-Hastings)
  accept <- rep(0,2)
  proposal.sd.rho <- 0.02
  
  ### current determinant (make sparse)
  if(!fix.rho)
  {
    Wstar <- diag(W.quants$n.neighbours) - W
    Wstar.eigen <- eigen(Wstar)
    Wstar.val <- Wstar.eigen$values
    det.Q <- 0.5 * sum(log((rho * Wstar.val + (1-rho))))    
  }
  
  
  
  for (s in 1:(S + n_burn_in)) {
    
    
    
    # Metropolis/Gibbs for beta, nu2, sigma2, phi, rho
    
    ## beta
    fc.precision <- prior.precision.beta + data.precision.beta / nu2
    fc.var <- solve(fc.precision)
    beta.offset <- Y - phi
    beta.offset2 <- t(X) %*% beta.offset / nu2 + prior.precision.beta %*% prior.mean.beta
    fc.mean <- fc.var %*% beta.offset2
    chol.var <- t(chol(fc.var))
    beta <- fc.mean + chol.var %*% rnorm(p)
    
    ## nu2
    fitted.current <-  as.numeric(X %*% beta) + phi
    nu2.posterior.scale <- prior.nu2[2] + 0.5 * sum((Y - fitted.current)^2)
    nu2 <- 1 / rgamma(1, nu2.posterior.shape, scale=(1/nu2.posterior.scale))  
    
    ## phi 
    offset.phi <- (Y - as.numeric(X %*% beta)) / nu2    
    phi <- gaussiancarupdate(Wtriplet=W.triplet, Wbegfin=W.begfin, W.triplet.sum, nsites=K, phi=phi, tau2=sigma2, rho=rho, nu2=nu2, offset=offset.phi)
    if(rho<1)
    {
      phi <- phi - mean(phi)
    }
    
    ## sigma2
    temp2 <- quadform(W.triplet, W.triplet.sum, n.triplet, K, phi, phi, rho)
    sigma2.posterior.scale <- prior.sigma2[2] + temp2 
    sigma2 <- 1 / rgamma(1, sigma2.posterior.shape, scale=(1/sigma2.posterior.scale))
    
    ## rho 
    if(!fix.rho)
    {
      proposal.rho <- rtruncnorm(n=1, a=0, b=1, mean=rho, sd=proposal.sd.rho)  
      temp3 <- quadform(W.triplet, W.triplet.sum, n.triplet, K, phi, phi, proposal.rho)
      det.Q.proposal <- 0.5 * sum(log((proposal.rho * Wstar.val + (1-proposal.rho))))              
      logprob.current <- det.Q - temp2 / sigma2
      logprob.proposal <- det.Q.proposal - temp3 / sigma2
      hastings <- log(dtruncnorm(x=rho, a=0, b=1, mean=proposal.rho, sd=proposal.sd.rho)) - log(dtruncnorm(x=proposal.rho, a=0, b=1, mean=rho, sd=proposal.sd.rho)) 
      prob <- exp(logprob.proposal - logprob.current + hastings)
      
      ## Accept or reject the proposal
      if(prob > runif(1))
      {
        rho <- proposal.rho
        det.Q <- det.Q.proposal
        accept[1] <- accept[1] + 1           
      }
      accept[2] <- accept[2] + 1           
    }
    
    
    
    if(ceiling(s/100) == floor(s/100) & s < n_burn_in)
    {
      #### Update the proposal sds
      if(!fix.rho)
      {
        proposal.sd.rho <- common.accceptrates2(accept[1:2], proposal.sd.rho, 40, 50, 0.5)
      }
      accept <- c(0,0)
    }
    
    
    
    # # sigma
    # can_sigma <- exp(rnorm(1, log(sigma), can_sigma_sd))
    # can_log_post <- log_post_agdsq(Y, Xb, alpha, gamma, delta, can_sigma, q)
    # logR <- can_log_post - log_post
    # if (log(runif(1)) < logR) {
    #   sigma <- can_sigma
    #   log_post <- can_log_post
    # }
    
    
    
    
    
    # mcmc_ssvs[s, ] <- c(alpha, gamma, delta, sigma, q)
    
  }
  
  
  
  return(mcmc_ssvs)
  
}



