
library(Rcpp)
library(truncnorm)
library(coda)
library(Matrix)
library(RSpectra)

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



cppFunction('NumericVector gaussiancarupdate(NumericMatrix Wtriplet, NumericMatrix Wbegfin, 
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



# Taken from duncanplee's code
# Compute the DIC. WAIC,LMPL and loglikelihood
common.modelfit <- function(samples.loglike, deviance.fitted)
{
  #### WAIC
  p.w <- sum(apply(samples.loglike,2, var), na.rm=TRUE)
  mean.like <- apply(exp(samples.loglike),2,mean)
  mean.min <- min(mean.like[mean.like>0])
  mean.like[mean.like==0] <- mean.min
  lppd <- sum(log(mean.like), na.rm=TRUE)
  WAIC <- -2 * (lppd - p.w)
  
  
  #### Compute the Conditional Predictive Ordinate
  CPO <- 1/apply(exp(-samples.loglike), 2, mean)
  mean.min <- min(CPO[CPO>0])
  CPO[CPO==0] <- mean.min
  LMPL <- sum(log(CPO), na.rm=TRUE)    
  
  
  #### DIC
  mean.deviance <- -2 * sum(samples.loglike, na.rm=TRUE) /   nrow(samples.loglike)
  p.d <- mean.deviance - deviance.fitted
  DIC <- deviance.fitted + 2 * p.d
  
  
  #### loglikelihood
  loglike <- -0.5 * deviance.fitted
  
  
  #### Model fit criteria
  modelfit <- c(DIC, p.d, WAIC, p.w, LMPL, loglike)
  names(modelfit) <- c("DIC", "p.d", "WAIC", "p.w", "LMPL", "loglikelihood")
  return(modelfit)  
}



# Taken from duncanplee's code
# Compute the DIC. WAIC,LMPL and loglikelihood
common.modelfit.summarized.loglike <- function(mean.like, mean.recip.like, var.loglike, sum.loglike, n.keep, deviance.fitted)
{
  #### WAIC
  p.w <- sum(var.loglike, na.rm=TRUE)
  mean.min <- min(mean.like[mean.like>0])
  mean.like[mean.like==0] <- mean.min
  lppd <- sum(log(mean.like), na.rm=TRUE)
  WAIC <- -2 * (lppd - p.w)
  
  
  #### Compute the Conditional Predictive Ordinate
  CPO <- 1/mean.recip.like
  mean.min <- min(CPO[CPO>0])
  CPO[CPO==0] <- mean.min
  LMPL <- sum(log(CPO), na.rm=TRUE)    
  
  
  #### DIC
  mean.deviance <- -2 * sum.loglike / n.keep
  p.d <- mean.deviance - deviance.fitted
  DIC <- deviance.fitted + 2 * p.d
  
  
  #### loglikelihood
  loglike <- -0.5 * deviance.fitted
  
  
  #### Model fit criteria
  modelfit <- c(DIC, p.d, WAIC, p.w, LMPL, loglike)
  names(modelfit) <- c("DIC", "p.d", "WAIC", "p.w", "LMPL", "loglikelihood")
  return(modelfit)  
}



#' Metropolis-Gibbs algorithm for CAR model
#'
#' @param Y response variable (assuming no missing Y values for now)
#' @param data covariates data frame (without intercept). Assumed to consist of factor variables or standardized continuous variables.
#' @param W adjacency matrix (assuming no additional islands) in sparse matrix format (class ngCMatrix)
#' @param n_burn_in number of burn-in iterations
#' @param n_iter number of kept iterations
#' @param thin level of thinning to apply to the MCMC samples
#' @param keep_first vector of spatial units to keep the parameters for; for other subjects, 
#' keep track of the running posterior mean.
#' @return MCMC chain
#' @export
met_gibbs_car <- function(Y, data, W, n_burn_in, n_iter, thin = 1, 
                          keep_first = 1:nrow(data)) {
  
  X <- model.matrix(~., data = data)
  
  K <- nrow(data)
  
  p <- ncol(X)
  
  # Create empty matrix for MCMC samples
  
  S <- n_iter
  
  
  
  # Initial values
  
  # for missing Y values 
  
  n <- nrow(X)
  
  J <- length(Y) / n
  which.notna <- matrix(as.numeric(!is.na(Y)), nrow=n, ncol=J)
  if(J==1) which.notna <- as.numeric(which.notna)
  n.na <- n*J - sum(which.notna)
  
  Y.DA <- Y
  
  
  
  mod.glm <- lm(Y~X-1)
  beta.mean <- mod.glm$coefficients
  beta.sd <- sqrt(diag(summary(mod.glm)$cov.unscaled)) * summary(mod.glm)$sigma
  beta <- rnorm(n=length(beta.mean), mean=beta.mean, sd=beta.sd)
  
  res.temp <- Y - X %*% beta.mean
  res.sd <- sd(res.temp, na.rm=TRUE)/5
  phi <- rnorm(n=K, mean=rep(0,K), sd=res.sd)
  sigma2 <- var(phi) / 10
  nu2 <- sigma2
  fitted <- as.numeric(X %*% beta) + phi
  
  # Xb <- X %*% beta
  
  rho <- runif(1)
  fix.rho <- FALSE  
  
  
  
  #### Matrices to store samples
  n.keep <- floor(n_iter / thin)
  samples.beta <- array(NA, c(n.keep, p))
  samples.phi <- array(NA, c(n.keep, length(keep_first)))
  samples.nu2 <- array(NA, c(n.keep, 1))
  samples.sigma2 <- array(NA, c(n.keep, 1))
  if(!fix.rho) samples.rho <- array(NA, c(n.keep, 1))
  samples.loglike <- array(NA, c(n.keep, length(keep_first)))
  samples.fitted <- array(NA, c(n.keep, length(keep_first)))
  if(n.na>0) samples.Y <- array(NA, c(n.keep, n.na))
  
  
  
  # If length(keep_first) is less than K, then track the average of parameters across subjects
  # or other summary statistics, in the case of log-likelihood
  if (length(keep_first) < K) {
    
    mean.phi <- rep(0, K)
    
    mean.loglike <- rep(0, K)
    second.mom.loglike <- rep(0, K)
    
    mean.like <- rep(0, K)
    mean.recip.like <- rep(0, K)
    
    mean.fitted <- rep(0, K)
    
  }
  
  
  
  #### CAR quantities (made sparse)
  
  dp <- diff(W@p)
  W.triplet <- cbind(rep(seq_along(dp),dp), W@i + 1, 1) # W@i is 0-based
  n.triplet <- nrow(W.triplet)
  W.triplet.sum <- tapply(W.triplet[ ,3], W.triplet[ ,1], sum)
  W.begfin <- cbind(c(1, cumsum(W.triplet.sum[-nrow(W)])+1), cumsum(W.triplet.sum))
  
  
  
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
  ### This is actually the only place where the full W matrix is used
  if(!fix.rho)
  {
    Wstar <- Diagonal(x = W.triplet.sum) - W
    Wstar.eigen <- eigs_sym(Wstar, nrow(Wstar))
    Wstar.val <- Wstar.eigen$values
    det.Q <- 0.5 * sum(log((rho * Wstar.val + (1-rho))))    
  }
  
  
  
  for (s in 1:(S + n_burn_in)) {
    
    
    
    # Sample from Y - data augmentation
    
    if(n.na>0)
    {
      Y.DA[which.notna==0] <- rnorm(n=n.na, mean=fitted[which.notna==0], sd=sqrt(nu2))
    }
    
    # Metropolis/Gibbs for beta, nu2, sigma2, phi, rho
    
    ## beta
    fc.precision <- prior.precision.beta + data.precision.beta / nu2
    fc.var <- solve(fc.precision)
    beta.offset <- Y.DA - phi
    beta.offset2 <- t(X) %*% beta.offset / nu2 + prior.precision.beta %*% prior.mean.beta
    fc.mean <- fc.var %*% beta.offset2
    chol.var <- t(chol(fc.var))
    beta <- fc.mean + chol.var %*% rnorm(p)
    
    ## nu2
    fitted.current <-  as.numeric(X %*% beta) + phi
    nu2.posterior.scale <- prior.nu2[2] + 0.5 * sum((Y.DA - fitted.current)^2)
    nu2 <- 1 / rgamma(1, nu2.posterior.shape, scale=(1/nu2.posterior.scale))  
    
    ## phi 
    offset.phi <- (Y.DA - as.numeric(X %*% beta)) / nu2    
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
    
    ## calculate the deviance 
    fitted <- as.numeric(X %*% beta) + phi
    loglike <- dnorm(Y, mean = fitted, sd = rep(sqrt(nu2),K), log=TRUE)
    
    
    
    if(s > n_burn_in & (s-n_burn_in)%%thin==0)
    {
      ele <- (s - n_burn_in) / thin
      samples.beta[ele, ] <- beta
      samples.phi[ele, ] <- phi[keep_first]
      samples.nu2[ele] <- nu2
      samples.sigma2[ele] <- sigma2
      if(!fix.rho) samples.rho[ele] <- rho
      samples.loglike[ele, ] <- loglike[keep_first]
      samples.fitted[ele, ] <- fitted[keep_first]
      if(n.na>0) samples.Y[ele, ] <- Y.DA[which.notna==0]
      
      if (length(keep_first) < K) {
        mean.phi <- mean.phi + phi / n.keep
        
        mean.loglike <- mean.loglike + loglike / n.keep
        second.mom.loglike <- second.mom.loglike + loglike^2 / n.keep
        
        mean.like <- mean.like + exp(loglike) / n.keep
        mean.recip.like <- mean.recip.like + exp(-loglike) / n.keep
        
        mean.fitted <- mean.fitted + fitted / n.keep
      }
    }
    
    
    
    # Self-tune the acceptance probabilities
    if(ceiling(s/100) == floor(s/100) & s < n_burn_in)
    {
      #### Update the proposal sds
      if(!fix.rho)
      {
        proposal.sd.rho <- common.accceptrates2(accept[1:2], proposal.sd.rho, 40, 50, 0.5)
      }
      accept <- c(0,0)
    }
    
    
    
  }
  
  
  
  #### Compute the acceptance rates
  if(!fix.rho)
  {
    accept.rho <- 100 * accept[1] / accept[2]
  }else
  {
    accept.rho <- NA    
  }
  accept.final <- accept.rho
  
  
  
  #### Compute the fitted deviance
  mean.beta <- apply(samples.beta, 2, mean)
  
  if (length(keep_first) == K) {
    mean.phi <- apply(samples.phi, 2, mean)
  }
  
  fitted.mean <- X %*% mean.beta + mean.phi
  nu2.mean <- mean(samples.nu2)
  deviance.fitted <- -2 * sum(dnorm(Y, mean = fitted.mean, sd = rep(sqrt(nu2.mean),K), log = TRUE), na.rm=TRUE)
  
  
  
  #### Model fit criteria
  if (length(keep_first) == K) { 
    
    modelfit <- common.modelfit(samples.loglike, deviance.fitted) 
    
  } else {
    
    var.loglike <- (n.keep / (n.keep - 1)) * (second.mom.loglike - mean.loglike^2)
    
    sum.loglike <- sum(mean.loglike * n.keep, na.rm = T)
    
    modelfit <- common.modelfit.summarized.loglike(mean.like = mean.like, mean.recip.like = mean.recip.like, 
                                                   var.loglike = var.loglike, sum.loglike = sum.loglike, 
                                                   n.keep = n.keep, deviance.fitted = deviance.fitted) 
    
  }
  
  
  
  #### Create the Fitted values and residuals
  if (length(keep_first) == K) {
    mean.fitted <- apply(samples.fitted, 2, mean)
  }
  response.residuals <- Y - mean.fitted
  pearson.residuals <- response.residuals /sqrt(nu2.mean)
  residuals <- data.frame(response=response.residuals, pearson=pearson.residuals)
  
  
  
  if(n.na==0) samples.Y = NA
  
  if (length(keep_first) == K) {
    samples <- list(beta = mcmc(samples.beta), phi=mcmc(samples.phi), sigma2=mcmc(samples.sigma2),
                    nu2=mcmc(samples.nu2), rho=mcmc(samples.rho), fitted=mcmc(samples.fitted),
                    Y=mcmc(samples.Y))
  } else {
    samples <- list(beta = mcmc(samples.beta), phi=mcmc(samples.phi), mean.phi = mean.phi, sigma2=mcmc(samples.sigma2),
                    nu2=mcmc(samples.nu2), rho=mcmc(samples.rho), fitted=mcmc(samples.fitted),
                    Y=mcmc(samples.Y))
  }
  
  results <- list(samples=samples, mean.fitted=mean.fitted, residuals=residuals, modelfit=modelfit, accept=accept.final)
  
  
  
  return(results)
  
}



