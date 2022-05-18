
# Demonstration of the adjusted CARBayes code on a small example dataset

# This is based on the vignette for the CARBayes package 
# (https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf).
# See the vignette for more details on analyzing this dataset.

library(here)

# The Greater Glasgow property prices dataset is included in the 
# CARBayesdata package.
library(CARBayesdata)

library(sp)
library(dplyr)
library(spdep)
library(rgdal)
library(Matrix)
library(coda)

data(GGHB.IG)
data(pricedata)

head(pricedata)

# The response variable is the log of the median price (in thousands)
# of all properties sold in 2008 in each intermediate geography (IG).
pricedata <- pricedata %>% mutate(logprice = log(pricedata$price))

# The response variable is assumed to be Gaussian.
hist(pricedata$logprice)

# make type a factor 
pricedata$type <- as.factor(pricedata$type)



# retrieving the adjacency matrix of the IGs
pricedata.sp <- merge(x=GGHB.IG, y=pricedata, by="IG", all.x=FALSE)
pricedata.sp <- spTransform(pricedata.sp,
                            CRS("+proj=longlat +datum=WGS84 +no_defs"))
W.nb <- poly2nb(pricedata.sp, row.names = rownames(pricedata.sp@data))
W <- nb2mat(W.nb, style="B")

# turn W into ngCMatrix, which is the format expected by my 
# Leroux CAR implementation

W <- as(W, "ngCMatrix")



# loading my implementation of Leroux CAR, in the function met_gibbs_car
source(here("scripts/my_gaussian_leroux_car.R"))

#' Function to conveniently run several chains with my Leroux CAR implementation
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
  
  
  
  # dealing with missing values via mean/mode imputation
  
  is_factor <- sapply(X, function(vec) is.factor(vec))
  for (i in 1:length(is_factor)) {
    if (!is_factor[i]) { # skip the factors when scaling
      X[, i] <- scale(X[, i])
      X[is.na(X[, i]), i] <- 0
    } else { # replace missing factor with most common factor
      tt <- table(X[, i])
      most_freq_level <- names(tt[which.max(tt)])
      X[is.na(X[, i]), i] <- most_freq_level
    }
  }
  
  # if I do mean imputation, all the spatial units
  # will have neighbors in W
  
  X <- data.frame(X)
  
  
  
  chain_list <- vector("list", length = num_chains)
  
  for (i in 1:num_chains) {
    
    chain_list[[i]]  <- met_gibbs_car(Y, data = X, W, rho = rho, n_burn_in = n_burn_in, n_iter = n_iter, thin = thin,
                                      keep_first = keep_first)
    
  }
  
  
  
  return(chain_list)
  
}



# applying the function

set.seed(1158, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

chain_list <- fhs_car_chains(pricedata, first_var = 3, W = W, rho = NULL,
                             n_burn_in = 10000, n_iter = 100000, thin = 2,
                             num_chains = 3)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes

# Takes about 1.5 minutes to run

chain1 <- chain_list[[1]]

chain2 <- chain_list[[2]]

chain3 <- chain_list[[3]]



# Exploring the output of a chain

summary(chain1) # based on the output of the corresponding CARBayes function, S.CARleroux
# Length Class      Mode   
# samples       7    -none-     list   
# mean.fitted 270    -none-     numeric
# residuals     2    data.frame list   
# modelfit      6    -none-     numeric
# accept        1    -none-     numeric



# Model Diagnostics

beta.samples <- mcmc.list(chain1$samples$beta, chain2$samples$beta,
                          chain3$samples$beta)
# traceplot of coefficient corresponding to "sales"
plot(beta.samples[, 4])

gelman.diag(beta.samples)



# Inference

beta.samples.matrix <- rbind(chain1$samples$beta, chain2$samples$beta,
                             chain3$samples$beta)
colnames(beta.samples.matrix) <- c("Intercept", "Crime", "Rooms", 
                                   "Sales", "Driveshop", "TypeFlat", 
                                   "TypeSemi", "TypeTerrace")

round(t(apply(beta.samples.matrix, 2, quantile, c(0.5, 0.025, 0.975))),5)


