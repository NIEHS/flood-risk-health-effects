
library(CARBayes)
library(here)

# see spatial_markov_model_sim.Rmd for example CARBayes code



# Reading in the component data files

W <- readRDS(here("intermediary_data", "countyadj_reorganize.rds"))

fls_model_df <- readRDS(here("intermediary_data/fls_model_df.rds"))



# extract the response variable

Y <- fls_model_df$`Life expectancy, 2014*`

# extract the covariates matrix

X <- fls_model_df[, 17:(ncol(fls_model_df) - 1)]

X <- as.matrix(X)

# # omit rows with missing covariate values
# Y <- Y[complete.cases(X)]
# X_complete <- X[complete.cases(X), ]
# W <- W[complete.cases(X), ]
# W <- W[, complete.cases(X)]
# # otherwise, S.CARleroux returns "Error: the covariate matrix contains missing 'NA' values."
# # approach doesn't work: "Error: W has some areas with no neighbours (one of the row sums equals zero)."

X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the counties 
# will have neighbors in W



tick <- proc.time()[3]

model1  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



save(model1, file = here("modeling_files/fls_model_df.rds"))




# # trying a subset of the observations
# 
# set.seed(923)
# 
# subset_idx <- sample(1:nrow(X), size = 100)
# 
# Y_sub <- Y[subset_idx]
# 
# X_sub <- X[subset_idx,]
# 
# W_sub <- 







# #################################################
# #### Run the model on simulated data on a lattice
# #################################################
# #### Load other libraries required
# library(MASS)
# 
# #### Set up a square lattice region
# x.easting <- 1:10
# x.northing <- 1:10
# Grid <- expand.grid(x.easting, x.northing)
# K <- nrow(Grid)
# 
# #### set up distance and neighbourhood (W, based on sharing a common border) matrices
# distance <- as.matrix(dist(Grid))
# W <-array(0, c(K,K))
# W[distance==1] <-1 	
# 
# #### Generate the covariates and response data
# x1 <- rnorm(K)
# x2 <- rnorm(K)
# theta <- rnorm(K, sd=0.05)
# phi <- mvrnorm(n=1, mu=rep(0,K), Sigma=0.4 * exp(-0.1 * distance))
# logit <- x1 + x2 + theta + phi
# prob <- exp(logit) / (1 + exp(logit))
# trials <- rep(50,K)
# Y <- rbinom(n=K, size=trials, prob=prob)
# 
# 
# #### Run the Leroux model
# formula <- Y ~ x1 + x2
# # ## Not run: model <- S.CARleroux(formula=formula, family="binomial",
# # trials=trials, W=W, burnin=20000, n.sample=100000)
# # ## End(Not run)
# 
# #### Toy example for checking
# model <- S.CARleroux(formula=formula, family="binomial", 
#                      trials=trials, W=W, burnin=10, n.sample=50)
# 

