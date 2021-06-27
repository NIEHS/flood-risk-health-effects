
library(CARBayes)

# see spatial_markov_model_sim.Rmd for example CARBayes code



# Reading in the component data files

W <- readRDS(here("intermediary_data", "countyadj_reorganize.rds"))

fls_model_df <- readRDS(here("intermediary_data/fls_model_df.rds"))



# extract the response variable

Y <- fls_model_df$`Life expectancy, 2014*`

# extract the covariates matrix

X <- fls_model_df[, 17:(ncol(fls_model_df) - 1)]

X <- as.matrix(X)

# omit rows with missing covariate values
Y <- Y[complete.cases(X)]
X <- X[complete.cases(X), ]
# otherwise, S.CARleroux returns "Error: the covariate matrix contains missing 'NA' values."



X <- X[, 1:22]



X <- scale(X)



tick <- proc.time()[3]

model1  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 2000, n.sample = 10000, thin = 1, verbose=TRUE)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes


