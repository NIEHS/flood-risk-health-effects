# 
# library(CARBayes)
# library(here)
# 
# # see spatial_markov_model_sim.Rmd for example CARBayes code
# 
# 
# 
# # Reading in the component data files
# 
# W <- readRDS(here("intermediary_data", "countyadj_reorganize.rds"))
# 
# fls_model_df <- readRDS(here("intermediary_data/fls_model_df.rds"))
# 
# # checking that the fips in W and fhs_model_df align
# all.equal(as.numeric(colnames(W)), fls_model_df$fips)
# 
# 
# 
# # extract the response variable
# 
# Y <- fls_model_df$`Life expectancy, 2014*`
# 
# # extract the covariates matrix
# 
# X <- fls_model_df[, 12:(ncol(fls_model_df) - 1)]
# 
# 
# 
# X <- X[, names(X) != "pct_floodfactor1"]
# 
# # the age-related CDC SVI variables should probably not be in here
# X <- X[, !(names(X) %in% c("EP_AGE65", "EP_AGE17"))]
# 
# # exclude some more variables selected by vifstep, to account for multicollinearity
# # excluding all of the pct_fs_risk variables, as well as 3 of the avg_risk_score variables
# # omit daily_mean too, it's collinear with total_mean
# 
# collin_var_names <- c("avg_risk_score_all", "pct_fs_risk_2050_500", "pct_fs_risk_2020_500",
#                       "avg_risk_fsf_2020_500", "pct_fs_risk_2050_5", "pct_fs_risk_2020_100",
#                       "daily_mean", "pct_fs_risk_2050_100", "avg_risk_score_2_10", "pct_floodfactor10")
# 
# X <- X[, !(names(X) %in% collin_var_names)]
# 
# 
# 
# X <- as.matrix(X)
# 
# # # omit rows with missing covariate values
# # Y <- Y[complete.cases(X)]
# # X_complete <- X[complete.cases(X), ]
# # W <- W[complete.cases(X), ]
# # W <- W[, complete.cases(X)]
# # # otherwise, S.CARleroux returns "Error: the covariate matrix contains missing 'NA' values."
# # # approach doesn't work: "Error: W has some areas with no neighbours (one of the row sums equals zero)."
# 
# X           <- scale(X) # Scale covariates
# X[is.na(X)] <- 0        # Fill in missing values with the mean
# 
# # if I do mean imputation (which may be problematic), all the counties
# # will have neighbors in W
# 
# 
# 
# set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
#                        n.sample = 100000, thin = 5, verbose = TRUE)
# chain2  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
#                        n.sample = 100000, thin = 5, verbose = TRUE)
# chain3  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
#                        n.sample = 100000, thin = 5, verbose = TRUE)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# 
# 
# save(chain1, chain2, chain3, file = here("modeling_files/model_3chains_var_exclude.RData"))






#################

# 9/8 change: make W into a sparse "ngCMatrix"
# also use my_gaussian_leroux_car instead of CARBayes

# 10/14 change: omitting 10 observations to test data augmentation

library("here")

i_am("scripts/CARmodeling.R")

source(here("scripts/my_gaussian_leroux_car.R"))

# Reading in the component data files

W <- readRDS(here("intermediary_data", "countyadj_reorganize.rds"))

W <- as(W, "ngCMatrix")

fls_model_df <- readRDS(here("intermediary_data/fls_model_df.rds"))

# checking that the fips in W and fhs_model_df align
all.equal(as.numeric(colnames(W)), fls_model_df$fips)



# extract the response variable

Y <- fls_model_df$`Life expectancy, 2014*`

# randomly deleting observations to test data augmentation

set.seed(742)

Y[sample(1:length(Y), size = 10)] <- NA



# extract the covariates matrix

X <- fls_model_df[, 12:(ncol(fls_model_df) - 1)]



X <- X[, names(X) != "pct_floodfactor1"]

# the age-related CDC SVI variables should probably not be in here
X <- X[, !(names(X) %in% c("EP_AGE65", "EP_AGE17"))]

# exclude some more variables selected by vifstep, to account for multicollinearity
# excluding all of the pct_fs_risk variables, as well as 3 of the avg_risk_score variables
# omit daily_mean too, it's collinear with total_mean

collin_var_names <- c("avg_risk_score_all", "pct_fs_risk_2050_500", "pct_fs_risk_2020_500",
                      "avg_risk_fsf_2020_500", "pct_fs_risk_2050_5", "pct_fs_risk_2020_100",
                      "daily_mean", "pct_fs_risk_2050_100", "avg_risk_score_2_10", "pct_floodfactor10") 

X <- X[, !(names(X) %in% collin_var_names)]



X <- as.matrix(X)



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

X <- data.frame(X)



# only one chain instead of 3, like above

set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

chain1  <- met_gibbs_car(Y, data = X, W, rho = 1, n_burn_in = 10000, n_iter = 90000, thin = 5, keep_first = 1:5)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



save(chain1, file = here("modeling_files/model_1chain_var_exclude_sparse.RData"))


