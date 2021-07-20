
library(CARBayes)
library(here)

library(rstan)

library(CARBayesdata)
library(shapefiles)
library(sp)
library(spdep)

##### Reading in the component data files

W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_NC_census_tract.rds"))

fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_NC_census_tract.rds"))

# checking that the fips in W and fhs_model_df align
all.equal(as.numeric(colnames(W)), fhs_model_df$fips)



# extract the response variable

Y <- fhs_model_df$Data_Value_CHD

# extract the covariates matrix

X <- fhs_model_df[, 14:(ncol(fhs_model_df) - 1)]

X <- X[, names(X) != "pct_floodfactor1"]

X <- as.matrix(X)



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the counties 
# will have neighbors in W



# ##### CARBayes part
# 
# # TBC: quick version
# set.seed(1044, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
# 
# tick <- proc.time()[3]
# 
# chain1  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 1000,
#                        n.sample = 10000, thin = 5, verbose = TRUE)
# 
# tock <- proc.time()[3]
# 
# (tock-tick)/60 # time in minutes
# 
# save(chain1, file = here("modeling_files/model_NC_census_tract_quick_version.RData"))





set.seed(821, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

tick <- proc.time()[3]

chain1  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)
chain2  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)
chain3  <- S.CARleroux(Y ~ X, family="gaussian", W=W, burnin = 10000,
                       n.sample = 100000, thin = 5, verbose = TRUE)

tock <- proc.time()[3]

(tock-tick)/60 # time in minutes



save(chain1, chain2, chain3, file = here("modeling_files/model_3chains_model_NC_census_tract_fr_zip.RData"))







# ##### Stan part
# 
# # Example from Max Joseph's "Exact sparse CAR models in Stan"
# # rerun the example to see if you get similar results (DONE)
# # then run the example for NC census tracts, compare with above MCMC
# 
# library(rstan)
# 
# library(CARBayesdata)
# library(shapefiles)
# library(sp)
# library(spdep)
# library(CARBayes)
# data(lipdata)
# data(lipdbf)
# data(lipshp)
# 
# # processing the lipdata. This part is not necessary for the flood risk model, since I calculate
# # adjacency matrix W in a different way
# 
# lipdbf$dbf <- lipdbf$dbf[ ,c(2,1)]
# 
# data.combined <- combine.data.shapefile(data=lipdata, shp=lipshp, dbf=lipdbf)
# 
# W.nb <- poly2nb(data.combined, row.names = rownames(data.combined@data))
# W.list <- nb2listw(W.nb, style="B")
# moran.mc(x=rnorm(nrow(data.combined@data)), listw=W.list, nsim=1000) # using independent noise as the residuals. As expected, nonsignificant p-value
# 
# W <- nb2mat(W.nb, style="B")
# 
# 
# 
# 
# 
# 
# 
# options(mc.cores = parallel::detectCores())
# 
# 
# 
# scaled_x <- c(scale(data.combined@data$pcaff)) # just one variable
# X <- model.matrix(~scaled_x) # adding in intercept column
# 
# 
# 
# 
# 
# # below is the main input necessary for Stan, CAR with multi_normal_prec
# 
# full_d <- list(n = nrow(X),         # number of observations
#                p = ncol(X),         # number of coefficients
#                X = X,               # design matrix
#                y = data.combined@data$observed,               # observed number of cases
#                log_offset = log(data.combined@data$expected), # log(expected) num. cases
#                W = W)               # adjacency matrix
# 
# 
# 
# full_fit <- stan(here("scripts", "lipdata_stan_example.stan"), 
#                  data = full_d, 
#                  iter = 10000, chains = 4, verbose = FALSE)
# print(full_fit, pars = c('beta', 'tau', 'alpha', 'lp__'))
# 
# # visualize results 
# to_plot <- c('beta', 'tau', 'alpha', 'phi[1]', 'phi[2]', 'phi[3]', 'lp__')
# traceplot(full_fit, pars = to_plot)
# 
# 
# 
# 
# 
# # below is the main input necessary for Stan, sparse CAR
# 
# sp_d <- list(n = nrow(X),         # number of observations
#              p = ncol(X),         # number of coefficients
#              X = X,               # design matrix
#              y = data.combined@data$observed,               # observed number of cases
#              log_offset = log(data.combined@data$expected), # log(expected) num. cases
#              W_n = sum(W) / 2,    # number of neighbor pairs
#              W = W)               # adjacency matrix
# 
# sp_fit <- stan(here("scripts", "lipdata_stan_example.stan"), 
#                data = sp_d, 
#                iter = 10000, chains = 4, verbose = FALSE)
# 
# print(sp_fit, pars = c('beta', 'tau', 'alpha', 'lp__'))
# 
# to_plot <- c('beta', 'tau', 'alpha', 'phi[1]', 'phi[2]', 'phi[3]', 'lp__')
# 
# traceplot(sp_fit, pars = to_plot)





# ### Now, applying to NC census tracts
# 
# 
# 
# # below is the main input necessary for Stan, sparse CAR
# 
# nc_d <- list(n = nrow(X),         # number of observations
#              p = ncol(X),         # number of coefficients
#              X = X,               # design matrix
#              y = round(Y * fhs_model_df$TotalPopulation / 100),               # observed number of cases
#              log_offset = log(fhs_model_df$TotalPopulation), # log(expected) num. cases
#              W_n = sum(W) / 2,    # number of neighbor pairs
#              W = W)               # adjacency matrix
# 
# n_kept <- 1500
# n_warmup <- 500
# n_iter <- n_warmup + n_kept
# 
# nc_fit <- stan(here("scripts", "simpleCAR.stan"), 
#                data = nc_d, 
#                iter = n_iter, warmup = n_warmup, chains = 2, verbose = FALSE)
# 
# saveRDS(nc_fit, file = here("modeling_files/nc_fit2.rds"))
# 
# 
# 
# nc_fit <- readRDS(file = here("modeling_files/nc_fit2.rds"))
# 
# print(nc_fit, pars = c('beta', 'tau', 'alpha', 'lp__'))
# 
# to_plot <- c('tau', 'alpha', 'phi[1]', 'phi[2]', 'phi[3]', 'lp__')
# 
# traceplot(nc_fit, pars = to_plot)




# ### Same as above, but with Gaussian link instead
# 
# # below is the main input necessary for Stan, sparse CAR
# 
# nc_d <- list(n = nrow(X),         # number of observations
#              p = ncol(X),         # number of coefficients
#              X = X,               # design matrix
#              y = Y,               # observed number of cases
#              W_n = sum(W) / 2,    # number of neighbor pairs
#              W = W)               # adjacency matrix
# 
# n_kept <- 1500
# n_warmup <- 500
# n_iter <- n_warmup + n_kept
# 
# nc_fit <- stan(here("scripts", "simpleCAR_gaussian.stan"), 
#                data = nc_d, 
#                iter = n_iter, warmup = n_warmup, chains = 2, verbose = FALSE)
# 
# saveRDS(nc_fit, file = here("modeling_files/nc_fit3.rds"))
# 
# 
# 
# nc_fit <- readRDS(file = here("modeling_files/nc_fit3.rds"))
# 
# print(nc_fit, pars = c('beta', 'tau', 'alpha', 'lp__'))
# 
# to_plot <- c('tau', 'alpha', 'phi[1]', 'phi[2]', 'phi[3]', 'lp__')
# 
# traceplot(nc_fit, pars = to_plot)


