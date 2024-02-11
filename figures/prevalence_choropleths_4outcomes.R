
library(here)
library(rgdal)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(viridis)
library(stringr)
library(geomerge)
library(tmap)

select <- dplyr::select



# using 2010 census tract boundaries

ct_files <- list.files(here("imported_data/census_tract_shapefiles/"))

shp_list <- vector("list", length = length(ct_files))

for (i in 1:length(ct_files)) {
  shp_list[[i]] <- readOGR(dsn = here("imported_data/census_tract_shapefiles", ct_files[i], paste0(ct_files[i], ".shp")), verbose = F)
}

# I use 2019 census tract boundaries for Virginia and South Dakota, because some of their 
# fips codes changed in the 2010s

# making names consistent between 2019 versions and 2010 versions

names(shp_list[[which(ct_files == "tl_2019_46_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])
names(shp_list[[which(ct_files == "tl_2019_51_tract")]]) <- names(shp_list[[which(ct_files == "tl_2010_01_tract10")]])

all_ct_df <- do.call("rbind", shp_list)





# loading requisite data

load(here("modeling_files/stratified_analysis/model_stratif_rpls.RData"))
CHD.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3
CHD.mean.phi <- (chain1$samples$mean.phi + chain2$samples$mean.phi + chain3$samples$mean.phi) / 3
CHD.mean.resid <- (chain1$residuals$response + chain2$residuals$response + chain3$residuals$response) / 3
CHD.mean.beta <- apply(rbind(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta), 2, mean)

load(here("modeling_files/stratified_analysis/model_stratif_rpls_BPHIGH.RData"))
BPHIGH.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3
BPHIGH.mean.phi <- (chain1$samples$mean.phi + chain2$samples$mean.phi + chain3$samples$mean.phi) / 3
BPHIGH.mean.resid <- (chain1$residuals$response + chain2$residuals$response + chain3$residuals$response) / 3
BPHIGH.mean.beta <- apply(rbind(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta), 2, mean)

load(here("modeling_files/stratified_analysis/model_stratif_rpls_CASTHMA.RData"))
CASTHMA.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3
CASTHMA.mean.phi <- (chain1$samples$mean.phi + chain2$samples$mean.phi + chain3$samples$mean.phi) / 3
CASTHMA.mean.resid <- (chain1$residuals$response + chain2$residuals$response + chain3$residuals$response) / 3
CASTHMA.mean.beta <- apply(rbind(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta), 2, mean)

load(here("modeling_files/stratified_analysis/model_stratif_rpls_MHLTH.RData"))
MHLTH.mean.fitted <- (chain1$mean.fitted + chain2$mean.fitted + chain3$mean.fitted) / 3
MHLTH.mean.phi <- (chain1$samples$mean.phi + chain2$samples$mean.phi + chain3$samples$mean.phi) / 3
MHLTH.mean.resid <- (chain1$residuals$response + chain2$residuals$response + chain3$residuals$response) / 3
MHLTH.mean.beta <- apply(rbind(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta), 2, mean)



fhs_model_df <- readRDS(here("intermediary_data/fhs_model_df_fr_and_pollute_pc.rds"))

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

# Flipped the flood_risk_pc1 variable to make it a "risk" variable

outcome_df <- data.frame(GEOID10 = fips_leading_zero, 
                         CHD_prevalence = fhs_model_df$Data_Value_CHD,
                         BPHIGH_prevalence = fhs_model_df$Data_Value_BPHIGH, 
                         CASTHMA_prevalence = fhs_model_df$Data_Value_CASTHMA,
                         MHLTH_prevalence = fhs_model_df$Data_Value_MHLTH,
                         CHD_prevalence_smoothed = CHD.mean.fitted, 
                         BPHIGH_prevalence_smoothed = BPHIGH.mean.fitted, 
                         CASTHMA_prevalence_smoothed = CASTHMA.mean.fitted, 
                         MHLTH_prevalence_smoothed = MHLTH.mean.fitted, 
                         CHD.mean.phi = CHD.mean.phi, 
                         BPHIGH.mean.phi = BPHIGH.mean.phi, 
                         CASTHMA.mean.phi = CASTHMA.mean.phi, 
                         MHLTH.mean.phi = MHLTH.mean.phi,
                         CHD.mean.resid = CHD.mean.resid, 
                         BPHIGH.mean.resid = BPHIGH.mean.resid, 
                         CASTHMA.mean.resid = CASTHMA.mean.resid, 
                         MHLTH.mean.resid = MHLTH.mean.resid,
                         flood_risk_pc1 = -fhs_model_df$flood_risk_pc1)

# truncating the occasional negative prevalence at zero
outcome_df$CHD_prevalence_smoothed <- pmax(outcome_df$CHD_prevalence_smoothed, 0)
outcome_df$BPHIGH_prevalence_smoothed <- pmax(outcome_df$BPHIGH_prevalence_smoothed, 0)
outcome_df$CASTHMA_prevalence_smoothed <- pmax(outcome_df$CASTHMA_prevalence_smoothed, 0)
outcome_df$MHLTH_prevalence_smoothed <- pmax(outcome_df$MHLTH_prevalence_smoothed, 0)





# make a subselected X matrix to do the lin prod with just flood risk PCs

# In case you're worried about the direction of the PCs, don't be. The signs
# of the beta coefficient and the PC will cancel out in the linear product.

first_var <- 19

# remove non-covariates, including the 4 response variables
X <- fhs_model_df[, -c(1:(first_var - 1), ncol(fhs_model_df) + c(-3, -2, -1, 0))]

strat_covariate <- fhs_model_df$RPL_THEMES
X <- select(X, -EP_POV, -EP_UNEMP, -EP_PCI, -EP_NOHSDP, 
                       -EP_AGE65, -EP_AGE17, -EP_DISABL, -EP_SNGPNT, 
                       -EP_MINRTY, -EP_LIMENG,
                       -EP_MUNIT, -EP_MOBILE, -EP_CROWD, -EP_NOVEH, -EP_GROUPQ)



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the spatial units
# will have neighbors in W

X <- data.frame(X)

# if the stratification variable has missing values, assume that they take the mean value, 
# which may be below or above the median value that everything is stratified on.

strat_covariate[is.na(strat_covariate)] <- mean(strat_covariate, na.rm = T)

strat_fn <- median

strat0 <- ifelse(strat_covariate <= strat_fn(strat_covariate), 1, 0)
strat1 <- ifelse(strat_covariate <= strat_fn(strat_covariate), 0, 1)



X_intx0 <- model.matrix(rep(1, nrow(X)) ~ strat0 + strat0:., data = X)[, -1]
X_intx1 <- model.matrix(rep(1, nrow(X)) ~ strat1 + strat1:., data = X)[, -1]

X_intx_cbind <- as.data.frame(cbind(X_intx0, X_intx1))



### Start of calculations for flood risk linear prediction

pc_idx <- c(2:6, 
            ncol(X_intx_cbind)/2 + 2:6)

X_intx_fr_pc <- X_intx_cbind[, pc_idx] # the X matrix to do the linear product, just for flood risk variables

# debug check: does the mean linear predictor of all variables (w/ X_intx_cbind) plus mean phi equal mean fitted?
# just for MHLTH
mean_lin_pred <- as.vector(as.matrix(X_intx_cbind) %*% MHLTH.mean.beta)
all.equal(mean_lin_pred + MHLTH.mean.phi, MHLTH.mean.fitted) # debug check passed

# Compute the linear predictor across MCMC loops, just for flood risk variables

CHD.mean.fr.pred <- as.vector(as.matrix(X_intx_fr_pc) %*% CHD.mean.beta[pc_idx])
BPHIGH.mean.fr.pred <- as.vector(as.matrix(X_intx_fr_pc) %*% BPHIGH.mean.beta[pc_idx])
CASTHMA.mean.fr.pred <- as.vector(as.matrix(X_intx_fr_pc) %*% CASTHMA.mean.beta[pc_idx])
MHLTH.mean.fr.pred <- as.vector(as.matrix(X_intx_fr_pc) %*% MHLTH.mean.beta[pc_idx])

# debug check: logical subsetting vs. numeric indexing
all.equal(CHD.mean.fr.pred, as.vector(as.matrix(X_intx_cbind) %*% (CHD.mean.beta*(1:26 %in% pc_idx)))) # debug check passed

# incorp into outcomes_df
outcome_df <- data.frame(outcome_df, CHD.mean.fr.pred, BPHIGH.mean.fr.pred, CASTHMA.mean.fr.pred, MHLTH.mean.fr.pred)



# merge outcome_df to all_ct_df's dataframe (@data)
all_ct_df@data <- left_join(all_ct_df@data, outcome_df)



# # plotting just for NC to test out code
#
all_ct_df_NC <- all_ct_df[all_ct_df$STATEFP10 == 37, ]
#
# p <- tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "Coronary Heart Disease\nPrevalence", style = "cont")
# tmap_save(p, here("figures/final_figures/NC_test_tmap.pdf"))
#
# # turn off boundaries and legend, use png with low res
# png(file = here("figures/final_figures/NC_test_tmap.png"), res = 100)
# tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", legend.show = F, palette = "Greens")
# dev.off()
#
# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/NC_test_tmap.jpeg"), width = 700)
tm_shape(all_ct_df_NC) +
  tm_fill("CHD.mean.fr.pred", palette = "Greens", midpoint = 0,
          title = "Coronary Heart Disease\nFlood Risk Linear Predictions", style = "cont", legend.reverse = TRUE)
# dev.off()



# Raw prevalences

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/CHD_prev.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CHD_prevalence", palette = "Greens",
          title = "Coronary Heart Disease\nPrevalence", style = "cont", breaks = c(0, 5, 10, 15, 20, 25), legend.reverse = TRUE) +
  guide_legend(reverse = T)
dev.off()

# there are just 2 values above the breakpoint of 25--those are omitted from the choropleth map. 

# # combining plots
# jpeg(file = here("figures/final_figures/test_combined.jpeg"), width = 700, height = 1050)
# tmap_arrange(test_plot, test_plot, test_plot, chd_plot, test_plot, ncol = 2, nrow = 3)
# dev.off()

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/BPHIGH_prev.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("BPHIGH_prevalence", palette = "Greens",
          title = "High Blood Pressure\nPrevalence", style = "cont", breaks = c(0, 16, 32, 48, 64, 80), legend.reverse = TRUE)
dev.off()

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/CASTHMA_prev.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CASTHMA_prevalence", palette = "Greens",
          title = "Asthma\nPrevalence", style = "cont", breaks = c(0, 4, 8, 12, 16, 20), legend.reverse = TRUE)
dev.off()

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/MHLTH_prev.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("MHLTH_prevalence", palette = "Greens",
          title = "Poor Mental Health\nPrevalence", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE)
dev.off()

# There occurs 
# Warning message: Values have found that are higher than the highest break 
# but the proportion of values above the bound is less than 0.00001 for each health outcome





# 4/5/2023: recalling how breaks are determined--breaks[1] is zero, breaks[6] is the maximum prevalence rounded up to a nice number
# breaks[2:5] are the 4 equispaced points between breaks[1] and breaks[6]

# for whole continental U.S.
# Pdf seems better, png is too blurry

# flood risk PC1, or "consistent flood risk"
pA <- tm_shape(all_ct_df) +
  tm_fill("flood_risk_pc1", palette = "PRGn", title = "A) Consistent Flood Risk Score", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26), legend.reverse = TRUE)
tmap_save(pA, here("figures/final_figures/consistent_flood_risk_fitted.pdf"))

# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/consistent_flood_risk_fitted.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("flood_risk_pc1", palette = "Greens",
#           title = "Consistent Flood Risk Score", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26))
# dev.off()

pB <- tm_shape(all_ct_df) +
  tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "B) Coronary Heart Disease\nPrevalence", style = "cont", breaks = c(0, 5, 10, 15, 20, 25), legend.reverse = TRUE)
tmap_save(pB, here("figures/final_figures/CHD_mean_fitted.pdf"))

# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/CHD_mean_fitted.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens",
#           title = "Coronary Heart Disease\nPrevalence", style = "cont", breaks = c(0, 5, 10, 15, 20, 25))
# dev.off()



pC <- tm_shape(all_ct_df) +
  tm_fill("BPHIGH_prevalence_smoothed", palette = "Greens", title = "C) High Blood Pressure\nPrevalence", style = "cont", breaks = c(0, 16, 32, 48, 64, 80), legend.reverse = TRUE)
tmap_save(pC, here("figures/final_figures/BPHIGH_mean_fitted.pdf"))

# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/BPHIGH_mean_fitted.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("BPHIGH_prevalence_smoothed", palette = "Greens",
#           title = "High Blood Pressure\nPrevalence", style = "cont", breaks = c(0, 16, 32, 48, 64, 80))
# dev.off()



pD <- tm_shape(all_ct_df) +
  tm_fill("CASTHMA_prevalence_smoothed", palette = "Greens", title = "D) Asthma\nPrevalence", style = "cont", breaks = c(0, 4, 8, 12, 16, 20), legend.reverse = TRUE)
tmap_save(pD, here("figures/final_figures/CASTHMA_mean_fitted.pdf"))

# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/CASTHMA_mean_fitted.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("CASTHMA_prevalence_smoothed", palette = "Greens",
#           title = "Asthma\nPrevalence", style = "cont", breaks = c(0, 4, 8, 12, 16, 20))
# dev.off()



pE <- tm_shape(all_ct_df) +
  tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "E) Poor Mental Health\nPrevalence", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE)
tmap_save(pE, here("figures/final_figures/MHLTH_mean_fitted.pdf"))

# # trying out jpeg with lossy compression
# jpeg(file = here("figures/final_figures/MHLTH_mean_fitted.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("MHLTH_prevalence_smoothed", palette = "Greens",
#           title = "Poor Mental Health\nPrevalence", style = "cont", breaks = c(0, 7, 14, 21, 28, 35))
# dev.off()


 
# combining plots
jpeg(file = here("figures/final_figures/figure1_combined.jpeg"), width = 800, height = 1200)
tmap_arrange(pA, pB, pC, pD, pE, ncol = 2, nrow = 3)
dev.off()

p_combined <- tmap_arrange(pA, pB, pC, pD, pE, ncol = 2, nrow = 3)
tmap_save(p_combined, here("figures/final_figures/figure1_combined.pdf"))

 

# # Checking out raw prevalences for MHLTH, just to check
#
# outcome_df_raw <- data.frame(GEOID10 = fips_leading_zero,
#                          MHLTH_prevalence = fhs_model_df$Data_Value_MHLTH)
#
# # merge outcome_df to all_ct_df's dataframe (@data)
#
# all_ct_df@data <- left_join(all_ct_df@data, outcome_df_raw)
#
# jpeg(file = here("figures/MHLTH_raw_prevalence.jpeg"), width = 700)
# tm_shape(all_ct_df) +
#   tm_fill("MHLTH_prevalence", palette = "Greens",
#           title = "Poor Mental Health\nRaw Prevalence", style = "cont")
# dev.off()





# # Plotting choropleths of the spatial random effects
# 
# p <- tm_shape(all_ct_df) +
#   tm_fill("CHD.mean.phi", palette = "Greens", title = "Coronary Heart Disease\nSpatial Random Effect", style = "cont")
# tmap_save(p, here("figures/final_figures/CHD_mean_phi.pdf"))
# 
# p <- tm_shape(all_ct_df) +
#   tm_fill("BPHIGH.mean.phi", palette = "Greens", title = "High Blood Pressure\nSpatial Random Effect", style = "cont")
# tmap_save(p, here("figures/final_figures/BPHIGH_mean_phi.pdf"))
# 
# p <- tm_shape(all_ct_df) +
#   tm_fill("CASTHMA.mean.phi", palette = "Greens", title = "Asthma\nSpatial Random Effect", style = "cont")
# tmap_save(p, here("figures/final_figures/CASTHMA_mean_phi.pdf"))
# 
# p <- tm_shape(all_ct_df) +
#   tm_fill("MHLTH.mean.phi", palette = "Greens", title = "Poor Mental Health\nSpatial Random Effect", style = "cont")
# tmap_save(p, here("figures/final_figures/MHLTH_mean_phi.pdf"))



# Plotting choropleths of the spatial random effects, jpeg versions

# 3/17/23 change: using divergent color palette instead of Greens

# trying out jpeg with lossy compression
jpeg(file = here("figures/final_figures/CHD_mean_phi.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CHD.mean.phi", palette = "PRGn", midpoint = 0, title = "Coronary Heart Disease\nSpatial Random Effect", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/BPHIGH_mean_phi.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("BPHIGH.mean.phi", palette = "PRGn", midpoint = 0, title = "High Blood Pressure\nSpatial Random Effect", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/CASTHMA_mean_phi.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CASTHMA.mean.phi", palette = "PRGn", midpoint = 0, title = "Asthma\nSpatial Random Effect", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/MHLTH_mean_phi.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("MHLTH.mean.phi", palette = "PRGn", midpoint = 0, title = "Poor Mental Health\nSpatial Random Effect", style = "cont", legend.reverse = TRUE)
dev.off()



# # Plotting choropleths of the non-spatial residuals
# 
# p <- tm_shape(all_ct_df) +
#   tm_fill("CHD.mean.resid", palette = "Greens", title = "Coronary Heart Disease\nNon-Spatial Residuals", style = "cont")
# tmap_save(p, here("figures/final_figures/CHD_mean_resid.pdf"))



# Plotting the linear predictions of flood risk predictors only

jpeg(file = here("figures/final_figures/CHD_mean_fr_pred.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CHD.mean.fr.pred", palette = "PRGn", midpoint = 0, title = "Coronary Heart Disease\nFlood Risk Linear Predictions", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/BPHIGH_mean_fr_pred.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("BPHIGH.mean.fr.pred", palette = "PRGn", midpoint = 0, title = "High Blood Pressure\nFlood Risk Linear Predictions", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/CASTHMA_mean_fr_pred.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("CASTHMA.mean.fr.pred", palette = "PRGn", midpoint = 0, title = "Asthma\nFlood Risk Linear Predictions", style = "cont", legend.reverse = TRUE)
dev.off()

jpeg(file = here("figures/final_figures/MHLTH_mean_fr_pred.jpeg"), width = 700)
tm_shape(all_ct_df) +
  tm_fill("MHLTH.mean.fr.pred", palette = "PRGn", midpoint = 0, title = "Poor Mental Health\nFlood Risk Linear Predictions", style = "cont", legend.reverse = TRUE)
dev.off()


