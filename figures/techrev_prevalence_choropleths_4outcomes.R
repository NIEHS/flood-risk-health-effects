
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
library(sf)

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



# plotting just for NC to test out code

all_ct_df_NC <- all_ct_df[all_ct_df$STATEFP10 == 37, ]

# p0 <- tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "Coronary Heart Disease\nPrevalence", style = "cont") + 
#   tm_layout(legend.position = c("left", "bottom"), frame = F)
# p <- tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "Coronary Heart Disease\nPrevalence", style = "cont", legend.show = F)
# p2 <- tm_shape(all_ct_df_NC) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "Coronary Heart Disease\nPrevalence", style = "cont") + 
#   tm_layout(legend.only = T, legend.position = c("left", "bottom"))
# p_combined <- tmap_arrange(p0, p, p, p2, ncol = 2, nrow = 2)



# # 2/14/24 make legend smaller, without frame, increasing bounding box, see if it stays consistent throughout the panels
# # source: https://www.jla-data.net/eng/adjusting-bounding-box-of-a-tmap-map/
# 
# bbox_new <- st_bbox(all_ct_df_NC) # current bounding box
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# # bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
# # bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom
# # bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon
# 
# p_adj <- tm_shape(all_ct_df_NC, bbox = bbox_new) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "", style = "cont") +
#   tm_layout(legend.position = c("left", "bottom"), frame = F)
# 
# p_adj2 <- tm_shape(all_ct_df_NC, bbox = bbox_new) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "", style = "cont", legend.show = F) +
#   tm_layout(legend.position = c("left", "bottom"), frame = F)
# 
# p_adj3 <- tm_shape(all_ct_df_NC, bbox = bbox_new) +
#   tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "", style = "cont") +
#   tm_layout(legend.only = T, legend.position = c("left", "bottom"), frame = F)
# 
# p_comb <- tmap_arrange(p_adj, p_adj, p_adj2, p_adj3, ncol = 2, nrow = 2)
# tmap_save(p_comb, here("figures/final_figures/NC_test_tmap.pdf"),
#           width = 7, height = 8)







# Adjusting bounding box for all the plots in Figure 1

bbox_fig <- st_bbox(all_ct_df) # current bounding box
xrange <- bbox_fig$xmax - bbox_fig$xmin # range of x values
yrange <- bbox_fig$ymax - bbox_fig$ymin # range of y values

shift_multiple <- 0.5

# bbox_fig[1] <- bbox_fig[1] - (shift_multiple * xrange) # xmin - left
# bbox_fig[3] <- bbox_fig[3] + (shift_multiple * xrange) # xmax - right
bbox_fig[2] <- bbox_fig[2] - (shift_multiple * yrange) # ymin - bottom
# bbox_fig[4] <- bbox_fig[4] + (shift_multiple * yrange) # ymax - top

bbox_fig <- bbox_fig %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon





# # focusing on MHLTH as test case
# pE <- tm_shape(all_ct_df, bbox = bbox_fig) +
#   tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE) +
#   tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
# 
# # tmap_save(pE, here("figures/final_figures/MHLTH_mean_fitted_test.pdf"))
# 
# pE1 <- tm_shape(all_ct_df, bbox = bbox_fig) +
#   tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE, legend.show = F) +
#   tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
# 
# pE2 <- tm_shape(all_ct_df, bbox = bbox_fig) +
#   tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE) +
#   tm_layout(legend.only = T, frame = FALSE, legend.position = c("left", "bottom"))
# 
# p_combined <- tmap_arrange(pE, pE1, pE, pE2, pE1, pE2, ncol = 2, nrow = 3)
# tmap_save(p_combined, here("figures/final_figures/figure1_combined_test.pdf"))





# Plotting the maps, without legend

# flood risk PC1, or "consistent flood risk"
pA <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("flood_risk_pc1", palette = "PRGn", title = "", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26), legend.reverse = TRUE, legend.show = F) +
  tm_layout(frame = FALSE)
tmap_save(pA, here("figures/final_figures/consistent_flood_risk_fitted_no_legend.pdf"))

pB <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 5, 10, 15, 20, 25), legend.reverse = TRUE, legend.show = F) +
  tm_layout(frame = FALSE)
tmap_save(pB, here("figures/final_figures/CHD_mean_fitted_no_legend.pdf"))

pC <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("BPHIGH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 16, 32, 48, 64, 80), legend.reverse = TRUE, legend.show = F) +
  tm_layout(frame = FALSE)
tmap_save(pC, here("figures/final_figures/BPHIGH_mean_fitted_no_legend.pdf"))

pD <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("CASTHMA_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 4, 8, 12, 16, 20), legend.reverse = TRUE, legend.show = F) +
  tm_layout(frame = FALSE)
tmap_save(pD, here("figures/final_figures/CASTHMA_mean_fitted_no_legend.pdf"))

pE <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE, legend.show = F) +
  tm_layout(frame = FALSE)
tmap_save(pE, here("figures/final_figures/MHLTH_mean_fitted_no_legend.pdf"))

p_combined <- tmap_arrange(pA, pB, pC, pD, pE, ncol = 2, nrow = 3)
tmap_save(p_combined, here("figures/final_figures/figure1_combined_no_legend.pdf"))





# Plotting the maps, but only the legend instead of map

# flood risk PC1, or "consistent flood risk"
pA <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("flood_risk_pc1", palette = "PRGn", title = "", style = "cont", breaks = c(-4, 2, 8, 14, 20, 26), legend.reverse = TRUE) +
  tm_layout(legend.only = T, legend.position = c("left", "bottom"))
tmap_save(pA, here("figures/final_figures/consistent_flood_risk_fitted_legend_only.pdf"))

pB <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("CHD_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 5, 10, 15, 20, 25), legend.reverse = TRUE) +
  tm_layout(legend.only = T, legend.position = c("left", "bottom"))
tmap_save(pB, here("figures/final_figures/CHD_mean_fitted_legend_only.pdf"))

pC <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("BPHIGH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 16, 32, 48, 64, 80), legend.reverse = TRUE) +
  tm_layout(legend.only = T, legend.position = c("left", "bottom"))
tmap_save(pC, here("figures/final_figures/BPHIGH_mean_fitted_legend_only.pdf"))

pD <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("CASTHMA_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 4, 8, 12, 16, 20), legend.reverse = TRUE) +
  tm_layout(legend.only = T, legend.position = c("left", "bottom"))
tmap_save(pD, here("figures/final_figures/CASTHMA_mean_fitted_legend_only.pdf"))

pE <- tm_shape(all_ct_df, bbox = bbox_fig) +
  tm_fill("MHLTH_prevalence_smoothed", palette = "Greens", title = "", style = "cont", breaks = c(0, 7, 14, 21, 28, 35), legend.reverse = TRUE) +
  tm_layout(legend.only = T, legend.position = c("left", "bottom"))
tmap_save(pE, here("figures/final_figures/MHLTH_mean_fitted_legend_only.pdf"))

p_combined <- tmap_arrange(pA, pB, pC, pD, pE, ncol = 2, nrow = 3)
tmap_save(p_combined, here("figures/final_figures/figure1_combined_legend_only.pdf"))




