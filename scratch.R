# diagnosis:

aberrant_rows <- c()

for (i in 1:nrow(countyadj)) {
  
  if (!identical(countyadj[i, ], countyadj[, i])) {
    
    aberrant_rows <- c(aberrant_rows, i)
    
  }
  
}



fl_count_prop <- data.frame(flood_risk_prev$fips, flood_risk_prev$count_property)
head(fl_count_prop)
fl_count_prop <- data.frame(fips = flood_risk_prev$fips, fl_count_prop = flood_risk_prev$count_property)
head(fl_count_prop)
cdc_count_hu <- data.frame(fips = cdc_svi$fips, cdc_count_hu = cdc_svi$E_HU)
head(cdc_count_hu)
fl_cdc_hu <- left_join(fl_count_prop, cdc_count_hu, by = "fips")
plot(fl_cdc_hu$fl_count_prop, fl_cdc_hu$cdc_count_hu)



# census tract fips to modify

all_ct_df_va <- all_ct_df[as.numeric(all_ct_df$GEOID10) %/% 1e9 == 51,]

all_ct_df_sd <- all_ct_df[as.numeric(all_ct_df$GEOID10) %/% 1e9 == 46,]



all_ct_df_va$GEOID10[!(all_ct_df_va$GEOID10 %in% shp_va$GEOID)]
# [1] "51515050100"

all_ct_df_sd$GEOID10[!(all_ct_df_sd$GEOID10 %in% shp_sd$GEOID)]
# [1] "46113940800" "46113940900" "46113940500"



shp_va$GEOID[!(shp_va$GEOID %in% all_ct_df$GEOID10)]
# [1] "51019050100"

shp_sd$GEOID[!(shp_sd$GEOID %in% all_ct_df$GEOID10)]
# [1] "46102940900" "46102940800" "46102940500"



any(c("46113940800", "46113940900", "46113940500") %in% unique(fhs_model_df$fips))
all(c("46102940900", "46102940800", "46102940500") %in% unique(fhs_model_df$fips))
51515050100 %in% unique(fhs_model_df$fips)
51019050100 %in% unique(fhs_model_df$fips)



sum(nrow(W_ne) + nrow(W_at) + nrow(W_mw) + nrow(W_se) + nrow(W_sw) + nrow(W_we) + nrow(W_nw))




W.list<- mat2listw(W)
W.nb <- W.list$neighbours
W.islands <- n.comp.nb(W.nb)
islands <- W.islands$comp.id
n.islands <- max(W.islands$nc)
if(rho==1) tau2.posterior.shape <- prior.tau2[1] + 0.5 * (K-n.islands) 




head(scale(as.matrix(flood_risk[complete.cases(flood_risk), ])) %*% fr_loadings_scaled[, 1:5])





library(here)
W <- readRDS(here("intermediary_data", "census_tract_adj_reorganize_all_census_tract.rds"))
dim(W)
W_mat <- as.matrix(W) # sparse->dense coercion: allocating vector of size 19.6 GiB
summary(diag(W_mat))

dp <- diff(W@p)
W.triplet <- cbind(rep(seq_along(dp),dp), W@i + 1, 1) # W@i is 0-based
n.triplet <- nrow(W.triplet)
W.triplet.sum <- tapply(W.triplet[ ,3], W.triplet[ ,1], sum)
W.begfin <- cbind(c(1, cumsum(W.triplet.sum[-nrow(W)])+1), cumsum(W.triplet.sum))



summary(W.triplet.sum) # number of neighbors of each census tract

