
library(dplyr)
select <- dplyr::select

fhs_model_df <- readRDS("intermediary_data/fhs_model_df_all_census_tract_reorg.rds")


Y <- fhs_model_df$Data_Value_CHD

X <- fhs_model_df[, 19:(ncol(fhs_model_df) - 4)]

X <- X %>% select(-pct_floodfactor1)

# exclude some more variables selected by vifstep, to account for multicollinearity
# excluding all of the pct_fs_risk variables, as well as 3 of the avg_risk_score variables

collin_var_names <- c("avg_risk_score_all", "pct_fs_risk_2050_500", "pct_fs_risk_2020_500", "avg_risk_fsf_2020_500", "pct_fs_risk_2050_5", "pct_fs_risk_2020_100", "no2", "pct_fs_risk_2050_100")

X <- X[, !(names(X) %in% collin_var_names)]

# # also removing avg_risk_score_sfha due to large numbers of NAs
# X <- X[, names(X) != "avg_risk_score_sfha"]



X           <- scale(X) # Scale covariates
X[is.na(X)] <- 0        # Fill in missing values with the mean

# if I do mean imputation (which may be problematic), all the counties 
# will have neighbors in W

# X <- data.frame(X)

fhs_lm <- lm(Y ~ X)

saveRDS(fhs_lm, file = "modeling_files/fhs_lm.rds")


