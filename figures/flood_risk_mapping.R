
# Source: 
# https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html

library(here)
library(rgdal)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(viridis)
library(stringr)
# gpclibPermit()

i_am("figures/flood_risk_mapping.R")



# # making example_NC_plot.pdf
#
# shp <- readOGR(dsn = here("figures", "census_tract_shapefiles", "tl_2010_37_tract10/tl_2010_37_tract10.shp"))  
# 
# 
# shp@data <- shp@data %>% mutate(probs = runif(nrow(shp@data), 0, 1))
# 
# shp_df <- broom::tidy(shp, region = "GEOID")
# # Warning message:
# # In proj4string(SpP) : CRS object has comment, which is lost in output
# 
# shp_df <- shp_df %>% left_join(shp@data, by = c("id" = "GEOID"))
# 
# map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = probs, color = probs)) + theme_void()
# map + ggtitle("Prior Probabilities of Inclusion")





# reading in the shapefiles for all SE states

shp_nc <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_37_tract10", paste0("tl_2010_37_tract10", ".shp")))  

shp_sc <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_45_tract10", paste0("tl_2010_45_tract10", ".shp")))  

shp_tn <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_47_tract10", paste0("tl_2010_47_tract10", ".shp"))) 

shp_ga <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_13_tract10", paste0("tl_2010_13_tract10", ".shp"))) 

shp_al <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_01_tract10", paste0("tl_2010_01_tract10", ".shp"))) 

shp_ms <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_28_tract10", paste0("tl_2010_28_tract10", ".shp"))) 

shp_fl <- readOGR(dsn = here("imported_data/census_tract_shapefiles", "tl_2010_12_tract10", paste0("tl_2010_12_tract10", ".shp"))) 



shp_se_states <- do.call(rbind, list(shp_nc, shp_sc, shp_tn, shp_ga, shp_al, shp_ms, shp_fl))



# # testing if the plot will work
# 
# shp_se_states@data <- shp_se_states@data %>% mutate(probs = runif(nrow(shp_se_states@data), 0, 1))
# 
# shp_se_states_df <- broom::tidy(shp_se_states, region = "GEOID")
# # Warning message:
# # In proj4string(SpP) : CRS object has comment, which is lost in output
# 
# shp_se_states_df <- shp_se_states_df %>% left_join(shp_se_states@data, by = c("id" = "GEOID"))
# 
# map_test_se_states <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = probs, color = probs)) + theme_void()
# map_test_se_states + ggtitle("Prior Probabilities of Inclusion")



# check map against the one in the Rmd

fhs_model_df <- readRDS("intermediary_data/fhs_model_df_fr_and_pollute_pc.rds")

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

outcome_df <- data.frame(GEOID = fips_leading_zero, Prevalence = fhs_model_df$Data_Value_CHD)



shp_nc@data <- left_join(shp_nc@data, outcome_df, by = c("GEOID10" = "GEOID"))

shp_nc_df <- broom::tidy(shp_nc, region = "GEOID10")
# Warning message:
# In proj4string(SpP) : CRS object has comment, which is lost in output

shp_nc_df <- shp_nc_df %>% left_join(shp_nc@data, by = c("id" = "GEOID10"))

map_NC_CHD <- ggplot() + geom_polygon(data = shp_nc_df, aes(x = long, y = lat, group = group, fill = Prevalence)) + theme_void() + 
  ggtitle("CHD Prevalence") + theme(plot.title = element_text(size=28)) +
  scale_fill_viridis()
map_NC_CHD

# also change the legend name



# CHD prevalence plot for all SE states

fhs_model_df <- readRDS("/Users/Alvin/Documents/NCSU_Fall_2021/NIH_SIP/flood-risk-health-effects/intermediary_data/fhs_model_df_sw_states_census_tract.rds")

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

outcome_df <- data.frame(GEOID = fips_leading_zero, Prevalence = fhs_model_df$Data_Value_CHD)



shp_se_states@data <- left_join(shp_se_states@data, outcome_df, by = c("GEOID10" = "GEOID"))

shp_se_states_df <- broom::tidy(shp_se_states, region = "GEOID10")



shp_se_states_df <- shp_se_states_df %>% left_join(shp_se_states@data, by = c("id" = "GEOID10"))

map_se_states_CHD <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = Prevalence)) + theme_void() + 
  scale_fill_viridis()
map_se_states_CHD





# plan: make a blue plot (like example_NC_plot.pdf) for the significant flood risk variable

## floodfactor10

fhs_model_df <- readRDS("/Users/Alvin/Documents/NCSU_Fall_2021/NIH_SIP/flood-risk-health-effects/intermediary_data/fhs_model_df_sw_states_census_tract.rds")

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

fl_df <- data.frame(GEOID = fips_leading_zero, Percentage = fhs_model_df$pct_floodfactor10)



shp_se_states <- do.call(rbind, list(shp_nc, shp_sc, shp_tn, shp_ga, shp_al, shp_ms, shp_fl))

shp_se_states@data <- left_join(shp_se_states@data, fl_df, by = c("GEOID10" = "GEOID"))

shp_se_states_df <- broom::tidy(shp_se_states, region = "GEOID10")



shp_se_states_df <- shp_se_states_df %>% left_join(shp_se_states@data, by = c("id" = "GEOID10"))

map_se_states_pct_floodfactor10 <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = Percentage)) + theme_void() + 
  scale_fill_viridis()
map_se_states_pct_floodfactor10



## floodfactor 2 to 10

fhs_model_df <- readRDS("/Users/Alvin/Documents/NCSU_Fall_2021/NIH_SIP/flood-risk-health-effects/intermediary_data/fhs_model_df_sw_states_census_tract.rds")

fips <- as.character(fhs_model_df$fips)

fips_leading_zero <- sapply(fips, FUN = function(fip) {
  if (str_length(fip) == 10) {paste0("0", fip)}
  else {fip}
})

# for interpretability, add all of the pct_floodfactor2...pct_floodfactor10, to get 
# percentage of properties with more than minimal risk (0.2%) of flooding. 

# On average, 75.92% of properties have flood factor 1, so it'll be interesting to examine
# all higher flood factors. 

pct_floodfactor2to10 <- rowSums(select(fhs_model_df, starts_with("pct_floodfactor") & 
                                         !ends_with("floodfactor1")))

fl_df <- data.frame(GEOID = fips_leading_zero, Percentage = pct_floodfactor2to10)



shp_se_states <- do.call(rbind, list(shp_nc, shp_sc, shp_tn, shp_ga, shp_al, shp_ms, shp_fl))

shp_se_states@data <- left_join(shp_se_states@data, fl_df, by = c("GEOID10" = "GEOID"))

shp_se_states_df <- broom::tidy(shp_se_states, region = "GEOID10")



shp_se_states_df <- shp_se_states_df %>% left_join(shp_se_states@data, by = c("id" = "GEOID10"))

map_se_states_pct_floodfactor2to10 <- ggplot() + geom_polygon(data = shp_se_states_df, aes(x = long, y = lat, group = group, fill = Percentage)) + theme_void() + 
  scale_fill_viridis()
map_se_states_pct_floodfactor2to10


















## small example that's quick to plot

# When using geom_polygon, you will typically need two data frames:
# one contains the coordinates of each polygon (positions),  and the
# other the values associated with each polygon (values).  An id
# variable links the two together

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = 1:6
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))

p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = value, group = id)) + theme_void() + 
  ggtitle("Prior Probabilities of Inclusion") + theme(plot.title = element_text(size=28)) +
  scale_fill_viridis()
p



