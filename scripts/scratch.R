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


