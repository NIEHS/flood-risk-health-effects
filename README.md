# flood-risk-health-effects
GitHub repository for work related to the manuscript "Effects of Flood Risk on Census Tract-Level Health Outcomes in the United States"

The work is ongoing. The most up-to-date results are in reports/basic_CAR_model_all_census_tract.pdf. 



# Data Analysis Pipeline

1. scripts/imported_data_wrangling_census_tract.R: does data cleaning for all the datafiles. See FloodRiskPlanOfWork.pdf for a comprehensive description of the data.  
  * PLACES Local Data for Better Health: contains the four health outcomes of interest, i.e., Coronary Heart Disease, High Blood Pressure, Current Asthma, and Poor Mental Health.
  * First Street Foundation property flood risk for each ZCTA: normalized the count_floodfactor variables by count_property. Used a ZCTA crosswalk to merge the ZCTA flood risk information with the census tracts, weighting by the percent of housing units in the overlap with the census tract. Used PCA to reduce the large number of flood risk variables to a small number of principal component scores. 
  * CDC Social Vulnerability Index: replaced missing value indicators -999 with NA. 
  * CACES LUR Air Pollution: averaged 6 pollutant concentrations over 16 years 2000-2015. 
  * GRIDMET: maximum temperature and maximum relative humidity, summer or winter, averaged over 16 years 2005-2020 and averaged over each census tract. The raster data was processed in both Google Earth Engine and R (see GRIDMET/ folder). 
  * 2010 TIGER/Line Shapefiles (https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2010&layergroup=Census+Tracts): used to get list of census tracts for states within the contiguous USA. Corrected the census tract fip codes for two states, Virginia and South Dakota, according to https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf. 
  * Census Tract Adjacency (https://s4.ad.brown.edu/projects/diversity/index.htm): used to construct the adjacency matrix for the census tracts
2. scripts/my_gaussian_leroux_car.R: MCMC implementation of the Bayesian conditionally autoregressive (CAR) model. The code is adjusted from the CARBayes package (Lee, 2013, https://github.com/duncanplee/CARBayes/blob/master/R/gaussian.lerouxCAR.R) to accept a large adjacency matrix with class ngCMatrix from the Matrix package. 
3. CARmodeling_local_job_CHD.R, CARmodeling_local_job_BPHIGH.R, CARmodeling_local_job_CASTHMA.R, CARmodeling_local_job_MHLTH.R in scripts/ folder: runs the CAR model for 3 chains on each of the four health outcomes.
4. reports/basic_CAR_model_all_census_tract.Rmd: conducts Bayesian model diagnostics and displays model results for each of the health outcomes.



Citation for CARBayes package: Duncan Lee (2013). CARBayes: An R Package for Bayesian
  Spatial Modeling with Conditional Autoregressive Priors.
  Journal of Statistical Software, 55(13), 1-24. URL
  https://www.jstatsoft.org/htaccess.php?volume=55&type=i&issue=13.


