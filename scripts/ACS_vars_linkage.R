################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to download ACS data and link to patient data
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(sf)

################################################################################
## ACS census tract variables
################################################################################
## percent_bs_higher
# population 25 and older with educational attainment of bachelors or higher
# run this for 2012,2016,2021
# B15003_001E: total
# B15003_{022E-025E}: n
acs_edu_2021 <- get_acs(geography = "tract", # census tract level
                        variables = c(total_pop = "B15003_001E",
                                      bs = "B15003_022E", 
                                      masters = "B15003_023E",
                                      prof_school = "B15003_024E",
                                      doctorate = "B15003_025E"), 
                        year = 2021, # year of ACS ***** NEED TO CHANGE THIS WHEN RUNNING DIF YEAR *****
                        state = 42, # state to pull data for (PA)
                        county = 101, # county to pull data for (Philadelphia)
                        survey = "acs5", # 5-year or 1-year estimates
                        output = "wide", 
                        geometry = T) %>% # merge ACS data with geometry
  mutate(percent_bs_higher = ((bs+masters+prof_school+doctorate) / total_pop) * 100) %>%
  select(tract = GEOID, percent_bs_higher)

## percent_before_1960s
# housing units built before 1960s
# run this for 2011
# B25034_001E: total
# B25034_{008E-010E}: n
acs_housing_2011 <- get_acs(geography = "tract",
                            variables = c(total_housing_units = "B25034_001E", 
                                          num_1950_1959 = "B25034_008E",
                                          num_1940_1949 = "B25034_009E",
                                          num_1939_earlier = "B25034_010E"), 
                            year = 2011, 
                            state = 42, 
                            county = 101,
                            survey = "acs5",
                            output = "wide", 
                            geometry = T) %>% 
  mutate(percent_before_1960s = ( ( num_1950_1959 + num_1940_1949 + num_1939_earlier) / total_housing_units) * 100) %>%
  select(tract = GEOID, percent_before_1960s)

# run this for 2016,2021
# B25034_001E: total
# B25034_{009E-011E}: n
acs_housing_2021 <- get_acs(geography = "tract", 
                            variables = c(total_housing_units = "B25034_001E", 
                                          num_1950_1959 = "B25034_009E", 
                                          num_1940_1949 = "B25034_010E",
                                          num_1939_earlier = "B25034_011E"), 
                            year = 2021, 
                            state = 42, 
                            county = 101,
                            survey = "acs5", 
                            output = "wide", 
                            geometry = T) %>% 
  mutate(percent_before_1960s = ( ( num_1950_1959 + num_1940_1949 + num_1939_earlier) / total_housing_units) * 100) %>%
  select(tract = GEOID, percent_before_1960s)

## percent_poverty
# income in the past 12 months below poverty level
# run this for 2011,2016,2021
# B17001_001E: total
# B17001_002E: n
acs_poverty_2021 <- get_acs(geography = "tract", 
                            variables = c(total_population = "B17001_001E", 
                                          num_poverty = "B17001_002E"), 
                            year = 2021, 
                            state = 42, 
                            county = 101,
                            survey = "acs5", 
                            output = "wide", 
                            geometry = T) %>% 
  mutate(percent_poverty = (num_poverty / total_population) * 100) %>%
  select(tract = GEOID, percent_poverty)

## percent_employ
# number of people employed in labor force
# run this for 2011,2016,2021
# B23025_001E: total
# B23025_004E : n
acs_employ_2021 <- get_acs(geography = "tract", 
                           variables = c(total_pop = "B23025_001E", 
                                         num_employ = "B23025_004E"), 
                           year = 2021, 
                           state = 42, 
                           county = 101,
                           survey = "acs5", 
                           output = "wide", 
                           geometry = T) %>% 
  mutate(percent_employ = (num_employ / total_pop) * 100) %>%
  select(tract = GEOID, percent_employ)

## ICE_race_income_metric
# ICE Race-Income 
# run this for 2011,2016,2021
# B19001A_001E, B19001B_001E: total
# B19001B_{002E-004E}, B19001A_{015E-017E} : n
acs_ICE_2021 <- get_acs(geography = "tract", 
                        variables = c(total_population_w_ri = "B19001A_001E", # total white population for whom household income is determined 
                                      total_population_b_ri = "B19001B_001E", # total black population for whom household income is determined
                                      num_l10_b_ri = "B19001B_002E", # less than 10,000 for black households
                                      num_10_14_b_ri = "B19001B_003E", # 10,000-14,999 for black households
                                      num_15_19_b_ri = "B19001B_004E", # 15,000-19,999 for black households
                                      num_125_149_w_ri = "B19001A_015E", # 125,000-149,999 for white households
                                      num_150_199_w_ri = "B19001A_016E", # 150,00-199,999 for white households
                                      num_m200_w_ri = "B19001A_017E"), # 200,000 or more for white households
                        year = 2021, 
                        state = 42,
                        county = 101,
                        survey = "acs5", 
                        output = "wide", 
                        geometry = T) %>% 
  mutate(ICE_race_income_metric = ((num_125_149_w_ri +  num_150_199_w_ri + num_m200_w_ri) - (num_15_19_b_ri + num_10_14_b_ri + num_l10_b_ri)) / (total_population_w_ri + total_population_b_ri)) %>%
  select(tract = GEOID, ICE_race_income_metric)

################################################################################
## ACS variables maps
################################################################################
# numeric
ggplot() +
  geom_sf(data = acs_ICE_2021, aes(fill = ICE_race_income_metric)) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "ICE Race-Income",
                        limit = c(-1,1),
                        labels = scales::label_number(
                          accuracy = 0.1,
                          scale = 1,
                          prefix = "",
                          suffix = "",
                          big.mark = " ",
                          decimal.mark = ".",
                          trim = TRUE)) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

# percentage
ggplot() +
  geom_sf(data = acs_edu_2021, aes(fill = percent_bs_higher)) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "% of people with bachelor's degree or higher",
                        limit = c(0,100),
                        labels = scales::label_percent(
                          accuracy = 1,
                          scale = 1,
                          prefix = "",
                          suffix = "%",
                          big.mark = " ",
                          decimal.mark = ".",
                          trim = TRUE)) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

################################################################################
## Spatial join participants + ACS variables
################################################################################
## load patient address data (select columns needed)
# user MUST PROVIDE their own patient address history data, please name it 
# preg_addhx_data.csv and have the following columns:
# - x coordinate of residential address
# - y coordinate of residential address
# - patient mrn number
# - delivery date
# - pregnancyID
# - start date of residential address
# - end date of residential address

patient_addrs <- read.csv("./data/preg_addhx_data.csv") %>%
  select(x, y, mrn, delivery_date, pregnancyID, start_date_c, end_date_c)

## convert to an sf object
# set coordinate system (we set it to 4326 for GeoBirth)
patient_addrs_sf <- patient_addrs %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

# transform data to CRS 4269
patient_addrs_sf <- st_transform(patient_addrs_sf, 4269)

# check worked correctly
print(st_crs(patient_addrs_sf))

## subset patients based on LMP year
# user MUST PROVIDE their own patient data, please name it 
# preg_data.csv and have the following columns:
# - patient mrn number
# - delivery date 
# - LMP year

# load patient data
preg_data <- read.csv("./data/preg_data.csv") %>%
  select(mrn, delivery_date, LMP_year)

# link to patient addhx
patient_addrs_sf_w_LMP <- left_join(patient_addrs_sf, preg_data, by = c("mrn", "delivery_date"))

# subset patients by LMP year
patient_addrs_sf_2011 <- subset(patient_addrs_sf_w_LMP, LMP_year > 2006 & LMP_year < 2012)
patient_addrs_sf_2016 <- subset(patient_addrs_sf_w_LMP, LMP_year > 2011 & LMP_year < 2017)
patient_addrs_sf_2021 <- subset(patient_addrs_sf_w_LMP, LMP_year > 2016 & LMP_year < 2022)

## spatial join
# link each patient address to its ACS values based on census tract residence
# educational attainment
ACS_edu_exposure_2012 <- st_join(patient_addrs_sf_2011, acs_edu_data_2012, join = st_within)
ACS_edu_exposure_2016 <- st_join(patient_addrs_sf_2016, acs_edu_data_2016, join = st_within)
ACS_edu_exposure_2021 <- st_join(patient_addrs_sf_2021, acs_edu_data_2021, join = st_within)

# housing units built before 1980s
ACS_lead_paint_exposure_2011 <- st_join(patient_addrs_sf_2011, acs_housing_data_2011, join = st_within)
ACS_lead_paint_exposure_2016 <- st_join(patient_addrs_sf_2016, acs_housing_data_2016, join = st_within)
ACS_lead_paint_exposure_2021 <- st_join(patient_addrs_sf_2021, acs_housing_data_2021, join = st_within)

# poverty
ACS_poverty_exposure_2011 <- st_join(patient_addrs_sf_2011, acs_poverty_data_2011, join = st_within)
ACS_poverty_exposure_2016 <- st_join(patient_addrs_sf_2016, acs_poverty_data_2016, join = st_within)
ACS_poverty_exposure_2021 <- st_join(patient_addrs_sf_2021, acs_poverty_data_2021, join = st_within)

# employment
ACS_employment_exposure_2011 <- st_join(patient_addrs_sf_2011, acs_employment_data_2011, join = st_within)
ACS_employment_exposure_2016 <- st_join(patient_addrs_sf_2016, acs_employment_data_2016, join = st_within)
ACS_employment_exposure_2021 <- st_join(patient_addrs_sf_2021, acs_employment_data_2021, join = st_within)

# ICE race-income metric
ACS_ICE_exposure_2011 <- st_join(patient_addrs_sf_2011, acs_ICE_data_2011, join = st_within)
ACS_ICE_exposure_2016 <- st_join(patient_addrs_sf_2016, acs_ICE_data_2016, join = st_within)
ACS_ICE_exposure_2021 <- st_join(patient_addrs_sf_2021, acs_ICE_data_2021, join = st_within)

## combine ACS vals for all LMP years
ACS_edu_exposure1 <- rbind(ACS_edu_exposure_2012, ACS_edu_exposure_2016)
ACS_edu_exposure <- rbind(ACS_edu_exposure1, ACS_edu_exposure_2021)

ACS_lead_paint_exposure1 <- rbind(ACS_lead_paint_exposure_2011, ACS_lead_paint_exposure_2016)
ACS_lead_paint_exposure <- rbind(ACS_lead_paint_exposure1, ACS_lead_paint_exposure_2021)

ACS_poverty_exposure1 <- rbind(ACS_poverty_exposure_2011, ACS_poverty_exposure_2016)
ACS_poverty_exposure <- rbind(ACS_poverty_exposure1, ACS_poverty_exposure_2021)

ACS_employment_exposure1 <- rbind(ACS_employment_exposure_2011, ACS_employment_exposure_2016)
ACS_employment_exposure <- rbind(ACS_employment_exposure1, ACS_employment_exposure_2021)

ACS_ICE_exposure1 <- rbind(ACS_ICE_exposure_2011, ACS_ICE_exposure_2016)
ACS_ICE_exposure <- rbind(ACS_ICE_exposure1, ACS_ICE_exposure_2021)

#### clean + save --------------------------------------------------------------
# educational attainment
ACS_edu_exposure_clean <- ACS_edu_exposure[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "percent_bs_higher")] %>%
  sf::st_drop_geometry(ACS_edu_exposure_clean)

write.csv(ACS_edu_exposure_clean,"./data/neighborhood_features/ACS_edu_cont_exp_vals.csv", row.names = FALSE) 

# housing units built before 1980s
ACS_lead_paint_exposure_clean <- ACS_lead_paint_exposure[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "percent_before_1960s")] %>%
  sf::st_drop_geometry(ACS_lead_paint_exposure_clean)

write.csv(ACS_lead_paint_exposure_clean,"./data/neighborhood_features/ACS_lead_paint_cont_exp_vals.csv", row.names = FALSE) 

# poverty
ACS_poverty_exposure_clean <- ACS_poverty_exposure[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "percent_poverty")] %>%
  sf::st_drop_geometry(ACS_poverty_exposure_clean)

write.csv(ACS_poverty_exposure_clean,"./data/neighborhood_features/ACS_poverty_cont_exp_vals.csv", row.names = FALSE) 

head(ACS_poverty_exposure_clean)

# employment
ACS_employment_exposure_clean <- ACS_employment_exposure[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "percent_employ")] %>%
  sf::st_drop_geometry(ACS_employment_exposure_clean)

write.csv(ACS_employment_exposure_clean,"./data/neighborhood_features/ACS_employment_cont_exp_vals.csv", row.names = FALSE) 

# ICE
ACS_ICE_exposure_clean <- ACS_ICE_exposure[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "ICE_race_income_metric")] %>%
  sf::st_drop_geometry(ACS_ICE_exposure_clean)

write.csv(ACS_ICE_exposure_clean,"./data/neighborhood_features/ACS_ICE_cont_exp_vals.csv", row.names = FALSE) 
