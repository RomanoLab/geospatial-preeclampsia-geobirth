################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script for paper Figure 3
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)
library(tigris)
library(cowplot)

################################################################################
## Figure 3A
################################################################################
# load philly census tracts
phl_ct <- tracts("PA", "Philadelphia", year = '2020')

# load in PLACES data
PLACES <- read.csv("./data/PLACES/processed/PHL_places_clean.csv")

# clean up data
names(PLACES)[names(PLACES) == 'FIPS'] <- 'GEOID'
PLACES$GEOID <- as.character(PLACES$GEOID)
phl_ct$GEOID <- as.character(phl_ct$GEOID)

# join census tracts with PLACES
phl_ct_w_PLACES <- left_join(phl_ct, PLACES, by = "GEOID")

# plot
p1 <- ggplot() +
  geom_sf(data = phl_ct_w_PLACES, aes(fill = hbp_pl)) +
  labs(title = "") +
  scale_fill_continuous(low = "white", high = "maroon", 
                        name = "Prevalence of HBP among adults",
                        limits = c(9,60),
                        labels = scales::percent_format(
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
## Figure 3B
################################################################################
## percent_bs_higher
# population 25 and older with educational attainment of bachelors or higher
acs_edu_2016 <- get_acs(geography = "tract", # census tract level
                        variables = c(total_pop = "B15003_001E",
                                      bs = "B15003_022E", 
                                      masters = "B15003_023E",
                                      prof_school = "B15003_024E",
                                      doctorate = "B15003_025E"), 
                        year = 2016, # year of ACS 
                        state = 42, # state to pull data for (PA)
                        county = 101, # county to pull data for (Philadelphia)
                        survey = "acs5", # 5-year or 1-year estimates
                        output = "wide", 
                        geometry = T) %>% # merge ACS data with geometry
  mutate(percent_bs_higher = ((bs+masters+prof_school+doctorate) / total_pop) * 100) %>%
  select(tract = GEOID, percent_bs_higher)

# plot
p2 <- ggplot() +
  geom_sf(data = acs_edu_2016, aes(fill = percent_bs_higher)) +
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
## Figure 3C
################################################################################
acs_ICE_data_2016 <- get_acs(geography = "tract", # census tract level
                             variables = c(total_population_w_ri = "B19001A_001E", # total white population for whom household income is determined 
                                           total_population_b_ri = "B19001B_001E", # total black population for whom household income is determined
                                           num_l10_b_ri = "B19001B_002E", # less than 10,000 for black households
                                           num_10_14_b_ri = "B19001B_003E", # 10,000-14,999 for black households
                                           num_15_19_b_ri = "B19001B_004E", # 15,000-19,999 for black households
                                           num_125_149_w_ri = "B19001A_015E", # 125,000-149,999 for white households
                                           num_150_199_w_ri = "B19001A_016E", # 150,00-199,999 for white households
                                           num_m200_w_ri = "B19001A_017E"), # 200,000 or more for white households
                             year = 2016, # year of ACS 
                             state = 42, # state to pull data for (PA) 
                             county = 101, # county to pull data for (Philadelphia)
                             survey = "acs5", # 5-year or 1-year estimates
                             output = "wide", 
                             geometry = T) %>% # merge ACS data with geographic dataset
  # calculate ICE metrics and add to dataframe
  mutate(ICE_race_income_metric = ((num_125_149_w_ri +  num_150_199_w_ri + num_m200_w_ri) - (num_15_19_b_ri + num_10_14_b_ri + num_l10_b_ri)) / (total_population_w_ri + total_population_b_ri)) %>%
  select(tract = GEOID, ICE_race_income_metric)

p3 <- ggplot() +
  geom_sf(data = acs_ICE_data_2016, aes(fill = ICE_race_income_metric)) +
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

################################################################################
## Figure 3 -- panel
################################################################################
# arrange the three plots in a single row
prow <- plot_grid(
  p1 + theme(legend.position=c(0.85, 0.2), 
             legend.key.size = unit(0.35, 'cm'), 
             legend.text = element_text(size=8), 
             legend.title = element_text(size=8)),
  p2 + theme(legend.position=c(0.85, 0.2), 
             legend.key.size = unit(0.35, 'cm'), 
             legend.text = element_text(size=8), 
             legend.title = element_text(size=8)),
  p3 + theme(legend.position=c(0.85, 0.2), 
             legend.key.size = unit(0.35, 'cm'), 
             legend.text = element_text(size=8), 
             legend.title = element_text(size=8)),
  align = 'vh',
  labels = c("A", "B", "C"),
  nrow = 1, 
  label_x = 0.05, 
  label_y = 0.8,
  hjust = -0.5, 
  vjust = -1.5
)