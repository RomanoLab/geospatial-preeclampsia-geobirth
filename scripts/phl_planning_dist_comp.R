################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to get average neighborhood feature for Philadelphia planning districts
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
## load in data
################################################################################
## read in PHL planning districts
# download GeoJSON file of PHL planning districts
phl_geojson_file <- tempfile(fileext = ".geojson")
download.file(
  "https://hub.arcgis.com/api/v3/datasets/0960ea0f38f44146bb562f2b212075aa_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1",
  phl_geojson_file
)

# read in as sf object
phl_planning_districts <- read_sf(phl_geojson_file)
print(st_crs(phl_planning_districts))

# change CRS
phl_planning_districts <- st_transform(phl_planning_districts, 4269)

## get XY of each planning district
# get centroid of each polygon
dists_centroids <- st_centroid(phl_planning_districts)

# get coordinates of centroids
centroid_coords <- st_coordinates(dists_centroids)

# join coordinates with planning district sf object
phl_planning_districts <- cbind(phl_planning_districts, centroid_coords)

# make column of position of labels on map
phl_planning_districts$nudge_x <- 0
phl_planning_districts$nudge_x[phl_planning_districts$dist_name == "West"] <- -0.03
phl_planning_districts$nudge_x[phl_planning_districts$dist_name == "Central"] <- 0.04

# make column of labels we want to plot
phl_planning_districts$dist_plot <- NA
phl_planning_districts$dist_plot[phl_planning_districts$dist_name == "West"] <- "West"
phl_planning_districts$dist_plot[phl_planning_districts$dist_name == "Central"] <- "Central"

## load in PLACES census tract data  
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
print(st_crs(phl_ct_w_PLACES))

## load in ACS
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

################################################################################
## merge PLACES data with PHL planning districts
################################################################################
PLACES_districts_joined_sf <- st_join(phl_ct_w_PLACES, phl_planning_districts, join = st_intersects, largest = TRUE)
acs_edu_2016_districts_joined_sf <- st_join(acs_edu_2016, phl_planning_districts, join = st_intersects, largest = TRUE)
acs_ICE_2016_districts_joined_sf <- st_join(acs_ICE_data_2016, phl_planning_districts, join = st_intersects, largest = TRUE)

################################################################################
## filter out central and west districts
################################################################################
PLACES_central_joined_sf <- PLACES_districts_joined_sf %>% filter(dist_name == 'Central')
PLACES_west_joined_sf <- PLACES_districts_joined_sf %>% filter(dist_name == 'West')

ACS_edu_central_joined_sf <- acs_edu_2016_districts_joined_sf %>% filter(dist_name == 'Central')
ACS_edu_west_joined_sf <- acs_edu_2016_districts_joined_sf %>% filter(dist_name == 'West')

ACS_ICE_central_joined_sf <- acs_ICE_2016_districts_joined_sf %>% filter(dist_name == 'Central')
ACS_ICE_west_joined_sf <- acs_ICE_2016_districts_joined_sf %>% filter(dist_name == 'West')

################################################################################
## calculate means for central and west districts
################################################################################
hbp_central_mean <- mean(PLACES_central_joined_sf$hbp)
hbp_west_mean <- mean(PLACES_west_joined_sf$hbp)

edu_central_mean <- mean(ACS_edu_central_joined_sf$percent_bs_higher)
edu_west_mean <- mean(ACS_edu_west_joined_sf$percent_bs_higher)

ICE_central_mean <- mean(ACS_ICE_central_joined_sf$ICE_race_income_metric)
ICE_west_mean <- mean(ACS_ICE_west_joined_sf$ICE_race_income_metric)

################################################################################
## paper figure 3 plot
################################################################################
p1 <- ggplot() +
  geom_sf(data = PLACES_districts_joined_sf, aes(fill = hbp)) +
  geom_sf(data = phl_planning_districts, fill = NA, color = 'green', size=3) + 
  geom_label(data = phl_planning_districts, aes(X, Y, label = dist_plot), size = 3, fontface = "bold", 
             nudge_x = phl_planning_districts$nudge_x) +
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
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

p2 <- ggplot() +
  geom_sf(data = acs_edu_2016_districts_joined_sf, aes(fill = percent_bs_higher)) +
  geom_sf(data = phl_planning_districts, fill = NA, color = 'green', size=3) + 
  geom_label(data = phl_planning_districts, aes(X, Y, label = dist_plot), size = 3, fontface = "bold", 
             nudge_x = phl_planning_districts$nudge_x) +
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
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

p3 <- ggplot() +
  geom_sf(data = acs_ICE_2016_districts_joined_sf, aes(fill = ICE_race_income_metric)) +
  geom_sf(data = phl_planning_districts, fill = NA, color = 'green', size=3) + 
  geom_label(data = phl_planning_districts, aes(X, Y, label = dist_plot), size = 3, fontface = "bold", 
             nudge_x = phl_planning_districts$nudge_x) +
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
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())


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

