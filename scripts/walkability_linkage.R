################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to link walkability to patient data
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

################################################################################
## load walkability data
################################################################################
## read in EPA walkability index data
walkability_data <- st_read("./data/walkability/Natl_WI.gdb")

## filter data to philadelphia and transform
# filter to Philadelphia
walkability_data_PA <- walkability_data %>% filter(STATEFP == 42)
walkability_data_PHL <- walkability_data_PA %>% filter(COUNTYFP == 101)

# transform data to CRS 4269
walkability_data_PHL <- st_transform(walkability_data_PHL, 4269)
head(walkability_data_PHL)

################################################################################
## walkability map
################################################################################
# bucket walkability to match EPA buckets
walkability_data_PHL_buckets <- walkability_data_PHL %>% mutate(walk_bin = cut(as.numeric(NatWalkInd), breaks=c(1, 5.75, 10.5, 15.25, 20)))

# map
ggplot() +
  geom_sf(data = walkability_data_PHL_buckets, aes(fill = walk_bin)) +
  labs(title = "",
       caption = "Data source: 2019, EPA National Walkability Index") +
  scale_fill_manual(values = c("(1,5.7]" = "white",
                               "(5.7,10.5]" = "linen",
                               "(10.5,15.2]" = "palegoldenrod",
                               "(15.2,20]" = "sandybrown"),
                    labels = c('Least walkable','Below average walkable', 'Above average walkable', 'Most walkable')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  guides(
    fill = guide_legend(title = "EPA 2019 national walkability index score")
  )

# continuous map
ggplot() +
  geom_sf(data = walkability_data_PHL, aes(fill = NatWalkInd)) +
  labs(title = "",
       caption = "Data source: 2019, EPA National Walkability Index") +
  scale_fill_continuous(low = "white", high = "darkgreen", 
                        name = "EPA 2019 national walkability index score",
                        limit = c(0,20),
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
## Spatial join patients and walkability
################################################################################
## load patient address history data
patient_addrs <- read.csv("./data/preg_addhx_data.csv") %>%
  select(x, y, mrn, delivery_date, pregnancyID, start_date_c, end_date_c)

## convert to an sf object
# set coordinate system (we set to 4326)
patient_addrs_sf <- patient_addrs %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

# transform data to CRS 4269
patient_addrs_sf <- st_transform(patient_addrs_sf, 4269)

## spatial join
walkability_exposure_WITHIN <- st_join(patient_addrs_sf, walkability_data_PHL, join = st_within)

################################################################################
## Clean + save
################################################################################
walkability_exposure_clean <- walkability_exposure_WITHIN[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "NatWalkInd")] %>% 
  sf::st_drop_geometry(walkability_exposure_clean)

write.csv(walkability_exposure_clean,"./data/neighborhood_features/walkability_cont_exp_vals.csv", row.names = FALSE) # save 
