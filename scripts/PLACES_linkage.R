################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to link PLACES data to patient data
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

################################################################################
## load PLACES and composite indices data
################################################################################
## read in data
PLACES_data <- read.csv("./data/PLACES/processed/PHL_places_clean.csv")

################################################################################
## Join PHL census tracts and PLACES data
################################################################################
# load philly census tracts
phl_ct_2020 <- tracts("PA", "Philadelphia", year = '2020')

# clean up data
names(PLACES_data)[names(PLACES_data) == 'FIPS'] <- 'GEOID'
PLACES_data$GEOID <- as.character(PLACES_data$GEOID)
phl_ct_2020$GEOID <- as.character(phl_ct_2020$GEOID)

# join census tracts with PLACES
phl_ct_w_PLACES <- left_join(phl_ct_2020, PLACES_data, by = "GEOID")

################################################################################
## map
################################################################################
ggplot() +
  geom_sf(data = phl_ct_w_PLACES, aes(fill = hbp_pl)) +
  labs(title = "") +
  scale_fill_continuous(low = "white", high = "darkmagenta", 
                        name = "",
                        labels = scales::number_format(
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
## Spatial join patients and PLACES data + composite indices
################################################################################
## load patient address history data
patient_addrs <- read.csv("./data/preg_addhx_data.csv") %>%
  select(x, y, mrn, delivery_date, pregnancyID, start_date_c, end_date_c)

## convert to an sf object
# set coordinate system (we set to 4326 for GeoBirth)
patient_addrs_sf <- patient_addrs %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

# transform data to CRS 4269
patient_addrs_sf <- st_transform(patient_addrs_sf, 4269)

## spatial join
PLACES_WITHIN <- st_join(patient_addrs_sf, phl_ct_w_PLACES, join = st_within)

#### clean + save --------------------------------------------------------------
# PLACES
drop <- c("x", "y", "STATEFP", "COUNTYFP", "TRACTCE", "NAME", "NAMELSAD", 'MTFCC', 'FUNCSTAT', 'ALAND', 'AWATER', 'INTPTLAT', "INTPTLON", "GEOID")
PLACES_clean <- PLACES_WITHIN[ , !(names(PLACES_WITHIN) %in% drop)] %>% 
  sf::st_drop_geometry(PLACES_clean)

write.csv(PLACES_clean,"./data/neighborhood_features/PLACES_cont_exp_vals.csv", row.names = FALSE) # save 



