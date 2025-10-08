################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to link tree canopy data to patient data
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
library(measurements)

################################################################################
## Load and format tree canopy data
################################################################################
## read in data from open data philly
tree_canopy_outlines <- st_read("./data/tree_canopy/ppr_tree_canopy_outlines_2015/ppr_tree_canopy_outlines_2015.shp")

# data is in WGS 84: NAD_1983_StatePlane_Pennsylvania_South_FIPS_3702_Feet (ESPG 102729)
# shape area is in feet
print(st_crs(tree_canopy_outlines))

# change CRS to ESPG 32129 (CRS is in meters)
tree_canopy_outlines <- st_transform(tree_canopy_outlines, 32129)

print(st_crs(tree_canopy_outlines))

# calculate area in m2 from tree outline polygons and add as column
tree_canopy_outlines <- tree_canopy_outlines %>% 
  mutate(area=as.numeric(st_area(tree_canopy_outlines)))

## load philly census tracts
phl_ct <- st_read("./data/philly_spatial_data/Census_Tracts_2010-shp/c16590ca-5adf-4332-aaec-9323b2fa7e7d2020328-1-1jurugw.pr6w.shp")

# original CRS is WGS 84 (ESPG 4326)
print(st_crs(phl_ct))

# set CRS to ESPG 32129 (to match the one we are using for the tree canopy data)
phl_ct <- st_transform(phl_ct, 32129)

################################################################################
## Spatial join census tracts + tree canopy
################################################################################
## INTERSECTION JOIN (amount of each tree outline that is in each census tract)
# get intersection between tree canopy outlines and all phl census tracts 
# add column for intersection area (how much of tree outline is in each census tract)
tree_canopy_outlines_INTERSECTIONS_ct_sf <- st_intersection(tree_canopy_outlines, phl_ct) %>% 
  mutate(intersect_area = st_area(geometry) %>% 
           as.numeric())

################################################################################
## Calculate tree canpy cover for census tracts
################################################################################
## calculate total tree canopy cover area for each census tract
total_tree_area_by_ct <- tree_canopy_outlines_INTERSECTIONS_ct_sf %>% 
  as.data.frame() %>% 
  group_by(GEOID10) %>% 
  summarise(total_tree_area = sum(intersect_area))

## use a regular left_join to get the info back to phl ct shapefile
phl_ct_w_total_tree_area <- left_join(phl_ct, total_tree_area_by_ct, by = "GEOID10")

## calculate percentage of tree canopy cover in each census tract
# ALAND10 is census tract area provided by ACS (more accurate than calculating area from polygon like we do for tree canopy outlines)
greenness_ct_data <- phl_ct_w_total_tree_area %>% 
  mutate(per_tree_canopy_cover = (total_tree_area/ALAND10) * 100)

################################################################################
## Tree canopy cover map
################################################################################
ggplot() +
  geom_sf(data = greenness_ct_data, aes(fill = per_tree_canopy_cover)) +
  labs(title = "",
       caption = "Data source: 2015, Open Data Philly PPR Tree Canopy") +
  scale_fill_continuous(low = "white", high = "darkgreen", 
                        name = "% tree canopy cover",
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
## Spatial join patients + tree canopy cover
################################################################################
## load patient address history data
patient_addrs <- read.csv("./data/preg_addhx_data.csv") %>%
  select(x, y, mrn, delivery_date, pregnancyID, start_date_c, end_date_c)

## convert to an sf object
# set coordinate system (we set to 4326)
patient_addrs_sf <- patient_addrs %>%
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

print(st_crs(patient_addrs_sf))

# transform data to CRS 32129
patient_addrs_sf <- st_transform(patient_addrs_sf, 32129)

## spatial join (find which census tract each address is in)
greenness_exposure_WITHIN <- st_join(patient_addrs_sf, greenness_ct_data, join = st_within)

################################################################################
## Clean + save
################################################################################
greenness_exposure_clean <- greenness_exposure_WITHIN[,c("mrn", "delivery_date", "pregnancyID", "start_date_c", "end_date_c", "per_tree_canopy_cover")] %>% 
  sf::st_drop_geometry(greenness_exposure_clean)

write.csv(greenness_exposure_clean,"./data/neighborhood_features/greenness_cont_exp_vals.csv", row.names = FALSE) # save 
