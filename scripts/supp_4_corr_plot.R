################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to make correlation plot for supplements
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(sf)
library(corrplot)

################################################################################
## load and format data
################################################################################
# load in data
gb_exp_data <- as.data.frame(read.csv("./data/average_features_dataset.csv"))

# subset to exposure columns
exp_data <- subset(gb_exp_data, select = c(percent_poverty, percent_employ, percent_bs_higher, 
                                           ICE_race_income_metric, percent_before_1960s, per_tree_canopy_cover,
                                           NatWalkInd, pm25_ug_m3, temp_mean_K, obesity_pl, heart_disease_pl,
                                           high_cholesterol_pl, hbp_pl, diabetes_pl, cancer_pl, stroke_pl, asthma_pl, 
                                           COPD_pl, arthritis_pl, poor_health_pl, disability_pl, physical_distress_pl, 
                                           phys_act_pl, short_sleep_pl, mental_distress_pl, depression_pl))

# convert to numeric
exp_data <- exp_data %>% mutate_if(is.character, as.numeric) 

# change variable names 
colnames(exp_data) <- c("People below the federal poverty threshold (%)", 
                        "People employed (%)",
                        "People with bachelor's degree or higher (%)",
                        "ICE Race-Income",
                        "Housing units built before 1960 (%)",
                        "Tree canopy cover (%)",
                        "National Walkability Index score",
                        "Daily PM2.5",
                        "Daily mean temperature",
                        "Obesity (%)",
                        "Coronary heart disease (%)",
                        "High cholesterol (%)",
                        "High blood pressure (%)",
                        "Diabetes (%)",
                        "Cancer (%)",
                        "Stroke (%)",
                        "Asthma (%)",
                        "COPD (%)",
                        "Arthritis (%)",
                        "Fair or poor self-rated health status (%)",
                        "Any disability (%)",
                        "Frequent physical distress (%)",
                        "No leisure-time physical activity (%)",
                        "Short sleep duration (%)",
                        "Frequent mental distress (%)",
                        "Depression (%)")

# calculate the correlation
exp_corrs <- cor(exp_data, use = "complete.obs", method = c("spearman"))

# plot and save
corrplot(exp_corrs, 
         method = "circle", 
         type = 'upper',
         tl.col = "black",
         tl.srt = 45)