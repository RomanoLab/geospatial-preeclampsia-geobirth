################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to run bivariate associations of neighborhood features with PE
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
# load libraries
library(table1)
library(dplyr)
library(smd)

################################################################################
## load in data
################################################################################
geobirth_data <- as.data.frame(read.csv('./data/average_features_dataset.csv'))

# switch kelvin to celcius
geobirth_data$temp_mean_K <- geobirth_data$temp_mean_K - 273.15

################################################################################
## reformat variables to be used in table
################################################################################
# preeclampsia
geobirth_data$preeclampsia <- factor(geobirth_data$preeclampsia, 
                                     levels=c(1,0),
                                     labels=c("Cases","Controls"))

# tree canopy cover
label(geobirth_data$per_tree_canopy_cover) <- "Tree canopy cover"
units(geobirth_data$per_tree_canopy_cover) <- "%"

# walkability
label(geobirth_data$NatWalkInd) <- "National Walkability Index score"

# poverty
label(geobirth_data$percent_poverty) <- "People below the federal poverty threshold"
units(geobirth_data$percent_poverty) <- "%"

# employment
label(geobirth_data$percent_employ) <- "People employed"
units(geobirth_data$percent_employ) <- "%"

# educational attainment
label(geobirth_data$percent_bs_higher) <- "People with bachelor's degree or higher"
units(geobirth_data$percent_bs_higher) <- "%"

# ICE race/income
label(geobirth_data$ICE_race_income_metric) <- "ICE Race-Income"

# housing
label(geobirth_data$percent_before_1960s) <- "Housing units built before 1960"
units(geobirth_data$percent_before_1960s) <- "%"

# air pollution
label(geobirth_data$pm25_ug_m3) <- "Daily PM2.5"
units(geobirth_data$pm25_ug_m3) <- "ug/m3"

# temperature
label(geobirth_data$temp_mean_K) <- "Daily mean temperature"
units(geobirth_data$temp_mean_K) <- "C"

# PLACES variables
label(geobirth_data$obesity_pl) <- "Obesity"
label(geobirth_data$mental_distress_pl) <- "Frequent mental distress"
label(geobirth_data$short_sleep_pl) <- "Short sleep duration"
label(geobirth_data$physical_distress_pl) <- "Frequent physical distress"
label(geobirth_data$asthma_pl) <- "Asthma"
label(geobirth_data$phys_act_pl) <- "No leisure-time physical activity "
label(geobirth_data$heart_disease_pl) <- "Coronary heart disease"
label(geobirth_data$disability_pl) <- "Any disability "
label(geobirth_data$arthritis_pl) <- "Arthritis"
label(geobirth_data$cancer_pl) <- "Cancer"
label(geobirth_data$hbp_pl) <- "High blood pressure"
label(geobirth_data$COPD_pl) <- "Chronic obstructive pulmonary disease "
label(geobirth_data$poor_health_pl) <- "Fair or poor self-rated health status "
label(geobirth_data$stroke_pl) <- "Stroke"
label(geobirth_data$diabetes_pl) <- "Diabetes"
label(geobirth_data$depression_pl) <- "Depression "
label(geobirth_data$high_cholesterol_pl) <- "High cholesterol "

################################################################################
## add pval or SMD
################################################################################
# pvalue
pvalue <- function(x, ...) {
  y <- unlist(x)
  
  # create groups (basically saying which variable each entry in the vector corresponds to)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # for numeric variables (continuous), perform a standard 2-sample t-test
    p <- t.test(y ~ g, alternative = "two.sided", paired = FALSE, var.equal = FALSE, conf.level = 0.95)$p.value
  } else {
    # for categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  
  # format the p-value
  # empty string places p value on the line below the variable label
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# SMD
Csmd <- function(x, ...) {
    y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  # calculate smd rounding to 3 decimal places
  round(smd::smd(y, g, na.rm = TRUE)[2], 3)
  
  # format SMD
  # empty string places SMD on the line below the variable label
  c("", round(smd::smd(y, g, na.rm = TRUE)[2], 3))
}

################################################################################
## create table
################################################################################
table1(~ percent_poverty + percent_employ + percent_bs_higher + ICE_race_income_metric + percent_before_1960s +
         NatWalkInd + per_tree_canopy_cover + pm25_ug_m3 + temp_mean_K + obesity_pl + heart_disease_pl + high_cholesterol_pl +
         hbp_pl + diabetes_pl +  cancer_pl + stroke_pl + asthma_pl +  COPD_pl + arthritis_pl + poor_health_pl + disability_pl +
         physical_distress_pl + phys_act_pl + short_sleep_pl + mental_distress_pl + depression_pl | preeclampsia, 
       data=geobirth_data, 
       overall=F, 
       render.continuous=c("Median (Q1 &ndash; Q3)", "Mean (SD)"),
       render.missing=NULL,
       extra.col=list("p-value"= pvalue, "SMD"= Csmd))