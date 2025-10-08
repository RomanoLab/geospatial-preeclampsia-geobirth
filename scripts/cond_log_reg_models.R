################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script to run conditional logistic regression models
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(dplyr)
library(table1)
library(dplyr)
library(smd)
library(forcats)
library(lmtest)
library(sandwich)
library(ggplot2)
library(survival)
library(lme4)
library(mclogit)

################################################################################
## load in data
################################################################################
geobirth_data_exp <- as.data.frame(read.csv('./data/average_features_dataset.csv'))

# switch kelvin to celcius
geobirth_data_exp$temp_mean_K <- geobirth_data_exp$temp_mean_K - 273.15

################################################################################
## data prep
################################################################################
## turn categorical variables into factors
# age categorical
geobirth_data_exp$mage_cat_CP <- factor(geobirth_data_exp$mage_cat_CP, 
                                        levels=c(1,2,3),
                                        labels=c("<20","20 to <35", "35+"))
# hypertension
geobirth_data_exp$htn <- factor(geobirth_data_exp$htn, 
                                levels=c(1,0),
                                labels=c("Pre-existing hypertension","No pre-existing hypertension"))

# diabetes
geobirth_data_exp$diabetes <- factor(geobirth_data_exp$diabetes, 
                                     levels=c(1,2,3),
                                     labels=c("Pre-existing diabetes","Gestational diabetes", "No diabetes"))
# smoking
geobirth_data_exp$smoker_doh <- factor(geobirth_data_exp$smoker_doh, 
                                       levels=c(1,2,3),
                                       labels=c("Current smoker","Former smoker", "Never smoker"))
label(geobirth_data_exp$smoker_doh) <- "Smoking status"

# hospital
geobirth_data_exp$hospital_del <- factor(geobirth_data_exp$hospital_del, 
                                         levels=c("PAH", "HUP"),
                                         labels=c("PAH","HUP"))
label(geobirth_data_exp$hospital_del) <- "Hospital of delivery"

## change reference level
geobirth_data_exp <- geobirth_data_exp %>% mutate(htn = fct_relevel(htn, "No pre-existing hypertension", after = 0))
geobirth_data_exp <- geobirth_data_exp %>% mutate(diabetes = fct_relevel(diabetes, "No diabetes", after = 0))
geobirth_data_exp <- geobirth_data_exp %>% mutate(mage_cat_CP = fct_relevel(mage_cat_CP, "20 to <35", after = 0))
geobirth_data_exp <- geobirth_data_exp %>% mutate(smoker_doh = fct_relevel(smoker_doh, "Never smoker", after = 0))

## transform exposure columns
# divide by interquartile range
geobirth_data_exp$percent_poverty <- as.numeric(geobirth_data_exp$percent_poverty) / IQR(geobirth_data_exp$percent_poverty, na.rm=TRUE)
geobirth_data_exp$percent_employ <- as.numeric(geobirth_data_exp$percent_employ) / IQR(geobirth_data_exp$percent_employ, na.rm=TRUE)
geobirth_data_exp$percent_bs_higher <- as.numeric(geobirth_data_exp$percent_bs_higher) / IQR(geobirth_data_exp$percent_bs_higher, na.rm=TRUE)
geobirth_data_exp$ICE_race_income_metric <- as.numeric(geobirth_data_exp$ICE_race_income_metric) / IQR(geobirth_data_exp$ICE_race_income_metric, na.rm=TRUE)
geobirth_data_exp$percent_before_1960s <- as.numeric(geobirth_data_exp$percent_before_1960s) / IQR(geobirth_data_exp$percent_before_1960s, na.rm=TRUE)
geobirth_data_exp$per_tree_canopy_cover <- as.numeric(geobirth_data_exp$per_tree_canopy_cover) / IQR(geobirth_data_exp$per_tree_canopy_cover, na.rm=TRUE)
geobirth_data_exp$NatWalkInd <- as.numeric(geobirth_data_exp$NatWalkInd) / IQR(geobirth_data_exp$NatWalkInd, na.rm=TRUE)
geobirth_data_exp$pm25_ug_m3 <- as.numeric(geobirth_data_exp$pm25_ug_m3) / IQR(geobirth_data_exp$pm25_ug_m3, na.rm=TRUE)
geobirth_data_exp$temp_mean_K <- as.numeric(geobirth_data_exp$temp_mean_K) / IQR(geobirth_data_exp$temp_mean_K, na.rm=TRUE)

geobirth_data_exp$obesity_pl <- as.numeric(geobirth_data_exp$obesity_pl) / IQR(geobirth_data_exp$obesity_pl, na.rm=TRUE)
geobirth_data_exp$heart_disease_pl <- as.numeric(geobirth_data_exp$heart_disease_pl) / IQR(geobirth_data_exp$heart_disease_pl, na.rm=TRUE)
geobirth_data_exp$high_cholesterol_pl <- as.numeric(geobirth_data_exp$high_cholesterol_pl) / IQR(geobirth_data_exp$high_cholesterol_pl, na.rm=TRUE)
geobirth_data_exp$hbp_pl <- as.numeric(geobirth_data_exp$hbp_pl) / IQR(geobirth_data_exp$hbp_pl, na.rm=TRUE)
geobirth_data_exp$diabetes_pl <- as.numeric(geobirth_data_exp$diabetes_pl) / IQR(geobirth_data_exp$diabetes_pl, na.rm=TRUE)
geobirth_data_exp$cancer_pl <- as.numeric(geobirth_data_exp$cancer_pl) / IQR(geobirth_data_exp$cancer_pl, na.rm=TRUE)
geobirth_data_exp$stroke_pl <- as.numeric(geobirth_data_exp$stroke_pl) / IQR(geobirth_data_exp$stroke_pl, na.rm=TRUE)
geobirth_data_exp$asthma_pl <- as.numeric(geobirth_data_exp$asthma_pl) / IQR(geobirth_data_exp$asthma_pl, na.rm=TRUE)
geobirth_data_exp$COPD_pl <- as.numeric(geobirth_data_exp$COPD_pl) / IQR(geobirth_data_exp$COPD_pl, na.rm=TRUE)
geobirth_data_exp$arthritis_pl <- as.numeric(geobirth_data_exp$arthritis_pl) / IQR(geobirth_data_exp$arthritis_pl, na.rm=TRUE)
geobirth_data_exp$poor_health_pl <- as.numeric(geobirth_data_exp$poor_health_pl) / IQR(geobirth_data_exp$poor_health_pl, na.rm=TRUE)
geobirth_data_exp$disability_pl <- as.numeric(geobirth_data_exp$disability_pl) / IQR(geobirth_data_exp$disability_pl, na.rm=TRUE)
geobirth_data_exp$physical_distress_pl <- as.numeric(geobirth_data_exp$physical_distress_pl) / IQR(geobirth_data_exp$physical_distress_pl, na.rm=TRUE)
geobirth_data_exp$phys_act_pl <- as.numeric(geobirth_data_exp$phys_act_pl) / IQR(geobirth_data_exp$phys_act_pl, na.rm=TRUE)
geobirth_data_exp$short_sleep_pl <- as.numeric(geobirth_data_exp$short_sleep_pl) / IQR(geobirth_data_exp$short_sleep_pl, na.rm=TRUE)
geobirth_data_exp$mental_distress_pl <- as.numeric(geobirth_data_exp$mental_distress_pl) / IQR(geobirth_data_exp$mental_distress_pl, na.rm=TRUE)
geobirth_data_exp$depression_pl <- as.numeric(geobirth_data_exp$depression_pl) / IQR(geobirth_data_exp$depression_pl, na.rm=TRUE)

################################################################################
# random effect model + conditional logistic regression
################################################################################
### complete cases
geobirth_data_exp_CC <- geobirth_data_exp[complete.cases(geobirth_data_exp[,c("percent_employ")]),]

## unadjusted model 0
model0.mclogit.fit <- mclogit(cbind(preeclampsia, setnumber) ~ percent_employ,
                           random = ~1|FIPS, 
                           data = geobirth_data_exp_CC)
summary(model0.mclogit.fit)
exp(coef(model0.mclogit.fit)) # odds ratios.
confint(model0.mclogit.fit) # confidence intervals for fit
round(exp(cbind(OR = coef(model0.mclogit.fit), CI = confint(model0.mclogit.fit))),2) # OR and 95%CI

## adjusted model 1
model1.mclogit.fit <- mclogit(cbind(preeclampsia, setnumber) ~ percent_employ + mage_cat_CP + smoker_doh + hospital_del,
                           random = ~1|FIPS, 
                           data = geobirth_data_exp_CC)
summary(model1.mclogit.fit)
exp(coef(model1.mclogit.fit)) # odds ratios.
confint(model1.mclogit.fit) # confidence intervals for fit
round(exp(cbind(OR = coef(model1.mclogit.fit), CI = confint(model1.mclogit.fit))),2) # OR and 95%CI

## adjusted model 2
model2.mclogit.fit <- mclogit(cbind(preeclampsia, setnumber) ~ percent_employ + mage_cat_CP + smoker_doh + hospital_del + bmi_imp + htn + diabetes,
                           random = ~1|FIPS, 
                           data = geobirth_data_exp_CC)
summary(model2.mclogit.fit)
exp(coef(model2.mclogit.fit)) # odds ratios.
confint(model2.mclogit.fit) # confidence intervals for fit
round(exp(cbind(OR = coef(model2.mclogit.fit), CI = confint(model2.mclogit.fit))),2) # OR and 95%CI
