################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script for participant characteristics bivariate analysis
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(table1)
library(dplyr)
library(smd)

################################################################################
## load in data
################################################################################
geobirth_data <- as.data.frame(read.csv("./data/preg_data.csv"))

################################################################################
## reformat variables to be used in table 
################################################################################
# preeclampsia
geobirth_data$preeclampsia <- factor(geobirth_data$preeclampsia, 
                                   levels=c(1,0),
                                   labels=c("Cases","Controls"))

# age categorical
geobirth_data$mage_cat_CP <- factor(geobirth_data$mage_cat_CP, 
                                   levels=c(1,2,3),
                                   labels=c("<20","20 to <35", "35+"))
label(geobirth_data$mage_cat_CP) <- "Age"
units(geobirth_data$mage_cat_CP) <- "y"

# BMI
label(geobirth_data$bmi_imp) <- "Pre-pregnancy body mass index"
units(geobirth_data$bmi_imp) <- "kg/m2"

# gestational age (weeks)
label(geobirth_data$gestational_age_CP_wks) <- "Gestational age at delivery"
units(geobirth_data$gestational_age_CP_wks) <- "weeks"

# parity
geobirth_data$nulliparous <- factor(geobirth_data$nulliparous, 
                                    levels=c(1,0),
                                    labels=c("Nulliparous","Parous"))
label(geobirth_data$nulliparous) <- "Parity"

# preterm birth
geobirth_data$preterm_yn <- factor(geobirth_data$preterm_yn, 
                                    levels=c(1,0),
                                    labels=c("Preterm","Term"))
label(geobirth_data$preterm_yn) <- "Birth outcome"

# hypertension
geobirth_data$htn <- factor(geobirth_data$htn, 
                                   levels=c(1,0),
                                   labels=c("Pre-existing hypertension","No pre-existing hypertension"))
label(geobirth_data$htn) <- "Pre-existing hypertension"

# diabetes
geobirth_data$diabetes <- factor(geobirth_data$diabetes, 
                            levels=c(1,2,3),
                            labels=c("Pre-existing diabetes","Gestational diabetes", "No diabetes"))
label(geobirth_data$diabetes) <- "Diabetes"

# education
geobirth_data$edu_cat_CP <- factor(geobirth_data$edu_cat_CP, 
                                       levels=c(1,2),
                                       labels=c("Some college or less", "College degree or higher"))
label(geobirth_data$edu_cat_CP) <- "Educational attainment"

# insurance
geobirth_data$insur_cat <- factor(geobirth_data$insur_cat, 
                                levels=c(1,2),
                                labels=c("Private","Public, self-pay, other"))
label(geobirth_data$insur_cat) <- "Insurance type"

# moving
geobirth_data$moving_ind <- factor(geobirth_data$moving_ind, 
                                  levels=c(1,0),
                                  labels=c("One or more moves","No move"))
label(geobirth_data$moving_ind) <- "Residential mobility"

# race and ethnicity
geobirth_data$race_cat <- factor(geobirth_data$race_cat, 
                                  levels=c(2,1,3,4,5),
                                  labels=c("Non-Hispanic Black","Non-Hispanic White", "Asian", "Hispanic", "Multiracial/other"))
label(geobirth_data$race_cat) <- "Race and ethnicity"

# smoking
geobirth_data$smoker_doh <- factor(geobirth_data$smoker_doh, 
                                 levels=c(1,2,3),
                                 labels=c("Current smoker","Former smoker", "Never smoker"))
label(geobirth_data$smoker_doh) <- "Smoking status"

# hospital
geobirth_data$hospital_del <- factor(geobirth_data$hospital_del, 
                                 levels=c("PAH", "HUP"),
                                 labels=c("PAH","HUP"))
label(geobirth_data$hospital_del) <- "Hospital of delivery"

################################################################################
## add pval or SMD to table
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
  
  # calculate SMD rounding to 3 decimal places
  round(smd::smd(y, g, na.rm = TRUE)[2], 3)
  
  # format SMD
  # empty string places SMD on the line below the variable label
  c("", round(smd::smd(y, g, na.rm = TRUE)[2], 3))
}

################################################################################
## create table 
################################################################################
table1(~ mage_cat_CP + race_cat + edu_cat_CP + insur_cat + nulliparous + preterm_yn + bmi_imp + htn + diabetes + smoker_doh +
       moving_ind + hospital_del | preeclampsia, 
       data=geobirth_data, 
       overall=F, 
       render.continuous=c(.="Mean (SD)"), 
       render.missing=NULL,
       extra.col=list("P value"= pvalue, "SMD"= Csmd))
