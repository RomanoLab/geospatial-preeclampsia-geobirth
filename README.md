# geospatial-preeclampsia-geobirth

[![DOI](https://zenodo.org/badge/900919653.svg)](https://doi.org/10.5281/zenodo.17309276)

This repository contains the code used to run the analyses from the paper “A Machine Learning-Based Approach to Disentangle the Relative Importance of Features of the Maternal Neighborhood Environment on Preeclampsia Diagnosis” 
(Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD). Note that the data from the preeclampsia case-control study used in the paper is not publicly available due to patient privacy concerns. To run these analyses the user must provide their own patient data. Further information on how to download the original neighborhood feature data sources is provided in `data/`. 

** Please note: the neighborhood features PM2.5 and temperature were part of our patient data and so no data curation/linkage was necessary for our analysis.

Please follow these steps to reproduce the analyses with the input of a case-control study:

***Prepare publicly available data***

Instructions on how to download and prepare the data used in the analyses are available in `data/README.md`.

***Curate and link the neighborhood features to the patient data***  

- `PLACES_data_cleaning.py` : script to clean the PLACES data
- `ACS_vars_linkage.R` : script to download ACS variables through the United States Census Bureau’s API and link to the patient data
- `tree_canopy_linkage.R` : script to link tree canopy cover to patient data
- `walkability_linkage.R` : script to link walkability to patient data
- `PLACES_linkage.R` : script to link PLACES data to patient data

***Create neighborhood features dataset***

- `make_features_seqs.py` : script to make feature sequences from LMP-to-preeclampsia diagnosis as described in supplements of the paper
- `make_avg_dataset.py` : script to average each participant’s neighborhood feature

***Run bivariate analysis and conditional logistic regression models:*** 

- `pt_char_bivariate_analysis.R` : script to run a bivariate analysis on the participant characteristics
- `phl_planning_dist_comp.R` : script to run the Philadelphia planning district comparison of neighborhood feature averages
- `neighborhood_features_bivariate_analysis.py` : script to run a bivariate analysis on the neighborhood features
- `cond_log_reg_models.py` : script to run conditional logistic regression models

***Run machine learning models:***

- `PE_XGBoost_model.py` : script to train and evaluate XGBoost models + run SHAP analysis

***Paper figures code***

- `figure_4.R`
- `supp_4_corr_plot.R` : script for supplemental appendix 4 correlation plot
