######################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

# README!
# Code to make dataset out of features sequences.

# FILE OUTPUT:
# average neighborhood feature dataset
# rows: patients
# columns: neighborhood features + patient info
######################################################################################
import argparse
import pandas as pd
from sklearn.model_selection import train_test_split
import numpy as np
import glob
from functools import reduce
import pickle

######################################################################################
# AVERAGE EXPOSURES
######################################################################################
def avg_exp(all_pts_matrices):
    '''
    Function to get average of exposure sequence.

    Parameters
    ----------
    all_pts_matrices : exposure sequences for all patients 

    Outputs
    ----------
    feat_list : list of average exposures for all patients
    '''
    # go through every patient
    feat_list = []
    for i in all_pts_matrices:
        mean_feats = np.mean(i, axis=1) # get the mean of every feature
        feat_list.append(mean_feats)

    return feat_list

if __name__=="__main__":
    ######################################################################################
    # INITIALIZATION OF ARGUMENTS
    ######################################################################################
    parser = argparse.ArgumentParser(description = "Make avg neighborhood feature dataset.")

    parser.add_argument("--pt-data-path", type=str, default = './data/preg_data.csv',
                        help = "File path for patient data.")
    parser.add_argument("--exp-seqs-path", type=str, default = './data/PDT_exp_seqs_list',
                        help = "File path for feature sequences.")
    
    parser.set_defaults(validation = True)
    args = parser.parse_args()

    ######################################################################################
    # MAKE AVERAGE EXPOSURE DATASET
    ######################################################################################
    # load in exposure sequences
    exp_seqs = pd.read_pickle(args.exp_seqs_path)

    # average over all exposure sequence
    feat_list = avg_exp(exp_seqs)

    # turn into an exposure feature dataframe
    pt_exposure_df = pd.DataFrame(feat_list)

    # get list of exposure feature names
    exp_feat_names = ['per_tree_canopy_cover', 'NatWalkInd', 'percent_poverty', 'percent_employ', 'percent_bs_higher',
                      'ICE_race_income_metric', 'percent_before_1960s', 'pm25_ug_m3', 'temp_mean_K', 'obesity_pl', 'mental_distress_pl', 
                      'short_sleep_pl', 'physical_distress_pl', 'asthma_pl', 'phys_act_pl', 'heart_disease_pl', 'disability_pl', 'arthritis_pl', 
                      'cancer_pl', 'hbp_pl', 'COPD_pl', 'poor_health_pl', 'stroke_pl', 'diabetes_pl', 'depression_pl', 'high_cholesterol_pl']

    pt_exposure_df.columns = exp_feat_names

    # merge with participant data  
    pt_data = pd.read_csv(args.pt_data_path)
    avg_exposure_dataset = pd.concat([pt_data,pt_exposure_df], axis = 1)

    # save
    avg_exposure_dataset.to_csv('./data/average_features_dataset.csv', index = False)