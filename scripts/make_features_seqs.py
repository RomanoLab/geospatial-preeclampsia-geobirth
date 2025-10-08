######################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

# README!
# Code to make feature sequences from LMP to PE diagnosis (referred to as PDT). 
# Using geobirth data + neighborhood features data.

# FILE OUTPUT:
# pickled list of arrays of feature sequences for each participant
######################################################################################
import argparse
import pandas as pd
from sklearn.model_selection import train_test_split
import numpy as np
import glob
from functools import reduce
import pickle

######################################################################################
# CLEAN PREGNANCY EXPOSURE VALUES DICT
######################################################################################
def clean_preg_exp_vals_dict(preg_exp_vals_dict):
    '''
    Function to clean dictionary of everyday of pregnancy exposure values.

    Parameters
    ----------
    preg_exp_vals_dict : dictionary of everyday of pregnancy exposure values for a specific exposure type

    Outputs
    ----------
    preg_exp_vals_dict_clean : cleaned dictionary of everyday of pregnancy exposure values for a specific exposure type
    '''

    for key, val in preg_exp_vals_dict.items():
        # convert mrn to int
        val['mrn'] = int(val['mrn'])

        # convert start and end dates from string to datetime
        val['start_date_dt']= pd.to_datetime(val['start_date_c']) 
        val['end_date_dt']= pd.to_datetime(val['end_date_c']) 

        # calculate how many days participants was at a specific address (i.e. had a specific exposure value)
        val['days'] = val['end_date_dt'] - val['start_date_dt']

        # turn into int
        val['days'] = val['days'].days

    preg_exp_vals_dict_clean = preg_exp_vals_dict

    return preg_exp_vals_dict_clean

######################################################################################
# CLEAN PREGNANCY EXPOSURE VALUES DF
######################################################################################
def clean_preg_exp_vals_df(preg_exp_vals_df):
    '''
    Function to clean dataframe of everyday of pregnancy exposure values.

    Parameters
    ----------
    preg_exp_vals_df : dataframe of everyday of pregnancy exposure values for a specific exposure type

    Outputs
    ----------
    preg_exp_vals_df_clean : cleaned dataframe of everyday of pregnancy exposure values for a specific exposure type
    '''

    # convert mrn to int
    preg_exp_vals_df['mrn'] = preg_exp_vals_df['mrn'].astype(np.int64)

    # convert start and end dates from string to datetime
    preg_exp_vals_df['start_date_dt']= pd.to_datetime(preg_exp_vals_df['start_date_c']) 
    preg_exp_vals_df['end_date_dt']= pd.to_datetime(preg_exp_vals_df['end_date_c']) 

    # calculate how many days participants was at a specific address (i.e. had a specific exposure value)
    preg_exp_vals_df['days'] = preg_exp_vals_df['end_date_dt'] - preg_exp_vals_df['start_date_dt']

    # turn into int
    preg_exp_vals_df['days'] = preg_exp_vals_df['days'].dt.days
    preg_exp_vals_df_clean = preg_exp_vals_df

    return preg_exp_vals_df_clean

######################################################################################
# GET EXPOSURE SEQUENCE FOR ALL PATIENTS (CHANGING EXPOSURE VALUES)
######################################################################################
def get_exp_seq(exp_seq_empty, spec_pt_exp_val_info, exp_feat):
    '''
    Function to get exposure sequence for a specific exposure where the exposure value can change during pregnancy.

    Parameters
    ----------
    exp_seq_empty : empty exposure sequence as array of length PDT
    spec_pt_exp_val_info : exposure values to fill
    exp_feat : exposure feature name

    Outputs
    ----------
    exp_seq_fill : filled exposure sequence as array of length PDT
    '''
    ###################
    # check if participant moved or not during pregnancy #
    ##################
    ## pts that didn't move 
    if len(spec_pt_exp_val_info) == 1: # ie addhx only has one record
        # put same exposure value for their whole PDT
        exp_seq_empty.fill(spec_pt_exp_val_info.iloc[0][exp_feat])
        exp_seq_fill = exp_seq_empty

    ## pts that moved
    elif len(spec_pt_exp_val_info) > 1: # ie addhx only has more than one record
        # start with empty gestational age exposure vector to fill in
        g_age_exposure_vec = []

        # go through the participant's address record
        for index, row in spec_pt_exp_val_info.iterrows():

            # get exposure length from the number of days a patient was at an address
            if index == spec_pt_exp_val_info.index[-1]: # if this is the last address in address history, then exposure length = days at this address
                len_of_exp = row['days'] # exposure length

            else: # if not last address add an extra day to take into account the "move" days
                len_of_exp = row['days'] + 1

            # get the exposure value at this address
            exp_val = row[exp_feat]

            # make an exposure value vector of size exposure length (daily)
            exp_vec = [exp_val] * (len_of_exp)

            # append to gestational age exposure vector--doing this for the whole gestational age
            g_age_exposure_vec = g_age_exposure_vec + exp_vec

        # we now have the exposure values for all of a patient's gestational age, however, we only need for PDT
        # note: if PDT = gestational age, nothing needs to change, however for the rest we break this up into two buckets of patients: 
        # (1) PDT > gestational age: since we only have address history/exposure values until delivery for patients with PDT longer 
        #                            than gestational age (because of postpartum PE) we use the exposure value at delivery address 
        #                            for the PDT days past gestational age
        # (2) PDT < gestational age: got preeclampsia before they delivered, for this we just cut the gestational age exposure vector to the length of the PDT

        ## BUCKET 1: PDT > gestational age
        # for patients where the PDT is longer than the gestational age we add extra exposure values
        if exp_seq_empty.shape[1] > len(g_age_exposure_vec):
            # get last exposure value (exposure value @ delivery address ie the one we will be duplicating)
            last_exp_val = g_age_exposure_vec[-1]

            # make a list of the exposure value duplicated for however long the gap between the gestational age and PDT is
            addition = [last_exp_val] * (exp_seq_empty.shape[1] - len(g_age_exposure_vec))

            # add the extra exposure values to gestational age exposure vec to make PDT exposure vec
            PDT_exposure_vec = g_age_exposure_vec + addition

        ## BUCKET 2: PDT < gestational age
        # for the other patients make sure length is the same PDT (cut if PDT is shorter than gestational age)
        else:
            PDT_exposure_vec = g_age_exposure_vec[0:exp_seq_empty.shape[1]]
        
        # convert to array
        exp_seq_fill = np.array(PDT_exposure_vec)
        
    return exp_seq_fill

######################################################################################
# GET EXPOSURE SEQUENCE FOR ALL PATIENTS (AIR POLLUTION/TEMP EXPOSURE VALUES)
######################################################################################
def get_exp_seq_AT_exps(spec_pt_AT_exp_val_info, exp_feat, pt_PDT):
    '''
    Function to get exposure sequence for air pollution/temp exposures.

    Parameters
    ----------
    spec_pt_AT_exp_val_info : exposure value to fill
    exp_feat : exposure feature name
    pt_PDT : patient PDT

    Outputs
    ----------
    exp_seq_fill : filled exposure sequence as array of length PDT
    '''
    # get the exposure sequence (8 weeks postpartum)
    g_pp_exposure_vec = list(spec_pt_AT_exp_val_info[exp_feat])

    # cut to PDT
    PDT_exposure_vec = g_pp_exposure_vec[0:pt_PDT]

    # convert to array
    exp_seq_fill = np.array(PDT_exposure_vec)

    return exp_seq_fill

if __name__=="__main__":
    ######################################################################################
    # INITIALIZATION OF ARGUMENTS
    ######################################################################################
    parser = argparse.ArgumentParser(description = "Make feature matrices for all patients.")

    parser.add_argument("--pt-data-path", type=str, default = './data/preg_data.csv',
                        help = "File path for patient data.")
    parser.add_argument("--AT-exp-vals-path", type=str, default = './data/neighborhood_features/temp_PM25_data.csv',
                        help = "File path for air pollution and temperature exposure values.")
    parser.add_argument("--ALL-exp-vals-path", type=str, default = './data/neighborhood_features/',
                        help = "File path for all exposure values.")
    
    parser.set_defaults(validation = True)
    args = parser.parse_args()

    ######################################################################################
    # MAIN 
    ######################################################################################
    # load in patient data 
    pt_data = pd.read_csv(args.pt_data_path)

    ######################################################################################
    # GET EXPOSURE DATA
    ######################################################################################
    # read in exposure values for every day of pregnancy for each participant for every exposure
    air_temp_vals = pd.read_csv(args.AT_exp_vals_path)
    exp_files = glob.glob(args.ALL_exp_vals_path + '*' + '_exp_vals.csv') 
    exp_dfs = []

    # merge all exposure data (except air pollution/temperature) for each exposure feature into one dataframe
    for i in exp_files:
        exp_dfs.append(pd.read_csv(i))

    all_exp_df = reduce(lambda left,right: pd.merge(left,right,on=['mrn', 'delivery_date', 'pregnancyID', 'start_date_c', 'end_date_c'], how='outer'), exp_dfs)

    # clean exposure dataframe
    preg_exp_vals_df_clean = clean_preg_exp_vals_df(all_exp_df)

    # get list of exposure feature names
    exp_feat_names = ['per_tree_canopy_cover', 'NatWalkInd', 'percent_poverty', 'percent_employ', 'percent_bs_higher', 'ICE_race_income_metric', 
                      'percent_before_1960s', 'pm25_ug_m3', 'temp_mean_K', 'obesity_pl', 'mental_distress_pl', 'short_sleep_pl', 
                      'physical_distress_pl', 'asthma_pl', 'phys_act_pl', 'heart_disease_pl', 'disability_pl', 'arthritis_pl', 
                      'cancer_pl', 'hbp_pl', 'COPD_pl', 'poor_health_pl', 'stroke_pl', 'diabetes_pl', 'depression_pl', 'high_cholesterol_pl']

    # go through each participant
    all_pts_matrices = []
    num_pt_check = 0
    
    for index,row in pt_data.iterrows():
        print(num_pt_check)
        # get the exposure value records based on the participant's mrn
        spec_pt_exp_val_info = preg_exp_vals_df_clean[(preg_exp_vals_df_clean['mrn'] == row['mrn'])]

        # special one for air pollution/temp because data was done in long format
        spec_pt_AT_exp_val_info = air_temp_vals[(air_temp_vals['mrn'] == row['mrn'])]

        # empty list of exposure sequences
        all_exp_seqs = []

        # go through each exposure
        for exp_feat in exp_feat_names:
            # get patient PDT
            pt_PDT = row['PDT']
    
            # make empty array for exposure sequence the length of PDT
            exp_seq_empty = np.empty([1, pt_PDT])
            
            # based on which exposure feature get exposure sequence
            if ((exp_feat == 'per_tree_canopy_cover') | (exp_feat == 'NatWalkInd') | (exp_feat == 'percent_poverty') | (exp_feat == 'percent_employ') |
            (exp_feat == 'percent_bs_higher') | (exp_feat == 'ICE_race_income_metric') |
            (exp_feat == 'percent_before_1960s') | (exp_feat == 'obesity_pl') | (exp_feat == 'mental_distress_pl') | (exp_feat == 'short_sleep_pl') | 
            (exp_feat == 'physical_distress_pl') | (exp_feat == 'asthma_pl') | (exp_feat == 'phys_act_pl') |
            (exp_feat == 'heart_disease_pl') | (exp_feat == 'disability_pl') | (exp_feat == 'arthritis_pl') | (exp_feat == 'cancer_pl') | (exp_feat == 'hbp_pl') |
            (exp_feat == 'COPD_pl') | (exp_feat == 'poor_health_pl') | (exp_feat == 'stroke_pl') | (exp_feat == 'diabetes_pl') | (exp_feat == 'depression_pl') |
            (exp_feat == 'high_cholesterol_pl')):
                exp_seq_fill = get_exp_seq(exp_seq_empty, spec_pt_exp_val_info, exp_feat)

            elif ((exp_feat == 'pm25_ug_m3') | (exp_feat == 'temp_mean_K')):
                exp_seq_fill = get_exp_seq_AT_exps(spec_pt_AT_exp_val_info, exp_feat, pt_PDT)
            
            # add to list of all exposure sequences for patient
            all_exp_seqs.append(exp_seq_fill)

        # concatenate all exposure sequence arrays together
        pt_exp_matrix = np.vstack(all_exp_seqs)

        # save to list
        all_pts_matrices.append(pt_exp_matrix)
        num_pt_check = num_pt_check + 1

    # save list
    exp_seq_list = open('./data/PDT_exp_seqs_list', 'wb') 
    pickle.dump(all_pts_matrices, exp_seq_list) 
    exp_seq_list.close()