######################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

# README!
# Code for data processing of PLACES raw data (for health vulnerability variables)

# FILE OUTPUT:
# cleaned PLACES data as csv file
######################################################################################
import argparse
import pandas as pd
from functools import reduce

######################################################################################
# PLACES
######################################################################################
def clean_PLACES_data(places_data):
    '''
    Clean PLACES raw data, subset to relevant columns.

    Parameters
    ----------
    data : data to clean (PLACES raw data)

    Outputs
    ----------
    places_data_clean : cleaned data
    '''
    # take out relevant variables
    places_obesity = places_data[places_data['Measure'] == 'Obesity among adults']
    places_mental_distress = places_data[places_data['Measure'] == 'Frequent mental distress among adults']
    places_short_sleep = places_data[places_data['Measure'] == 'Short sleep duration among adults']
    places_physical_distress = places_data[places_data['Measure'] == 'Frequent physical distress among adults']
    places_asthma = places_data[places_data['Measure'] == 'Current asthma among adults']
    places_phys_act = places_data[places_data['Measure'] == 'No leisure-time physical activity among adults']
    places_heart_disease = places_data[places_data['Measure'] == 'Coronary heart disease among adults']
    places_disability = places_data[places_data['Measure'] == 'Any disability among adults']
    places_arthritis = places_data[places_data['Measure'] == 'Arthritis among adults']
    places_cancer = places_data[places_data['Measure'] == 'Cancer (non-skin) or melanoma among adults']
    places_hbp = places_data[places_data['Measure'] == 'High blood pressure among adults']
    places_COPD = places_data[places_data['Measure'] == 'Chronic obstructive pulmonary disease among adults']
    places_poor_health = places_data[places_data['Measure'] == 'Fair or poor self-rated health status among adults']
    places_stroke = places_data[places_data['Measure'] == 'Stroke among adults']
    places_diabetes = places_data[places_data['Measure'] == 'Diagnosed diabetes among adults']
    places_depression = places_data[places_data['Measure'] == 'Depression among adults']
    places_high_cholesterol = places_data[places_data['Measure'] == 'High cholesterol among adults who have ever been screened']

    # rename columns
    places_obesity = places_obesity[['LocationName', 'Data_Value']]
    places_obesity.columns = ['FIPS','obesity_pl']

    places_mental_distress = places_mental_distress[['LocationName', 'Data_Value']]
    places_mental_distress.columns = ['FIPS','mental_distress_pl']

    places_short_sleep = places_short_sleep[['LocationName', 'Data_Value']]
    places_short_sleep.columns = ['FIPS','short_sleep_pl']

    places_physical_distress = places_physical_distress[['LocationName', 'Data_Value']]
    places_physical_distress.columns = ['FIPS','physical_distress_pl']

    places_asthma = places_asthma[['LocationName', 'Data_Value']]
    places_asthma.columns = ['FIPS','asthma_pl']

    places_phys_act = places_phys_act[['LocationName', 'Data_Value']]
    places_phys_act.columns = ['FIPS','phys_act_pl']

    places_heart_disease = places_heart_disease[['LocationName', 'Data_Value']]
    places_heart_disease.columns = ['FIPS','heart_disease_pl']

    places_disability = places_disability[['LocationName', 'Data_Value']]
    places_disability.columns = ['FIPS','disability_pl']

    places_arthritis = places_arthritis[['LocationName', 'Data_Value']]
    places_arthritis.columns = ['FIPS','arthritis_pl']

    places_cancer = places_cancer[['LocationName', 'Data_Value']]
    places_cancer.columns = ['FIPS','cancer_pl']

    places_hbp = places_hbp[['LocationName', 'Data_Value']]
    places_hbp.columns = ['FIPS','hbp_pl']

    places_COPD = places_COPD[['LocationName', 'Data_Value']]
    places_COPD.columns = ['FIPS','COPD_pl']

    places_poor_health = places_poor_health[['LocationName', 'Data_Value']]
    places_poor_health.columns = ['FIPS','poor_health_pl']

    places_stroke = places_stroke[['LocationName', 'Data_Value']]
    places_stroke.columns = ['FIPS','stroke_pl']

    places_diabetes = places_diabetes[['LocationName', 'Data_Value']]
    places_diabetes.columns = ['FIPS','diabetes_pl']

    places_depression = places_depression[['LocationName', 'Data_Value']]
    places_depression.columns = ['FIPS','depression_pl']

    places_high_cholesterol = places_high_cholesterol[['LocationName', 'Data_Value']]
    places_high_cholesterol.columns = ['FIPS','high_cholesterol_pl']

    # merge data
    data_frames = [places_obesity,places_mental_distress,places_short_sleep,places_physical_distress,
                places_asthma,places_phys_act,places_heart_disease,places_disability,places_arthritis,places_cancer,
                places_hbp,places_COPD,places_poor_health,places_stroke,places_diabetes,places_depression,
                places_high_cholesterol]

    places_data_clean = reduce(lambda left,right: pd.merge(left, right, on=['FIPS'], how='outer'), data_frames)

    return places_data_clean

if __name__=="__main__":
    ######################################################################################
    # INITIALIZATION OF ARGUMENTS
    ######################################################################################
    parser = argparse.ArgumentParser(description = "Clean PLACES raw data.")

    parser.add_argument("--data-path", type=str, default = './data/PLACES/raw/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20250320.csv',
                        help = "File location where PLACES raw data is.")

    parser.set_defaults(validation = True)
    args = parser.parse_args()
    ######################################################################################
    # MAIN 
    ######################################################################################
    # load in data
    places_data = pd.read_csv(args.data_path)

    # clean data
    places_data_clean = clean_PLACES_data(places_data)

    # save
    places_data_clean.to_csv('./data/PLACES/processed/PHL_places_clean.csv')