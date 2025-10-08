######################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

# README!
# Code to train XGBoost model on neighborhood features

# FILE OUTPUT:
# model performance and SHAP plot (Figure 4 and 5)
######################################################################################
import pandas as pd
import numpy as np
import shap
import scipy.stats as stats
import scipy.stats
import xgboost as xgb
from numpy import mean
from numpy import std
import argparse
import pickle
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_validate
from sklearn.model_selection import RandomizedSearchCV

from sklearn import metrics  
from sklearn.metrics import RocCurveDisplay, auc

######################################################################################
# HYPERPARAMETER SEARCH
######################################################################################
def xgboost_hyperparam_search(train_pts, train_labels, skf):
    '''
    XGBoost model hyperparameter search.

    Parameters
    ----------
    train_pts : training patients
    train_labels : training labels
    skf : cross validation strategy

    Outputs
    ----------
    best_model : best performing model for dataset
    '''
    
    # define hyperparameter distributions
    param_dist = {
        'max_depth': stats.randint(3, 10),
        'learning_rate': stats.uniform(0.01, 0.1),
        'subsample': stats.uniform(0.5, 0.5),
        'n_estimators':stats.randint(50, 200)
    }

    # make XGBoost model
    xgb_model = xgb.XGBClassifier()

    # make RandomizedSearchCV object
    random_search = RandomizedSearchCV(xgb_model,
                                       param_distributions=param_dist, 
                                       n_iter=1000, 
                                       scoring='roc_auc',
                                       cv = skf,
                                       random_state=42)

    # fit RandomizedSearchCV to training data
    random_search.fit(train_pts, train_labels)

    # print best score
    print("Best score: ", random_search.best_score_)

    # save best model
    best_model = random_search.best_estimator_

    return best_model

######################################################################################
# PERFORMANCE METRICS
######################################################################################
def perf_metrics(cross_val_scores):
    '''
    Calculate mean + CI of performance metric.

    Parameters
    ----------
    cross_val_scores : scores from cross validation

    Outputs
    ----------
    mean_acc : mean
    std_dev : standard deviation
    ci : confidence interval
    '''
    # calculate metrics
    mean_acc = np.mean(cross_val_scores)
    std_dev = np.std(cross_val_scores, ddof=1)
    confidence = 0.95
    alpha = 1 - confidence
    df = len(cross_val_scores) - 1
    ci = scipy.stats.t.interval(confidence, df, loc=mean_acc, scale = std_dev/np.sqrt(len(cross_val_scores)))

    return mean_acc, std_dev, ci

if __name__=="__main__":
    ######################################################################################
    # INITIALIZATION OF ARGUMENTS
    ######################################################################################
    parser = argparse.ArgumentParser(description = "Train models on average exposure dataset.")

    parser.add_argument("--gb-data-path", type=str, default = './data/average_features_dataset.csv',
                        help = "File path for average exposure dataset.")
    
    parser.set_defaults(validation = True)
    args = parser.parse_args()

    ######################################################################################
    # DATASET PREP
    ######################################################################################
    # load in data
    gb_prenatal_exp_data = pd.read_csv(args.gb_data_path)
    
    # initialize feature lists
    model_feats = ['per_tree_canopy_cover', 'NatWalkInd', 'percent_poverty', 'percent_employ', 'percent_bs_higher',
                      'ICE_race_income_metric', 'percent_before_1960s', 'pm25_ug_m3', 'temp_mean_K', 'obesity_pl', 'mental_distress_pl', 
                      'short_sleep_pl', 'physical_distress_pl', 'asthma_pl', 'phys_act_pl', 'heart_disease_pl', 'disability_pl', 'arthritis_pl', 
                      'cancer_pl', 'hbp_pl', 'COPD_pl', 'poor_health_pl', 'stroke_pl', 'diabetes_pl', 'depression_pl', 'high_cholesterol_pl']

    ######################################################################################
    # SPLIT DATA
    ######################################################################################
    # make feature and label datasets
    X_df = gb_prenatal_exp_data[model_feats].copy()
    y_df = gb_prenatal_exp_data[['preeclampsia']].copy()

    # turn labels into array
    y_df_array = np.array(y_df['preeclampsia'])

    # 80/20 split, stratifying by label
    train_pts, test_pts, train_labels, test_labels = train_test_split(X_df, y_df, stratify=y_df, test_size=0.2)

    # turn labels into array
    train_labels = np.array(train_labels['preeclampsia'])
    test_labels = np.array(test_labels['preeclampsia'])

    # reset index
    train_pts.reset_index(inplace=True, drop=True)
    test_pts.reset_index(inplace=True, drop=True)

    ######################################################################################
    # HYPERPARAMETER SEARCH
    ######################################################################################
    # set the cross validation strategy
    skf = StratifiedKFold(n_splits=5)

    # get best model for dataset from hyperparameter search
    best_model = xgboost_hyperparam_search(train_pts, train_labels, skf)

    ######################################################################################
    # CROSS VALIDATION EVALUATION
    ######################################################################################
    cv_results = cross_validate(best_model, X_df, y_df_array, cv=skf, scoring = ['roc_auc', 'accuracy'], return_train_score = True)
    AUC_scores = cv_results['test_roc_auc']
    accuracy_scores = cv_results['test_accuracy']

    ######################################################################################
    # PERFORMANCE METRICS
    ######################################################################################
    AUC_mean, AUC_std, AUC_ci = perf_metrics(AUC_scores)
    accuracy_mean, accuracy_std, accuracy_ci = perf_metrics(accuracy_scores)
    print('AUC mean +/- std:', round(float(AUC_mean),3), '+/-', round(float(AUC_std),3), ', CI:', round(float(AUC_ci[0]),3), round(float(AUC_ci[1]),3))
    print('Accuracy mean +/- std:', round(float(accuracy_mean),3), '+/-', round(float(accuracy_std),3), ', CI:', round(float(accuracy_ci[0]),3), round(float(accuracy_ci[1]),3))

    ######################################################################################
    # SHAP ANALYSIS
    ######################################################################################
    # copy of data
    PE_data = X_df.copy()
    
    # rename exposure names
    PE_data.columns = ['Tree canopy cover (%)', 'National Walkability Index score', 'People below the federal poverty threshold (%)',
                                'People employed (%)', 'People with bachelors degree or higher (%)', 'ICE Race-Income', 'Housing units built before 1960 (%)',
                                'Daily PM2.5', 'Daily mean temperature', 'Obesity (%)', 'Frequent mental distress (%)', 'Short sleep duration (%)', 
                                'Frequent physical distress (%)', 'Asthma (%)', 'No leisure-time physical activity (%)', 'Coronary heart disease (%)',
                                'Any disability (%)', 'Arthritis (%)', 'Cancer (%)', 'High blood pressure (%)',
                                'COPD (%)', 'Fair or poor self-rated health status (%)', 'Stroke (%)',
                                'Diabetes (%)', 'Depression (%)', 'High cholesterol (%)']
    
    # shap values for xgboost model
    explainer = shap.TreeExplainer(best_model)
    shap_values = explainer.shap_values(PE_data)
    shap.summary_plot(shap_values, PE_data, show=False, max_display=10)

    ## figure 4
    # get SHAP values into dataframe
    shap_vals_df = pd.DataFrame(shap_values)

    # absolute value
    abs_shap_vals_df = shap_vals_df.abs()

    # mean absolute shap value for each feature
    mean_abs_shap_vals = list(abs_shap_vals_df.mean())

    # feature list
    full_names_feats = ['Tree canopy cover (%)', 'National Walkability Index score', 'People below the federal poverty threshold (%)',
                                    'People employed (%)', 'People with bachelors degree or higher (%)', 'ICE Race-Income', 'Housing units built before 1960 (%)',
                                    'Daily PM2.5', 'Daily mean temperature', 'Obesity (%)', 'Frequent mental distress (%)', 'Short sleep duration (%)', 
                                    'Frequent physical distress (%)', 'Asthma (%)', 'No leisure-time physical activity (%)', 'Coronary heart disease (%)',
                                    'Any disability (%)', 'Arthritis (%)', 'Cancer (%)', 'High blood pressure (%)',
                                    'Chronic obstructive pulmonary disease (COPD) (%)', 'Fair or poor self-rated health status (%)', 'Stroke (%)',
                                    'Diabetes (%)', 'Depression (%)', 'High cholesterol (%)']

    # make dataframe with features and their mean absolute shap value
    feat_shap_val_df = pd.DataFrame(
        {'feature' : full_names_feats,
        'mean_shap_val': mean_abs_shap_vals
        })

    # sort by highest shap value
    feat_shap_val_df.sort_values(by=['mean_shap_val'], ascending=False, inplace=True)

    # add percent contribution
    feat_shap_val_df['per_contrib'] = (feat_shap_val_df['mean_shap_val'] / feat_shap_val_df['mean_shap_val'].sum())

    # get first 10 features
    first_10_shap_vals_df = feat_shap_val_df.head(10)

    # get other 16
    last_16_shap_vals_df = feat_shap_val_df.iloc[-16:]

    # put into dataframe to plot in R
    shap_Rplot_df = first_10_shap_vals_df[['feature','per_contrib']]

    # add sum of last 16 to df
    shap_Rplot_df.loc[-1] = ['Sum of 16 other features', last_16_shap_vals_df['per_contrib'].sum()]

    # save
    shap_Rplot_df.to_csv('./data/shap_mean_vals_bar_plot.csv', index=False)

    ## figure 5
    # modify main plot parameters
    fig, ax = plt.gcf(), plt.gca()
                
    ax.tick_params(labelsize=18)
    ax.set_xlabel("SHAP value (impact on model output)", fontsize=18)

    # Get colorbar
    cb_ax = fig.axes[1] 

    # Modifying color bar parameters
    cb_ax.tick_params(labelsize=16)
    cb_ax.set_ylabel("Feature value", fontsize=20)

    fig.set_size_inches(18.5, 10.5)
    fig.savefig("Figure_5_SHAP_plot.pdf") #, dpi=100

