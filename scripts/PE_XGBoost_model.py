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
    cv_results = cross_validate(best_model, X_df, y_df_array, cv=skf, scoring = ['roc_auc', 'accuracy'], return_train_score = True, return_estimator = True, return_indices=True)
    AUC_scores = cv_results['test_roc_auc']
    accuracy_scores = cv_results['test_accuracy']

    # get dataset used as validation set in each of the 5 folds
    fld_1_ind = cv_results['indices']['test'][0]
    fld_2_ind = cv_results['indices']['test'][1]
    fld_3_ind = cv_results['indices']['test'][2]
    fld_4_ind = cv_results['indices']['test'][3]
    fld_5_ind = cv_results['indices']['test'][4]

    ######################################################################################
    # PERFORMANCE METRICS
    ######################################################################################
    AUC_mean, AUC_std, AUC_ci = perf_metrics(AUC_scores)
    accuracy_mean, accuracy_std, accuracy_ci = perf_metrics(accuracy_scores)
    print('AUC mean +/- std:', round(float(AUC_mean),3), '+/-', round(float(AUC_std),3), ', CI:', round(float(AUC_ci[0]),3), round(float(AUC_ci[1]),3))
    print('Accuracy mean +/- std:', round(float(accuracy_mean),3), '+/-', round(float(accuracy_std),3), ', CI:', round(float(accuracy_ci[0]),3), round(float(accuracy_ci[1]),3))

    ######################################################################################
    # SHAP ANALYSIS PART 1 -- FIGURE 4
    ######################################################################################
    # get validation data for 5 different folds
    test_data_fld_1 = X_df.iloc[fld_1_ind]
    test_data_fld_1.reset_index(inplace=True, drop=True)

    test_data_fld_2 = X_df.iloc[fld_2_ind]
    test_data_fld_2.reset_index(inplace=True, drop=True)

    test_data_fld_3 = X_df.iloc[fld_3_ind]
    test_data_fld_3.reset_index(inplace=True, drop=True)

    test_data_fld_4 = X_df.iloc[fld_4_ind]
    test_data_fld_4.reset_index(inplace=True, drop=True)

    test_data_fld_5 = X_df.iloc[fld_5_ind]
    test_data_fld_5.reset_index(inplace=True, drop=True)

    # feature list
    full_names_feats = ['Tree canopy cover (%)', 'National Walkability Index score', 'People below the federal poverty threshold (%)',
                                    'People employed (%)', 'People with bachelors degree or higher (%)', 'ICE Race-Income', 'Housing units built before 1960 (%)',
                                    'Daily PM2.5', 'Daily mean temperature', 'Obesity (%)', 'Frequent mental distress (%)', 'Short sleep duration (%)', 
                                    'Frequent physical distress (%)', 'Asthma (%)', 'No leisure-time physical activity (%)', 'Coronary heart disease (%)',
                                    'Any disability (%)', 'Arthritis (%)', 'Cancer (%)', 'High blood pressure (%)',
                                    'Chronic obstructive pulmonary disease (COPD) (%)', 'Fair or poor self-rated health status (%)', 'Stroke (%)',
                                    'Diabetes (%)', 'Depression (%)', 'High cholesterol (%)']
    
    # shap values for each fold
    explainer = shap.TreeExplainer(best_model)
    shap_values_fld_1 = explainer.shap_values(test_data_fld_1)
    shap_values_fld_2 = explainer.shap_values(test_data_fld_2)
    shap_values_fld_3 = explainer.shap_values(test_data_fld_3)
    shap_values_fld_4 = explainer.shap_values(test_data_fld_4)
    shap_values_fld_5 = explainer.shap_values(test_data_fld_5)

    # get SHAP values into dataframe
    shap_values_fld_1_df = pd.DataFrame(shap_values_fld_1)
    shap_values_fld_2_df = pd.DataFrame(shap_values_fld_2)
    shap_values_fld_3_df = pd.DataFrame(shap_values_fld_3)
    shap_values_fld_4_df = pd.DataFrame(shap_values_fld_4)
    shap_values_fld_5_df = pd.DataFrame(shap_values_fld_5)

    # get absolute value and return mean of every column (feature)
    shap_values_fld_1_df = shap_values_fld_1_df.abs()
    shap_values_fld_2_df = shap_values_fld_2_df.abs()
    shap_values_fld_3_df = shap_values_fld_3_df.abs()
    shap_values_fld_4_df = shap_values_fld_4_df.abs()
    shap_values_fld_5_df = shap_values_fld_5_df.abs()

    shap_values_fld_1_mean = list(shap_values_fld_1_df.mean())
    shap_values_fld_2_mean = list(shap_values_fld_2_df.mean())
    shap_values_fld_3_mean = list(shap_values_fld_3_df.mean())
    shap_values_fld_4_mean = list(shap_values_fld_4_df.mean())
    shap_values_fld_5_mean = list(shap_values_fld_5_df.mean())

    # make dataframe with features and their mean absolute shap value
    feat_shap_val_df = pd.DataFrame(
        {'feature' : full_names_feats,
        'fold_1_mean': shap_values_fld_1_mean,
        'fold_2_mean': shap_values_fld_2_mean,
        'fold_3_mean': shap_values_fld_3_mean,
        'fold_4_mean': shap_values_fld_4_mean,
        'fold_5_mean': shap_values_fld_5_mean
        })

    # sort by highest shap value (using fold 2)
    feat_shap_val_df.sort_values(by=['fold_2_mean'], ascending=False, inplace=True)

    # add percent contribution
    feat_shap_val_df['fold_1_per_contrib'] = (feat_shap_val_df['fold_1_mean'] / feat_shap_val_df['fold_1_mean'].sum())
    feat_shap_val_df['fold_2_per_contrib'] = (feat_shap_val_df['fold_2_mean'] / feat_shap_val_df['fold_2_mean'].sum())
    feat_shap_val_df['fold_3_per_contrib'] = (feat_shap_val_df['fold_3_mean'] / feat_shap_val_df['fold_3_mean'].sum())
    feat_shap_val_df['fold_4_per_contrib'] = (feat_shap_val_df['fold_4_mean'] / feat_shap_val_df['fold_4_mean'].sum())
    feat_shap_val_df['fold_5_per_contrib'] = (feat_shap_val_df['fold_5_mean'] / feat_shap_val_df['fold_5_mean'].sum())

    # FIGURE 4 PLOT (using fold 2)
    # get first 10 features
    first_10_shap_vals_df = feat_shap_val_df.head(10)

    # get other 16
    last_16_shap_vals_df = feat_shap_val_df.iloc[-16:]

    # put into dataframe to plot in R
    shap_Rplot_df = first_10_shap_vals_df[['feature','fold_2_per_contrib']]

    # add sum of last 16 to df
    shap_Rplot_df.loc[-1] = ['Sum of 16 other features', last_16_shap_vals_df['fold_2_per_contrib'].sum()]

    # save
    shap_Rplot_df.to_csv('./data/shap_mean_vals_bar_plot.csv', index=False)

    # FIGURE 4 TOP 3 ANALYSIS
    # get first 3 features
    first_3_shap_vals_df = feat_shap_val_df.head(3)

    # top 3 sum of contribution 
    first_3_shap_sum = list(first_3_shap_vals_df[['fold_1_per_contrib', 'fold_2_per_contrib', 'fold_3_per_contrib', 'fold_4_per_contrib', 'fold_5_per_contrib']].sum())

    # mean and std dev
    mean_contrib = np.mean(first_3_shap_sum)
    std_dev = np.std(first_3_shap_sum, ddof=1)

    print('Mean of top 3 SHAP vals contribution +/- std:', round(float(mean_contrib),3), '+/-', round(float(std_dev),3))

    # get mean and std of each of the top 3 features individually
    first_3_shap_vals_df['ind_feat_folds_mean'] = first_3_shap_vals_df[['fold_1_per_contrib', 'fold_2_per_contrib', 'fold_3_per_contrib', 'fold_4_per_contrib', 'fold_5_per_contrib' ]].mean(axis=1)
    first_3_shap_vals_df['ind_feat_folds_std'] = first_3_shap_vals_df[['fold_1_per_contrib', 'fold_2_per_contrib', 'fold_3_per_contrib', 'fold_4_per_contrib', 'fold_5_per_contrib' ]].std(axis=1)

    ######################################################################################
    # SHAP ANALYSIS PART 1 -- FIGURE 5
    ######################################################################################
    # copy of data
    PE_data = test_data_fld_2.copy()
    
    # rename exposure names
    PE_data.columns = full_names_feats
    
    # shap beeswarm plot 
    shap.summary_plot(shap_values_fld_2, PE_data, show=False, max_display=3, cmap= plt.get_cmap("plasma"))

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
    fig.savefig("Figure_4_SHAP_plot.pdf") #, dpi=100

