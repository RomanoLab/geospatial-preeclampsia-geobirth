################################################################################
# A Machine Learning-Based Approach to Disentangle the Relative Importance of 
# Features of the Maternal Neighborhood Environment in Preeclampsia Diagnosis
# Paris CF, Ledyard R, Just AC, South EC, Nguemeni Tiako MJ, Canelón SP, Burris HH, Romano JD

## README
# script for paper Figure 4
################################################################################

################################################################################
## load libraries (install if don't have them)
################################################################################
library(dplyr)
library(ggplot2)
library(tidyverse)

################################################################################
## Figure 4
################################################################################
# load shap data
shap_data <- as.data.frame(read.csv("./data/shap_mean_vals_bar_plot.csv"))

# reorder feats in order of shap value
order_feat <- c("Obesity (%)","High blood pressure (%)","Short sleep duration (%)",
                "ICE Race-Income","No leisure-time physical activity (%)",
                "Asthma (%)","Diabetes (%)","People below the federal poverty threshold (%)",
                "Daily mean temperature","People with bachelors degree or higher (%)",
                "Sum of 16 other features")

# plot
ggplot(data = shap_data, aes(x=factor(feature,rev(order_feat)), y=per_contrib, fill=feature)) +
  geom_bar(stat='identity', width = 0.8) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "snow3",linetype = 1),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("Short sleep duration (%)" = "#B998DDFF",
                               "Obesity (%)" = "#B998DDFF",
                               "No leisure-time physical activity (%)" = "#B998DDFF",
                               "High blood pressure (%)" = "#B998DDFF",
                               "Diabetes (%)" = "#B998DDFF",
                               "Asthma (%)" = "#B998DDFF",
                               "People with bachelors degree or higher (%)" = "#B998DDFF",
                               "People below the federal poverty threshold (%)" = "#B998DDFF",
                               "ICE Race-Income" = "#B998DDFF",
                               "Daily mean temperature" = "#B998DDFF",
                               "Sum of 16 other features" = "#B998DDFF")) +
  guides(fill = guide_none()) +
  labs(title = "",
       x = "",
       y = "Proportion of Mean Absolute SHAP value (%)") +
  coord_flip()