###### 0. Load Packages ####
# Purpose: Load necessary libraries and prepare the working environment to calculate 
# the mean and 0.15*standard deviation of specific columns for different climate zones.

library(tidyverse)
library(dplyr)

# Set the working directory
setwd("D:/VegetationImpact")

# Read the dataset
df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")

# Calculate the specified columns for different climate zones
# average_diff_21_mean: Immediate temperature effect (ΔLST) after greenup for Ameriflux sites
# average_diff_26_mean: Immediate temperature effect (ΔLST) after dormancy for Ameriflux sites
# sum_Diff_16_mean: Cumulative temperature across the entire growing season
# days_16_mean: Growing season length
# mean_Diff_16_mean: Mean temperature effect (ΔLST) during the growing season

target_columns <- c("average_diff_21_mean", "average_diff_26_mean", "sum_Diff_16_mean",
                    "days_16_mean","mean_Diff_16_mean")

# Create three subset data frames
df_sum <- df
df_cXa <- df[df$Clim == "Cfa", ]  # Subset for Cfa climate
df_DXb <- df[df$Clim == "Dfb", ]  # Subset for Dfb climate

# Define a function to calculate the mean and 0.15*standard deviation for specified columns
calculate_mean_sd <- function(data, class_label) {
  mean_values <- round(sapply(data[, target_columns], mean, na.rm = TRUE), 2)
  sd_015_values <- round(sapply(data[, target_columns], function(x) 0.15 * sd(x, na.rm = TRUE)), 2)
  
  # Summarize the mean and standard deviation into a data frame and add the "Class" column
  result <- data.frame(
    Variable = target_columns,
    Mean = mean_values,
    SD_015 = sd_015_values,
    Class = class_label
  )
  
  return(result)
}

# Calculate the mean and 0.15*standard deviation for each subset
df_sum_stats <- calculate_mean_sd(df_sum, "df_sum")  # Overall data
df_cXa_stats <- calculate_mean_sd(df_cXa, "df_cXa")  # Cfa climate
df_DXb_stats <- calculate_mean_sd(df_DXb, "df_DXb")  # Dfb climate

# Combine the results from the three subsets into a single data frame
final_df <- rbind(df_sum_stats, df_cXa_stats, df_DXb_stats)

# Print the final combined data frame
final_df <- final_df
print(final_df)

# Variable   Mean SD_015  Class
# average_diff_21_mean  average_diff_21_mean    0.45   0.34 df_sum
# average_diff_26_mean  average_diff_26_mean   -0.39   0.26 df_sum
# sum_Diff_16_mean          sum_Diff_16_mean -558.74  94.98 df_sum
# days_16_mean                  days_16_mean -184.18   5.08 df_sum
# mean_Diff_16_mean        mean_Diff_16_mean   -2.62   0.43 df_sum
# average_diff_21_mean1 average_diff_21_mean    0.89   0.30 df_cXa
# average_diff_26_mean1 average_diff_26_mean   -1.66   0.18 df_cXa
# sum_Diff_16_mean1         sum_Diff_16_mean -950.68  94.23 df_cXa
# days_16_mean1                 days_16_mean -214.88   2.72 df_cXa
# mean_Diff_16_mean1       mean_Diff_16_mean   -4.27   0.41 df_cXa
# average_diff_21_mean2 average_diff_21_mean    0.06   0.39 df_DXb
# average_diff_26_mean2 average_diff_26_mean    0.74   0.19 df_DXb
# sum_Diff_16_mean2         sum_Diff_16_mean -210.35  61.20 df_DXb
# days_16_mean2                 days_16_mean -156.89   2.25 df_DXb
# mean_Diff_16_mean2       mean_Diff_16_mean   -1.14   0.33 df_DXb