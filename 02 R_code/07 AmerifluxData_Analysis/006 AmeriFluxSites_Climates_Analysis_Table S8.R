###### 0. Load Required Packages ####
library(tidyverse)
library(dplyr)

setwd("D:/VegetationImpact")

# Read the dataset
df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")

# Specify the columns to calculate
target_columns <- c("average_diff_21_mean", "average_diff_26_mean", "sum_Diff_16_mean",
                    "days_16_mean","mean_Diff_16_mean")

# Create three subset data frames
df_sum <- df
df_cXa <- df[df$Clim == "Cfa", ]  # Warm-humid temperate climate
df_DXb <- df[df$Clim == "Dfb", ]  # Cold-humid temperate climate

# Define a function to calculate the mean and 0.15 * standard deviation for selected columns
calculate_mean_sd <- function(data, class_label) {
  mean_values <- round(sapply(data[, target_columns], mean, na.rm = TRUE), 2)
  sd_015_values <- round(sapply(data[, target_columns], function(x) 0.15 * sd(x, na.rm = TRUE)), 2)
  
  # Combine mean and sd into one data frame with class label
  result <- data.frame(
    Variable = target_columns,
    Mean = mean_values,
    SD_015 = sd_015_values,
    Class = class_label
  )
  
  return(result)
}

# Compute mean and 0.15*SD for each subset
df_sum_stats <- calculate_mean_sd(df_sum, "df_sum")
df_cXa_stats <- calculate_mean_sd(df_cXa, "df_cXa")
df_DXb_stats <- calculate_mean_sd(df_DXb, "df_DXb")

# Merge the results
final_df <- rbind(df_sum_stats, df_cXa_stats, df_DXb_stats)

final_df <- final_df

# Display the final result
print(final_df)

# > print(final_df)
# Variable     Mean SD_015  Class
# average_diff_21_mean  average_diff_21_mean     0.45   0.32 df_sum
# average_diff_26_mean  average_diff_26_mean    -0.37   0.27 df_sum
# sum_Diff_16_mean          sum_Diff_16_mean  -622.73  99.47 df_sum
# days_16_mean                  days_16_mean   184.29   5.07 df_sum
# mean_Diff_16_mean        mean_Diff_16_mean    -2.96   0.45 df_sum
# average_diff_21_mean1 average_diff_21_mean     1.14   0.32 df_cXa
# average_diff_26_mean1 average_diff_26_mean    -1.63   0.19 df_cXa
# sum_Diff_16_mean1         sum_Diff_16_mean -1004.91 103.30 df_cXa
# days_16_mean1                 days_16_mean   214.88   2.72 df_cXa
# mean_Diff_16_mean1       mean_Diff_16_mean    -4.51   0.45 df_cXa
# average_diff_21_mean2 average_diff_21_mean    -0.16   0.31 df_DXb
# average_diff_26_mean2 average_diff_26_mean     0.75   0.21 df_DXb
# sum_Diff_16_mean2         sum_Diff_16_mean  -283.02  65.15 df_DXb
# days_16_mean2                 days_16_mean   157.11   2.29 df_DXb
# mean_Diff_16_mean2       mean_Diff_16_mean    -1.57   0.35 df_DXb