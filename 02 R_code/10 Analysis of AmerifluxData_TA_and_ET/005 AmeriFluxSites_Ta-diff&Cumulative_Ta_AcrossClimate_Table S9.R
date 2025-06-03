###### 0. Load Required Packages ######
library(tidyverse)
library(dplyr)

# Set working directory
setwd("D:/VegetationImpact")

# Load site-level average results data
df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)

# Specify target variables for summary statistics
target_columns <- c(
  "AF_TA_average_diff_1_mean",    # ΔTa mean over 5 days post-SOS
  "AF_TA_average_diff_6_mean",    # ΔTa mean over 5 days post-EOS
  "AF_TA_sum_diff_16_mean",       # Cumulative ΔTa during the growing season
  "days_16_mean",                 # Growing season length (days)
  "mean_AF_TA_sumdiff_16_mean_mean"  # Mean daily ΔTa over the growing season
)

# Create three subsets:
df_sum <- df                           # All sites
df_cXa <- df[df$Clim == "Cfa", ]       # Warm-humid temperate sites
df_DXb <- df[df$Clim == "Dfb", ]       # Cold-humid temperate sites

# Function to compute mean and 0.15*SD for each target variable
calculate_mean_sd <- function(data, class_label) {
  mean_values <- round(sapply(data[, target_columns], mean, na.rm = TRUE), 2)
  sd_015_values <- round(sapply(data[, target_columns], function(x) 0.15 * sd(x, na.rm = TRUE)), 2)
  
  # Return summarized data frame with class label
  result <- data.frame(
    Variable = target_columns,
    Mean = mean_values,
    SD_015 = sd_015_values,
    Class = class_label
  )
  
  return(result)
}

# Apply summary function to each subset
df_sum_stats  <- calculate_mean_sd(df_sum, "df_sum")   # All AmeriFlux sites
df_cXa_stats  <- calculate_mean_sd(df_cXa, "df_cXa")   # Cfa climate zone
df_DXb_stats  <- calculate_mean_sd(df_DXb, "df_DXb")   # Dfb climate zone

# Combine all results into a final summary table
final_df <- rbind(df_sum_stats, df_cXa_stats, df_DXb_stats)

# Output the final summary statistics
print(final_df)


# > print(final_df)
# Variable    Mean SD_015  Class
# AF_TA_average_diff_1_mean              AF_TA_average_diff_1_mean    0.69   0.31 df_sum
# AF_TA_average_diff_6_mean              AF_TA_average_diff_6_mean    0.23   0.25 df_sum
# AF_TA_sum_diff_16_mean                    AF_TA_sum_diff_16_mean -186.49  77.50 df_sum
# days_16_mean                                        days_16_mean  183.95   5.07 df_sum
# mean_AF_TA_sumdiff_16_mean_mean  mean_AF_TA_sumdiff_16_mean_mean   -0.76   0.36 df_sum
# AF_TA_average_diff_1_mean1             AF_TA_average_diff_1_mean    1.53   0.36 df_cXa
# AF_TA_average_diff_6_mean1             AF_TA_average_diff_6_mean   -0.83   0.22 df_cXa
# AF_TA_sum_diff_16_mean1                   AF_TA_sum_diff_16_mean -351.16 102.51 df_cXa
# days_16_mean1                                       days_16_mean  214.59   2.72 df_cXa
# mean_AF_TA_sumdiff_16_mean_mean1 mean_AF_TA_sumdiff_16_mean_mean   -1.48   0.45 df_cXa
# AF_TA_average_diff_1_mean2             AF_TA_average_diff_1_mean   -0.06   0.23 df_DXb
# AF_TA_average_diff_6_mean2             AF_TA_average_diff_6_mean    1.17   0.19 df_DXb
# AF_TA_sum_diff_16_mean2                   AF_TA_sum_diff_16_mean  -40.11  40.81 df_DXb
# days_16_mean2                                       days_16_mean  156.71   2.29 df_DXb
# mean_AF_TA_sumdiff_16_mean_mean2 mean_AF_TA_sumdiff_16_mean_mean   -0.11   0.24 df_DXb