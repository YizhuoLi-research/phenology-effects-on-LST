###### 0. Load Required Packages ######

library(terra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###################################   01. Merge site-level results and consolidate information  ####################################

########### Load input data with site-year statistics
df_sites <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_years.csv")

########### Remove rows with NA values in SOS- or EOS-based variables
df_sites <- df_sites %>%
  filter(!is.na(AF_5meanTA_post_SOS) & 
           !is.na(AF_5meanTA_post_EOS))

# Count number of unique sites
unique_sites <- unique(df_sites$site_id)  # Replace with actual site ID column if different
num_unique_sites <- length(unique_sites)
cat("After removing NAs, there are", num_unique_sites, "unique sites in the dataset.\n")  # 17 sites expected


###################################   01-1. Select sites with complete data from 2013 to 2021   ####################################

# Filter sites with complete years and calculate mean ± 15%SD for key variables
average_values <- df_sites %>%
  group_by(site_id) %>%
  summarise(
    year_count = n_distinct(Year),  # Recalculate number of years per site
    
    ME_1_AF_TA_mean  = round(mean(ME_1_AF_TA , na.rm = TRUE), 2),
    ME_1_AF_TA_sd = round(0.15 * sd(ME_1_AF_TA , na.rm = TRUE), 2),
    
    R_1_AF_TA_mean = round(mean(R_1_AF_TA , na.rm = TRUE), 2),
    R_1_AF_TA_sd = round(0.15 * sd(R_1_AF_TA , na.rm = TRUE), 2),
    
    ME_2_AF_TA_mean = round(mean(ME_2_AF_TA , na.rm = TRUE), 2),
    ME_2_AF_TA_sd = round(0.15 * sd(ME_2_AF_TA , na.rm = TRUE), 2),
    
    R_2_AF_TA_mean = round(mean(R_2_AF_TA, na.rm = TRUE), 2),
    R_2_AF_TA_sd = round(0.15 * sd(R_2_AF_TA, na.rm = TRUE), 2),
    
    AF_TA_average_diff_1_mean = round(mean(AF_TA_average_diff_1 , na.rm = TRUE), 2),
    AF_TA_average_diff_1_sd = round(0.15 * sd(AF_TA_average_diff_1 , na.rm = TRUE), 2),
    
    AF_TA_average_diff_6_mean = round(mean(AF_TA_average_diff_6 , na.rm = TRUE), 2),
    AF_TA_average_diff_6_sd = round(0.15 * sd(AF_TA_average_diff_6 , na.rm = TRUE), 2),
    
    AF_TA_sum_diff_16_mean = round(mean(AF_TA_sum_diff_16 , na.rm = TRUE), 2),
    AF_TA_sum_diff_16_sd = round(0.15 * sd(AF_TA_sum_diff_16 , na.rm = TRUE), 2),
    
    AF_LST_sum_diff_16_mean = round(mean(AF_LST_sum_diff_16 , na.rm = TRUE), 2),
    AF_LST_sum_diff_16_sd = round(0.15 * sd(AF_LST_sum_diff_16 , na.rm = TRUE), 2),
    
    mean_AF_TA_sumdiff_16_mean_mean = round(mean(mean_AF_TA_sumdiff_16_mean , na.rm = TRUE), 2),
    mean_AF_TA_sumdiff_16_mean_sd = round(0.15 * sd(mean_AF_TA_sumdiff_16_mean , na.rm = TRUE), 2),
    
    days_16_mean = round(mean(days_16 , na.rm = TRUE), 2),
    days_16_sd = round(0.15 * sd(days_16 , na.rm = TRUE), 2),
    
    .groups = 'drop'
  )

# Output summary results
print(average_values)

# Save to CSV
write.csv(average_values, file = "./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff.csv", row.names = TRUE)
