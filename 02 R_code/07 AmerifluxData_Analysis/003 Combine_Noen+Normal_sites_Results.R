###### 0. Load Packages ####

library(terra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###################################   01 Merge Results for Sites and Data Integration ####################################
# Purpose of this section: The goal of this section is to merge the data from Noen and Normal sites, 
# filter out missing values, and analyze data for 23 valid sites (with at least one year of data between 2013-2021).
# Detailed steps: Load and clean data；Combine the Noen and Normal site data；Filter out rows with missing values；
# Group the data by site and year, then calculate summary statistics (mean and standard deviation) for each site.
# Save the results to CSV files for further analysis.


df1_noen <- read.csv("./AmerifluxData_Analysis/Noen_results.csv")
df2_norm <-  read.csv("./AmerifluxData_Analysis/Normal_results.csv")

########### Remove rows with missing values after SOS and EOS
df1_noen <- df1_noen %>%
  filter(!is.na(average_diff_21) & 
           # !is.na(average_diff_22) & 
           # !is.na(average_diff_23) & 
           # !is.na(average_diff_24) & 
           # !is.na(average_diff_25) & 
           !is.na(average_diff_26))
df2_norm <- df2_norm %>%
  filter(!is.na(average_diff_21) & 
           !is.na(average_diff_26))

########### Combine Noen and Normal sites

# Add a new column 'site_type' to differentiate between sites
df1_noen <- df1_noen %>% mutate(site_type = "noen")
df2_norm <- df2_norm %>% mutate(site_type = "norm")

# Combine the two dataframes
combined_df_1330 <- bind_rows(df1_noen, df2_norm)

# View the result after merging
head(combined_df_1330)

# Calculate the number of unique sites
unique_sites <- unique(combined_df_1330$site_id)  # Replace with actual site column name
num_unique_sites <- length(unique_sites)
cat("After removing rows with missing values, there are", num_unique_sites, "unique sites.\n")  # 23 sites

write.csv(combined_df_1330, file = "./AmerifluxData_Analysis/1330_Noen+Normal_results.csv", row.names = FALSE)
aa = read.csv( "D:/01Rawdata/AmerifluxData_Analysis/1330_Noen&Normal_Results.csv")


###################################   (√)01-1 Select Sites with Annual Data for 2013-2021 ####################################
# 23 sites

# Count the number of years for each site and add 'site_type'
site_year_counts <- combined_df_1330 %>%
  group_by(site_id, site_type) %>%
  summarise(year_count = n_distinct(Year), .groups = 'drop')

# Filter sites that have data for at least one year
sites_with_one_years <- site_year_counts %>%
  filter(year_count >= 1)

# Get the site IDs that have at least three years of data
valid_sites <- sites_with_one_years$site_id

# Filter the data for valid sites and calculate the annual mean values, while keeping site_type and year_count
average_values <- combined_df_1330 %>%
  filter(site_id %in% valid_sites) %>%
  group_by(site_id, site_type) %>%
  summarise(
    year_count = n_distinct(Year),  # Recalculate the number of years
    RMSE_1_mean = round(mean(RMSE_1, na.rm = TRUE), 2),
    RMSE_1_sd = round(0.15*sd(RMSE_1, na.rm = TRUE), 2),
    R_1_mean = round(mean(R_1, na.rm = TRUE), 2),
    R_1_sd = round(0.15*sd(R_1, na.rm = TRUE), 2),
    RMSE_2_mean = round(mean(RMSE_2, na.rm = TRUE), 2),
    RMSE_2_sd = round(0.15*sd(RMSE_2, na.rm = TRUE), 2),
    R_2_mean = round(mean(R_2, na.rm = TRUE), 2),
    R_2_sd = round(0.15*sd(R_2, na.rm = TRUE), 2),
    days_16_mean = ceiling(mean(days_16, na.rm = TRUE)),
    days_16_sd = round(0.15*sd(days_16, na.rm = TRUE), 2),
    average_diff_21_mean = round(mean(average_diff_21, na.rm = TRUE), 2),
    average_diff_21_sd = round(0.15*sd(average_diff_21, na.rm = TRUE), 2),
    average_diff_26_mean = round(mean(average_diff_26, na.rm = TRUE), 2),
    average_diff_26_sd = round(0.15*sd(average_diff_26, na.rm = TRUE), 2),
    sum_Diff_16_mean = round(mean(sum_Diff_16, na.rm = TRUE), 2),
    sum_Diff_16_sd = round(0.15*sd(sum_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_mean = round(mean(mean_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_sd = round(0.15*sd(mean_Diff_16, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# Output the results
print(average_values)

write.csv(average_values, file = "./AmerifluxData_Analysis/1330_Noen+Normal_Results_23.csv", row.names = T)
