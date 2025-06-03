###### 0. Load packages ####
library(terra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")

################################### 01. Merge site results and integrate information ####################################
########### 13:30
df1_noen <- read.csv("./AmerifluxData_Analysis/Noen_results.csv")
df2_norm <- read.csv("./AmerifluxData_Analysis/Normal_results.csv")

########### Remove rows with missing SOS or EOS values
df1_noen <- df1_noen %>%
  filter(!is.na(average_diff_21) & 
           !is.na(average_diff_26))
df2_norm <- df2_norm %>%
  filter(!is.na(average_diff_21) & 
           !is.na(average_diff_26))

########### Combine Noen and Normal site data

# Add site_type column
df1_noen <- df1_noen %>% mutate(site_type = "noen")
df2_norm <- df2_norm %>% mutate(site_type = "norm")

# Merge both dataframes
combined_df_1330 <- bind_rows(df1_noen, df2_norm)

# View merged results
head(combined_df_1330)

# Count number of unique sites
unique_sites <- unique(combined_df_1330$site_id)  # Replace with actual site ID column name
num_unique_sites <- length(unique_sites)
cat("After removing NA values, there are", num_unique_sites, "unique sites.\n")  # 23 sites

write.csv(combined_df_1330, file = "./AmerifluxData_Analysis/1330_Noen+Normal_results.csv", row.names = FALSE)
aa <- read.csv("D:/01Rawdata/AmerifluxData_Analysis/1330_Noen&Normal_Results.csv")

################################### (√) 01-1. Select sites with full-year data from 2013–2021 ####################################
# 23 sites

# Count the number of years per site, keeping site_type info
site_year_counts <- combined_df_1330 %>%
  group_by(site_id, site_type) %>%
  summarise(year_count = n_distinct(Year), .groups = 'drop')

# Filter sites with at least 1 year of data
sites_with_one_years <- site_year_counts %>%
  filter(year_count >= 1)

# Get IDs of valid sites (≥ 1 year of data)
valid_sites <- sites_with_one_years$site_id

# Filter valid sites and compute annual mean and variability for key metrics
average_values <- combined_df_1330 %>%
  filter(site_id %in% valid_sites) %>%
  group_by(site_id, site_type) %>%
  summarise(
    year_count = n_distinct(Year),
    ME_1_mean = round(mean(ME_1, na.rm = TRUE), 2),
    ME_1_sd = round(0.15 * sd(ME_1, na.rm = TRUE), 2),
    R_1_mean = round(mean(R_1, na.rm = TRUE), 2),
    R_1_sd = round(0.15 * sd(R_1, na.rm = TRUE), 2),
    ME_2_mean = round(mean(ME_2, na.rm = TRUE), 2),
    ME_2_sd = round(0.15 * sd(ME_2, na.rm = TRUE), 2),
    R_2_mean = round(mean(R_2, na.rm = TRUE), 2),
    R_2_sd = round(0.15 * sd(R_2, na.rm = TRUE), 2),
    days_16_mean = ceiling(mean(days_16, na.rm = TRUE)),
    days_16_sd = round(0.15 * sd(days_16, na.rm = TRUE), 2),
    average_diff_21_mean = round(mean(average_diff_21, na.rm = TRUE), 2),
    average_diff_21_sd = round(0.15 * sd(average_diff_21, na.rm = TRUE), 2),
    average_diff_26_mean = round(mean(average_diff_26, na.rm = TRUE), 2),
    average_diff_26_sd = round(0.15 * sd(average_diff_26, na.rm = TRUE), 2),
    sum_Diff_16_mean = round(mean(sum_Diff_16, na.rm = TRUE), 2),
    sum_Diff_16_sd = round(0.15 * sd(sum_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_mean = round(mean(mean_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_sd = round(0.15 * sd(mean_Diff_16, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# Output result
print(average_values)

# Save result to CSV
write.csv(average_values, file = "./AmerifluxData_Analysis/1330_Noen+Normal_Results_23.csv", row.names = TRUE)
