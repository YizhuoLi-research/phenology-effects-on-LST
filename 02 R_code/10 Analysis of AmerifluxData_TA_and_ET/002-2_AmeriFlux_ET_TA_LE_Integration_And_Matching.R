###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

# Set working directory
setwd("D:/VegetationImpact")

##############################################################
### 01. Integrate AmeriFlux TA and LE data, filter for 13:30 observations ###
##############################################################

# Define folder path and load all site files
folder_path <- "D:/VegetationImpact/01 Download/05 AmerifluxData/AmeriFlux_TA_LE/"
file_list <- list.files(path = folder_path, pattern = "^AMF_US.*\\.csv$", full.names = TRUE)

# Sites with special TA or LE column handling
special_TA_sites <- c("US-SSH", "US-UMB", "US-UMd")  # Use TA_1_1_1 if available
special_LE_site <- "US-WCr"  # Use LE_1_1_1 if available

# Initialize a list to store each site's data
site_data_list <- list()

# Loop over each site file
for (file_path in file_list) {
  file_name <- basename(file_path)
  site_id <- substr(file_name, 5, 10)
  cat("Processing site:", site_id, "\n")
  
  # Read data (skip metadata rows)
  df <- read.csv(file_path, skip = 2)
  
  # Add site ID column
  df$Site_ID <- site_id
  
  # Replace TA and LE columns for specific cases (before filtering!)
  if (!(site_id %in% special_TA_sites) && "TA_1_1_1" %in% names(df)) {
    df$TA <- df$TA_1_1_1
  }
  if (site_id == special_LE_site && "LE_1_1_1" %in% names(df)) {
    df$LE <- df$LE_1_1_1
  }
  
  # Filter for records at 13:30 (local time)
  df <- df %>%
    filter(substr(as.character(TIMESTAMP_START), nchar(as.character(TIMESTAMP_START)) - 3, nchar(as.character(TIMESTAMP_START))) == "1330") %>%
    mutate(
      date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
      year = year(date),
      DOY = yday(date)
    ) %>%
    filter(year %in% 2013:2021)  # Keep only target years
  
  # Append to list
  if (site_id %in% names(site_data_list)) {
    site_data_list[[site_id]] <- bind_rows(site_data_list[[site_id]], df)
  } else {
    site_data_list[[site_id]] <- df
  }
}

# Combine all site data into a single data frame
AmeriFlux_TA_LE_all <- bind_rows(site_data_list)

# Preview the result
head(AmeriFlux_TA_LE_all)
colnames(AmeriFlux_TA_LE_all)

# Keep only relevant columns
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  dplyr::select(Site_ID, date, year, DOY, TA, LE)

# Replace -9999 with NA
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  mutate_at(vars(TA, LE), ~na_if(., -9999))

# Calculate ET (Evapotranspiration in mm/day)
# LE is in W/m², using latent heat of vaporization λ = 2.45 * 10^6 J/kg
lambda <- 2.45e6  # J/kg
AmeriFlux_TA_LE_all$ET <-  AmeriFlux_TA_LE_all$LE / lambda * 86400  # 86400 = seconds per day

# Final cleanup: replace -9999 with NA again (in case) and rename variables
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  mutate_at(vars(TA, LE, ET), ~na_if(., -9999)) %>%
  rename(SiteID = Site_ID,
         TA_download = TA,
         LE_download = LE,
         ET_calculated = ET)

# Check column names
colnames(AmeriFlux_TA_LE_all)

# Save processed dataset with TA/LE/ET values
write.csv(
  AmeriFlux_TA_LE_all,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv",
  row.names = FALSE
)


#################################################################
### 02. Compare downloaded TA (from AmeriFlux) with processed TA ###
#################################################################

# Load TA dataset containing manually extracted AmeriFlux and MODIS LST data
# This includes AF_TA, AF_LST, MOD_LST_smoothed
TA_Pro <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv")

# Reload the downloaded and processed AmeriFlux TA/LE/ET dataset
AmeriFlux_TA_LE_download <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv")

# Check column consistency
colnames(AmeriFlux_TA_LE_all)
colnames(TA_Pro)

# Merge processed_data with downloaded data by SiteID, year, DOY
# This aligns ground measurements (AF_TA, AF_LST) with downloaded tower data (TA_download, LE_download, ET_calculated)
merge_df <- merge(
  TA_Pro,
  AmeriFlux_TA_LE_all[, c("SiteID", "year", "DOY", "TA_download", "LE_download", "ET_calculated")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Save the merged result for further analysis
write.csv(
  merge_df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--2.AmeriFlux_All_TA_ET_variables.csv",
  row.names = FALSE
)
