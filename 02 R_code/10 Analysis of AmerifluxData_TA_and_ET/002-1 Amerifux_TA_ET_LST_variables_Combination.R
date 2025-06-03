###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")

##############################################
### 01. Load and Combine AmeriFlux TA and LE Data, Filter for 13:30 Timepoint ###
##############################################

# Set the folder containing TA and LE data
folder_path <- "D:/VegetationImpact/01 Download/05 AmerifluxData/AmeriFlux_TA_LE/"
file_list <- list.files(path = folder_path, pattern = "^AMF_US.*\\.csv$", full.names = TRUE)

# Special cases for specific TA and LE variable columns
special_TA_sites <- c("US-SSH", "US-UMB", "US-UMd")  # These use different TA column names
special_LE_site <- "US-WCr"  # This site uses a different LE column name

# Create an empty list to store each site's processed data
site_data_list <- list()

# Loop through all site files
for (file_path in file_list) {
  file_name <- basename(file_path)
  site_id <- substr(file_name, 5, 10)
  cat("Processing site:", site_id, "\n")
  
  # Read the file (skipping the first two lines)
  df <- read.csv(file_path, skip = 2)
  df$Site_ID <- site_id
  
  # Replace TA and LE columns with appropriate alternatives if needed
  if (!(site_id %in% special_TA_sites) && "TA_1_1_1" %in% names(df)) {
    df$TA <- df$TA_1_1_1
  }
  if (site_id == special_LE_site && "LE_1_1_1" %in% names(df)) {
    df$LE <- df$LE_1_1_1
  }
  
  # Filter to only include records from 13:30 (local solar time)
  df <- df %>%
    filter(substr(as.character(TIMESTAMP_START), nchar(as.character(TIMESTAMP_START)) - 3, nchar(as.character(TIMESTAMP_START))) == "1330") %>%
    mutate(
      date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
      year = year(date),
      DOY = yday(date)
    ) %>%
    filter(year %in% 2013:2021)  # Keep only years from 2013 to 2021
  
  # Append the filtered data to the site-specific list
  if (site_id %in% names(site_data_list)) {
    site_data_list[[site_id]] <- bind_rows(site_data_list[[site_id]], df)
  } else {
    site_data_list[[site_id]] <- df
  }
}

# Combine data from all sites into a single data frame
AmeriFlux_TA_LE_all <- bind_rows(site_data_list)

# Keep only selected columns for further analysis
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  dplyr::select(Site_ID, date, year, DOY, TA, LE)

# Replace missing values (-9999) with NA
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  mutate_at(vars(TA, LE), ~na_if(., -9999))

# Convert LE (latent heat flux in W/m²) to ET (evapotranspiration in mm/day)
lambda <- 2.45e6  # Latent heat of vaporization in J/kg
AmeriFlux_TA_LE_all$ET <- AmeriFlux_TA_LE_all$LE / lambda * 86400  # 86400 = seconds per day

# Rename and clean up column names
AmeriFlux_TA_LE_all <- AmeriFlux_TA_LE_all %>%
  mutate_at(vars(TA, LE, ET), ~na_if(., -9999)) %>%
  rename(SiteID = Site_ID,
         TA_download = TA,
         LE_download = LE,
         ET_calculated = ET)

# Save the filtered and processed dataset
write.csv(
  AmeriFlux_TA_LE_all,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv",
  row.names = FALSE
)


##################################################
### 02. Compare Downloaded TA vs. Manually Extracted TA (AF_TA) ###
##################################################

# Load reference data with AF_TA (from previous merging)
TA_Jen <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv")

# Load newly computed ET and downloaded TA/LE dataset
AmeriFlux_TA_LE_download <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv")

# Merge TA/LE/ET data with reference AF_TA, AF_LST, MOD_LST_smoothed using SiteID, year, and DOY
merge_df <- merge(
  AmeriFlux_TA_LE_download,
  TA_Jen[, c("SiteID", "year", "DOY", "AF_TA", "AF_LST", "MOD_LST_smoothed")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Save the merged dataset with all variables
write.csv(
  merge_df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--2.AmeriFlux_All_TA_ET_variables.csv",
  row.names = FALSE
)
