###### 0. Load Libraries ####

# Purpose: Loading necessary libraries for data manipulation, analysis, and visualization.
library(terra)
library(tidyverse)
library(dplyr)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")  # Set the working directory

###################################   01 Data Integration, Filter DB Vegetation Sites #################################

# Purpose: The following section integrates temperature observation data (from file1) and phenological observation data (from file2).
# The goal is to filter out the "DB" (Deciduous Broadleaf) vegetation sites and find the common sites between both datasets.

file1 <- "01 Download/05 AmerifluxData/LST_Ameriflux_Filtered.csv/LST_Ameriflux_Filtered.csv"  # Temperature observation data file
file2 <- "01 Download/05 AmerifluxData/TransitionDates.csv"  # Phenological observation data file

# Read CSV files
data1 <- read_csv(file1)  # Load the first file containing temperature data
data2 <- read_csv(file2)  # Load the second file containing phenological data

# Display the column names of the first file
header1 <- colnames(data1)
print("File 1 Columns:")
print(header1)

# Display the column names of the second file
header2 <- colnames(data2)
print("File 2 Columns:")
print(header2)

# Check the unique vegetation types in the second file (data2)
unique_veg_types <- unique(data2$veg_type)
print(unique_veg_types)

# Display the number of unique vegetation types
num_veg_types <- length(unique_veg_types)
print(num_veg_types)                 
# [1] "DB" "SH" "EN" "TN" "AG" "GR" "XX" "WL" "UN" "EB"                        
# Filter for "DB" (Deciduous Broadleaf) vegetation sites
data2_DB <- subset(data2, veg_type == "DB")  

# Get unique SiteIDs for "DB" vegetation sites
unique_SiteID <- unique(data2_DB$SiteID)
print(unique_SiteID)

# Find common SiteIDs between the two datasets
common_sites <- intersect(unique(data1$SiteID), unique(data2_DB$SiteID))  # Get common SiteIDs between data1 and data2_DB
# Output the common sites
print(common_sites)

# Calculate the number of common sites
num_common_sites <- length(common_sites)
print(num_common_sites)
# [1] "US-Ton" "US-xRN" "US-NC4" "US-xBL" "US-Slt" "US-SSH" "US-xLE" "US-Dk2" "US-xML"
# [10] "US-xUN" "US-xSC" "US-xSE" "US-xTR" "US-xHA" "US-xCL" "US-xBR" "US-MMS" "US-xUK"
# [19] "US-xST" "US-xDL" "US-xGR" "US-xJE" "US-UMB" "US-UMd" "US-MOz" "US-WCr" "US-Cwt"
# 27
# Subset data1 to include only common sites
data1_DB <- subset(data1, SiteID %in% common_sites)

# Filter out "XX" vegetation type data from data2
xx_data <- data2[data2$veg_type == "XX", ]

# Display the filtered data for "XX" vegetation type
print(xx_data)



###########################################    02 Convert PhenCam Dates to Day of Year (DOY) ################################################

# Purpose: This section processes the phenological data (from data2) by converting the date columns (transition_10, transition_25, and transition_50)
# into Day of Year (DOY) and filters the data for the years 2013 to 2021. It also handles duplicates and combines data for rising and falling phases.

# library(dplyr)
# library(lubridate)

# Filter rows where the year in the "transition_10" column is between 2013 and 2021
data2_DB_DOY <- data2_DB %>%
  filter(as.numeric(substr(transition_10, nchar(transition_10) - 1, nchar(transition_10))) %in% 13:21)

# Extract the year and prepend "20" to generate the full year in 20XX format
data2_DB_DOY <- data2_DB_DOY %>%
  mutate(year = paste0("20", substr(transition_10, nchar(transition_10) - 1, nchar(transition_10))))

# Convert to date format and calculate Day of Year (DOY) for the transition dates
data2_DB_DOY <- data2_DB_DOY %>%
  mutate(date_10 = mdy(transition_10),
         date_25 = mdy(transition_25),
         date_50 = mdy(transition_50),
         DOY_10 = yday(date_10),
         DOY_25 = yday(date_25),
         DOY_50 = yday(date_50))

# View the result
head(data2_DB_DOY)
header2 <- colnames(data2_DB_DOY)
print("File 2 Columns:")
print(header2)

# Select relevant columns for further processing
data2_DB_DOY_selected <- data2_DB_DOY[, c("SiteID", "site", "veg_type", "direction",
                                          "year", "DOY_10", "DOY_25", "DOY_50")]

# Check for duplicates and handle them
duplicates <- data2_DB_DOY_selected %>%
  group_by(SiteID, site, veg_type, year, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
print(duplicates)

# View the duplicate data for "US-Slt" site
data_US_Slt <- data2_DB_DOY_selected %>% 
  filter(SiteID == "US-Slt")
print(data_US_Slt)  

# In the original data, there are two entries for the "US-Slt" site in 2016 with the "rising" direction.
# We should select the one with the smaller DOY values that match the trend.
data2_DB_DOY_selected <- data2_DB_DOY_selected %>%
  group_by(SiteID, site, veg_type, year, direction) %>%
  filter(DOY_10 == min(DOY_10) &  DOY_25 == min(DOY_25) & DOY_50 == min(DOY_50)
  ) %>%
  ungroup()
print(data2_DB_DOY_selected)

# Convert the data to a wide format, combining rising and falling data for each year.
# Group and rename columns for rising and falling directions
rising_data <- data2_DB_DOY_selected %>%
  filter(direction == "rising") %>%
  rename(rising_DOY_10 = DOY_10, rising_DOY_25 = DOY_25, rising_DOY_50 = DOY_50) %>%
  dplyr::select(SiteID, site, veg_type, year, rising_DOY_10, rising_DOY_25, rising_DOY_50)

falling_data <- data2_DB_DOY_selected %>%
  filter(direction == "falling") %>%
  rename(falling_DOY_10 = DOY_10, falling_DOY_25 = DOY_25, falling_DOY_50 = DOY_50) %>%
  dplyr::select(SiteID, site, veg_type, year, falling_DOY_50, falling_DOY_25, falling_DOY_10)

# Merge the two datasets (rising and falling data)
combined_data <- full_join(rising_data, falling_data, by = c("SiteID", "site", "veg_type", "year"))
# Remove rows with missing phenological data (NA values)
data2_DB_DOY_combined <- na.omit(combined_data)

# Count the number of unique SiteIDs
num_sites <- data2_DB_DOY_combined %>%
  distinct(SiteID) %>%
  nrow()
# Output the number of unique sites                
print(num_sites)    # 27

# Save the processed data to a CSV file
if (!dir.exists("./AmerifluxData_Analysis")) {
  dir.create("./AmerifluxData_Analysis", recursive = TRUE)
}
write.csv(data2_DB_DOY_combined, "./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv", row.names = FALSE)



###########################################    03 Filter Data1-LST at 13:30 and Combine with PHE Data ################################################

# Purpose: This section filters data from `data1_DB` based on the timestamp for 13:30 and years between 2013 and 2021.
# It then extracts the date and Day of Year (DOY) for further analysis, and splits the data based on the presence of the "tau" column.

# Filter rows where TIMESTAMP_START ends with "1330" and the year is between 2013 and 2021
data1_DB_selected <- data1_DB %>%
  filter(
    substr(as.character(TIMESTAMP_START), nchar(as.character(TIMESTAMP_START)) - 3, nchar(as.character(TIMESTAMP_START))) == "1330"
  ) %>%
  # Extract year, date, and DOY
  mutate(
    date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
    year = year(date),
    DOY = yday(date)
  ) %>%
  # Keep only the rows where the year is between 2013 and 2021
  filter(year %in% 2013:2021)

# View the result
print(data1_DB_selected)

# Count the number of unique SiteIDs in the selected data
num_sites <- data1_DB_selected %>%
  distinct(SiteID) %>%
  nrow()
# Output the number of unique sites                23 sites
print(num_sites)

# Save the filtered data to a CSV file
# Save the complete data1_DB_selected to the specified path
write.csv(data1_DB_selected, "./AmerifluxData_Analysis/data1_DB_LST_selected_sum.csv", row.names = FALSE)



#################################    Split Analysis by "noen" and "non-noen" Sites, as Different Temperature Columns are Used #################################

# Save the rows where the "tau" column is not NA to the specified path
data1_DB_selected_noen <- data1_DB_selected %>% filter(!is.na(tau))
write.csv(data1_DB_selected_noen, "./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv", row.names = FALSE)

# Save the rows where the "tau" column is NA to the specified path
data1_DB_selected_normal <- data1_DB_selected %>% filter(is.na(tau))
write.csv(data1_DB_selected_normal, "./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv", row.names = FALSE)


