###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

# Set working directory
setwd("D:/VegetationImpact")

###################   01 Process MODIS Site-Level LST Data with Cloud Flag  ###########################

# Define the folder containing MODIS LST files with cloud flag info
folder_path <- "D:/VegetationImpact/01 Download/05 AmerifluxData/Get_AmeriFlux_Cloud_flag/"
file_list <- list.files(folder_path, full.names = TRUE)

# Initialize a list to group data by site
site_data_list <- list()

# Loop through each file and extract year and site ID
for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Extract year and site ID from filename (e.g., "2015_US-MOz.csv")
  year <- substr(file_name, 1, 4)
  site_id <- substr(file_name, 6, 11)
  
  # Read the file
  df <- read.csv(file_path)
  
  # Add columns for site ID and year
  df$SiteID <- site_id
  df$year <- year
  
  # Merge into site-specific data frame in the list
  if (site_id %in% names(site_data_list)) {
    site_data_list[[site_id]] <- bind_rows(site_data_list[[site_id]], df)
  } else {
    site_data_list[[site_id]] <- df
  }
}

# Combine all site data into one large data frame
MOD_Cloud_all <- bind_rows(site_data_list)
head(MOD_Cloud_all)

# Remove unnecessary columns: "system.index" and ".geo"
MOD_Cloud_all_clean <- MOD_Cloud_all %>%
  select(-system.index, -.geo)

# Create a complete DOY list from 1 to 365
all_doy <- data.frame(DOY = 1:365)

######### Fill in missing DOY records to ensure completeness (i.e., ensure every year has DOY 1–365) #########
MOD_Cloud_all_complete <- MOD_Cloud_all_clean %>%
  complete(SiteID, year, DOY = 1:365, fill = list(LST = NA)) %>%
  arrange(SiteID, year, DOY)

# Preview the processed result
head(MOD_Cloud_all_complete)

# Export the cleaned and completed data to CSV
write.csv(
  MOD_Cloud_all_complete,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud--Modis_for_siteCloud.csv",
  row.names = FALSE
)


###################  02 Merge AmeriFlux (TA LST - unsmoothed) and MODIS (LST - smoothed) Data  ###################

# Read AmeriFlux and MODIS LST datasets
AmeriFlux_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv")
MOD_Cloud_all <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud--Modis_for_siteCloud.csv")

# Merge MODIS and AmeriFlux LST data by site, year, and DOY
AF_merged_LST_cloud <- merge(
  MOD_Cloud_all,
  AmeriFlux_TA_LST[, c("SiteID", "year", "DOY", "AF_LST")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Rename AF_LST column to "LST" for consistency
colnames(AF_merged_LST_cloud)[colnames(AF_merged_LST_cloud) == "AF_LST"] <- "LST"
head(AF_merged_LST_cloud)

# Apply cloud mask: Set LST to NA when Cloud flag is 1
AF_merged_LST_cloud$LST_QC <- ifelse(AF_merged_LST_cloud$Cloud == 1, NA, AF_merged_LST_cloud$LST)

# Check example: number of valid LST values with and without cloud filter for a site-year
AF_merged_LST_cloud %>%
  filter(SiteID == "US-SSH", year == 2019) %>%
  summarise(
    n_LST = sum(!is.na(LST)),
    n_LST_QC = sum(!is.na(LST_QC))
  )

# Save merged LST and cloud-flagged data
write.csv(
  AF_merged_LST_cloud,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud--AF_merged_LST_Cloud.csv",
  row.names = FALSE
)


###################  03 Fit and Calculate Mean LST Post-SOS and EOS, and Temperature Impacts  ###################

AF_merged_LST_cloud <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud--AF_merged_LST_Cloud.csv")

# Separate dataframes with and without cloud filtering
df_LST <- AF_merged_LST_cloud[, !(names(AF_merged_LST_cloud) %in% c("Cloud", "LST_QC"))]
df_LST_QC <- AF_merged_LST_cloud[, !(names(AF_merged_LST_cloud) %in% c("Cloud", "LST"))]

# Load phenology transition dates (SOS and EOS) for each site-year
AF_PHE_DOY <- read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

# Initialize results dataframe
results <- data.frame(
  site_id = character(),
  Year = numeric(),
  LST_average_diff_1 = numeric(),
  LST_average_diff_6 = numeric(),
  LST_QC_average_diff_1 = numeric(),
  LST_QC_average_diff_6 = numeric(),
  LST_5mean_post_SOS = numeric(),
  LST_5mean_post_EOS = numeric(),
  LST_QC_5mean_post_SOS = numeric(),
  LST_QC_5mean_post_EOS = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
site_ids <- unique(df_LST$SiteID)

for (site_id in site_ids) {
  
  df_LST_site <- df_LST[df_LST$SiteID == site_id, ]
  df_LST_site$LST <- as.numeric(df_LST_site$LST)
  
  df_LST_QC_site <- df_LST_QC[df_LST_QC$SiteID == site_id, ]
  df_LST_QC_site$LST_QC <- as.numeric(df_LST_QC_site$LST_QC)
  
  AF_PHE_DOY_site <- AF_PHE_DOY[AF_PHE_DOY$SiteID == site_id, ]
  
  years <- unique(df_LST_site$year)
  
  for (year in years) {
    
    df_LST_site_year <- df_LST_site[df_LST_site$year == year, ]
    df_LST_QC_site_year <- df_LST_QC_site[df_LST_QC_site$year == year, ]
    
    AF_PHE_DOY_site_year <- AF_PHE_DOY_site[AF_PHE_DOY_site$year == year, ]
    
    # Skip if all LST or LST_QC values are NA
    if (all(is.na(df_LST_site_year$LST)) | all(is.na(df_LST_QC_site_year$LST_QC))) {
      cat("Skipping site:", site_id, "year:", year, "- All temperature values are NA.\n")
      next
    }
    
    # Skip if phenology dates are missing for this site-year
    if (nrow(AF_PHE_DOY_site_year) == 0) {
      cat("Skipping site:", site_id, "year:", year, "- No phenology DOY data available.\n")
      next
    }

    # ------------------------ 5-day rolling average ------------------------
    
    ## 1. For df_LST_site_year (raw LST)
    # Step 1: Create a full DOY sequence from 1 to 365 and left join to fill missing DOYs
    df_full_LST_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_LST_site_year, by = "DOY")
    
    # Step 2: Calculate 5-day rolling average (center-aligned), allowing NA
    df_LST_site_year <- df_full_LST_site_year %>%
      mutate(
        LST = as.numeric(LST),  # Ensure numeric type
        LST_smoothed = rollapply(LST, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                 fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%  # Remove edges with incomplete window
      dplyr::select(DOY, LST = LST_smoothed)
    
    ## 2. For df_LST_QC_site_year (cloud-filtered LST)
    df_full_LST_QC_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_LST_QC_site_year, by = "DOY")
    
    df_LST_QC_site_year <- df_full_LST_QC_site_year %>%
      mutate(
        LST_QC = as.numeric(LST_QC),  # Ensure numeric type
        LST_QC_smoothed = rollapply(LST_QC, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                    fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, LST_QC = LST_QC_smoothed)
    
    # ------------------------ Extract phenological DOY values for the site-year ------------------------
    
    doy_values_site_year <- AF_PHE_DOY_site_year %>%
      filter(SiteID == site_id & year == year) %>%
      dplyr::select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
                    falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
      unlist()
    
    # Extract individual phenological transition days
    day_1 <- as.numeric(doy_values_site_year["rising_DOY_10"])
    day_2 <- as.numeric(doy_values_site_year["rising_DOY_25"])
    day_3 <- as.numeric(doy_values_site_year["rising_DOY_50"])
    day_4 <- as.numeric(doy_values_site_year["falling_DOY_50"])
    day_5 <- as.numeric(doy_values_site_year["falling_DOY_25"])
    day_6 <- as.numeric(doy_values_site_year["falling_DOY_10"])
    
    # ------------------------ First ATC model fit (entire year) ------------------------
    
    # Define initial parameter values and boundaries for model fitting
    start_1 <- list(aa = 20, bb = 20, cc = -0.5 * pi)  # For LST
    start_2 <- list(ee = 20, ff = 20, gg = -0.5 * pi)  # For LST_QC
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    
    # 1. Fit sinusoidal model for LST (no cloud mask)
    atc_LST <- nlsLM(LST ~ aa + bb * sin(2 * pi * DOY / 365 + cc), data = df_LST_site_year,
                     start = start_1, lower = lower, upper = upper,
                     algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    
    # Extract fitted parameters
    aa <- as.numeric(coef(atc_LST)[1])
    bb <- as.numeric(coef(atc_LST)[2])
    cc <- as.numeric(coef(atc_LST)[3])
    
    # Define fitted function for reuse
    fit_1_atc_LST <- function(DOY, aa, bb, cc) {
      aa + bb * sin(2 * pi * DOY / 365 + cc)
    }
    
    # 2. Fit sinusoidal model for LST_QC (cloud-filtered)
    atc_LST_QC <- nlsLM(LST_QC ~ ee + ff * sin(2 * pi * DOY / 365 + gg), data = df_LST_QC_site_year,
                        start = start_2, lower = lower, upper = upper,
                        algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    
    # Extract fitted parameters
    ee <- as.numeric(coef(atc_LST_QC)[1])
    ff <- as.numeric(coef(atc_LST_QC)[2])
    gg <- as.numeric(coef(atc_LST_QC)[3])
    
    # Define fitted function for reuse
    fit_1_atc_LST_QC <- function(DOY, ee, ff, gg) {
      ee + ff * sin(2 * pi * DOY / 365 + gg)
    }
    
    # ------------------------ Second fit continues below... ------------------------
    
    # ------------------------ Second fit: exclude growing season ------------------------
    
    # Remove growing season days (between SOS and EOS) by setting LST values to NA
    df2_LST <- df_LST_site_year
    df2_LST$LST[df2_LST$DOY > day_1 & df2_LST$DOY < day_6] <- NA
    
    df2_LST_QC <- df_LST_QC_site_year
    df2_LST_QC$LST_QC[df2_LST_QC$DOY > day_1 & df2_LST_QC$DOY < day_6] <- NA
    
    # Skip this site-year if all non-growing season values are NA
    if (all(is.na(df2_LST$LST)) | all(is.na(df2_LST_QC$LST_QC))) {
      cat("Skipped site:", site_id, "year:", year, "due to all non-growing season values being NA.\n")
    } else {
      
      # ------------------------ Second sinusoidal fit: exclude growing season ------------------------
      
      # Initial parameters for second ATC model fit
      start2_1 <- list(aa2 = 20, bb2 = 20)    # For LST
      start2_2 <- list(ee2 = 20, ff2 = 20)    # For LST_QC
      
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      
      # Fit second ATC model for LST (non-growing season only, cc from first fit is fixed)
      atc2_LST <- nlsLM(LST ~ aa2 + bb2 * sin(2 * pi * DOY / 365 + cc), data = df2_LST,
                        start = start2_1, lower = lower2, upper = upper2,
                        algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      aa2 <- as.numeric(coef(atc2_LST)[1])
      bb2 <- as.numeric(coef(atc2_LST)[2])
      fit_2_atc_LST <- function(DOY, aa2, bb2, cc) { aa2 + bb2 * sin(2 * pi * DOY / 365 + cc) }
      
      # Fit second ATC model for LST_QC
      atc2_LST_QC <- nlsLM(LST_QC ~ ee2 + ff2 * sin(2 * pi * DOY / 365 + gg), data = df2_LST_QC,
                           start = start2_2, lower = lower2, upper = upper2,
                           algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      ee2 <- as.numeric(coef(atc2_LST_QC)[1])
      ff2 <- as.numeric(coef(atc2_LST_QC)[2])
      fit_2_atc_LST_QC <- function(DOY, ee2, ff2, gg) { ee2 + ff2 * sin(2 * pi * DOY / 365 + gg) }
      
      # ------------------------ Calculate residuals (actual - fitted) ------------------------
      
      # Use second fit to get modeled values over the full year
      df_LST_site_year$fit_2_LST <- fit_2_atc_LST(df_LST_site_year$DOY, aa2, bb2, cc)
      df_LST_QC_site_year$fit_2_LST_QC <- fit_2_atc_LST_QC(df_LST_QC_site_year$DOY, ee2, ff2, gg)
      
      # Calculate residuals (actual LST - modeled non-growing-season fit)
      df_LST_site_year$LST_diff <- df_LST_site_year$LST - df_LST_site_year$fit_2_LST
      df_LST_QC_site_year$LST_QC_diff <- df_LST_QC_site_year$LST_QC - df_LST_QC_site_year$fit_2_LST_QC
      
      # ------------------------ Calculate 5-day post-phenophase means and impact ------------------------
      
      # Mean difference between actual and non-growing-season fit after SOS and EOS
      LST_average_diff_1 <- round(mean(df_LST_site_year$LST_diff[df_LST_site_year$DOY >= day_1 + 1 & df_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      LST_average_diff_6 <- round(mean(df_LST_site_year$LST_diff[df_LST_site_year$DOY >= day_6 + 1 & df_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      LST_QC_average_diff_1 <- round(mean(df_LST_QC_site_year$LST_QC_diff[df_LST_QC_site_year$DOY >= day_1 + 1 & df_LST_QC_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      LST_QC_average_diff_6 <- round(mean(df_LST_QC_site_year$LST_QC_diff[df_LST_QC_site_year$DOY >= day_6 + 1 & df_LST_QC_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # Mean LST values (actual, smoothed) in 5-day windows after SOS and EOS
      LST_5mean_post_SOS <- round(mean(df_LST_site_year$LST[df_LST_site_year$DOY >= day_1 + 1 & df_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      LST_5mean_post_EOS <- round(mean(df_LST_site_year$LST[df_LST_site_year$DOY >= day_6 + 1 & df_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      LST_QC_5mean_post_SOS <- round(mean(df_LST_QC_site_year$LST_QC[df_LST_QC_site_year$DOY >= day_1 + 1 & df_LST_QC_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      LST_QC_5mean_post_EOS <- round(mean(df_LST_QC_site_year$LST_QC[df_LST_QC_site_year$DOY >= day_6 + 1 & df_LST_QC_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # ------------------------ Save results ------------------------
      
      results <- rbind(results, data.frame(
        site_id, Year = year,
        LST_average_diff_1 = LST_average_diff_1, 
        LST_average_diff_6 = LST_average_diff_6,
        LST_QC_average_diff_1 = LST_QC_average_diff_1, 
        LST_QC_average_diff_6 = LST_QC_average_diff_6,
        LST_5mean_post_SOS = LST_5mean_post_SOS,
        LST_5mean_post_EOS = LST_5mean_post_EOS,
        LST_QC_5mean_post_SOS = LST_QC_5mean_post_SOS,
        LST_QC_5mean_post_EOS = LST_QC_5mean_post_EOS
      ))
      
    }
  }
}

write.csv(
  results,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud-LST-RESULTS_sites_years.csv",
  row.names = FALSE
)


