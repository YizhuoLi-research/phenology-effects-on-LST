###### 0. Load required packages ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")

################### 01. Integrate AmeriFlux TA and LST data for Noen and Normal stations ###################

AF_LST_noen <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv")
AF_LST_normal <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
files_TA <- "D:/VegetationImpact/01 Download/05 AmerifluxData/AmeriFlux_TA/TA_Data_Organized/"

# Define the list of target sites
site_list <- c("US-MOz", "US-SSH", "US-UMB", "US-UMd", "US-WCr", 
               "US-xBL", "US-xBR", "US-xGR", "US-xHA", "US-xLE", 
               "US-xML", "US-xRN", "US-xSC", "US-xSE", "US-xST", 
               "US-xUK", "US-xUN")

# Filter the datasets to only include the defined sites
AF_LST_noen_filtered <- AF_LST_noen[AF_LST_noen$SiteID %in% site_list, ]
num_sites <- length(unique(AF_LST_noen_filtered$SiteID))
# cat("Number of sites after filtering (Noen):", num_sites, "\n")  # 11

AF_LST_normal_filtered <- AF_LST_normal[AF_LST_normal$SiteID %in% site_list, ]
num_sites <- length(unique(AF_LST_normal_filtered$SiteID))
# cat("Number of sites after filtering (Normal):", num_sites, "\n")  # 6

# Convert to numeric and assign unified column names
AF_LST_noen_filtered$AF_TA <- as.numeric(AF_LST_noen_filtered$TA)
AF_LST_noen_filtered$AF_LST <- as.numeric(AF_LST_noen_filtered$tau_corrected_surfaceTemp_97)
AF_LST_noen_filtered$DOY <- as.numeric(AF_LST_noen_filtered$DOY)
AF_LST_noen_filtered$year <- as.numeric(AF_LST_noen_filtered$year)

AF_LST_normal_filtered$AF_TA <- as.numeric(AF_LST_normal_filtered$TA)
AF_LST_normal_filtered$AF_LST <- as.numeric(AF_LST_normal_filtered$corrected_surfaceTemp_97)
AF_LST_normal_filtered$DOY <- as.numeric(AF_LST_normal_filtered$DOY)
AF_LST_normal_filtered$year <- as.numeric(AF_LST_normal_filtered$year)

# Merge noen and normal datasets and select relevant columns
AmeriFlux_TA_LST <- bind_rows(AF_LST_noen_filtered, AF_LST_normal_filtered)

AmeriFlux_TA_LST <- AmeriFlux_TA_LST %>%
  dplyr::select(SiteID, TIMESTAMP_START, TIMESTAMP_END, date, year, DOY, AF_TA, AF_LST)

# Save the combined data
write.csv(
  AmeriFlux_TA_LST,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv",
  row.names = FALSE
)

################### 02. Process MODIS site-specific LST data ###################

folder_path <- "D:/VegetationImpact/01 Download/05 AmerifluxData/Get_AmeriFlux_MODIS_LST/"
file_list <- list.files(folder_path, full.names = TRUE)

# Initialize a list to store data by site
site_data_list <- list()

for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Extract year and site ID from file name
  year <- substr(file_name, 1, 4)
  site_id <- substr(file_name, 6, 11)
  
  # Read file
  df <- read.csv(file_path)
  
  # Add SiteID and year columns
  df$SiteID <- site_id
  df$year <- year
  
  # Append to the list, grouped by site
  if (site_id %in% names(site_data_list)) {
    site_data_list[[site_id]] <- bind_rows(site_data_list[[site_id]], df)
  } else {
    site_data_list[[site_id]] <- df
  }
}

# Combine all site data into one dataframe
MOD_LST_all <- bind_rows(site_data_list)
head(MOD_LST_all)

# Remove unnecessary columns: system.index and .geo
MOD_LST_all_clean <- MOD_LST_all %>%
  select(-system.index, -.geo)

# Create a full list of DOY from 1 to 365
all_doy <- data.frame(DOY = c(1:365))

# Complete missing DOY rows for each site and year (ensure DOY 1–365 are present)
MOD_LST_all_complete <- MOD_LST_all_clean %>%
  complete(SiteID, year, DOY = c(1:365), fill = list(LST = NA)) %>%
  arrange(SiteID, year, DOY)

# Preview the result
head(MOD_LST_all_complete)

# Save the cleaned MODIS LST dataset
write.csv(
  MOD_LST_all_complete,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--Modis_for_siteLST.csv",
  row.names = FALSE
)



################### 03. Merge AmeriFlux (TA, LST - unsmoothed) and MODIS (LST - smoothed) data ###################

AmeriFlux_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv")
MOD_LST_all <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--Modis_for_siteLST.csv")

# Merge the two dataframes by site, year, and DOY
AF_MOD_merged_TA_LST <- merge(
  MOD_LST_all,
  AmeriFlux_TA_LST[, c("SiteID", "year", "DOY", "AF_TA", "AF_LST")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Rename the MODIS LST column
colnames(AF_MOD_merged_TA_LST)[colnames(AF_MOD_merged_TA_LST) == "LST"] <- "MOD_LST_smoothed"

# Check the first few rows to verify the merge
head(AF_MOD_merged_TA_LST)

# Save the merged dataset
write.csv(
  AF_MOD_merged_TA_LST,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv",
  row.names = FALSE
)



###################   04 Fit models – Simultaneously calculate mean temperatures and impacts after SOS and EOS for three temperature datasets   ################################################

AF_MOD_merged_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv")

df_AF_TA <- AF_MOD_merged_TA_LST[, !(names(AF_MOD_merged_TA_LST) %in% c("AF_LST", "MOD_LST_smoothed"))]
df_AF_LST <- AF_MOD_merged_TA_LST[, !(names(AF_MOD_merged_TA_LST) %in% c("AF_TA", "MOD_LST_smoothed"))]
df_MOD_LST <- AF_MOD_merged_TA_LST[, !(names(AF_MOD_merged_TA_LST) %in% c("AF_TA", "AF_LST"))]

AF_PHE_DOY <- read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

results <- data.frame(
  site_id = character(),
  Year = numeric(),
  AF_TA_average_diff_1 = numeric(), 
  AF_TA_average_diff_6 = numeric(),
  AF_LST_average_diff_1 = numeric(), 
  AF_LST_average_diff_6 = numeric(),
  MOD_LST_average_diff_1 = numeric(), 
  MOD_LST_average_diff_6 = numeric(),
  AF_TA_sum_diff_16 = numeric(),
  AF_LST_sum_diff_16 = numeric(),
  MOD_LST_sum_diff_16 = numeric(),
  AF_5meanTA_post_SOS = numeric(),
  AF_5meanTA_post_EOS = numeric(),
  AF_5meanLST_post_SOS = numeric(),
  AF_5meanLST_post_EOS = numeric(),
  MOD_5meanLST_post_SOS = numeric(),
  MOD_5meanLST_post_EOS = numeric(),
  ME_1_AF_TA = numeric(), R_1_AF_TA = numeric(),
  ME_1_AF_LST = numeric(), R_1_AF_LST = numeric(),
  ME_1_MOD_LST = numeric(), R_1_MOD_LST = numeric(),
  ME_2_AF_TA = numeric(), R_2_AF_TA = numeric(), maxdiff_AF_TA = numeric(),
  ME_2_AF_LST = numeric(), R_2_AF_LST = numeric(), maxdiff_AF_LST = numeric(),
  ME_2_MOD_LST = numeric(), R_2_MOD_LST = numeric(), maxdiff_MOD_LST = numeric(),
  days_16 = numeric(), mean_AF_TA_sumdiff_16_mean = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through each site and year

site_ids <- unique(df_AF_TA$SiteID)

for (site_id in site_ids) {
  
  df_AF_TA_site <- df_AF_TA[df_AF_TA$SiteID == site_id, ]
  df_AF_TA_site$AF_TA <- as.numeric(df_AF_TA_site$AF_TA)
  
  df_AF_LST_site <- df_AF_LST[df_AF_LST$SiteID == site_id, ]
  df_AF_LST_site$AF_LST <- as.numeric(df_AF_LST_site$AF_LST)
  
  df_MOD_LST_site <- df_MOD_LST[df_MOD_LST$SiteID == site_id, ]
  df_MOD_LST_site$MOD_LST_smoothed <- as.numeric(df_MOD_LST_site$MOD_LST_smoothed)
  
  AF_PHE_DOY_site <- AF_PHE_DOY[AF_PHE_DOY$SiteID == site_id, ]
  
  years <- unique(df_AF_TA_site$year)
  
  for (year in years) {
    
    df_AF_TA_site_year <- df_AF_TA_site[df_AF_TA_site$year == year, ]
    df_AF_LST_site_year <- df_AF_LST_site[df_AF_LST_site$year == year, ]
    df_MOD_LST_site_year <- df_MOD_LST_site[df_MOD_LST_site$year == year, ]
    
    AF_PHE_DOY_site_year <- AF_PHE_DOY_site[AF_PHE_DOY_site$year == year, ]
    
    # 1. Skip analysis if any of the temperature datasets contain only NA
    if (all(is.na(df_AF_TA_site_year$AF_TA)) | 
        all(is.na(df_AF_LST_site_year$AF_LST)) | 
        all(is.na(df_MOD_LST_site_year$MOD_LST_smoothed))) {
      cat("Skipping site:", site_id, "year:", year, "because temperature column contains only NA/NaN\n")
      next
    }
    
    # 2. Skip if no phenology data is available for the site in this year
    if (nrow(AF_PHE_DOY_site_year) == 0) {
      cat("Skipping site:", site_id, "year:", year, "because phenology data is missing\n") 
      next
    }
    
    # 5-day moving average
    ## 1. Apply to df_AF_TA_site_year
    # Step 1: Create a complete DOY = 1:365 table to fill missing days
    df_full_AF_TA_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_AF_TA_site_year, by = "DOY")
    # Step 2: Apply 5-day moving average (ignore NA)
    df_AF_TA_site_year <- df_full_AF_TA_site_year %>%
      mutate(
        AF_TA = as.numeric(AF_TA),
        AF_TA_smoothed = rollapply(AF_TA, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                   fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, AF_TA = AF_TA_smoothed)
    
    ## 2. Apply to df_AF_LST_site_year
    df_full_AF_LST_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_AF_LST_site_year, by = "DOY")
    df_AF_LST_site_year <- df_full_AF_LST_site_year %>%
      mutate(
        AF_LST = as.numeric(AF_LST),
        AF_LST_smoothed = rollapply(AF_LST, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                    fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, AF_LST = AF_LST_smoothed)
    
    ## 3. df_MOD_LST_site_year is already smoothed
    
    ## Extract phenological data for the current year
    doy_values_site_year <- AF_PHE_DOY_site_year %>%
      filter(SiteID == site_id & year == year) %>%
      dplyr::select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
                    falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
      unlist()
    
    day_1 <- as.numeric(doy_values_site_year["rising_DOY_10"])
    day_2 <- as.numeric(doy_values_site_year["rising_DOY_25"])
    day_3 <- as.numeric(doy_values_site_year["rising_DOY_50"])
    day_4 <- as.numeric(doy_values_site_year["falling_DOY_50"])
    day_5 <- as.numeric(doy_values_site_year["falling_DOY_25"])
    day_6 <- as.numeric(doy_values_site_year["falling_DOY_10"])
    
    
    # First fitting
    start_1 <- list(aa = 20, bb = 20, cc = -0.5 * pi)    ## 1. For AF_TA
    start_2 <- list(ee = 20, ff = 20, gg = -0.5 * pi)    ## 2. For AF_LST
    start_3 <- list(hh = 20, ii = 20, jj = -0.5 * pi)    ## 3. For MOD_LST
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    
    # 1. Fit AF_TA
    atc_AF_TA <- nlsLM(AF_TA ~ aa + bb * sin(2 * pi * DOY / 365 + cc), data = df_AF_TA_site_year,
                       start = start_1, lower = lower, upper = upper,
                       algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    aa <- as.numeric(coef(atc_AF_TA)[1])
    bb <- as.numeric(coef(atc_AF_TA)[2])
    cc <- as.numeric(coef(atc_AF_TA)[3])
    fit_1_atc_AF_TA <- function(DOY, aa, bb, cc) { aa + bb * sin(2 * pi * DOY / 365 + cc) }
    
    # 2. Fit AF_LST
    atc_AF_LST <- nlsLM(AF_LST ~ ee + ff * sin(2 * pi * DOY / 365 + gg), data = df_AF_LST_site_year,
                        start = start_2, lower = lower, upper = upper,
                        algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    ee <- as.numeric(coef(atc_AF_LST)[1])
    ff <- as.numeric(coef(atc_AF_LST)[2])
    gg <- as.numeric(coef(atc_AF_LST)[3])
    fit_1_atc_AF_LST <- function(DOY, ee, ff, gg) { ee + ff * sin(2 * pi * DOY / 365 + gg) }
    
    # 3. Fit MOD_LST
    atc_MOD_LST <- nlsLM(MOD_LST_smoothed ~ hh + ii * sin(2 * pi * DOY / 365 + jj), data = df_MOD_LST_site_year,
                         start = start_3, lower = lower, upper = upper,
                         algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    hh <- as.numeric(coef(atc_MOD_LST)[1])
    ii <- as.numeric(coef(atc_MOD_LST)[2])
    jj <- as.numeric(coef(atc_MOD_LST)[3])
    fit_1_atc_MOD_LST <- function(DOY, hh, ii, jj) { hh + ii * sin(2 * pi * DOY / 365 + jj) }
    
    # Calculate ME and R for the first fit
    # 1. For AF_TA
    lst_atc_1_AF_TA <- na.omit(predict(atc_AF_TA))   # Predicted values from the model
    lst_raw_1_AF_TA <- na.omit(df_AF_TA_site_year$AF_TA)
    ME_1_AF_TA = round(mean((lst_raw_1_AF_TA - lst_atc_1_AF_TA)), 2)
    R_1_AF_TA = round(cor(lst_atc_1_AF_TA, lst_raw_1_AF_TA), 2)
    # Get the DOY of the maximum fitted value
    DOY_max_AF_TA <- (pi/2 - cc) * 365 / (2 * pi)
    AF_TA_max <- fit_1_atc_AF_TA(DOY_max_AF_TA, aa, bb, cc)
    
    # 2. For AF_LST
    lst_atc_1_AF_LST <- na.omit(predict(atc_AF_LST))
    lst_raw_1_AF_LST <- na.omit(df_AF_LST_site_year$AF_LST)
    ME_1_AF_LST = round(mean((lst_raw_1_AF_LST - lst_atc_1_AF_LST)), 2)
    R_1_AF_LST = round(cor(lst_atc_1_AF_LST, lst_raw_1_AF_LST), 2)
    DOY_max_AF_LST <- (pi/2 - gg) * 365 / (2 * pi)
    AF_LST_max <- fit_1_atc_AF_LST(DOY_max_AF_LST, ee, ff, gg)
    
    # 3. For MOD_LST
    lst_atc_1_MOD_LST <- na.omit(predict(atc_MOD_LST))
    lst_raw_1_MOD_LST <- na.omit(df_MOD_LST_site_year$MOD_LST_smoothed)
    ME_1_MOD_LST = round(mean((lst_raw_1_MOD_LST - lst_atc_1_MOD_LST)), 2)
    R_1_MOD_LST = round(cor(lst_atc_1_MOD_LST, lst_raw_1_MOD_LST), 2)
    DOY_max_MOD_LST <- (pi/2 - jj) * 365 / (2 * pi)
    MOD_LST_max <- fit_1_atc_MOD_LST(DOY_max_MOD_LST, hh, ii, jj)
    
    ####################################### Second fitting
    # For second fitting: set non-growing season LST values to NA
    df2_AF_TA <- df_AF_TA_site_year
    df2_AF_TA$AF_TA[df2_AF_TA$DOY > day_1 & df2_AF_TA$DOY < day_6] <- NA
    
    df2_AF_LST <- df_AF_LST_site_year
    df2_AF_LST$AF_LST[df2_AF_LST$DOY > day_1 & df2_AF_LST$DOY < day_6] <- NA
    
    df2_MOD_LST <- df_MOD_LST_site_year
    df2_MOD_LST$MOD_LST_smoothed[df2_MOD_LST$DOY > day_1 & df2_MOD_LST$DOY < day_6] <- NA
    
    # Check whether any dataset has valid values for the second fit
    if (all(is.na(df2_AF_TA$AF_TA)) | 
        all(is.na(df2_AF_LST$AF_LST)) | 
        all(is.na(df2_MOD_LST$MOD_LST_smoothed))) {
      cat("Skipping site:", site_id, "year:", year, 
          "because all temperature data outside growing season are NA. Further analysis cannot proceed.\n")
    } else {
      # Continue with subsequent analysis, e.g., site_id <- "US-xML" in year 2017
      
      # Second fitting
      start2_1 <- list(aa2 = 20, bb2 = 20)
      start2_2 <- list(ee2 = 20, ff2 = 20)
      start2_3 <- list(hh2 = 20, ii2 = 20)
      
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      
      # 1. Fit AF_TA
      atc2_AF_TA <- nlsLM(AF_TA ~ aa2 + bb2 * sin(2 * pi * DOY / 365 + cc), data = df2_AF_TA,
                          start = start2_1, lower = lower2, upper = upper2,
                          algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      aa2 <- as.numeric(coef(atc2_AF_TA)[1])
      bb2 <- as.numeric(coef(atc2_AF_TA)[2])
      fit_2_atc_AF_TA <- function(DOY, aa2, bb2, cc) { aa2 + bb2 * sin(2 * pi * DOY / 365 + cc) }
      
      # 2. Fit AF_LST
      atc2_AF_LST <- nlsLM(AF_LST ~ ee2 + ff2 * sin(2 * pi * DOY / 365 + gg), data = df2_AF_LST,
                           start = start2_2, lower = lower2, upper = upper2,
                           algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      ee2 <- as.numeric(coef(atc2_AF_LST)[1])
      ff2 <- as.numeric(coef(atc2_AF_LST)[2])
      fit_2_atc_AF_LST <- function(DOY, ee2, ff2, gg) { ee2 + ff2 * sin(2 * pi * DOY / 365 + gg) }  
      
      # 3. Fit MOD_LST
      atc2_MOD_LST <- nlsLM(MOD_LST_smoothed ~ hh2 + ii2 * sin(2 * pi * DOY / 365 + jj), data = df2_MOD_LST,
                            start = start2_3, lower = lower2, upper = upper2,
                            algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      hh2 <- as.numeric(coef(atc2_MOD_LST)[1])
      ii2 <- as.numeric(coef(atc2_MOD_LST)[2])
      fit_2_atc_MOD_LST <- function(DOY, hh2, ii2, jj) { hh2 + ii2 * sin(2 * pi * DOY / 365 + jj) }  
      
      
      # Generate fitted values from second fitting for each day to compute differences
      df_AF_TA_site_year$fit_2_AF_TA <- fit_2_atc_AF_TA(df_AF_TA_site_year$DOY, aa2, bb2, cc)
      df_AF_LST_site_year$fit_2_AF_LST <- fit_2_atc_AF_LST(df_AF_LST_site_year$DOY, ee2, ff2, gg)
      df_MOD_LST_site_year$fit_2_MOD_LST <- fit_2_atc_MOD_LST(df_MOD_LST_site_year$DOY, hh2, ii2, jj)
      
      # Compute difference between observed and fitted values (observed - fitted)
      # 1. For AF_TA
      df_AF_TA_site_year$AF_TA_diff <- df_AF_TA_site_year$AF_TA - df_AF_TA_site_year$fit_2_AF_TA
      ME_2_AF_TA <- round(mean((df2_AF_TA$AF_TA - predict(atc2_AF_TA, df2_AF_TA)), na.rm = TRUE), 2)
      R_2_AF_TA <- round(cor(predict(atc2_AF_TA, df2_AF_TA), df2_AF_TA$AF_TA, use = "complete.obs"), 2)
      
      # 2. For AF_LST
      df_AF_LST_site_year$AF_LST_diff <- df_AF_LST_site_year$AF_LST - df_AF_LST_site_year$fit_2_AF_LST
      ME_2_AF_LST <- round(mean((df2_AF_LST$AF_LST - predict(atc2_AF_LST, df2_AF_LST)), na.rm = TRUE), 2)
      R_2_AF_LST <- round(cor(predict(atc2_AF_LST, df2_AF_LST), df2_AF_LST$AF_LST, use = "complete.obs"), 2)
      
      # 3. For MOD_LST
      df_MOD_LST_site_year$MOD_LST_diff <- df_MOD_LST_site_year$MOD_LST_smoothed - df_MOD_LST_site_year$fit_2_MOD_LST
      ME_2_MOD_LST <- round(mean((df2_MOD_LST$MOD_LST_smoothed - predict(atc2_MOD_LST, df2_MOD_LST)), na.rm = TRUE), 2)
      R_2_MOD_LST <- round(cor(predict(atc2_MOD_LST, df2_MOD_LST), df2_MOD_LST$MOD_LST_smoothed, use = "complete.obs"), 2)
      
      
      # Compute peak values of the second fit and the difference from the first fit
      # 1. For AF_TA
      DOY_max_2_AF_TA <- (pi/2 - cc) * 365 / (2 * pi)
      AF_TA_max_2 <- fit_2_atc_AF_TA(DOY_max_2_AF_TA, aa2, bb2, cc)
      maxdiff_AF_TA <- round(AF_TA_max_2 - AF_TA_max, 2)
      
      # 2. For AF_LST
      DOY_max_2_AF_LST <- (pi/2 - gg) * 365 / (2 * pi)
      AF_LST_max_2 <- fit_2_atc_AF_LST(DOY_max_2_AF_LST, ee2, ff2, gg)
      maxdiff_AF_LST <- round(AF_LST_max_2 - AF_LST_max, 2)
      
      # 3. For MOD_LST
      DOY_max_2_MOD_LST <- (pi/2 - jj) * 365 / (2 * pi)
      MOD_LST_max_2 <- fit_2_atc_MOD_LST(DOY_max_2_MOD_LST, hh2, ii2, jj)
      maxdiff_MOD_LST <- round(MOD_LST_max_2 - MOD_LST_max, 2)
      
      
      # Calculate mean difference within 5 days after SOS and EOS
      AF_TA_average_diff_1 <- round(mean(df_AF_TA_site_year$AF_TA_diff[df_AF_TA_site_year$DOY >= day_1 + 1 & df_AF_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_TA_average_diff_6 <- round(mean(df_AF_TA_site_year$AF_TA_diff[df_AF_TA_site_year$DOY >= day_6 + 1 & df_AF_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      AF_LST_average_diff_1 <- round(mean(df_AF_LST_site_year$AF_LST_diff[df_AF_LST_site_year$DOY >= day_1 + 1 & df_AF_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_LST_average_diff_6 <- round(mean(df_AF_LST_site_year$AF_LST_diff[df_AF_LST_site_year$DOY >= day_6 + 1 & df_AF_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      MOD_LST_average_diff_1 <- round(mean(df_MOD_LST_site_year$MOD_LST_diff[df_MOD_LST_site_year$DOY >= day_1 + 1 & df_MOD_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      MOD_LST_average_diff_6 <- round(mean(df_MOD_LST_site_year$MOD_LST_diff[df_MOD_LST_site_year$DOY >= day_6 + 1 & df_MOD_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # Calculate total accumulated difference during the growing season
      AF_TA_sum_diff_16 <- round(sum(df_AF_TA_site_year$AF_TA_diff[day_1:(day_6)], na.rm = TRUE), 2)
      AF_LST_sum_diff_16 <- round(sum(df_AF_LST_site_year$AF_LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      MOD_LST_sum_diff_16 <- round(sum(df_MOD_LST_site_year$MOD_LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      
      # Calculate 5-day average raw temperature values after SOS and EOS
      AF_5meanTA_post_SOS <- round(mean(df_AF_TA_site_year$AF_TA[df_AF_TA_site_year$DOY >= day_1 + 1 & df_AF_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_5meanTA_post_EOS <- round(mean(df_AF_TA_site_year$AF_TA[df_AF_TA_site_year$DOY >= day_6 + 1 & df_AF_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      AF_5meanLST_post_SOS <- round(mean(df_AF_LST_site_year$AF_LST[df_AF_LST_site_year$DOY >= day_1 + 1 & df_AF_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_5meanLST_post_EOS <- round(mean(df_AF_LST_site_year$AF_LST[df_AF_LST_site_year$DOY >= day_6 + 1 & df_AF_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      MOD_5meanLST_post_SOS <- round(mean(df_MOD_LST_site_year$MOD_LST_smoothed[df_MOD_LST_site_year$DOY >= day_1 + 1 & df_MOD_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      MOD_5meanLST_post_EOS <- round(mean(df_MOD_LST_site_year$MOD_LST_smoothed[df_MOD_LST_site_year$DOY >= day_6 + 1 & df_MOD_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # Calculate length of the growing season
      days_16 <- day_6 - day_1 + 1
      mean_AF_TA_sumdiff_16_mean <- round(AF_TA_sum_diff_16 / days_16, 2)
      
      # Return results
      
      
      # Save the results
      results <- rbind(results, data.frame(
        site_id, Year = year,
        AF_TA_average_diff_1 = AF_TA_average_diff_1, 
        AF_TA_average_diff_6 = AF_TA_average_diff_6,
        AF_LST_average_diff_1 = AF_LST_average_diff_1, 
        AF_LST_average_diff_6 = AF_LST_average_diff_6,
        MOD_LST_average_diff_1 = MOD_LST_average_diff_1, 
        MOD_LST_average_diff_6 = MOD_LST_average_diff_6,
        
        AF_TA_sum_diff_16 = AF_TA_sum_diff_16,
        AF_LST_sum_diff_16 = AF_LST_sum_diff_16,
        MOD_LST_sum_diff_16 = MOD_LST_sum_diff_16,  
        
        AF_5meanTA_post_SOS  = AF_5meanTA_post_SOS,
        AF_5meanTA_post_EOS  = AF_5meanTA_post_EOS,
        AF_5meanLST_post_SOS = AF_5meanLST_post_SOS,
        AF_5meanLST_post_EOS = AF_5meanLST_post_EOS,
        MOD_5meanLST_post_SOS= MOD_5meanLST_post_SOS,
        MOD_5meanLST_post_EOS= MOD_5meanLST_post_EOS,
        
        ME_1_AF_TA = ME_1_AF_TA, R_1_AF_TA = R_1_AF_TA,
        ME_1_AF_LST = ME_1_AF_LST, R_1_AF_LST = ME_1_AF_LST,
        ME_1_MOD_LST = ME_1_MOD_LST, R_1_MOD_LST = R_1_MOD_LST,
        ME_2_AF_TA = ME_2_AF_TA, R_2_AF_TA = R_2_AF_TA, maxdiff_AF_TA = maxdiff_AF_TA,
        ME_2_AF_LST = ME_2_AF_LST, R_2_AF_LST = R_2_AF_LST, maxdiff_AF_LST = maxdiff_AF_LST,
        ME_2_MOD_LST = ME_2_MOD_LST, R_2_MOD_LST = R_2_MOD_LST, maxdiff_MOD_LST = maxdiff_MOD_LST,
        
        days_16 = days_16,
        mean_AF_TA_sumdiff_16_mean = mean_AF_TA_sumdiff_16_mean
      ))
      
      # Export the results to CSV
      write.csv(
        results,
        file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_years.csv",
        row.names = FALSE
      )
      