###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

# Set working directory
setwd("D:/VegetationImpact")

######################################################################
### 01. Filter site-year combinations with available ET measurements ###
######################################################################

# Load combined dataset with AF_TA, MOD_LST, ET, etc.
df <- read.csv("./AmerifluxData_Analysis/Test_for_ET--2.AmeriFlux_All_TA_ET_variables.csv")
head(df)

# Identify site-year combinations where all ET values are NA
removed_site_years <- df %>%
  group_by(SiteID, year) %>%
  summarise(all_na = all(is.na(ET_calculated)), .groups = "drop") %>%
  filter(all_na) %>%
  select(SiteID, year)

# Print the excluded site-year combinations
print(removed_site_years)

# Remove those site-year combinations from the dataset
df_clean <- df %>%
  anti_join(removed_site_years, by = c("SiteID", "year"))

# Save removed combinations
write.csv(
  removed_site_years,
  file = "./AmerifluxData_Analysis/Test_for_ET--3.AmeriFlux_site_year_removedby_NA-ET.csv",
  row.names = FALSE
)

# Save cleaned dataset
write.csv(
  df_clean,
  file = "./AmerifluxData_Analysis/Test_for_ET--4.AmeriFlux_remain_site_year_for_ET.csv",
  row.names = FALSE
)

######################################################################
### 02. Apply 5-day moving average smoothing to remaining ET records ###
######################################################################

df_clean <- read.csv("./AmerifluxData_Analysis/Test_for_ET--4.AmeriFlux_remain_site_year_for_ET.csv")
head(df_clean)

site_ids <- unique(df_clean$SiteID)

# List to store smoothed ET results
all_smoothed <- list()

# Loop through each site
for (site_id in site_ids) {
  df_clean_site <- df_clean[df_clean$SiteID == site_id, ]
  years <- unique(df_clean_site$year)
  
  # Loop through each year
  for (year in years) {
    cat("Processing site:", site_id, "year:", year, "\n")
    df_clean_site_year <- df_clean_site[df_clean_site$year == year, ]
    
    # Step 1: Create a full DOY=1:365 template and merge with actual data
    df_full <- data.frame(DOY = 1:365) %>%
      left_join(df_clean_site_year, by = "DOY")
    
    # Ensure ET is numeric
    df_full$ET_calculated <- as.numeric(df_full$ET_calculated)
    
    # Step 2: Calculate 5-day centered moving average
    df_smoothed <- df_full %>%
      mutate(
        SiteID = site_id,
        year = year,
        ET_smoothed = rollapply(
          ET_calculated,
          width = 5,
          FUN = function(x) mean(x, na.rm = TRUE),
          fill = NA,
          align = "center"
        )
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%  # Avoid edge artifacts
      select(SiteID, year, DOY, ET_smoothed)
    
    # Add to results list
    all_smoothed[[length(all_smoothed) + 1]] <- df_smoothed
  }
}

# Combine all smoothed data
df_all_smoothed <- bind_rows(all_smoothed)

# Merge smoothed ET back into cleaned dataset
df_clean_add_ET_smoothed <- merge(
  df_clean,
  df_all_smoothed[, c("SiteID", "year", "DOY", "ET_smoothed")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Save final dataset for fitting
write.csv(
  df_clean_add_ET_smoothed,
  file = "./AmerifluxData_Analysis/Test_for_ET--5.AmeriFlux_afterET_smooth_for_fitting.csv",
  row.names = FALSE
)



################### 03. Fit AF_LST and AF_TA per site-year and compute daily difference ###################

# Load preprocessed dataset with AF_LST, AF_TA, ET, etc.
df <- read.csv("./AmerifluxData_Analysis/Test_for_ET--5.AmeriFlux_afterET_smooth_for_fitting.csv")

# Load phenological transition dates (e.g., SOS, EOS)
df_PHE_DOY <- read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

# Get list of site IDs
site_ids <- unique(df$SiteID)

# Initialize dataframe to store all results across site-years
df_all_results <- data.frame()

# Loop through each site
for (site_id in site_ids) {
  
  # Subset site-specific temperature and LST data
  df_AF_TA_site <- df[df$SiteID == site_id, ]
  df_AF_TA_site$AF_TA <- as.numeric(df_AF_TA_site$AF_TA)
  
  df_AF_LST_site <- df[df$SiteID == site_id, ]
  df_AF_LST_site$AF_LST <- as.numeric(df_AF_LST_site$AF_LST)
  
  df_PHE_DOY_site <- df_PHE_DOY[df_PHE_DOY$SiteID == site_id, ]
  
  # Loop through years for each site
  years <- unique(df_AF_TA_site$year)
  for (year in years) {
    
    # Subset data for current year
    df_AF_TA_site_year <- df_AF_TA_site[df_AF_TA_site$year == year, ]
    df_AF_LST_site_year <- df_AF_LST_site[df_AF_LST_site$year == year, ]
    df_PHE_DOY_site_year <- df_PHE_DOY_site[df_PHE_DOY_site$year == year, ]
    
    # Skip site-year with all NA values in either AF_TA or AF_LST
    if (all(is.na(df_AF_TA_site_year$AF_TA)) | 
        all(is.na(df_AF_LST_site_year$AF_LST))) {
      cat("Skipping site:", site_id, "year:", year, "due to all NA temperature values.\n")
      next
    }
    
    # Skip site-year if no phenological transition data is available
    if (nrow(df_PHE_DOY_site_year) == 0) {
      cat("Skipping site:", site_id, "year:", year, "due to missing phenological DOY values.\n")
      next
    }
    
    #### Apply 5-day centered moving average to AF_TA ####
    df_full_AF_TA_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_AF_TA_site_year, by = "DOY")
    
    df_AF_TA_site_year <- df_full_AF_TA_site_year %>%
      mutate(
        AF_TA = as.numeric(AF_TA),
        AF_TA_smoothed = rollapply(AF_TA, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                   fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, AF_TA = AF_TA_smoothed)
    
    #### Apply 5-day centered moving average to AF_LST ####
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
    
    #### Extract phenological transition DOYs ####
    doy_values_site_year <- df_PHE_DOY_site_year %>%
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
    start_1 <- list(aa = 20, bb = 20, cc = -0.5 * pi)        ## 1. For AF_TA
    start_2 <- list(ee = 20, ff = 20, gg = -0.5 * pi)        ## 2. For AF_LST
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    
    # 1. For AF_TA
    atc_AF_TA <- nlsLM(AF_TA ~ aa + bb * sin(2 * pi * DOY / 365 + cc), data = df_AF_TA_site_year,
                       start = start_1, lower = lower, upper = upper,
                       algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    aa <- as.numeric(coef(atc_AF_TA)[1])
    bb <- as.numeric(coef(atc_AF_TA)[2])
    cc <- as.numeric(coef(atc_AF_TA)[3])
    fit_1_atc_AF_TA <- function(DOY, aa, bb, cc) { aa + bb * sin(2 * pi * DOY / 365 + cc) }
    
    # 2. For AF_LST
    atc_AF_LST <- nlsLM(AF_LST ~ ee + ff * sin(2 * pi * DOY / 365 + gg), data = df_AF_LST_site_year,
                        start = start_2, lower = lower, upper = upper,
                        algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    ee <- as.numeric(coef(atc_AF_LST)[1])
    ff <- as.numeric(coef(atc_AF_LST)[2])
    gg <- as.numeric(coef(atc_AF_LST)[3])
    fit_1_atc_AF_LST <- function(DOY, ee, ff, gg) { ee + ff * sin(2 * pi * DOY / 365 + gg) }
    
    
    ####################################### Second fitting
    # Second fitting: Set LST values outside the growing season to NA
    df2_AF_TA <- df_AF_TA_site_year
    df2_AF_TA$AF_TA[df2_AF_TA$DOY > day_1 & df2_AF_TA$DOY < day_6] <- NA
    
    df2_AF_LST <- df_AF_LST_site_year
    df2_AF_LST$AF_LST[df2_AF_LST$DOY > day_1 & df2_AF_LST$DOY < day_6] <- NA
    
    if (all(is.na(df2_AF_TA$AF_TA)) | 
        all(is.na(df2_AF_LST$AF_LST))) {
      cat("Site", site_id, "in year", year, "has no valid data outside the growing season; further analysis is not possible.\n")
    } else {
      
      # Continue with subsequent analysis code, e.g., site_id <- "US-xML" in year 2017
      
      # Second fitting
      start2_1 <- list(aa2 = 20, bb2 = 20)
      start2_2 <- list(ee2 = 20, ff2 = 20)
      
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      
      # 1. For AF_TA
      atc2_AF_TA <- nlsLM(AF_TA ~ aa2 + bb2 * sin(2 * pi * DOY / 365 + cc), data = df2_AF_TA,
                          start = start2_1, lower = lower2, upper = upper2,
                          algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      aa2 <- as.numeric(coef(atc2_AF_TA)[1])
      bb2 <- as.numeric(coef(atc2_AF_TA)[2])
      fit_2_atc_AF_TA <- function(DOY, aa2, bb2, cc) { aa2 + bb2 * sin(2 * pi * DOY / 365 + cc) }
      
      
      # 2. For AF_LST
      atc2_AF_LST <- nlsLM(AF_LST ~ ee2 + ff2 * sin(2 * pi * DOY / 365 + gg), data = df2_AF_LST,
                           start = start2_2, lower = lower2, upper = upper2,
                           algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      ee2 <- as.numeric(coef(atc2_AF_LST)[1])
      ff2 <- as.numeric(coef(atc2_AF_LST)[2])
      fit_2_atc_AF_LST <- function(DOY, ee2, ff2, gg) { ee2 + ff2 * sin(2 * pi * DOY / 365 + gg) }  
      
      
      # Calculate differences and cumulative/mean values for each phenophase
      # Generate fitted values from the second fitted curve to be used as the subtrahend in difference calculations
      df_AF_TA_site_year$fit_2_AF_TA <- fit_2_atc_AF_TA(df_AF_TA_site_year$DOY, aa2, bb2, cc)
      df_AF_LST_site_year$fit_2_AF_LST <- fit_2_atc_AF_LST(df_AF_LST_site_year$DOY, ee2, ff2, gg)
      
      # Calculate difference: actual value - fitted value
      # 1. For AF_TA
      df_AF_TA_site_year$AF_TA_diff <- df_AF_TA_site_year$AF_TA - df_AF_TA_site_year$fit_2_AF_TA
      # ME_2_AF_TA <- round(mean((df2_AF_TA$AF_TA - predict(atc2_AF_TA, df2_AF_TA)), na.rm = TRUE), 2)
      # R_2_AF_TA <- round(cor(predict(atc2_AF_TA, df2_AF_TA), df2_AF_TA$AF_TA, use = "complete.obs"), 2)
      
      head(df_AF_TA_site_year)
      
      # 2. For AF_LST
      df_AF_LST_site_year$AF_LST_diff <- df_AF_LST_site_year$AF_LST - df_AF_LST_site_year$fit_2_AF_LST
      # ME_2_AF_LST <- round(mean((df2_AF_LST$AF_LST - predict(atc2_AF_LST, df2_AF_LST)), na.rm = TRUE), 2)
      # R_2_AF_LST <- round(cor(predict(atc2_AF_LST, df2_AF_LST), df2_AF_LST$AF_LST, use = "complete.obs"), 2)
      
      head(df_AF_LST_site_year)
      
      
      # Organize results
      df_result <- cbind(df_AF_TA_site_year, df_AF_LST_site_year)
      
      
      df_result$SOS_DOY <- day_1
      df_result$MGP_DOY <- day_2
      df_result$GMO_DOY <- day_3
      df_result$GDO_DOY <- day_4
      df_result$MSP_DOY <- day_5
      df_result$EOS_DOY <- day_6
      
      df_result$year <- year
      df_result$SiteID <- site_id
      
      df_all_results <- rbind(df_all_results, df_result)
    }
  }
}

# Note: Sites without successful fitting -- no Tdiff available
# Skipped site: US-UMB, skipped year: 2013 due to missing phenology data
# Skipped site: US-xST, skipped year: 2017 due to missing phenology data
# Skipped site: US-xUK, skipped year: 2017 due to missing phenology data
# Skipped site: US-xUK, skipped year: 2018 due to missing phenology data

# print(df_all_results)
colnames(df_all_results)

# Merge two data frames
df_add_Tdiff <- merge(
  df,
  df_all_results[, c("SiteID", "year", "DOY", "AF_TA_diff", "AF_LST_diff", 
                     "SOS_DOY", "MGP_DOY", "GMO_DOY", "GDO_DOY", "MSP_DOY", "EOS_DOY")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

head(df_add_Tdiff)

# Note: Sites without successful fitting -- no Tdiff available
# Skipped site: US-UMB, skipped year: 2013 due to missing phenology data
# Skipped site: US-xST, skipped year: 2017 due to missing phenology data
# Skipped site: US-xUK, skipped year: 2017 due to missing phenology data
# Skipped site: US-xUK, skipped year: 2018 due to missing phenology data

all_ET_Tdiff_match_Results <- df_add_Tdiff %>%
  filter(
    !(SiteID == "US-UMB" & year == 2013),
    !(SiteID == "US-xST" & year == 2017),
    !(SiteID == "US-xUK" & year %in% c(2017, 2018))
  )

# Add climate zone (Clim) and vegetation type (Veg) info for all sites

climate_df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")
head(climate_df)

all_ET_Tdiff_match_Results <- merge(all_ET_Tdiff_match_Results, 
                                    climate_df[, c("site_id", "Veg", "Clim")], 
                                    by.x = "SiteID", 
                                    by.y = "site_id", 
                                    all.x = TRUE)

write.csv(
  all_ET_Tdiff_match_Results,
  file = "./AmerifluxData_Analysis/Test_for_ET--6.AmeriFlux_ET_Tdiff_ALLResults_for_Calculation.csv",
  row.names = FALSE
)

