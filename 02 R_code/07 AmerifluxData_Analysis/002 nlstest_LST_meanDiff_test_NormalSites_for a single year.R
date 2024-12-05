###### 0. Loading packages ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###########################################    01 Analyzing Normal Site Data   ################################################
# Purpose: This section performs two ATC (Annual Temperature Cycle) fittings to analyze the temperature dynamics 
# in relation to phenological dates at the Normal site, and calculates key vegetation-related temperature effects.
# The recommended LST for the Normal site is corrected_surfaceTemp_97.

# Read data for LST and phenology DOY for normal sites
df1_LST_norm <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

head(df1_LST_norm)

# Data frame to store the final results
results <- data.frame(
  site_id = character(),
  Year = numeric(),
  RMSE_1 = numeric(), R_1 = numeric(),
  RMSE_2 = numeric(), R_2 = numeric(), LST_maxdiff = numeric(),
  average_diff_21 = numeric(), average_diff_22 = numeric(),
  average_diff_23 = numeric(), average_diff_24 = numeric(),
  average_diff_25 = numeric(), average_diff_26 = numeric(),
  days_12 = numeric(), days_23 = numeric(), days_34 = numeric(),
  days_45 = numeric(), days_56 = numeric(), days_16 = numeric(),
  sum_Diff_12 = numeric(), sum_Diff_23 = numeric(), sum_Diff_34 = numeric(),
  sum_Diff_45 = numeric(), sum_Diff_56 = numeric(), sum_Diff_16 = numeric(),
  mean_diff_12 = numeric(), mean_diff_23 = numeric(), mean_diff_34 = numeric(),
  mean_Diff_45 = numeric(), mean_Diff_56 = numeric(), mean_Diff_16 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site and year
site_ids <- unique(df1_LST_norm$SiteID)

for (site_id in site_ids) {
  
  # Filter data for the current site
  df1_LST_norm_site <- df1_LST_norm[df1_LST_norm$SiteID == site_id, ]
  df1_LST_norm_site$LST <- as.numeric(df1_LST_norm_site$corrected_surfaceTemp_97)
  df1_LST_norm_site$DOY <- as.numeric(df1_LST_norm_site$DOY)
  df1_LST_norm_site$year <- as.numeric(df1_LST_norm_site$year)
  
  df2_PHE_DOY_site <- df2_PHE_DOY[df2_PHE_DOY$SiteID == site_id, ]
  
  years <- unique(df1_LST_norm_site$year)
  
  for (year in years) {
    
    # Filter data for the current year
    df1_LST_norm_site_year <- df1_LST_norm_site[df1_LST_norm_site$year == year, ]
    df2_PHE_DOY_site_year <- df2_PHE_DOY_site[df2_PHE_DOY_site$year == year, ]
    
    # Skip years without phenology data for the site (e.g., "US-xUK" 2017 has temperature data but no DOY data)
    if (nrow(df2_PHE_DOY_site_year) == 0) {
      cat("Skipping year:", year, "because df2_PHE_DOY_site_year has no data\n")
      next  # Skip the analysis for this year
    }
    
    # 5-day moving average for LST
    df1_LST_norm_site_year <- df1_LST_norm_site_year %>%
      mutate(LST_smoothed = rollmean(LST, k = 5, fill = NA, align = "center")) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      select(DOY, LST_smoothed) %>%
      rename(LST = LST_smoothed)
    
    # Extract phenological day of year (DOY) values
    doy_values <- df2_PHE_DOY_site_year %>%
      filter(SiteID == site_id & year == year) %>%
      select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
             falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
      unlist()
    
    day_1 <- as.numeric(doy_values["rising_DOY_10"])
    day_2 <- as.numeric(doy_values["rising_DOY_25"])
    day_3 <- as.numeric(doy_values["rising_DOY_50"])
    day_4 <- as.numeric(doy_values["falling_DOY_50"])
    day_5 <- as.numeric(doy_values["falling_DOY_25"])
    day_6 <- as.numeric(doy_values["falling_DOY_10"])
    
    # First fitting (ATC model)
    start <- list(tl = 20, da = 20, st = -0.5 * pi)
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), data = df1_LST_norm_site_year,
                 start = start, lower = lower, upper = upper,
                 algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    tl <- as.numeric(coef(atc)[1])
    da <- as.numeric(coef(atc)[2])
    st <- as.numeric(coef(atc)[3])
    fit_1 <- function(DOY, tl, da, st) { tl + da * sin(2 * pi * DOY / 365 + st) }
    
    # Calculate RMSE and R for the first fit
    lst_atc_1 <- na.omit(predict(atc))   # Predicted values from the model
    lst_raw_1 <- na.omit(df1_LST_norm_site_year$LST)
    RMSE_1 = round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)), 2)
    R_1 = round(cor(lst_atc_1, lst_raw_1), 2)
    
    # Find the maximum value of the sine part of the first fit and compute the corresponding LST value
    DOY_max <- (pi/2 - st) * 365 / (2 * pi)
    LST_max <- fit_1(DOY_max, tl, da, st)
  }
  
  
  #######################################
  #######################################
  # Second fitting: Set LST in the non-growing season to NA-representing a non-vagetation senario
  
  df2 <- df1_LST_norm_site_year
  df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA  # Set LST to NA for the non-growing season (DOY between day_1 and day_6)
  
  if (all(is.na(df2$LST))) {  # Check if all values are NA for the second fitting
    cat("Since station", site_id, "in year", year, "has no valid data, further analysis cannot be performed.\n")
  } else {
    # Continue with the second fitting
    start2 <- list(tl2 = 20, da2 = 20)
    lower2 <- c(0, 0)
    upper2 <- c(40, 40)
    atc2 <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2,
                  start = start2, lower = lower2, upper = upper2,
                  algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    
    tl2 <- as.numeric(coef(atc2)[1])  # Extract the coefficient for tl2
    da2 <- as.numeric(coef(atc2)[2])  # Extract the coefficient for da2
    fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }  # Define the second fit model
    
    # Calculate the difference between the actual values and the fit values for LST
    df1_LST_norm_site_year$fit_2_value <- fit_2(df1_LST_norm_site_year$DOY, tl2, da2, st)
    df1_LST_norm_site_year$LST_diff <- df1_LST_norm_site_year$LST - df1_LST_norm_site_year$fit_2_value
    
    RMSE_2 <- round(sqrt(mean((df2$LST - predict(atc2, df2))^2, na.rm = TRUE)), 2)  # Calculate RMSE for the second fit
    R_2 <- round(cor(predict(atc2, df2), df2$LST, use = "complete.obs"), 2)  # Calculate R-squared for the second fit
    
    # Calculate the maximum value of LST for the second fit
    DOY_max_2 <- (pi / 2 - st) * 365 / (2 * pi)  # Calculate the day of year corresponding to the maximum LST
    LST_max_2 <- fit_2(DOY_max_2, tl2, da2, st)  # Calculate the maximum LST value
    LST_maxdiff <- round(LST_max_2 - LST_max, 2)  # Calculate the difference between the first and second LST max values
    
    # Calculate the average differences between the actual and fitted values for different phases
    average_diff_21 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_1 + 1 & df1_LST_norm_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
    average_diff_22 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_2 + 1 & df1_LST_norm_site_year$DOY <= day_2 + 5], na.rm = TRUE), 2)
    average_diff_23 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_3 + 1 & df1_LST_norm_site_year$DOY <= day_3 + 5], na.rm = TRUE), 2)
    average_diff_24 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_4 + 1 & df1_LST_norm_site_year$DOY <= day_4 + 5], na.rm = TRUE), 2)
    average_diff_25 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_5 + 1 & df1_LST_norm_site_year$DOY <= day_5 + 5], na.rm = TRUE), 2)
    average_diff_26 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_6 + 1 & df1_LST_norm_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
    
    # Calculate the cumulative difference for each phase
    sum_diff_12 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_2 - 1)], na.rm = TRUE), 2)
    sum_diff_23 <- round(sum(df1_LST_norm_site_year$LST_diff[day_2:(day_3 - 1)], na.rm = TRUE), 2)
    sum_diff_34 <- round(sum(df1_LST_norm_site_year$LST_diff[day_3:(day_4 - 1)], na.rm = TRUE), 2)
    sum_diff_45 <- round(sum(df1_LST_norm_site_year$LST_diff[day_4:(day_5 - 1)], na.rm = TRUE), 2)
    sum_diff_56 <- round(sum(df1_LST_norm_site_year$LST_diff[day_5:(day_6)], na.rm = TRUE), 2)
    sum_diff_16 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
    
    # Calculate the number of days in each phase
    days_12 <- day_2 - day_1
    days_23 <- day_3 - day_2
    days_34 <- day_4 - day_3
    days_45 <- day_5 - day_4
    days_56 <- day_6 - day_5 + 1
    days_16 <- day_6 - day_1 + 1
    
    # Calculate the mean difference for each phase (checked)
    mean_diff_12 <- round(sum_diff_12 / days_12, 2)
    mean_diff_23 <- round(sum_diff_23 / days_23, 2)
    mean_diff_34 <- round(sum_diff_34 / days_34, 2)
    mean_diff_45 <- round(sum_diff_45 / days_45, 2)
    mean_diff_56 <- round(sum_diff_56 / days_56, 2)
    mean_diff_16 <- round(sum_diff_16 / days_16, 2)
    
    # Save the results
    results <- rbind(results, data.frame(site_id, Year = year,
                                         RMSE_1 = RMSE_1, R_1 = R_1, RMSE_2 = RMSE_2, R_2 = R_2, LST_maxdiff = LST_maxdiff,
                                         average_diff_21 = average_diff_21, average_diff_22 = average_diff_22, average_diff_23 = average_diff_23,
                                         average_diff_24 = average_diff_24, average_diff_25 = average_diff_25, average_diff_26 = average_diff_26,
                                         days_12 = days_12, days_23 = days_23, days_34 = days_34,
                                         days_45 = days_45, days_56 = days_56, days_16 = days_16,
                                         sum_Diff_12 = sum_diff_12, sum_Diff_23 = sum_diff_23, sum_Diff_34 = sum_diff_34,
                                         sum_Diff_45 = sum_diff_45, sum_Diff_56 = sum_diff_56, sum_Diff_16 = sum_diff_16,
                                         mean_diff_12 = mean_diff_12, mean_diff_23 = mean_diff_23, mean_diff_34 = mean_diff_34,
                                         mean_Diff_45 = mean_diff_45, mean_Diff_56 = mean_diff_56, mean_Diff_16 = mean_diff_16))
    print(results)
  }
  
}

# Write the results to a CSV file
write.csv(results, file = "./AmerifluxData_Analysis/Normal_results.csv", row.names = FALSE)

  