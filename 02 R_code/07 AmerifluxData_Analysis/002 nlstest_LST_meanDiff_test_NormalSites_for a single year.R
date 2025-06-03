###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)
library(tidyr)

setwd("D:/VegetationImpact")

###########################################  01 Analyze Normal Sites Data  #############################################

df1_LST_norm <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

head(df1_LST_norm)

# Initialize a data frame to store final results
results <- data.frame(
  site_id = character(),
  Year = numeric(),
  ME_1 = numeric(), R_1 = numeric(),
  ME_2 = numeric(), R_2 = numeric(), LST_maxdiff = numeric(),
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

# Iterate through each site and year
site_ids <- unique(df1_LST_norm$SiteID)

for (site_id in site_ids) {
  
  # site_id <- "US-MOz"
  
  df1_LST_norm_site <- df1_LST_norm[df1_LST_norm$SiteID == site_id, ]
  df1_LST_norm_site$LST <- as.numeric(df1_LST_norm_site$corrected_surfaceTemp_97)
  df1_LST_norm_site$DOY <- as.numeric(df1_LST_norm_site$DOY)
  df1_LST_norm_site$year <- as.numeric(df1_LST_norm_site$year)
  
  df2_PHE_DOY_site <- df2_PHE_DOY[df2_PHE_DOY$SiteID == site_id, ]
  # df2_PHE_DOY_site$year <- as.numeric(df2_PHE_DOY$year)  # Convert 'year' column in df2_PHE_DOY to numeric
  
  years <- unique(df1_LST_norm_site$year)
  
  for (year in years) {
    
    # year <- 2013
    df1_LST_norm_site_year <- df1_LST_norm_site[df1_LST_norm_site$year == year, ]
    df2_PHE_DOY_site_year <- df2_PHE_DOY_site[df2_PHE_DOY_site$year == year, ]
    
    # Skip analysis if the site has LST data but no DOY data for the year
    if (nrow(df2_PHE_DOY_site_year) == 0) {
      cat("Skipping year:", year, "because df2_PHE_DOY_site_year has no data\n")
      next
    }
    
    # 5-day moving average
    # Step 1: Create complete DOY = 1:365 to fill missing days
    df_full <- data.frame(DOY = 1:365) %>%
      left_join(df1_LST_norm_site_year, by = "DOY")
    
    # Step 2: Apply 5-day moving average (allowing NA)
    df1_LST_norm_site_year <- df_full %>%
      mutate(
        LST = as.numeric(LST),  # Ensure values are numeric
        LST_smoothed = rollapply(LST, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                 fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      select(DOY, LST = LST_smoothed)
    
    doy_values <- df2_PHE_DOY_site_year %>%
      filter(SiteID == site_id & year == year) %>%
      dplyr::select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
                    falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
      unlist()
    
    day_1 <- as.numeric(doy_values["rising_DOY_10"])
    day_2 <- as.numeric(doy_values["rising_DOY_25"])
    day_3 <- as.numeric(doy_values["rising_DOY_50"])
    day_4 <- as.numeric(doy_values["falling_DOY_50"])
    day_5 <- as.numeric(doy_values["falling_DOY_25"])
    day_6 <- as.numeric(doy_values["falling_DOY_10"])
    
    # First ATC model fit
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
    
    # Calculate ME and R for first model fit
    lst_atc_1 <- na.omit(predict(atc))   # Predicted LST from model
    lst_raw_1 <- na.omit(df1_LST_norm_site_year$LST)
    ME_1 <- round(mean((lst_raw_1 - lst_atc_1)), 2)
    R_1 <- round(cor(lst_atc_1, lst_raw_1), 2)
    
    # Determine peak LST from first sinusoidal fit
    DOY_max <- (pi / 2 - st) * 365 / (2 * pi)
    LST_max <- fit_1(DOY_max, tl, da, st)
    
    #######################################
    # Second model fit: Set LST to NA during the growing season
    # Second model fitting: set LST values to NA during the growing season
    df2 <- df1_LST_norm_site_year
    df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA
    
    # If all LST values are NA after masking, skip further analysis
    if (all(is.na(df2$LST))) {
      cat("Skipping analysis for site", site_id, "in year", year, "due to all values being NA.\n")
    } else {
      # Proceed with further analysis (e.g., for site_id = "US-xML" in 2017)
      
      start2 <- list(tl2 = 20, da2 = 20)
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      atc2 <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2,
                    start = start2, lower = lower2, upper = upper2,
                    algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      tl2 <- as.numeric(coef(atc2)[1])
      da2 <- as.numeric(coef(atc2)[2])
      fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }
      
      # Compute residuals and predicted values from the second fit
      df1_LST_norm_site_year$fit_2_value <- fit_2(df1_LST_norm_site_year$DOY, tl2, da2, st)
      
      # Calculate temperature deviation: observed - predicted
      df1_LST_norm_site_year$LST_diff <- df1_LST_norm_site_year$LST - df1_LST_norm_site_year$fit_2_value
      ME_2 <- round(mean((df2$LST - predict(atc2, df2)), na.rm = TRUE), 2)
      R_2 <- round(cor(predict(atc2, df2), df2$LST, use = "complete.obs"), 2)
      
      # Find peak DOY and LST from the second fit
      DOY_max_2 <- (pi / 2 - st) * 365 / (2 * pi)
      LST_max_2 <- fit_2(DOY_max_2, tl2, da2, st)
      # Difference between peaks of second and first fits
      LST_maxdiff <- round(LST_max_2 - LST_max, 2)
      
      # Compute 5-day average differences around phenophase dates
      average_diff_21 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_1 + 1 & df1_LST_norm_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      average_diff_22 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_2 + 1 & df1_LST_norm_site_year$DOY <= day_2 + 5], na.rm = TRUE), 2)
      average_diff_23 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_3 + 1 & df1_LST_norm_site_year$DOY <= day_3 + 5], na.rm = TRUE), 2)
      average_diff_24 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_4 + 1 & df1_LST_norm_site_year$DOY <= day_4 + 5], na.rm = TRUE), 2)
      average_diff_25 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_5 + 1 & df1_LST_norm_site_year$DOY <= day_5 + 5], na.rm = TRUE), 2)
      average_diff_26 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_6 + 1 & df1_LST_norm_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # Compute cumulative temperature differences for each phenophase
      sum_diff_12 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_2 - 1)], na.rm = TRUE), 2)
      sum_diff_23 <- round(sum(df1_LST_norm_site_year$LST_diff[day_2:(day_3 - 1)], na.rm = TRUE), 2)
      sum_diff_34 <- round(sum(df1_LST_norm_site_year$LST_diff[day_3:(day_4 - 1)], na.rm = TRUE), 2)
      sum_diff_45 <- round(sum(df1_LST_norm_site_year$LST_diff[day_4:(day_5 - 1)], na.rm = TRUE), 2)
      sum_diff_56 <- round(sum(df1_LST_norm_site_year$LST_diff[day_5:(day_6)], na.rm = TRUE), 2)
      sum_diff_16 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      
      # Calculate number of days in each phase
      days_12 <- day_2 - day_1
      days_23 <- day_3 - day_2
      days_34 <- day_4 - day_3
      days_45 <- day_5 - day_4
      days_56 <- day_6 - day_5 + 1
      days_16 <- day_6 - day_1 + 1
      
      # Compute mean temperature differences for each phase (already validated)
      mean_diff_12 <- round(sum_diff_12 / days_12, 2)
      mean_diff_23 <- round(sum_diff_23 / days_23, 2)
      mean_diff_34 <- round(sum_diff_34 / days_34, 2)
      mean_diff_45 <- round(sum_diff_45 / days_45, 2)
      mean_diff_56 <- round(sum_diff_56 / days_56, 2)
      mean_diff_16 <- round(sum_diff_16 / days_16, 2)
      
      # Store results
      results <- rbind(results, data.frame(site_id, Year = year,
                                           ME_1 = ME_1, R_1 = R_1, ME_2 = ME_2, R_2 = R_2, LST_maxdiff = LST_maxdiff,
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
    
    # Export results to CSV
    write.csv(results, file = "./AmerifluxData_Analysis/Normal_results.csv", row.names = FALSE)
    
    
    