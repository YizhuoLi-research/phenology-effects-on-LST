###### 0. Load packages ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###########################################    01 Analysis of NOEN Site Data   ################################################

# Purpose: This section performs two ATC (Annual Temperature Cycle) fittings to analyze the temperature dynamics 
# in relation to phenological dates at the NOEN site, and calculates key vegetation-related temperature effects.
# The recommended LST for the Noen site is tau_corrected_surfaceTemp_97.


df1_LST_noen <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv")
df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

head(df1_LST_noen)

# Data frame to store final results
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
site_ids <- unique(df1_LST_noen$SiteID)
# years <- unique(df1_LST_noen$year)
 
for (site_id in site_ids) {
  
  # site_id <- 	"US-xML"
  # site_id <-  "US-xUK"
  
  df1_LST_noen_site <- df1_LST_noen[df1_LST_noen$SiteID == site_id, ]
  df1_LST_noen_site$LST <- as.numeric(df1_LST_noen_site$tau_corrected_surfaceTemp_97)
  df1_LST_noen_site$DOY <- as.numeric(df1_LST_noen_site$DOY)
  df1_LST_noen_site$year <- as.numeric(df1_LST_noen_site$year)
  
  df2_PHE_DOY_site <- df2_PHE_DOY[df2_PHE_DOY$SiteID == site_id, ]
  # df2_PHE_DOY_site$year <- as.numeric(df2_PHE_DOY$year)  # Convert the year column of df2_PHE_DOY to numeric type
  # str(df2_PHE_DOY)
  
years <- unique(df1_LST_noen_site$year)
  
  for (year in years) {
    # year <- 2017
    df1_LST_noen_site_year <- df1_LST_noen_site[df1_LST_noen_site$year == year, ]
    df2_PHE_DOY_site_year <- df2_PHE_DOY_site[ df2_PHE_DOY_site$year == year, ]
    
    # If the site has temperature data but no DOY data for the given year, skip the analysis
    # For example, "US-xUK" has temperature data for 2017 but no DOY data
    if (nrow(df2_PHE_DOY_site_year) == 0) {
      cat("Skipping year:", year, "because df2_PHE_DOY_site_year has no data\n")
      next  # Skip this year's analysis
    }
  
    # 5-day rolling average
    df1_LST_noen_site_year <- df1_LST_noen_site_year %>%
    mutate(LST_smoothed = rollmean(LST, k = 5, fill = NA, align = "center")) %>%
    filter(DOY >= 3 & DOY <= 363) %>%
    select(DOY, LST_smoothed) %>%
    rename(LST = LST_smoothed)

    
    
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
  
  # First fitting
  start <- list(tl = 20, da = 20, st = -0.5 * pi)
  lower <- c(0, 0, -pi)
  upper <- c(40, 40, pi)
  atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), data = df1_LST_noen_site_year,
               start = start, lower = lower, upper = upper,
               algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
  tl <- as.numeric(coef(atc)[1])
  da <- as.numeric(coef(atc)[2])
  st <- as.numeric(coef(atc)[3])
  fit_1 <- function(DOY, tl, da, st) { tl + da * sin(2 * pi * DOY / 365 + st) }
  
  # # Calculate RMSE and R for the first fitting
  lst_atc_1 <- na.omit(predict(atc))   # Predicted values from the fitted model
  lst_raw_1 <- na.omit(df1_LST_noen_site_year$LST)
  RMSE_1 = round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)), 2)
  R_1 = round(cor(lst_atc_1, lst_raw_1), 2)
  
  # # Find the maximum point of the sine component in the first fit and calculate the corresponding LST value
  DOY_max <- (pi/2 - st) * 365 / (2 * pi)
  LST_max <- fit_1(DOY_max, tl, da, st)  
  }
  
   #######################################
   #######################################
   # Second fitting: Set LST in the non-growing season to NA-representing a non-vagetation senario
   
    df2 <- df1_LST_noen_site_year
    df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA
    
    if (all(is.na(df2$LST))) {
      cat("Due to site", site_id, "in year", year, "having all data as missing values, further analysis cannot be performed.\n")
    } else {
      # Proceed with subsequent analysis code, e.g., site_id <- "US-xML" in 2017
      
      start2 <- list(tl2 = 20, da2 = 20)
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      atc2 <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2,
                    start = start2, lower = lower2, upper = upper2,
                    algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      tl2 <- as.numeric(coef(atc2)[1])
      da2 <- as.numeric(coef(atc2)[2])
      fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }
      
      # Calculate the differences and cumulative/average values for each phase
      df1_LST_noen_site_year$fit_2_value <- fit_2(df1_LST_noen_site_year$DOY, tl2, da2, st)
      
      # Calculate differences using actual values - fitted values
      df1_LST_noen_site_year$LST_diff <- df1_LST_noen_site_year$LST - df1_LST_noen_site_year$fit_2_value
      RMSE_2 <- round(sqrt(mean((df2$LST - predict(atc, df2))^2, na.rm = TRUE)), 2)
      R_2 <- round(cor(predict(atc, df2), df2$LST, use = "complete.obs"), 2)
      
      # Compute the maximum value points for both fittings
      # Maximum point for the second fitting
      DOY_max_2 <- (pi / 2 - st) * 365 / (2 * pi)
      # Calculate the corresponding LST value for the second fitting's maximum point
      LST_max_2 <- fit_2(DOY_max_2, tl2, da2, st)
      # Calculate the difference between the maximum points of the first and second fittings (second - first)
      LST_maxdiff <- round(LST_max_2 - LST_max, 2)
      
      # Compute differences and cumulative/average values for each stage
      # Average difference for each stage
      average_diff_21 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_1 + 1 & df1_LST_noen_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      average_diff_22 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_2 + 1 & df1_LST_noen_site_year$DOY <= day_2 + 5], na.rm = TRUE), 2)
      average_diff_23 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_3 + 1 & df1_LST_noen_site_year$DOY <= day_3 + 5], na.rm = TRUE), 2)
      average_diff_24 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_4 + 1 & df1_LST_noen_site_year$DOY <= day_4 + 5], na.rm = TRUE), 2)
      average_diff_25 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_5 + 1 & df1_LST_noen_site_year$DOY <= day_5 + 5], na.rm = TRUE), 2)
      average_diff_26 <- round(mean(df1_LST_noen_site_year$LST_diff[df1_LST_noen_site_year$DOY >= day_6 + 1 & df1_LST_noen_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      
      # Cumulative difference for each stage
      sum_diff_12 <- round(sum(df1_LST_noen_site_year$LST_diff[day_1:(day_2 - 1)], na.rm = TRUE), 2)
      sum_diff_23 <- round(sum(df1_LST_noen_site_year$LST_diff[day_2:(day_3 - 1)], na.rm = TRUE), 2)
      sum_diff_34 <- round(sum(df1_LST_noen_site_year$LST_diff[day_3:(day_4 - 1)], na.rm = TRUE), 2)
      sum_diff_45 <- round(sum(df1_LST_noen_site_year$LST_diff[day_4:(day_5 - 1)], na.rm = TRUE), 2)
      sum_diff_56 <- round(sum(df1_LST_noen_site_year$LST_diff[day_5:(day_6)], na.rm = TRUE), 2)
      sum_diff_16 <- round(sum(df1_LST_noen_site_year$LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      
      # Calculate the number of days for each stage
      days_12 <- day_2 - day_1
      days_23 <- day_3 - day_2
      days_34 <- day_4 - day_3
      days_45 <- day_5 - day_4
      days_56 <- day_6 - day_5 + 1
      days_16 <- day_6 - day_1 + 1
      
      # Calculate the average differences for each stage
      mean_diff_12 <- round(sum_diff_12 / days_12, 2)
      mean_diff_23 <- round(sum_diff_23 / days_23, 2)
      mean_diff_34 <- round(sum_diff_34 / days_34, 2)
      mean_diff_45 <- round(sum_diff_45 / days_45, 2)
      mean_diff_56 <- round(sum_diff_56 / days_56, 2)
      mean_diff_16 <- round(sum_diff_16 / days_16, 2)
      
      # Save results
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
  write.csv(results, file = "./AmerifluxData_Analysis/Noen_results.csv", row.names = FALSE)
  

# ########  Example: 单一站点调试 "US-xRN"  2018   #########
  # # ######## Example: Debugging for a single site "US-xRN" in 2018 #########
  # 
  # # Load data
  # df1_LST_noen <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv")
  # df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")
  # 
  # # View the first few rows of both datasets
  # head(df1_LST_noen)
  # head(df2_PHE_DOY)
  # 
  # # Convert the temperature data to numeric format
  # df1_LST_noen$LST <- as.numeric(df1_LST_noen$tau_corrected_surfaceTemp_97)
  # df1_LST_noen$DOY <- as.numeric(df1_LST_noen$DOY)
  # 
  # # Filter data for SiteID "US-Ton" and year 2018
  # df1_LST_noen_Site1 <- df1_LST_noen[df1_LST_noen$SiteID == "US-Ton" & df1_LST_noen$year == "2018", ]
  # 
  # # Apply a 5-day rolling average to smooth the temperature data
  # df1_LST_noen_Site1 <- df1_LST_noen_Site1 %>%
  #   arrange(DOY) %>%
  #   mutate(LST_smoothed = rollmean(LST, k = 5, fill = NA, align = "center"))
  # 
  # # Filter data to only keep DOY from 3 to 363 and select the smoothed LST data
  # df1_LST_noen_Site1 <- df1_LST_noen_Site1 %>%
  #   filter(DOY >= 3 & DOY <= 363) %>%
  #   select(DOY, LST_smoothed) %>%
  #   rename(LST = LST_smoothed)
  # 
  # # Extract phenological DOY values from another dataset
  # doy_values <- df2_PHE_DOY %>%
  #   filter(SiteID == "US-xRN", year == 2018) %>%
  #   select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
  #          falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
  #   unlist()
  # 
  # # Convert phenological DOYs to numeric values for further analysis
  # day_1 <- as.numeric(doy_values["rising_DOY_10"])
  # day_2 <- as.numeric(doy_values["rising_DOY_25"])
  # day_3 <- as.numeric(doy_values["rising_DOY_50"])
  # day_4 <- as.numeric(doy_values["falling_DOY_50"])
  # day_5 <- as.numeric(doy_values["falling_DOY_25"])
  # day_6 <- as.numeric(doy_values["falling_DOY_10"])
  # 
  # # First fitting:
  # # Define starting parameters and bounds for the nonlinear least squares fitting
  # start <- list(tl = 20, da = 20, st = -0.5 * pi)
  # lower <- c(0, 0, -pi)
  # upper <- c(40, 40, pi)
  # 
  # # Perform the nonlinear least squares fitting for the temperature data
  # atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), data = df1_LST_noen_Site1, start = start,
  #              lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
  # 
  # # Load the minpack.lm package for nonlinear least squares fitting
  # library(minpack.lm)
  # 
  # # Extract the fitted parameters from the model
  # f1 <- coef(atc)[1]
  # f2 <- coef(atc)[2]
  # f3 <- coef(atc)[3]
  # tl <- as.numeric(f1)
  # da <- as.numeric(f2)
  # st <- as.numeric(f3)
  # 
  # # Calculate the Root Mean Squared Error (RMSE) and R-squared (R) for the first fit
  # lst_atc_1 <- na.omit(predict(atc))   # Predicted values from the first fitting model
  # lst_raw_1 <- na.omit(df1_LST_noen_Site1$LST)  # Actual observed LST values
  # RMSE_1 = round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)), 2)
  # R_1 = round(cor(lst_atc_1, lst_raw_1), 2)
  # 
  # # Calculate the day of year (DOY) corresponding to the maximum temperature point
  # DOY_max <- (pi / 2 - st) * 365 / (2 * pi)
  # LST_max <- fit_1(DOY_max, tl, da, st)  # Calculate the maximum LST value corresponding to DOY_max
  # # Output the results for verification (this line is commented out)
  # # cat("DOY corresponding to maximum point:", round(DOY_max, 2), "\n")
  # # cat("Maximum LST value:", round(LST_max, 2), "\n")
  # 
  # #########################################
  # # # Second Fit: Set non-growing season LST values to NA
  # df2 <- df1_LST_noen_Site1
  # df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA
  # 
  # # Second fit parameter setup
  # start <- list(tl2=20,da2=20)
  # lower=c(0,0)
  # upper=c(40,40)
  # atc <- nlsLM(LST~tl2+da2*sin(2*pi*DOY/365+st),data=df2,start=start,
  #              lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 10^5))
  # # Extract the parameters of the second fit
  # f1=coef(atc)[1]
  # f2=coef(atc)[2]
  # #f3=coef(atc)[3]
  # tl2 <- as.numeric(f1)
  # da2 <- as.numeric(f2)
  # 
  # # Print the second fit results
  # print(atc)
  # 
  # # Define the prediction function for the second fit
  # fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }
  # 
  # # Output the final parameters of the fit
  # # cat("First fit parameters: tl =", tl, ", da =", da, ", st =", st, "\n")
  # # cat("Second fit parameters: tl2 =", tl2, ", da2 =", da2, ", st =", st, "\n")
  # 
  # # Calculate RMSE and R for the second fit
  # RMSE_2<- round(sqrt(mean((df2$LST- predict(atc,df2))^2, na.rm = TRUE)), 2)
  # R_2 <- round(cor(predict(atc,df2), df2$LST, use = "complete.obs"), 2)
  # 
  # 
  # # Calculate the difference values
  # df1_LST_noen_Site1$fit_2_value <- fit_2(df1_LST_noen_Site1$DOY, tl2, da2,st)
  # df1_LST_noen_Site1$fit_1_value <- fit_1(df1_LST_noen_Site1$DOY, tl, da, st)
  # 
  # # Calculate vegetation effect as Actual value - Fitted value 
  # df1_LST_noen_Site1$LST_diff <- df1_LST_noen_Site1$LST - df1_LST_noen_Site1$fit_2_value
  # 
  # # 5 days after phenological transition
  # average_diff_21 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_1+1 & df1_LST_noen_Site1$DOY <= day_1+5], na.rm = TRUE)
  # average_diff_22 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_2+1 & df1_LST_noen_Site1$DOY <= day_2+5], na.rm = TRUE)
  # average_diff_23 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_3+1 & df1_LST_noen_Site1$DOY <= day_3+5], na.rm = TRUE)
  # average_diff_24 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_4+1 & df1_LST_noen_Site1$DOY <= day_4+5], na.rm = TRUE)
  # average_diff_25 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_5+1 & df1_LST_noen_Site1$DOY <= day_5+5], na.rm = TRUE)
  # average_diff_26 <- mean(df1_LST_noen_Site1$LST_diff[df1_LST_noen_Site1$DOY >= day_6+1 & df1_LST_noen_Site1$DOY <= day_6+5], na.rm = TRUE)
  # 
  # # Cumulative over phases
  # sum_diff_12 <- sum(df1_LST_noen_Site1$LST_diff[day_1:(day_2-1)],na.rm = T)
  # sum_diff_23 <- sum(df1_LST_noen_Site1$LST_diff[day_2:(day_3-1)],na.rm = T)
  # sum_diff_34 <- sum(df1_LST_noen_Site1$LST_diff[day_3:(day_4-1)],na.rm = T)
  # sum_diff_45 <- sum(df1_LST_noen_Site1$LST_diff[day_4:(day_5-1)],na.rm = T)
  # sum_diff_56 <- sum(df1_LST_noen_Site1$LST_diff[day_5:day_6],na.rm = T)
  # sum_diff_16 <- sum(df1_LST_noen_Site1$LST_diff[day_1:day_6],na.rm = T)
  # 
  # ##### Average over phases
  # # Days in each phase
  # days_12 <- day_2 - day_1
  # days_23 <- day_3 - day_2
  # days_34 <- day_4 - day_3
  # days_45 <- day_5 - day_4
  # days_56 <- day_6 - day_5
  # days_16 <- day_6 - day_1
  # 
  # # Mean values
  # if (days_12 > 0) { sum_diff_12_mean <- sum_diff_12 / days_12 } else { sum_diff_12_mean <- NA   }
  # if (days_23 > 0) { sum_diff_23_mean <- sum_diff_23 / days_23 } else { sum_diff_23_mean <- NA   }
  # if (days_34 > 0) { sum_diff_34_mean <- sum_diff_34 / days_34 } else { sum_diff_34_mean <- NA   }
  # if (days_45 > 0) { sum_diff_45_mean <- sum_diff_45 / days_45 } else { sum_diff_45_mean <- NA   }
  # if (days_56 > 0) { sum_diff_56_mean <- sum_diff_56 / days_56 } else { sum_diff_56_mean <- NA   }
  # if (days_16 > 0) { sum_diff_16_mean <- sum_diff_16 / days_16 } else { sum_diff_16_mean <- NA   }
  # 
  # # Output average values
  # cat("sum_diff_12_mean:", sum_diff_12_mean, "\n")
  # cat("sum_diff_23_mean:", sum_diff_23_mean, "\n")
  # cat("sum_diff_34_mean:", sum_diff_34_mean, "\n")
  # cat("sum_diff_45_mean:", sum_diff_45_mean, "\n")
  # cat("sum_diff_56_mean:", sum_diff_56_mean, "\n")
  # cat("sum_diff_16_mean:", sum_diff_16_mean, "\n")
  # 
  # 
  # ################ Difference between the highest points of the two fits: (Second fit - First fit)
  # # Calculation of the highest value point for the second fit
  # DOY_max_2 <- (pi/2 - st) * 365 / (2 * pi)
  # 
  # # Calculate the LST value at the highest point of the second fit
  # LST_max_2 <- fit_2(DOY_max_2, tl2, da2,st)
  # # Calculate the difference with the first fit (Second - First)
  # LST_diff <- round(LST_max_2 - LST_max, 2)
  # 
  # # Output the result
  # cat("Difference in LST at the highest points of the two fits:", LST_diff, "\n")
  # 
  # 
  # # Create a data frame containing all specified variables
  # output_df <- data.frame(
  #   SiteID = "US-xRN",
  #   year = 2018,
  #   average_diff_21 = average_diff_21,
  #   average_diff_22 = average_diff_22,
  #   average_diff_23 = average_diff_23,
  #   average_diff_24 = average_diff_24,
  #   average_diff_25 = average_diff_25,
  #   average_diff_26 = average_diff_26,
  #   days_12 = days_12,
  #   days_23 = days_23,
  #   days_34 = days_34,
  #   days_45 = days_45,
  #   days_56 = days_56,
  #   days_16 = days_16,
  #   RMSE_1 = RMSE_1,
  #   R_1 = R_1,
  #   RMSE_2 = RMSE_2,
  #   R_2 = R_2,
  #   LST_diff = sum(df1_LST_noen_Site1$LST_diff, na.rm = TRUE) # Cumulative LST difference
  # )
  # 
  # # Print the data frame
  # print(output_df)
  