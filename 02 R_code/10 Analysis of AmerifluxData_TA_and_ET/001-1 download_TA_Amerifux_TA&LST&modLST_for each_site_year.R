###### 0. Load packages ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")

###################   01 Integrate TA and LST data from AmeriFlux Noen and Normal sites   ###################

AF_LST_noen <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv")
AF_LST_normal <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
files_TA <- "D:/VegetationImpact/01 Download/05 AmerifluxData/AmeriFlux_TA/TA_Data_Organized/"

# Define the target site list
site_list <- c("US-MOz", "US-SSH", "US-UMB", "US-UMd", "US-WCr", 
               "US-xBL", "US-xBR", "US-xGR", "US-xHA", "US-xLE", 
               "US-xML", "US-xRN", "US-xSC", "US-xSE", "US-xST", 
               "US-xUK", "US-xUN")

# Filter the datasets to include only the selected sites
AF_LST_noen_filtered <- AF_LST_noen[AF_LST_noen$SiteID %in% site_list, ]
num_sites <- length(unique(AF_LST_noen_filtered$SiteID))
# cat("Number of sites after filtering:", num_sites, "\n")  # 11

AF_LST_normal_filtered <- AF_LST_normal[AF_LST_normal$SiteID %in% site_list, ]
num_sites <- length(unique(AF_LST_normal_filtered$SiteID))
# cat("Number of sites after filtering:", num_sites, "\n")  # 6

# Convert key columns to numeric and standardize column names
AF_LST_noen_filtered$AF_TA <- as.numeric(AF_LST_noen_filtered$TA)
AF_LST_noen_filtered$AF_LST <- as.numeric(AF_LST_noen_filtered$tau_corrected_surfaceTemp_97)
AF_LST_noen_filtered$DOY <- as.numeric(AF_LST_noen_filtered$DOY)
AF_LST_noen_filtered$year <- as.numeric(AF_LST_noen_filtered$year)
names(AF_LST_noen_filtered)

AF_LST_normal_filtered$AF_TA <- as.numeric(AF_LST_normal_filtered$TA)
AF_LST_normal_filtered$AF_LST <- as.numeric(AF_LST_normal_filtered$corrected_surfaceTemp_97)
AF_LST_normal_filtered$DOY <- as.numeric(AF_LST_normal_filtered$DOY)
AF_LST_normal_filtered$year <- as.numeric(AF_LST_normal_filtered$year)
names(AF_LST_normal_filtered)

# Combine neon and normal datasets and retain selected columns
AmeriFlux_TA_LST <- bind_rows(AF_LST_noen_filtered, AF_LST_normal_filtered)

AmeriFlux_TA_LST <- AmeriFlux_TA_LST %>%
  dplyr::select(SiteID, TIMESTAMP_START, TIMESTAMP_END, date, year, DOY, AF_TA, AF_LST)

write.csv(
  AmeriFlux_TA_LST,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv",
  row.names = FALSE
)

###################   02 Process MODIS_site_LST data   ###################

folder_path <- "D:/VegetationImpact/01 Download/05 AmerifluxData/Get_AmeriFlux_MODIS_LST/"
file_list <- list.files(folder_path, full.names = TRUE)

# Initialize a list to store data grouped by site
site_data_list <- list()

for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Extract year and site ID
  year <- substr(file_name, 1, 4)
  site_id <- substr(file_name, 6, 11)
  
  # Read the file
  df <- read.csv(file_path)
  
  # Add SiteID and year columns
  df$SiteID <- site_id
  df$year <- year
  
  # Append to the corresponding site's data
  if (site_id %in% names(site_data_list)) {
    site_data_list[[site_id]] <- bind_rows(site_data_list[[site_id]], df)
  } else {
    site_data_list[[site_id]] <- df
  }
}

# Combine all site data into a single data frame
MOD_LST_all <- bind_rows(site_data_list)
head(MOD_LST_all)
# View(site_data_list[["US-MOz"]])

# Remove unnecessary columns: system.index and .geo
MOD_LST_all_clean <- MOD_LST_all %>%
  select(-system.index, -.geo)

# Create a complete list of DOY from 1 to 365
all_doy <- data.frame(DOY = c(1:365))

######### Fill missing DOY rows for each year and site, ensuring DOY 1, 2, 364, 365 are included #########
MOD_LST_all_complete <- MOD_LST_all_clean %>%
  complete(SiteID, year, DOY = c(1:365), fill = list(LST = NA)) %>%
  arrange(SiteID, year, DOY)

# Check results
head(MOD_LST_all_complete)

write.csv(
  MOD_LST_all_complete,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--Modis_for_siteLST.csv",
  row.names = FALSE
)



###################   03 整合 AmeriFlux（TA LST- unsmoothed）和MODIS(LST-smoothed)数据  ################################################

AmeriFlux_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv")
MOD_LST_all <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--Modis_for_siteLST.csv")

# 合并两个数据框
AF_MOD_merged_TA_LST <- merge(
  MOD_LST_all,
  AmeriFlux_TA_LST[, c("SiteID", "year", "DOY", "AF_TA", "AF_LST")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# 重命名MODIS的 LST 列
colnames(AF_MOD_merged_TA_LST)[colnames(AF_MOD_merged_TA_LST) == "LST"] <- "MOD_LST_smoothed"

# 查看前几行确认合并效果
head(AF_MOD_merged_TA_LST)


write.csv(
  AF_MOD_merged_TA_LST,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv",
  row.names = FALSE
)

###################      04补充： 补充我下载的全序列AT和LE数据到 TA_Jen    ############### 
# 用我下载的TA——download 滑动平均和ATC拟合后，做1：1回归线


############################   检查我的TA VS Jen的TA 整合数据 ####################################
# 这里的TA是我自己下载的通量塔最高点的TA ————对比一下！！！！！！！！！！！！！！！！！！！！！！！！！
TA_Jen <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv")

AmeriFlux_TA_LE_download <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv")

colnames(AmeriFlux_TA_LE_download)
colnames(TA_Jen)


# 合并AmeriFlux_TA_LE_all中的TA列到TA_Jen中
# 合并两个数据框
merge_df <- merge(
  AmeriFlux_TA_LE_download,
  TA_Jen[, c("SiteID", "year", "DOY", "AF_TA","AF_LST","MOD_LST_smoothed")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)


write.csv(
  merge_df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST_withDownload_TALE.csv",
  row.names = FALSE
)

###################   03 Merge AmeriFlux (TA, LST - unsmoothed) and MODIS (LST - smoothed) data   ###################

AmeriFlux_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AmeriFlux_TA_LST.csv")
MOD_LST_all <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--Modis_for_siteLST.csv")

# Merge the two data frames
AF_MOD_merged_TA_LST <- merge(
  MOD_LST_all,
  AmeriFlux_TA_LST[, c("SiteID", "year", "DOY", "AF_TA", "AF_LST")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

# Rename the MODIS LST column
colnames(AF_MOD_merged_TA_LST)[colnames(AF_MOD_merged_TA_LST) == "LST"] <- "MOD_LST_smoothed"

# View first few rows to verify the merge
head(AF_MOD_merged_TA_LST)

write.csv(
  AF_MOD_merged_TA_LST,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv",
  row.names = FALSE
)

###################   04 Supplement: Add full-sequence AT and LE (downloaded) data to TA_Jen   ###################
# Use my downloaded TA (from flux tower top) with smoothing and ATC fitting to perform 1:1 regression

############################   Check TA from my download vs Jen's TA merged data   ############################
# The TA here is my downloaded top-of-canopy air temperature from flux tower — perform comparison!

TA_Jen <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST.csv")

AmeriFlux_TA_LE_download <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--1.AF_ET_TA_download_Filtered.csv")

colnames(AmeriFlux_TA_LE_download)
colnames(TA_Jen)

# Merge TA column from AmeriFlux_TA_LE_download into TA_Jen
# Merge the two data frames
merge_df <- merge(
  AmeriFlux_TA_LE_download,
  TA_Jen[, c("SiteID", "year", "DOY", "AF_TA","AF_LST","MOD_LST_smoothed")],
  by = c("SiteID", "year", "DOY"),
  all.x = TRUE
)

write.csv(
  merge_df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST_withDownload_TALE.csv",
  row.names = FALSE
)

###################   04 Fitting: calculate post-SOS and post-EOS 5-day mean temperature and impact for 3 temperature types   ###################

AF_MOD_merged_TA_LST <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--AF_MOD_merged_TA_LST_withDownload_TALE.csv")
head(AF_MOD_merged_TA_LST)

df_AF_TA <- AF_MOD_merged_TA_LST[, c("SiteID", "year", "DOY", "AF_TA")]
df_AF_LST <- AF_MOD_merged_TA_LST[, c("SiteID", "year", "DOY", "AF_LST")]
df_download_TA <- AF_MOD_merged_TA_LST[, c("SiteID", "year", "DOY", "TA_download")]
df_MOD_LST <- AF_MOD_merged_TA_LST[, c("SiteID", "year", "DOY", "MOD_LST_smoothed")]

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
  download_TA_average_diff_1 = numeric(), 
  download_TA_average_diff_6 = numeric(),
  
  AF_TA_sum_diff_16 = numeric(),
  AF_LST_sum_diff_16 = numeric(),
  MOD_LST_sum_diff_16 = numeric(),
  download_TA_sum_diff_16 = numeric(), 
  
  AF_5meanTA_post_SOS = numeric(),
  AF_5meanTA_post_EOS = numeric(),
  AF_5meanLST_post_SOS  = numeric(),
  AF_5meanLST_post_EOS  = numeric(),
  MOD_5meanLST_post_SOS = numeric(),
  MOD_5meanLST_post_EOS = numeric(),
  download_5meanTA_post_SOS = numeric(),
  download_5meanTA_post_EOS = numeric(),
  
  ME_1_AF_TA = numeric(), R_1_AF_TA = numeric(),
  ME_1_AF_LST = numeric(), R_1_AF_LST = numeric(),
  ME_1_MOD_LST = numeric(), R_1_MOD_LST = numeric(),
  ME_1_download_TA = numeric(), R_1_download_TA = numeric(),
  
  ME_2_AF_TA = numeric(), R_2_AF_TA = numeric(), maxdiff_AF_TA = numeric(),
  ME_2_AF_LST = numeric(), R_2_AF_LST = numeric(), maxdiff_AF_LST = numeric(),
  ME_2_MOD_LST = numeric(), R_2_MOD_LST = numeric(), maxdiff_MOD_LST = numeric(),
  ME_2_download_TA = numeric(), R_2_download_TA = numeric(), maxdiff_download_TA = numeric(),
  
  stringsAsFactors = FALSE
)

# Loop through each site and year

site_ids <- unique(df_AF_TA$SiteID)

for (site_id in site_ids) {
  
  # site_id <-  "US-MOz"
  # site_id <-  "US-xML"
  
  df_AF_TA_site <- df_AF_TA[df_AF_TA$SiteID == site_id, ]
  df_AF_TA_site$AF_TA <- as.numeric(df_AF_TA_site$AF_TA)
  
  df_AF_LST_site <- df_AF_LST[df_AF_LST$SiteID == site_id, ]
  df_AF_LST_site$AF_LST <- as.numeric(df_AF_LST_site$AF_LST)
  
  df_MOD_LST_site <- df_MOD_LST[df_MOD_LST$SiteID == site_id, ]
  df_MOD_LST_site$MOD_LST_smoothed <- as.numeric(df_MOD_LST_site$MOD_LST_smoothed)
  
  df_download_TA_site <- df_download_TA[df_download_TA$SiteID == site_id, ]
  df_download_TA_site$TA_download <- as.numeric(df_download_TA_site$TA_download)
  
  AF_PHE_DOY_site <- AF_PHE_DOY[AF_PHE_DOY$SiteID == site_id, ]
  
  years <- unique(df_AF_TA_site$year)
  
  for (year in years) {
    
    df_AF_TA_site_year <- df_AF_TA_site[df_AF_TA_site$year == year, ]
    df_AF_LST_site_year <- df_AF_LST_site[df_AF_LST_site$year == year, ]
    df_MOD_LST_site_year <- df_MOD_LST_site[df_MOD_LST_site$year == year, ]
    df_download_TA_site_year <- df_download_TA_site[df_download_TA_site$year == year, ]
    
    AF_PHE_DOY_site_year <- AF_PHE_DOY_site[AF_PHE_DOY_site$year == year, ]
    
    # 1. Skip if all values are NA in any of the four temperature datasets
    if (all(is.na(df_AF_TA_site_year$AF_TA)) | 
        all(is.na(df_AF_LST_site_year$AF_LST)) | 
        all(is.na(df_MOD_LST_site_year$MOD_LST_smoothed)) | 
        all(is.na(df_download_TA_site_year$TA_download))) {
      cat("Skipping site:", site_id, "year:", year, "because temperature values are all NA\n")
      next
    }
    
    # 2. Skip if phenology data is missing for the year
    if (nrow(AF_PHE_DOY_site_year) == 0) {
      cat("Skipping site:", site_id,"year:" ,year,  "due to missing phenology data\n") 
      next
    }
    
    # 5-day moving average
    ## 1. For df_AF_TA_site_year
    # Step 1: Build full DOY table from 1 to 365 and left join
    df_full_AF_TA_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_AF_TA_site_year, by = "DOY")
    # Step 2: Calculate 5-day moving average (allow NA)
    df_AF_TA_site_year <- df_full_AF_TA_site_year %>%
      mutate(
        AF_TA = as.numeric(AF_TA),
        AF_TA_smoothed = rollapply(AF_TA, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                   fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, AF_TA = AF_TA_smoothed)
    
    ## 2. For df_AF_LST_site_year
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
    ## 4. For df_download_TA_site_year
    df_full_download_TA_site_year <- data.frame(DOY = 1:365) %>%
      left_join(df_download_TA_site_year, by = "DOY")
    df_download_TA_site_year <- df_full_download_TA_site_year %>%
      mutate(
        TA_download = as.numeric(TA_download),
        TA_download_smoothed = rollapply(TA_download, width = 5, FUN = function(x) mean(x, na.rm = TRUE),
                                         fill = NA, align = "center")
      ) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      dplyr::select(DOY, TA_download = TA_download_smoothed)
    
    ## Extract phenological transition DOY for the current year
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
    
    # First curve fitting
    start_1 <- list(aa = 20, bb = 20, cc = -0.5 * pi)  ## 1. For AF_TA
    start_2 <- list(ee = 20, ff = 20, gg = -0.5 * pi)  ## 2. For AF_LST
    start_3 <- list(hh = 20, ii = 20, jj = -0.5 * pi)  ## 3. For MOD_LST
    start_4 <- list(kk = 20, mm = 20, nn = -0.5 * pi)  ## 4. For download_TA
    
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    
    # 1. Fit for AF_TA
    atc_AF_TA <- nlsLM(AF_TA ~ aa + bb * sin(2 * pi * DOY / 365 + cc), data = df_AF_TA_site_year,
                       start = start_1, lower = lower, upper = upper,
                       algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    aa <- as.numeric(coef(atc_AF_TA)[1])
    bb <- as.numeric(coef(atc_AF_TA)[2])
    cc <- as.numeric(coef(atc_AF_TA)[3])
    fit_1_atc_AF_TA <- function(DOY, aa, bb, cc) { aa + bb * sin(2 * pi * DOY / 365 + cc) }
    
    # 2. Fit for AF_LST
    atc_AF_LST <- nlsLM(AF_LST ~ ee + ff * sin(2 * pi * DOY / 365 + gg), data = df_AF_LST_site_year,
                        start = start_2, lower = lower, upper = upper,
                        algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    ee <- as.numeric(coef(atc_AF_LST)[1])
    ff <- as.numeric(coef(atc_AF_LST)[2])
    gg <- as.numeric(coef(atc_AF_LST)[3])
    fit_1_atc_AF_LST <- function(DOY, ee, ff, gg) { ee + ff * sin(2 * pi * DOY / 365 + gg) }
    
    # 3. Fit for MOD_LST
    atc_MOD_LST <- nlsLM(MOD_LST_smoothed ~ hh + ii * sin(2 * pi * DOY / 365 + jj), data = df_MOD_LST_site_year,
                         start = start_3, lower = lower, upper = upper,
                         algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    hh <- as.numeric(coef(atc_MOD_LST)[1])
    ii <- as.numeric(coef(atc_MOD_LST)[2])
    jj <- as.numeric(coef(atc_MOD_LST)[3])
    fit_1_atc_MOD_LST <- function(DOY, hh, ii, jj) { hh + ii * sin(2 * pi * DOY / 365 + jj) }
    
    # 4. Fit for download_TA
    atc_download_TA <- nlsLM(TA_download ~ kk + mm * sin(2 * pi * DOY / 365 + nn), data = df_download_TA_site_year,
                             start = start_4, lower = lower, upper = upper,
                             algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    kk <- as.numeric(coef(atc_download_TA)[1])
    mm <- as.numeric(coef(atc_download_TA)[2])
    nn <- as.numeric(coef(atc_download_TA)[3])
    fit_1_atc_download_TA <- function(DOY, kk, mm, nn) { kk + mm * sin(2 * pi * DOY / 365 + nn) }
    
      
      
    # Calculate ME and R for the first fitting
    # 1. For AF_TA
    lst_atc_1_AF_TA <- na.omit(predict(atc_AF_TA))   # Predicted values from the fitted model
    lst_raw_1_AF_TA <- na.omit(df_AF_TA_site_year$AF_TA)
    ME_1_AF_TA = round(mean((lst_raw_1_AF_TA - lst_atc_1_AF_TA)), 2)
    R_1_AF_TA = round(cor(lst_atc_1_AF_TA, lst_raw_1_AF_TA), 2)
    # Find the peak DOY of the sine curve from the first fitting
    DOY_max_AF_TA <- (pi/2 - cc) * 365 / (2 * pi)
    AF_TA_max <- fit_1_atc_AF_TA(DOY_max_AF_TA, aa, bb, cc)
    
    # 2. For AF_LST
    lst_atc_1_AF_LST <- na.omit(predict(atc_AF_LST))   # Predicted values from the fitted model
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
    
    # 4. For download_TA
    lst_atc_1_download_TA <- na.omit(predict(atc_download_TA))
    lst_raw_1_download_TA <- na.omit(df_download_TA_site_year$TA_download)
    ME_1_download_TA = round(mean((lst_raw_1_download_TA - lst_atc_1_download_TA)), 2)
    R_1_download_TA = round(cor(lst_atc_1_download_TA, lst_raw_1_download_TA), 2)
    DOY_max_download_TA <- (pi/2 - nn) * 365 / (2 * pi)
    download_TA_max <- fit_1_atc_download_TA(DOY_max_download_TA, kk, mm, nn)
    
    ####################################### Second fitting
    # Second fitting: set non-growing-season LST values to NA
    df2_AF_TA <- df_AF_TA_site_year
    df2_AF_TA$AF_TA[df2_AF_TA$DOY > day_1 & df2_AF_TA$DOY < day_6] <- NA
    
    df2_AF_LST <- df_AF_LST_site_year
    df2_AF_LST$AF_LST[df2_AF_LST$DOY > day_1 & df2_AF_LST$DOY < day_6] <- NA
    
    df2_MOD_LST <- df_MOD_LST_site_year
    df2_MOD_LST$MOD_LST_smoothed[df2_MOD_LST$DOY > day_1 & df2_MOD_LST$DOY < day_6] <- NA
    
    df2_download_TA <- df_download_TA_site_year
    df2_download_TA$TA_download[df2_download_TA$DOY > day_1 & df2_download_TA$DOY < day_6] <- NA
    
    if (all(is.na(df2_AF_TA$AF_TA)) | 
        all(is.na(df2_AF_LST$AF_LST)) | 
        all(is.na(df2_MOD_LST$MOD_LST_smoothed))| 
        all(is.na(df2_download_TA$TA_download))) {
      cat("Skipping site", site_id, "year", year, "because all non-growing-season data are NA.\n")
    } else {
      
      # Continue with further analysis
      # For example, site_id <- "US-xML" in year 2017
      
      # Second fitting
      start2_1 <- list(aa2 = 20, bb2 = 20)
      start2_2 <- list(ee2 = 20, ff2 = 20)
      start2_3 <- list(hh2 = 20, ii2 = 20)
      start2_4 <- list(kk2 = 20, mm2 = 20)
      
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
      
      # 3. For MOD_LST
      atc2_MOD_LST <- nlsLM(MOD_LST_smoothed ~ hh2 + ii2 * sin(2 * pi * DOY / 365 + jj), data = df2_MOD_LST,
                            start = start2_3, lower = lower2, upper = upper2,
                            algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      hh2 <- as.numeric(coef(atc2_MOD_LST)[1])
      ii2 <- as.numeric(coef(atc2_MOD_LST)[2])
      fit_2_atc_MOD_LST <- function(DOY, hh2, ii2, jj) { hh2 + ii2 * sin(2 * pi * DOY / 365 + jj) }  
      
      # 4. For download_TA
      atc2_download_TA <- nlsLM(TA_download ~ kk2 + mm2 * sin(2 * pi * DOY / 365 + nn), data = df2_download_TA,
                                start = start2_4, lower = lower2, upper = upper2,
                                algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      kk2 <- as.numeric(coef(atc2_download_TA)[1])
      mm2 <- as.numeric(coef(atc2_download_TA)[2])
      fit_2_atc_download_TA <- function(DOY, kk2, mm2, nn) { kk2 + mm2 * sin(2 * pi * DOY / 365 + nn) }
      
      # Compute differences and seasonal accumulations/averages
      # Generate predicted values on the full DOY range for difference calculation
      df_AF_TA_site_year$fit_2_AF_TA <- fit_2_atc_AF_TA(df_AF_TA_site_year$DOY, aa2, bb2, cc)
      df_AF_LST_site_year$fit_2_AF_LST <- fit_2_atc_AF_LST(df_AF_LST_site_year$DOY, ee2, ff2, gg)
      df_MOD_LST_site_year$fit_2_MOD_LST <- fit_2_atc_MOD_LST(df_MOD_LST_site_year$DOY, hh2, ii2, jj)
      df_download_TA_site_year$fit_2_download_TA <- fit_2_atc_download_TA(df_download_TA_site_year$DOY, kk2, mm2, nn)
      
      # Calculate difference: actual - fitted
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
      
      # 4. For download_TA
      df_download_TA_site_year$TA_download_diff <- df_download_TA_site_year$TA_download - df_download_TA_site_year$fit_2_download_TA
      ME_2_download_TA <- round(mean((df2_download_TA$TA_download - predict(atc2_download_TA, df2_download_TA)), na.rm = TRUE), 2)
      R_2_download_TA <- round(cor(predict(atc2_download_TA, df2_download_TA), df2_download_TA$TA_download, use = "complete.obs"), 2)
      
      
     # Calculate the peak value point for the first and second fits

     # 1. AF_TA
      # Calculate the peak point of the second fit
      DOY_max_2_AF_TA <- (pi/2 - cc) * 365 / (2 * pi)
      # Calculate LST value at the peak of the second fit
      AF_TA_max_2 <- fit_2_atc_AF_TA(DOY_max_2_AF_TA, aa2, bb2, cc)
      # Compute the difference between first and second fit peaks (second - first)
      maxdiff_AF_TA <- round(AF_TA_max_2 - AF_TA_max, 2)

      # 2. AF_LST
      DOY_max_2_AF_LST <- (pi/2 - gg) * 365 / (2 * pi)
      AF_LST_max_2 <- fit_2_atc_AF_LST(DOY_max_2_AF_LST, ee2, ff2, gg)
      maxdiff_AF_LST <- round(AF_LST_max_2 - AF_LST_max, 2)

      # 3. MOD_LST
      DOY_max_2_MOD_LST <- (pi/2 - jj) * 365 / (2 * pi)
      MOD_LST_max_2 <- fit_2_atc_MOD_LST(DOY_max_2_MOD_LST, hh2, ii2, jj)
      maxdiff_MOD_LST <- round(MOD_LST_max_2 - MOD_LST_max, 2)

      # 4. download_TA
      DOY_max_2_download_TA <- (pi/2 - cc) * 365 / (2 * pi)
      download_TA_max_2 <- fit_2_atc_download_TA(DOY_max_2_download_TA, aa2, bb2, cc)
      maxdiff_download_TA <- round(download_TA_max_2 - download_TA_max, 2)

      # Compute 5-day post-SOS and post-EOS average differences
      AF_TA_average_diff_1 <- round(mean(df_AF_TA_site_year$AF_TA_diff[df_AF_TA_site_year$DOY >= day_1 + 1 & df_AF_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_TA_average_diff_6 <- round(mean(df_AF_TA_site_year$AF_TA_diff[df_AF_TA_site_year$DOY >= day_6 + 1 & df_AF_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      AF_LST_average_diff_1 <- round(mean(df_AF_LST_site_year$AF_LST_diff[df_AF_LST_site_year$DOY >= day_1 + 1 & df_AF_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_LST_average_diff_6 <- round(mean(df_AF_LST_site_year$AF_LST_diff[df_AF_LST_site_year$DOY >= day_6 + 1 & df_AF_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      MOD_LST_average_diff_1 <- round(mean(df_MOD_LST_site_year$MOD_LST_diff[df_MOD_LST_site_year$DOY >= day_1 + 1 & df_MOD_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      MOD_LST_average_diff_6 <- round(mean(df_MOD_LST_site_year$MOD_LST_diff[df_MOD_LST_site_year$DOY >= day_6 + 1 & df_MOD_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      download_TA_average_diff_1 <- round(mean(df_download_TA_site_year$TA_download_diff[df_download_TA_site_year$DOY >= day_1 + 1 & df_download_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      download_TA_average_diff_6 <- round(mean(df_download_TA_site_year$TA_download_diff[df_download_TA_site_year$DOY >= day_6 + 1 & df_download_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)

      # Compute cumulative difference during phenological period (e.g., SOS to EOS)
      AF_TA_sum_diff_16 <- round(sum(df_AF_TA_site_year$AF_TA_diff[day_1:(day_6)], na.rm = TRUE), 2)
      AF_LST_sum_diff_16 <- round(sum(df_AF_LST_site_year$AF_LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      MOD_LST_sum_diff_16 <- round(sum(df_MOD_LST_site_year$MOD_LST_diff[day_1:(day_6)], na.rm = TRUE), 2)
      download_TA_sum_diff_16 <- round(sum(df_download_TA_site_year$TA_download_diff[day_1:(day_6)], na.rm = TRUE), 2)

      # Calculate 5-day post-SOS and post-EOS average temperature
      AF_5meanTA_post_SOS <- round(mean(df_AF_TA_site_year$AF_TA[df_AF_TA_site_year$DOY >= day_1 + 1 & df_AF_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_5meanTA_post_EOS <- round(mean(df_AF_TA_site_year$AF_TA[df_AF_TA_site_year$DOY >= day_6 + 1 & df_AF_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      AF_5meanLST_post_SOS <- round(mean(df_AF_LST_site_year$AF_LST[df_AF_LST_site_year$DOY >= day_1 + 1 & df_AF_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      AF_5meanLST_post_EOS <- round(mean(df_AF_LST_site_year$AF_LST[df_AF_LST_site_year$DOY >= day_6 + 1 & df_AF_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      MOD_5meanLST_post_SOS <- round(mean(df_MOD_LST_site_year$MOD_LST_smoothed[df_MOD_LST_site_year$DOY >= day_1 + 1 & df_MOD_LST_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      MOD_5meanLST_post_EOS <- round(mean(df_MOD_LST_site_year$MOD_LST_smoothed[df_MOD_LST_site_year$DOY >= day_6 + 1 & df_MOD_LST_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)
      download_5meanTA_post_SOS <- round(mean(df_download_TA_site_year$TA_download[df_download_TA_site_year$DOY >= day_1 + 1 & df_download_TA_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      download_5meanTA_post_EOS <- round(mean(df_download_TA_site_year$TA_download[df_download_TA_site_year$DOY >= day_6 + 1 & df_AF_TA_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)

      results <- rbind(results, data.frame(
        site_id ,Year = year,
        AF_TA_average_diff_1 = AF_TA_average_diff_1, 
        AF_TA_average_diff_6 = AF_TA_average_diff_6,
        AF_LST_average_diff_1 = AF_LST_average_diff_1, 
        AF_LST_average_diff_6 = AF_LST_average_diff_6,
        MOD_LST_average_diff_1 = MOD_LST_average_diff_1, 
        MOD_LST_average_diff_6 = MOD_LST_average_diff_6,
        download_TA_average_diff_1 = download_TA_average_diff_1, 
        download_TA_average_diff_6 = download_TA_average_diff_6,
  
  
        AF_TA_sum_diff_16 = AF_TA_sum_diff_16,
        AF_LST_sum_diff_16 = AF_LST_sum_diff_16,
        MOD_LST_sum_diff_16 = MOD_LST_sum_diff_16,  
        download_TA_sum_diff_16 = download_TA_sum_diff_16,
  
        AF_5meanTA_post_SOS  = AF_5meanTA_post_SOS,
        AF_5meanTA_post_EOS  = AF_5meanTA_post_EOS,
        AF_5meanLST_post_SOS = AF_5meanLST_post_SOS,
        AF_5meanLST_post_EOS = AF_5meanLST_post_EOS,
        MOD_5meanLST_post_SOS= MOD_5meanLST_post_SOS,
        MOD_5meanLST_post_EOS= MOD_5meanLST_post_EOS,
        download_5meanTA_post_SOS  = download_5meanTA_post_SOS,
        download_5meanTA_post_EOS  = download_5meanTA_post_EOS,
  
        ME_1_AF_TA = ME_1_AF_TA,R_1_AF_TA = R_1_AF_TA,
        ME_1_AF_LST = ME_1_AF_LST,R_1_AF_LST = R_1_AF_LST,
        ME_1_MOD_LST = ME_1_MOD_LST,R_1_MOD_LST = R_1_MOD_LST,
        ME_1_download_TA = ME_1_download_TA,R_1_download_TA = R_1_download_TA,
  
        ME_2_AF_TA = ME_2_AF_TA,R_2_AF_TA = R_2_AF_TA,maxdiff_AF_TA = maxdiff_AF_TA,
        ME_2_AF_LST = ME_2_AF_LST,R_2_AF_LST = R_2_AF_LST,maxdiff_AF_LST = maxdiff_AF_LST,
        ME_2_MOD_LST = ME_2_MOD_LST,R_2_MOD_LST = R_2_MOD_LST,maxdiff_MOD_LST = maxdiff_MOD_LST,
        ME_2_download_TA = ME_2_download_TA,R_2_download_TA = R_2_download_TA,maxdiff_download_TA = maxdiff_download_TA))

      # print(results)
    }
  }
}



write.csv(
  results,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_years_withDownload_TALE.csv",
  row.names = FALSE
) 

