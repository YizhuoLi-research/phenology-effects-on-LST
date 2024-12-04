###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

################################   01  计算DOY-NorthAmerica  ##################################
###############################################################################################         
# NA
# # SOS: phe1_DOY_.   ;  MGP: phe2_DOY_.    ；  GMO：phe3_DOY_.
# # GDO: phe4_DOY_.   ;  MSP: phe5_DOY_.    ；  EOS：phe6_DOY_.

# #  The code modifies the pattern in list.files() to match all six phenological
# #  events (phe1_DOY_ to phe6_DOY_) in a single run for processing.


file_list <- list.files("./NA_Results/0.phe_DOY/0common_pixel/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)

# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

# 打印结果
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

##########计算逐渐趋势
# 定义用于计算趋势的函数


calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }
  
  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }
  
  # 定义年份
  years <- 2013:2021  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)
  
  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # 取整
            round(max_value, 2),                           # 保留2位小数
            round(min_value, 2),                           # 保留2位小数
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # 保留2位小数
            round(max(trend_raster[], na.rm = TRUE), 2),   # 保留2位小数
            round(min(trend_raster[], na.rm = TRUE), 2) )  # 保留2位小数
)

# 打印结果表格
print(results_df)

# #SOS  phe1_DOY_.
# 1   Mean_DOY   119 ± 3.18
# 2    Max_DOY          184
# 3    Min_DOY           15
# 4 Mean_Trend -0.21 ± 0.25
# 5  Max_Trend           19
# 6  Min_Trend        -20.5

##MGP  phe2_DOY_.
# 1   Mean_DOY  141 ± 2.78
# 2    Max_DOY         198
# 3    Min_DOY          40
# 4 Mean_Trend 0.01 ± 0.19
# 5  Max_Trend        16.5
# 6  Min_Trend         -17

##GMO  phe3_DOY_.
# 1   Mean_DOY 162 ± 2.48
# 2    Max_DOY        213
# 3    Min_DOY       88.5
# 4 Mean_Trend 0.21 ± 0.2
# 5  Max_Trend       13.5
# 6  Min_Trend      -15.5

##GDO  phe4_DOY_.
# 1   Mean_DOY  226 ± 1.25
# 2    Max_DOY         272
# 3    Min_DOY       125.5
# 4 Mean_Trend 0.09 ± 0.19
# 5  Max_Trend          15
# 6  Min_Trend      -13.25

##MSP  phe5_DOY_.
# 1   Mean_DOY   261 ± 1.83
# 2    Max_DOY       298.67
# 3    Min_DOY          167
# 4 Mean_Trend -0.05 ± 0.15
# 5  Max_Trend        14.25
# 6  Min_Trend         -9.5

##EOS  phe6_DOY_.
# 1   Mean_DOY  296 ± 3.28
# 2    Max_DOY      348.75
# 3    Min_DOY         195
# 4 Mean_Trend -0.19 ± 0.2
# 5  Max_Trend          21
# 6  Min_Trend       -17.1
#####################################   02  计算DOY-Euraisa  #################################
###############################################################################################      
####Eurasia
file_list <- list.files("./EA_Results/0.phe_DOY/0common_pixel/",
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)


# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

##########计算逐渐趋势
# 定义用于计算趋势的函数

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }

  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }

  # 定义年份
  years <- 2013:2021  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)

  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY",
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # 取整
            round(max_value, 2),                           # 保留一位小数
            round(min_value, 2),                           # 保留一位小数
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # 保留一位小数
            round(max(trend_raster[], na.rm = TRUE), 2),   # 保留一位小数
            round(min(trend_raster[], na.rm = TRUE), 2) )  # 保留一位小数
)

# 打印结果表格
print(results_df)

##SOS  phe1_DOY_.
# 1   Mean_DOY  123 ± 3.04
# 2    Max_DOY         190
# 3    Min_DOY          21
# 4 Mean_Trend -0.1 ± 0.18
# 5  Max_Trend          32
# 6  Min_Trend       -19.5

##MGP  phe2_DOY_.
# 1   Mean_DOY  164 ± 2.08
# 2    Max_DOY         228
# 3    Min_DOY          95
# 4 Mean_Trend 0.06 ± 0.18
# 5  Max_Trend        17.5
# 6  Min_Trend         -18

##GMO  phe3_DOY_.
# 1   Mean_DOY 164 ± 2.1
# 2    Max_DOY       228
# 3    Min_DOY        95
# 4 Mean_Trend 0.1 ± 0.2
# 5  Max_Trend      17.5
# 6  Min_Trend       -18

##GDO   phe4_DOY_.
# 1   Mean_DOY  225 ± 1.03
# 2    Max_DOY         275
# 3    Min_DOY         122
# 4 Mean_Trend 0.01 ± 0.18
# 5  Max_Trend          20
# 6  Min_Trend       -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY  255 ± 1.92
# 2    Max_DOY         304
# 3    Min_DOY         150
# 4 Mean_Trend 0.12 ± 0.14
# 5  Max_Trend       11.25
# 6  Min_Trend         -28

##EOS  phe6_DOY_.
# 1   Mean_DOY  286 ± 3.42
# 2    Max_DOY         353
# 3    Min_DOY         178
# 4 Mean_Trend 0.24 ± 0.17
# 5  Max_Trend       16.25
# 6  Min_Trend       -37.5
############################   03  计算DOY-Northern Hemisphere  ###############################
###############################################################################################      
file_list <- list.files("./EA+NA_Results/merge_Phe_DOY_years/merged_phe6_DOY/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)


#phe1_DOY_. phe2_DOY_. phe3_DOY_. phe4_DOY_. phe5_DOY_. phe6_DOY_.

# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- cellStats(mean_raster, stat = 'max', na.rm = TRUE)
min_value <- cellStats(mean_raster, stat = 'min', na.rm = TRUE)
sd_value <- cellStats(mean_raster, stat = 'sd', na.rm = TRUE)
spatial_sd_variation <- round(0.15 * sd_value,2)

# 打印结果
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

##########计算逐渐趋势
# 定义用于计算趋势的函数

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }
  
  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }
  
  # 定义年份
  years <- 2013:2021  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)
  
  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2021)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),
            max_value,
            min_value,
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)),
            round(max(trend_raster[], na.rm = TRUE), 2),  
            round(min(trend_raster[], na.rm = TRUE), 2) )
)

# 打印结果表格
print(results_df)


##SOS  phe1_DOY_.
# 1   Mean_DOY  122 ± 3.09
# 2    Max_DOY         190
# 3    Min_DOY          15
# 4 Mean_Trend -0.13 ± 0.2
# 5  Max_Trend          32
# 6  Min_Trend       -20.5

# ##MGP  phe2_DOY_.
# 1   Mean_DOY   143 ± 2.55
# 2    Max_DOY          209
# 3    Min_DOY           40
# 4 Mean_Trend -0.02 ± 0.16
# 5  Max_Trend         16.5
# 6  Min_Trend          -17

##GMO  phe3_DOY_.
# 1   Mean_DOY 164 ± 2.19
# 2    Max_DOY        228
# 3    Min_DOY       88.5
# 4 Mean_Trend 0.1 ± 0.18
# 5  Max_Trend       17.5
# 6  Min_Trend        -18

##GDO  phe4_DOY_.
# 1   Mean_DOY  225 ± 1.09
# 2    Max_DOY         275
# 3    Min_DOY         122
# 4 Mean_Trend 0.03 ± 0.18
# 5  Max_Trend          20
# 6  Min_Trend       -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY  257 ± 1.93
# 2    Max_DOY         304
# 3    Min_DOY         150
# 4 Mean_Trend 0.08 ± 0.14
# 5  Max_Trend       14.25
# 6  Min_Trend         -28

##EOS  phe6_DOY_.
# 1   Mean_DOY  288 ± 3.45
# 2    Max_DOY         353
# 3    Min_DOY         178
# 4 Mean_Trend 0.13 ± 0.18
# 5  Max_Trend          21
# 6  Min_Trend       -37.5