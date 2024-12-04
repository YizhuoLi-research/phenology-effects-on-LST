###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################   01 欧洲美洲sum_diff栅格数据 合并 (6)  #################################

# 定义需要处理的 sum_diff 文件类型
sum_diff_types <- c("sum_diff_12", "sum_diff_23", "sum_diff_34", "sum_diff_45", "sum_diff_56", "sum_diff_16")

# 循环处理每种 sum_diff 文件类型
for (j in 1:length(sum_diff_types)) {
  
  # 构建对应的文件列表，使用动态模式匹配每个 sum_diff_x
  file_list1 <- list.files("./NA_Results/0.sum_diff/0common_pixel/", 
                           pattern = paste0(sum_diff_types[j], ".*\\.tif$"), 
                           full.names = TRUE)
  file_list11 <- list.files("./EA_Results/0.sum_diff/0common_pixel/", 
                            pattern = paste0(sum_diff_types[j], ".*\\.tif$"), 
                            full.names = TRUE)
  
  # 将文件列表转换为栅格对象
  rasters1 <- lapply(file_list1, rast)
  rasters11 <- lapply(file_list11, rast)
  
  # 输出文件夹路径，动态创建文件夹名
  output_folder <- paste0("./EA+NA_Results/merge_Sum_diff_years/merged_", sum_diff_types[j])
  
  # 创建输出文件夹（如果不存在）
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # 循环遍历每个栅格文件并合并
  for (i in seq_along(rasters1)) {
    # 合并栅格
    merged_raster <- merge(rasters1[[i]], rasters11[[i]])
    
    # 提取年份（假设文件名后4位表示年份）
    year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
    
    # 定义输出文件路径
    filename <- file.path(output_folder, paste0("EA+NA_merged_", sum_diff_types[j], "_", year, ".tif"))
    
    # 写入合并后的栅格文件
    writeRaster(merged_raster, filename = filename, overwrite = TRUE)
  }
}
