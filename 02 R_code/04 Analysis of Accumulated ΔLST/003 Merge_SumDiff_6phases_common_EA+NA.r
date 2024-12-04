
###### 0.加载包和数据 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(scales)

setwd("D:/VegetationImpact")

################     0.获取9年差值的平均影像（6个PHE6张）   ####################
#####################  NA区域上的均值

file_list <- list.files("./NA_Results/0.sum_diff/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)
file_groups <- substr(basename(file_list), 1, 11)  #SOS --9年；... 提取前x个字符进行分组

file_groups_name <- unique(file_groups)

# 创建新文件夹
new_folder <- "./NA_Results/0.sum_diff/NA_SUM_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# 对每个分组中的影像求平均并保存
for (group in file_groups_name) {
  # 筛选属于当前分组的文件
  current_files <- file_list[file_groups == group]
  
  # 读取当前分组的影像并求平均
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images),na.rm = TRUE)
  
  # 生成新文件名并保存影像
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name,overwrite=TRUE)
}


#####################  EA区域上的均值

file_list <- list.files("./EA_Results/0.sum_diff/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)
file_groups <- substr(basename(file_list), 1, 11)  #SOS --9年；... 提取前x个字符进行分组

file_groups_name <- unique(file_groups)

# 创建新文件夹
new_folder <- "./EA_Results/0.sum_diff/EA_SUM_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# 对每个分组中的影像求平均并保存
for (group in file_groups_name) {
  # 筛选属于当前分组的文件
  current_files <- file_list[file_groups == group]
  
  # 读取当前分组的影像并求平均
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images),na.rm = TRUE)
  
  # 生成新文件名并保存影像
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name,overwrite=TRUE)
}



################    ## 0.合并9年差值的平均影像（6个PHE6张）   ####################

# 创建输出目录
dir.create(output_path <- "./EA+NA_Results/merged_sum_diff_average", showWarnings = FALSE, recursive = TRUE)

# 获取文件列表
NA_SUM_files <- list.files("./NA_Results/0.sum_diff/NA_SUM_9yearAverge", pattern = "\\.tif$", full.names = TRUE)
EA_SUM_files <- list.files("./EA_Results/0.sum_diff/EA_SUM_9yearAverge", pattern = "\\.tif$", full.names = TRUE)

# 确保文件数目一致
if (length(NA_SUM_files) != length(EA_SUM_files)) {
  stop("文件数目不一致，无法一一对应。")
}


# 一一对应合并文件
for (i in seq_along(NA_SUM_files)) {
  NA_raster <- rast(NA_SUM_files[i])
  EA_raster <- rast(EA_SUM_files[i])
  
  # 使用 merge 合并两个 raster
  merged_raster <- merge(NA_raster, EA_raster, 
                         filename = file.path(output_path, paste0("merged_", basename(NA_SUM_files[i]))))
  
  print(paste("已合并文件:", basename(NA_SUM_files[i])))
}

