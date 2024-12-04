###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################   01 提取共有像元的栅格图像 (9年)  #################################
#######为不同年份，改9次*2（EA/NA）
####### 1 读取             ###2013/2014/2015/2016/2017/2018/2019/2020/2021


# 创建输出文件夹（如果不存在）
new_folder <- "./EA_Results/0.actLSTmean_yr/0common_pixel/"
dir.create(new_folder, showWarnings = FALSE)

# 循环处理2013到2021年
for (year in 2013:2021) {
  # 读取 actLSTmean 文件
  actLSTmean <- rast(paste0("./EA_Results/0.actLSTmean_yr/", year, "_actLSTmean.tif"))
  
  # 读取与 diff 共有像元的文件
  aa <- rast(paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_1_", year, ".tif"))
  sample_diff <- aa
  sample_diff[is.finite(sample_diff)] <- 1
  
  # 获取 actLSTmean 的像元
  sample <- actLSTmean
  sample[is.finite(sample)] <- 1
  
  # 保留与共同像元的相交部分
  actLSTmean <- actLSTmean * sample[[1]] * sample_diff[[1]]
  
  # 计算非空像元数量并打印
  non_empty_pixels <- sum(!is.na(values(actLSTmean)))
  print(paste("年份:", year, "非空像元数量为:", non_empty_pixels))
  
  # 写入结果到新的文件夹
  output_filename <- paste0(new_folder, year, "_actLSTmean.tif")
  writeRaster(actLSTmean, output_filename, overwrite = TRUE)
}


# ####### 1 读取 ####
# actLSTmean <- rast("./EA_Results/0actLSTmean_yr/2013_actLSTmean.tif")
# 
# 
# ###### 2  取共diff和sum共有的像元####
# aa <- rast("./EA_Results/0diff_result/0common_pixel/average_diff_1_2013.tif")  #去找与diff共同像元
# sample_diff = aa
# sample_diff[is.finite(sample_diff)] = 1
# 
# sample = actLSTmean                         #actLSTmean- 2013有10034个像元
# sample[is.finite(sample)] = 1
# 
# actLSTmean = actLSTmean * sample[[1]] * sample_diff[[1]] 
# 
# non_empty_pixels <- sum(!is.na(values(actLSTmean)))
# print(paste("非空像元数量为:", non_empty_pixels))
# 
# new_folder <- "./EA_Results/0actLSTmean_yr/0common_pixel/"
# dir.create(new_folder, showWarnings = FALSE)
# 
# writeRaster(actLSTmean,"./EA_Results/0actLSTmean_yr/0common_pixel/2013_actLSTmean.tif",overwrite=T)