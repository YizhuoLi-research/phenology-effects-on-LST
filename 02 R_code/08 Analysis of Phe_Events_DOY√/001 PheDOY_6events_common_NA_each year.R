###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##取 每年- 能同时拥有phe值的像元与LST—diff共有像元匹配
##################   01 提取共有像元的栅格图像 (9年)  #################################运行18次

# Define the years to process
years <- 2013:2021

# 定义数据目录路径
input_dir  <- "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/"
output_dir <- "./EA_Results/0.phe_DOY/0common_pixel/"
dir.create(output_dir, showWarnings = FALSE)


for (year in years) {
  
  # 动态拼接文件路径
  phe1 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Increas_merged.tif"))  # SOS
  phe2 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Greenup_Phase__merged.tif"))  # MGP
  phe3 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Maximum_merged.tif"))  # GMO
  phe4 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Decreas_merged.tif"))  # GDO
  phe5 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Senescence_Pha_merged.tif"))  # MSP
  phe6 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Minimum_merged.tif"))  # EOS
  
  
  # 加载与 diff 相关的像元
  # 找与 diff 共同像元
  aa <- rast(paste0("./EA_Results/0.diff_result/0common_pixel/average_diff_1_", year, ".tif"))  
  sample_diff <- aa
  sample_diff[is.finite(sample_diff)] <- 1
  
  # 构建 mask 样本
  sample <- phe1
  sample[is.finite(sample)] <- 1
  
  common_mask <-  sample[[1]] * sample_diff[[1]]
    
  # Multiply all "phe" rasters by the common pixel mask
  phe1 <- phe1 * common_mask
  phe2 <- phe2 * common_mask
  phe3 <- phe3 * common_mask
  phe4 <- phe4 * common_mask
  phe5 <- phe5 * common_mask
  phe6 <- phe6 * common_mask
  
  # 打印非空像元数量
  non_empty_pixels <- sum(!is.na(values(aa)))
  print(paste("Year:", year, "- 非空像元数量为:", non_empty_pixels))
  
  non_empty_pixels <- sum(!is.na(values(phe1)))
  print(paste("Year:", year, "- phe6 非空像元数量为:", non_empty_pixels))
  
  
  ###### 3. Write the results #######
   # 保存处理后的栅格文件
   # 写入结果文件
   writeRaster(phe1, paste0(output_dir, "phe1_DOY_", year, ".tif"), overwrite = TRUE)
   writeRaster(phe2, paste0(output_dir, "phe2_DOY_", year, ".tif"), overwrite = TRUE)
   writeRaster(phe3, paste0(output_dir, "phe3_DOY_", year, ".tif"), overwrite = TRUE)
   writeRaster(phe4, paste0(output_dir, "phe4_DOY_", year, ".tif"), overwrite = TRUE)
   writeRaster(phe5, paste0(output_dir, "phe5_DOY_", year, ".tif"), overwrite = TRUE)
   writeRaster(phe6, paste0(output_dir, "phe6_DOY_", year, ".tif"), overwrite = TRUE)
   
}


#######为不同年份，改9次*2（EA/NA）
####### 1 读取             ###2013/2014/2015/2016/2017/2018/2019/2020/2021
# 
# phe1 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Increas_merged.tif")  #SOS
# phe2 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Date_Mid_Greenup_Phase__merged.tif")  #MGP
# phe3 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Maximum_merged.tif")  #GMO
# phe4 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Decreas_merged.tif")  #GDO
# phe5 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Date_Mid_Senescence_Pha_merged.tif")  #MSP
# phe6 <- rast("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Minimum_merged.tif")  #EOS
# 
# ###### 2  取共diff和sum共有的像元
# aa <- rast("./EA_Results/0.diff_result/0common_pixel/average_diff_1_2013.tif")  #去找与diff共同像元
# #这里不用重复用phe1*2、3、4、4、6因为有diff123456同时非空的像元一定有phe123456情况-
# vmphe123456一定非空
# sample_diff = aa
# sample_diff[is.finite(sample_diff)] = 1
# 
# sample = phe1                         #phe1有XX个像元
# sample[is.finite(sample)] = 1
# 
# 
# phe1 = phe1 * sample[[1]] * sample_diff[[1]]
# phe2 = phe2 * sample[[1]] * sample_diff[[1]]
# phe3 = phe3 * sample[[1]] * sample_diff[[1]]
# phe4 = phe4 * sample[[1]] * sample_diff[[1]]
# phe5 = phe5 * sample[[1]] * sample_diff[[1]]
# phe6 = phe6 * sample[[1]] * sample_diff[[1]]
# 
# 
# non_empty_pixels <- sum(!is.na(values(aa)))
# print(paste("非空像元数量为:", non_empty_pixels))
# non_empty_pixels <- sum(!is.na(values(phe6)))
# print(paste("非空像元数量为:", non_empty_pixels))
# summary(phe6)
# 
# new_folder <- "./NA_Results/0.phe_DOY/0common_pixel/"
# dir.create(new_folder, showWarnings = FALSE, recursive = TRUE)

# writeRaster(phe1,"./EA_Results/0.phe_DOY/0common_pixel/phe1_DOY_2013.tif",overwrite=T)
# writeRaster(phe2,"./EA_Results/0.phe_DOY/0common_pixel/phe2_DOY_2013.tif",overwrite=T)
# writeRaster(phe3,"./EA_Results/0.phe_DOY/0common_pixel/phe3_DOY_2013.tif",overwrite=T)
# writeRaster(phe4,"./EA_Results/0.phe_DOY/0common_pixel/phe4_DOY_2013.tif",overwrite=T)
# writeRaster(phe5,"./EA_Results/0.phe_DOY/0common_pixel/phe5_DOY_2013.tif",overwrite=T)
# writeRaster(phe6,"./EA_Results/0.phe_DOY/0common_pixel/phe6_DOY_2013.tif",overwrite=T)




# 18次运行结束后进行合并