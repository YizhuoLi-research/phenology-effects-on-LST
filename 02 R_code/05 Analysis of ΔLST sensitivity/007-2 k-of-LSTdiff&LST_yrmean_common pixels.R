###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
setwd("D:/VegetationImpact")

########################   03  取6个事件时间 共有的像元  #####################################


####### 1 读取            
k_1 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_1.tif")
k_2 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_2.tif")
k_3 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_3.tif")
k_4 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_4.tif")
k_5 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_5.tif")
k_6 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_6.tif")

non_na_count2 <- sum(!is.na(values(k_1)))
print(non_na_count2)

##2  取共有6个diff的像元

sample = k_1      
sample[is.finite(sample)] = 1

sample2 = k_2
sample2[is.finite(sample2)] = 1

sample3 = k_3
sample3[is.finite(sample3)] = 1

sample4 = k_4
sample4[is.finite(sample4)] = 1

sample5 = k_5
sample5[is.finite(sample5)] = 1

sample6 = k_6
sample6[is.finite(sample6)] = 1

k_1 = k_1 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
k_2 = k_2 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
k_3 = k_3 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
k_4 = k_4 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
k_5 = k_5 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]
k_6 = k_6 * sample[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] * sample5[[1]] * sample6[[1]]

non_empty_pixels <- sum(!is.na(values(k_1)))
print(paste("非空像素数量为:", non_empty_pixels))

new_folder <- "./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/"
dir.create(new_folder, showWarnings = FALSE)

writeRaster(k_1,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif",overwrite=T)
writeRaster(k_2,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif",overwrite=T)
writeRaster(k_3,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_3.tif",overwrite=T)
writeRaster(k_4,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif",overwrite=T)
writeRaster(k_5,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif",overwrite=T)
writeRaster(k_6,"./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif",overwrite=T)