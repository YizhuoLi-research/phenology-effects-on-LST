##取同时拥有diff123456值的像元
library(terra)
library(raster)
setwd("D:/VegetationImpact")

############### 01 读取tif文件  #################################################
#以平均差值共有像元为标准，获取参数共有像元数据


# 读取 common_tif_file
common_tif_file <- raster("./NA_DIFF_9yearAverge/average_diff_1.tif")   #NA  以SOS（6个diff像元一致）为标准
# 获取 parameter_tifs 文件夹下的所有tif文件路径
parameter_tif_paths <- list.files(path = "./NA_Results/0.atc_evaluation/",      #NA
                                  pattern = "\\.tif$", full.names = TRUE)
# 创建一个列表，存储共有像元的结果
common_parameter_tifs <- list()


############### 02 取共有像元  #################################################


# 遍历 parameter_tifs 中的每个tif文件
for (tif_path in parameter_tif_paths) {
  # 读取当前tif文件
  current_tif <- raster(tif_path)
  
  # 将当前tif文件中的非缺失值像元设为1，其余为NA
  common_tif_file[!is.finite(common_tif_file)] <- NA
  common_tif_file[is.finite(common_tif_file)] <- 1
  
  # 将当前tif文件与 common_tif_file 相乘，取共有像元
  common_parameter_tif <- current_tif * common_tif_file
  
  # 将结果添加到 common_parameter_tifs 列表中
  common_parameter_tifs[[tif_path]] <- common_parameter_tif
}


# 创建输出文件夹
output_folder <- "./NA_Results/0.atc_evaluation/0common_pixel/"               #NA
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 将共有像元的结果写入到输出文件夹
for (i in seq_along(parameter_tif_paths)) {
  output_file <- basename(parameter_tif_paths[i])
  output_path <- file.path(output_folder, output_file)
  writeRaster(common_parameter_tifs[[i]], filename = output_path, overwrite = TRUE)
}

# 打印非空像素数量
non_empty_pixels <- sum(!is.na(values(common_parameter_tifs[1])))
print(paste("非空像素数量为:", non_empty_pixels))

# 读取 TIF 文件
tif_file <- rast("./NA_DIFF_9yearAverge/average_diff_1.tif")
non_na_count <- sum(!is.na(values(tif_file)))
print(non_na_count)
       # NA：12527； EA：37302
# ##########################   检查像元数量--检查
tif_file2 <- rast("./NA_Results/0.atc_evaluation/0common_pixel/2013_rr_2.tif")
non_na_count2 <- sum(!is.na(values(tif_file2)))
# 打印结果
print(non_na_count2)
#    NA   2013 - 2013 ncells: 10369 
#    EA   2013 - 2013 ncells: 32020 


###############    03 计算多年平均值  #################################################
#计算9年平均值 rmse  rmse_2  rr rr_2  p  p2 *NA EA 共运行8次

file_list <- list.files("./NA_Results/0.atc_evaluation/0_common_pixel",           #NA
                        pattern = "\\.tif$", full.names = TRUE)
files <- file_list[grep("rmse\\.tif$", file_list, ignore.case = TRUE)]   #4个参数

# 创建新文件夹
new_folder <- "./NA_Results/0.atc_evaluation/NA_para_9yearAverge"                   #NA
dir.create(new_folder, showWarnings = FALSE)

# 对每个分组中的影像求平均并保存
average_images <- list()
for (file_path in files) {
  # 读取当前文件
  current_image <- raster::raster(file_path)
  
  # 将当前影像添加到列表中
  average_images[[basename(file_path)]] <- current_image
}

# 计算平均值
if (length(average_images) > 0) {
  average_stack <- stack(average_images)
  average_tif <- raster::mean(average_stack, na.rm = TRUE)
  
  # 生成新文件名并保存影像
  new_file_path <- file.path(new_folder, "rmse.tif")                     #4个参数
  raster::writeRaster(average_tif, filename = new_file_path, overwrite = TRUE)
}

plot(average_tif)
summary(average_tif)


df1 <- as.data.frame(average_tif, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average") 
df1$average <- as.numeric(as.character(df1$average))
summary(df1$average)



####查看数据
#RMSE
over_10_count <- sum(df1$average > 10, na.rm = TRUE)
total_count <- length(df1$average)
over_10_percentage <- over_10_count /  total_count * 100
cat("over_10_percentage:", over_10_percentage, "%\n")


#R
below_0.5_count <- sum(df1$average < 0.5, na.rm = TRUE)
total_count <- length(df1$average)
below_0.5_percentage <- below_0.5_count /  total_count * 100
cat("below_0.5_count:", below_0.5_count, "%\n")

#p
over_0.05_count <- sum(df1$average > 0.05, na.rm = TRUE)
total_count <- length(df1$average)
over_0.05_percentage <- over_0.05_count /  total_count * 100
cat("over_0.05_percentage:", over_0.05_percentage, "%\n")


# tif_file3 <- rast("./0.figure/pr_test/NA_para_9yearAverge/rr_2.tif")
# non_na_count3 <- sum(!is.na(values(average_tif)))
# # # 打印结果
# print(non_na_count3)   #12527 NA No problem!!!  #37302 EA
