##################  00 加载包   ##########################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 按气候--subtype类型取像元计算回归线斜率--给气候类型数据添加属性   ###################

r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]
r <- ratify(r) # Converts raster field to categorical data
rat <- levels(r)[[1]]
# Legend in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Remove the placeholders
r[1:30] <- r0
#将修改后的属性表重新赋值给对象r
levels(r) <- rat
# library(rasterVis);levelplot(r)

#划定filter_values_list中每个类型区域的边界
classify_border <- as.polygons(rast(r))
# 创建气候类型列表
climate_types <-rat$climate 
# 使用 expand.grid 创建所有气候类型的组合
all_climates <- expand.grid(climate_types, stringsAsFactors = FALSE)
colnames(all_climates) <- "Climate_Type"
# 将结果放入一个列表中
filter_values_list <- split(all_climates$Climate_Type, 1:nrow(all_climates))







###########################  02 在diff文件中筛选气候类型,查看每种类型像元数量   #####################################################################
#因为diff123456年数据是一样的像元量，所以以diff_1为代表

file_paths <-"./EA+NA_Results/merge_average_diff_years/merged_average_diff_1/" 
file_list  <- list.files(file_paths, pattern = "\\.tif$", full.names = TRUE)
# for (file in file_list) {
#   raster_data <- raster(file)
#   non_empty_pixels <- sum(!is.na(values(raster_data)))
#   cat("文件", file, "中的非空值数量:", non_empty_pixels, "\n")
# }

# 读取第一个文件作为底板
base_raster <- raster(file_list[1])

# 提取底板栅格数据的非空像元
base_non_empty <- ifelse(!is.na(values(base_raster)), 1, NA)

# 遍历文件列表，将其他文件的非空像元添加到底板中
for (file in file_list[-1]) {
  raster_data <- raster(file)
  non_empty_pixels <- ifelse(!is.na(values(raster_data)), 1, NA)
  base_non_empty <- base_non_empty | non_empty_pixels
}

# 创建包含其他文件非空像元的并集栅格数据
final_raster <- base_raster
final_raster[] <- ifelse(base_non_empty == 1, 1, NA)
plot(final_raster)   #2013-2021年出现过值的像元(即并集)
# non_empty_pixels <- sum(!is.na(values(final_raster)))

sample = final_raster      
sample[is.finite(final_raster)] = 1
sample2 = r
sample2[is.finite(sample2)] = 1
r_common = r * sample[[1]] * sample2[[1]]
final_raster_common = final_raster * sample[[1]] * sample2[[1]] 

non_empty_pixels <- sum(!is.na(values(r_common)))
plot(r)
plot(r_common)

r_df <- as.data.frame(rasterToPoints(r_common), xy = TRUE, na.rm = TRUE)


colnames(r_df) <- c("long","lat","koppen_val")

library(dplyr)
# 创建映射关系表
climate_cor <- tibble(
  koppen_val = 0:30,
  climate_type = c('Af', 'Am', 'As', 'Aw',
                   'BSh', 'BSk', 'BWh', 'BWk',
                   'Cfa', 'Cfb','Cfc', 
                   'Csa', 'Csb','Csc', 
                   'Cwa','Cwb', 'Cwc', 
                   'Dfa', 'Dfb', 'Dfc','Dfd', 
                   'Dsa', 'Dsb', 'Dsc','Dsd',
                   'Dwa', 'Dwb', 'Dwc','Dwd', 
                   'EF',  'ET')
)
# 将 koppen_val 映射为 climate_type
r_df <- r_df %>%
  left_join(climate_cor, by = "koppen_val")        #加第3列climate_type
class(r_df$climate_type)
r_df <- r_df %>%                                   #加第4列group
  mutate(group = paste0(substr(climate_type, 1, 1), "X", substr(climate_type, 3, 3)))

#查看每种气候类型内有多少像元
distinct_climate_types <- distinct(r_df, climate_type)  # 获取唯一的climate_type值
# 输出不同的climate_type值
cat("不同的climate_type种类有：", distinct_climate_types$climate_type, "\n")
# 计算各个climate_type的行数
climate_type_counts <- count(r_df, climate_type)
# 输出各个climate_type的行数
cat("各个climate_type的行数：\n")
print(climate_type_counts)
# > print(climate_type_counts)
# climate_type     n
# 1            Af     2
# 2           BSh    15
# 3           BSk   374
# 4           BWk    30
# 5           Cfa  4235
# 6           Cfb  4656
# 7           Cfc   120
# 8           Csa   546
# 9           Csb   588
# 10          Cwa   523
# 11          Cwb   155
# 12          Dfa   579
# 13          Dfb 11782
# 14          Dfc 17605
# 15          Dfd   741
# 16          Dsa     3
# 17          Dsb   103
# 18          Dsc   735
# 19          Dsd    77
# 20          Dwa   456
# 21          Dwb  1663
# 22          Dwc  3838
# 23          Dwd   214
# 24           ET   753

#选择C--Warm temperate\D--snow temperate进行分析
