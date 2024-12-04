###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
setwd("D:/VegetationImpact")

#####################################   01  计算  ############################################
# 目标描述：LST_diff <- rast(c(file_list1))9个图层 表示2013-2022年--9年,1年1个文件--第一组数据
#           LST_act  <- rast(c(file_list2))9个图层 表示2013-2022年--9年,1年1个文件--第二组数据
#           取共有像元后，对每个像元，像元上有两组数据：LST_Diff LST_act 
#           单就这个像元点来说，想做这两组数据散点的线性回归（拟合y~kx+b），
#         （不考虑年的问题，2013-2021的散点全部投进去拟合）
#           最后：形成一张k值的map
###############################################################################################      
# 创建一个向量表示所有的 average_diff 组
average_diff_groups <- 1:6

# 循环处理每个 group
for (i in average_diff_groups) {
  
  # 定义文件路径
  file_list1 <- list.files(paste0("./EA+NA_Results/merge_average_diff_years/merged_average_diff_", i, "/"), 
                           pattern = "\\.tif$", full.names = TRUE)
  file_list2 <- list.files("./EA+NA_Results/merged_actLSTmean_years/", pattern = "\\.tif$", full.names = TRUE)
  
  # 读取栅格数据
  LST_diff <- rast(c(file_list1))
  LST_act <- rast(c(file_list2))
  
  # 取共有像元
  sample = LST_diff      
  sample[is.finite(sample)] = 1
  plot(sample)
  
  sample2 = LST_act
  sample2[is.finite(sample2)] = 1
  
  LST_diff = LST_diff * sample[[1]] * sample2[[1]]
  LST_act = LST_act * sample[[1]] * sample2[[1]]
  plot(LST_diff)
  plot(LST_act)
  
  ########## 构建线性模型
  sr <- sds(LST_diff, LST_act)
  
  # 计算Pearson相关系数
  lm_result <- lapp(sr, \(y, x) {
    tryCatch({
      df = na.omit(data.frame(x=x, y=y))
      if (sum(!is.na(y)) < 3 || sum(!is.na(x)) < 3) { 
        return(c(NA, NA, NA))
      } else {
        model <- lm(y ~ x, data = df)
        
        return(
          c(
            coef(model)[2],                               # 斜率
            summary(model)[["r.squared"]],                # R平方
            summary(model)[["coefficients"]][2, 4]        # p值
          )
        )
      }
    }, error = function(e) {
      return(c(NA, NA, NA))
    })
  })
  
  # 绘制结果
  names(lm_result) = c("coef_x", "r2", "p_value")
  picture_k <- rast(lm_result$coef_x)
  
  # 绘制结果看下
  plot(lm_result)
  
  num_cells <- ncell(lm_result$coef_x)
  print(num_cells)
  num_cells_not_na <- sum(!is.na(values(lm_result$coef_x)))
  print(num_cells_not_na)
  
  k_map <- lm_result$coef_x
  class(k_map)
  
  df <- as.data.frame(k_map, xy = TRUE, na.rm = TRUE)  
  colnames(df) <- c("long", "lat", "k_value") 
  summary(df$k_value)  #39942--非零像元数量
  
  # 计算像元的均值和标准差
  mean_k_map <- mean(k_map[], na.rm = TRUE)
  sd_k_map <- sd(k_map[], na.rm = TRUE)
  
  # 设置阈值，即三倍标准差
  threshold <- 3 * sd_k_map
  
  # 剔除超出阈值的像元
  k_map_no_outliers <- k_map
  # 超出阈值的像元
  outliers <- k_map[] < (mean_k_map - threshold) | k_map[] > (mean_k_map + threshold)
  # 将异常值像元置为NA
  k_map_no_outliers[outliers] <- NA
  
  # 剔除异常值的像元
  k_map_no_outliers <- mask(k_map_no_outliers, is.na(k_map_no_outliers))
  num_cells_not_NA <- sum(!is.na(values(k_map_no_outliers)))
  print(num_cells_not_NA)
  
  # 输出路径
  output_dir <- "./EA+NA_Results/merged_diffLST&actLST/"
  # 如果目标文件夹不存在，则创建它
  if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE)  }
  
  # 输出文件路径
  output_file <- paste0(output_dir, "merged_diffLST&actLST_", i, ".tif")
  
  writeRaster(k_map_no_outliers, output_file, overwrite = TRUE)
}







##########################################原始单次代码
##########################################原始单次代码
##########################################原始单次代码
#average_diff_1  average_diff_2  average_diff_3 
#average_diff_4  average_diff_5  average_diff_6

file_list1 <- list.files("./EA+NA_Results/merge_average_diff_years/merged_average_diff_6/", pattern = "\\.tif$",       #注意修改
                         full.names = TRUE)


file_list2 <- list.files("./EA+NA_Results/merged_actLSTmean_years/",pattern = "\\.tif$",full.names = TRUE)

LST_diff <- rast(c(file_list1))
LST_act  <- rast(c(file_list2))
# plot(LST_act[[1]])
###### 缩小数据量-节省时间的试运行
# LST_diff = aggregate(LST_diff,10)
# LST_act  = aggregate(LST_act,10)

###### 取共有像元 
sample = LST_diff      
sample[is.finite(sample)] = 1
plot(sample)

sample2 = LST_act
sample2[is.finite(sample2)] = 1

LST_diff = LST_diff * sample[[1]] * sample2[[1]]
LST_act  = LST_act  * sample[[1]] * sample2[[1]]
plot(LST_diff)
plot(LST_act)
########################   02  构建线性模型  #####################################
#aLST_diff,LST_act是两个时间序列的tif
sr <- sds(LST_diff,LST_act) 
#计算pearson

lm_result <- lapp(sr, \(y, x) {
  tryCatch({
    df = na.omit(data.frame(x=x,
                            y=y))
    if (sum(!is.na(y)) < 3 || sum(!is.na(x)) < 3) { 
      return(c(NA, NA, NA))
    } else {
      model <- lm(y ~ x, data = df)
      
      return(
        c(
          coef(model)[2],                               # 斜率
          summary(model)[["r.squared"]],                # R平方
          summary(model)[["coefficients"]][2, 4]        # p值AS
        )
      )
    }
  }, error = function(e) {
    return(c(NA, NA, NA))
  })
})


`#绘制结果
names(lm_result) = c("coef_x","r2","p_value")
picture_k <-rast(lm_result$coef_x) 
# plot(lm_result$p_value)
# plot(lm_result$coef_x, breaks = seq(-5, 5, length.out = 10))


#绘制结果看下
names(lm_result) = c("coef_x","r2","p_value")
plot(lm_result)

#Exporting tif file
# writeRaster(lm_result,"lm_result.tif",overwrite=T)


num_cells <- ncell(lm_result$coef_x)
print(num_cells)
num_cells_not_na <- sum(!is.na(values(lm_result$coef_x)))
print(num_cells_not_na)


k_map <- lm_result$coef_x
class(k_map)

df <- as.data.frame(k_map, xy = TRUE, na.rm = TRUE)  
colnames(df) <- c("long", "lat","k_value") 
summary(df$k_value)  #39942--非零像元数量

# # 正态分布检验
# # 绘制QQ图
# qqnorm(data)
# qqline(data) #是一条直线，所以服从正态分布---数据清洗---±3标准差法

# # # 方法一：剔除异常值，计算四分位数法，剔除了7%的像元，不要这种方法   #39942--37056
# q <- quantile(df$k_value, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
# # 计算 IQR（四分位数间距）
# iqr_value <- IQR(df$k_value, na.rm = TRUE)
# # 确定异常值的界限
# lower_limit <- q[2] - 1.5 * iqr_value
# upper_limit <- q[2] + 1.5 * iqr_value
# # 选取不包含异常值的数据子集
# df_no_outliers <- df[df$k_value >= lower_limit & df$k_value <= upper_limit, ]

# # # 方法二：剔除异常值，计算3倍标准差
# mean_k_value <- mean(df$k_value)
# sd_k_value <- sd(df$k_value)
# # 设置阈值，即三倍标准差
# threshold <- 3 * sd_k_value
# # 剔除超出三倍标准差以外的数据点
# df_no_outliers <- df[abs(df$k_value - mean_k_value) <= threshold, ]
# summary(df_no_outliers$k_value)  #39527
# df <- as.data.frame(k_map, xy = TRUE, na.rm = TRUE)
# #查阅像元数量
# count_values1 <- sum(df_no_outliers$k_value >= -10 & df_no_outliers$k_value <= 10)
# count_values2 <- sum(df_no_outliers$k_value >= -5 & df_no_outliers$k_value <= 5)
# count_values3 <- sum(df_no_outliers$k_value >= -2 & df_no_outliers$k_value <= 2)
# count_values4 <- sum(df_no_outliers$k_value >= -1 & df_no_outliers$k_value <= 1)
# count_k_dy10 <- sum(df_no_outliers$k_value > 2, na.rm = TRUE)
# count_k_xyf10 <-  sum(df_no_outliers$k_value < -2, na.rm = TRUE)

# 计算像元的均值和标准差
mean_k_map <- mean(k_map[], na.rm = TRUE)
sd_k_map <- sd(k_map[], na.rm = TRUE)
# 设置阈值，即三倍标准差
threshold <- 3 * sd_k_map
# # 剔除超出阈值的像元
k_map_no_outliers <- k_map
# 超出阈值的像元
outliers <- k_map[] < (mean_k_map - threshold) | k_map[] > (mean_k_map + threshold)
# 将异常值像元置为NA
k_map_no_outliers[outliers] <- NA
# 剔除异常值的像元
k_map_no_outliers <- mask(k_map_no_outliers, is.na(k_map_no_outliers))
num_cells_not_NA <- sum(!is.na(values(k_map_no_outliers)))
print(num_cells_not_NA)   
# summary(k_map_no_outliers)

writeRaster(k_map_no_outliers,"./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_6.tif",overwrite=T)   ###输出

#检查像元数量是否一致
#r <- raster("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_6.tif")  
# 计算非空像元的数量
#non_empty_count <- sum(!is.na(getValues(r)))
# 打印非空像元的数量
#cat("非空像元的数量:", non_empty_count, "\n")
#   1:39568  2:39547  3:339415   4:39226    5：39397    6:39527


