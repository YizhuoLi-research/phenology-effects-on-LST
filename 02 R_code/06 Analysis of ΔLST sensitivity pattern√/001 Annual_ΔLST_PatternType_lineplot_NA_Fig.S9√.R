library(terra)
library(raster)
library(ggplot2)
library(dplyr)         ####这个type是变化的

setwd("D:/VegetationImpact")

#######  0 数据源于 02欧洲美洲diff_result栅格数据合并后的文件
#######  Note:  Type不是变化的：因为type的第一个值是年均△LST  第二个值是k  
#######  都是9年产生的一个值，相当于去跟不同物候事件的 type的pattern去match

#########################   01 9年时间序列折线图 ---North America  #####################3##################
# 改6组：1-SOS 2-MGP 3-GMO 4-GDO 5-MSP 6-EOS

library("scales")


r01 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")      ##注意修改
df01 <- as.data.frame(r01, xy = TRUE, na.rm = TRUE)  
colnames(df01) <- c("long", "lat","k_value") 
df01$k_value <- as.numeric(as.character(df01$k_value))           

r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   #NA                    ##注意修改
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  #EA                    ##注意修改

merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))  
df1$group <- "SOS"


# 提取共有的像元点
common_pixels <- intersect(paste(df01$long, df01$lat, sep="_"), paste(df1$long, df1$lat, sep="_"))

# 根据共有的像元点筛选数据
df01_common <- df01[paste(df01$long, df01$lat, sep="_") %in% common_pixels, ]
df1_common <- df1[paste(df1$long, df1$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df1 <- merge(df01_common, df1_common, by=c("long", "lat"))

# 创建新的列 type
merged_df1$type <- ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value >= 0, "Type1",
                          ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value < 0, "Type2",
                                 ifelse(merged_df1$average_diff < 0 & merged_df1$k_value >= 0, "Type3",
                                        ifelse(merged_df1$average_diff < 0 & merged_df1$k_value < 0, "Type4", NA))))





###  0000. 做四个不同区域的 △LST时间序列  
###  NA  6组:      ##########

file_list <- list.files(path = "./NA_Results/0.diff_result/0common_pixel/",
                        pattern = "^average_diff_6_.*\\.tif$", full.names = TRUE)     ##注意修改

# 创建一个空的列表，用于存储结果
results_list <- list()
years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, nchar(file_list) - 4)))

for (i in 1:length(file_list)) {
  file <- raster::raster(file_list[i])
  
  # 转换为数据框
  file_df <- as.data.frame(file, xy = TRUE)
  names(file_df) <- c("long", "lat", "average_diff")
  merged_df1_clean <- merged_df1[complete.cases(merged_df1), ]
  file_df_clean <- file_df[complete.cases(file_df), ]
  
  # 比较操作
  common_pixels <- merge(file_df_clean, merged_df1_clean[, c("long", "lat", "type")], 
                         by = c("long", "lat"))
  common_pixels <- na.omit(common_pixels)
  
  # 按照 type 列进行分组计算平均值和标准误差，并添加年份列
  result <- common_pixels %>%
    group_by(type) %>%
    summarize(year = years[i],
              average_diff_mean =  mean(average_diff),       
              average_diff_std = sd(average_diff),
              average_diff_std_cal = 0.15 * sd(average_diff))
  
  # 将结果存储到列表中
  results_list[[i]] <- result
}

final_result <- bind_rows(results_list)
final_result$type <- factor(final_result$type, levels = c("Type1", "Type2","Type3","Type4")) 
final_result$year <- as.numeric(final_result$year)


p1 <- ggplot(final_result, aes(x = year, y = average_diff_mean, group = type, color = type)) +
  geom_line(size = 1.5) +                       # 增加线条的粗细
  geom_point(size = 5.5) +  # 添加点，并设置大小为2
  geom_errorbar(aes(ymin = average_diff_mean - average_diff_std_cal, 
                    ymax = average_diff_mean + average_diff_std_cal), 
                width = 0.2,size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1) +
  labs(x = "Year", y = "ΔLST (℃)")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(final_result$year), max(final_result$year), by = 2)) +  # 设置 x 轴刻度为整数
  scale_color_manual(   values = c( "#CC3333","#FF9933","#66CCFF", "#006699" ),
                        labels = c( "Warming-Positive","Warming-Negative","Cooling-Positive", "Cooling-Negative"))+
  # geom_smooth(data = subset(final_df_1, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # 添加趋势线
  # geom_text(aes(label = equation_label), x = 2018, y = 14, size = 13, color = "#000000")+
  # geom_text(aes(label = regression_label), x = 2014.5, y = 14, size = 13, color = "#000000")+
  # # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.4) +
  theme(
    legend.position = "none",
    # legend.position = c(0.8, 0.85),
    # legend.position = c(0.8, 0.2),
    axis.text.x  = element_text(size = 40),
    axis.text.y  = element_text(size = 42),
    axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(0.3, "cm"),
    axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
    axis.title = element_text(size = 45,margin = margin(t = 10)),
    legend.title = element_blank(),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
    legend.margin = margin(20, 20, 20, 20),  # 调整图例的外边距
    panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # 调整行间距),    #图例线长
         fill = guide_legend(byrow = TRUE))+
  # ylim(-7, 15)
  # scale_y_continuous(breaks = c(-5, 0, 5, 10, 15), limits = c(-7.5, 16))   # 设置 y 轴的刻度和范围
  scale_y_continuous(breaks = c(-15,-10,-5, 0, 5), limits = c(-16, 7.5))   # 设置 y 轴的刻度和范围

p1

ggsave(filename = "./0.figure/Fig.S9-test-diff&years-NA_6.tiff",           ##注意修改
       plot = p1,  width = 15,  height = 13,  units = "in",  dpi = 300)

