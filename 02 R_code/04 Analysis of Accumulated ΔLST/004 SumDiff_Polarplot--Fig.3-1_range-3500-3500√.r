
###### 0.加载包和数据 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(scales)

setwd("D:/VegetationImpact")


############################### 01 积温极地图  ##################################

r <- raster("./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif")   
plot(r)
df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)  
colnames(df) <- c("long", "lat","sum_diff") 
df$sum_diff <- as.numeric(as.character(df$sum_diff))
summary(df$sum_diff)

# # 计算均值和标准差
# mean_val <- mean(df$sum_diff)
# sd_val <- sd(df$sum_diff)
# # 根据三倍标准差的原则定义异常值的上下限
# lower_limit <- mean_val - 3 * sd_val
# upper_limit <- mean_val + 3 * sd_val
# # 根据上下限过滤数据框
# df_filtered <- df[df$sum_diff >= lower_limit & df$sum_diff <= upper_limit, ]
# # 打印摘要统计信息
# summary(df_filtered$sum_diff)

# 计算大于3000的行数
count_greater_than_3000 <- sum(df$sum_diff > 3000)
# 打印结果
cat("有", count_greater_than_3000, "行的sum_diff大于3000。")
# 将小于-3500的值设置为-3500，将大于3500的值设置为3500
df$sum_diff <- ifelse(df$sum_diff < -3500, -3500, 
                      ifelse(df$sum_diff > 3500, 3500, df$sum_diff))


library(dplyr)
# library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

p <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df, aes(x = long, y = lat, fill = sum_diff)) +
  # geom_raster(data = r, aes(x = x, y = y, fill = sum_diff)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  
  scale_fill_gradientn(colours = c("#000303","#003366","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9933","#FF6633","#CC3333","#990000"),
                       na.value = "transparent",
                       name = "Cumulative ΔLST (℃·day)",
                       values = scales::rescale(c(-3500, -3000, -2000, -1000, 0, 1000, 2000, 3000, 3500)),
                       limits = c(-3500, 3500),
                       breaks = c(-3000,-2000,-1000, 0,1000,2000,3000),  # Specify the breaks for labels
                       labels = c("≤-3000","-2000","-1000", "0","1000","2000","≥3000"),  # Specify the corresponding labels
                       guide = guide_colorbar(title.position = "left",
                                              title.hjust = 0.5,
                                              barwidth = 60,
                                              title.vjust = 1,
                                              barheight = 1.8,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))+   #刻度线)) +
  
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    # panel.grid.major = element_line(size = 0.25, colour = "grey70",   #主网格线样式
    #                                 linetype = "dashed"), 
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    # legend.position = "none")+
    legend.title = element_text(size = 38) ,
    legend.text = element_text(size = 38),
    legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p


ggsave(
  filename = "./0.figure/Fig.3-polar_mean.tiff",
  plot = p,  width = 22,  height = 15,  units = "in",  dpi = 300)  #18


# ############################### 2.1  5段时间平均积温直方图 ##################################
# 
# r1 <- raster("./LST/NA/2014nls_result/sum_diff_12.tif")  # 读取多个栅格数据
# r2 <- raster("./LST/NA/2014nls_result/sum_diff_23.tif")
# r3 <- raster("./LST/NA/2014nls_result/sum_diff_34.tif")
# r4 <- raster("./LST/NA/2014nls_result/sum_diff_45.tif")
# r5 <- raster("./LST/NA/2014nls_result/sum_diff_56.tif")
# values <- list( "SOS-MGP" = na.omit(r1[]),  # 提取数据并去除缺失值
#                 "MGP-GMO" = na.omit(r2[]),
#                 "GMO-GDO" = na.omit(r3[]),
#                 "GDO-MSP" = na.omit(r4[]),
#                 "MSP-EOS" = na.omit(r5[]))
# mean_values <- sapply(values, mean)   #计算各数据的均值和标准差
# std_devs <- sapply(values, sd)
# error_bars <- std_devs * 0.15         # 计算0.15倍标准差
# # 创建数据框，按指定顺序
# data <- data.frame(Category = factor(names(values), 
#                                      levels = c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS")), 
#                    Mean = mean_values)
# colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
#             "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
# 
# # 绘制柱状图并添加误差线（细化线条）
# p2 <- ggplot(data, aes(x = Category, y = Mean, fill = Category)) +
#   geom_bar(stat = "identity", color = "black", fill = colors) +  # 绘制柱状图
#   geom_errorbar(aes(ymin = Mean - error_bars, ymax = Mean + error_bars), width = 0.2, color = "black") +  # 添加误差线
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # 添加x=0处的虚线
#   labs( x = "Vegetation proxies",     #title = "Mean Values with Error Bars",
#         y = "Average accumulated LST difference(℃)") +  # 添加标题和轴标签
#   theme_bw() +  # 设定主题
#   theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
#         panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
#         #panel.border = element_blank(),               # 隐藏面板边框
#         axis.line.x = element_line(color = "black"),  # 设置x轴颜色
#         axis.line.y = element_line(color = "black"))  # 设置y轴颜色
# p2
# 











