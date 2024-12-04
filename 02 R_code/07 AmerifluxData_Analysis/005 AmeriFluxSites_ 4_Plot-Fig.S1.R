###### 0. 加载包 ####
library(tidyverse)
library(dplyr)

setwd("D:/VegetationImpact")

# 创建地图
library(ggplot2)
library("scales")
library(sf)
library(ggrepel)

####################  01 绘制Ameriflux站点地图1   ######################


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")
head(df)

# 加载美国地图和州界线数据 #  https://catalog.data.gov/

us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
#如果是调用的sf中的
#usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))


# 查看更新后的数据框
head(df)
# 将 df 转换为 sf 对象
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))      # 转换坐标系统
df_sf$average_diff_21_mean <- as.numeric(df_sf$average_diff_21_mean)    

p1 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = average_diff_21_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # 使用 Albers 投影
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # 将图例放在左下角
        legend.text = element_text(size = 40),  # 修改图例文本大小
        legend.title = element_text(size = 42),  # 修改图例标题大小
        legend.direction = "horizontal",  # 设置图例方向为横向
        axis.text.x = element_text(size = 42, color = "gray20",margin = margin(t = 20)),  # 设置x轴标签字体大小
        axis.text.y = element_text(size = 42, color = "gray20",margin = margin(r = 10)),  # 设置y轴标签字体大小
        axis.ticks.length = unit(-8, "pt"),     # 设置刻度线长度
        axis.ticks = element_line(size = 2),  # 设置刻度线宽度
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(colours = c("#003366", "#3399CC","#66CCFF", "#FFFFFF",
                                    "#FF9900", "#990000"),
                        na.value = "transparent",
                        name = "ΔLST (℃)",
                        values = scales::rescale(c(-6,-4, -2, 0, 2, 4)),
                        breaks = c(-6,-4, -2, 0, 2, 4), 
                        limits = c(-6, 4),
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      ####宽度
                                               barheight = 3,     ####高度
                                               title.vjust = 0,  # 调整标题与图例的垂直位置
                                               ticks = T,
                                               ticks.colour = "white",
                                               ticks.linewidth = 3.0/.pt,
                                               nrow = 1))

# 显示图形
print(p1)


ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_1.tiff",
  plot = p1,  width = 20,  height = 15,  units = "in",  dpi = 300)

####################  02 绘制Ameriflux站点地图2   ######################


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

# 加载美国地图和州界线数据
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
#如果是调用的sf中的
#usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# 将 df 转换为 sf 对象
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # 转换坐标系统
df_sf$average_diff_26_mean <- as.numeric(df_sf$average_diff_26_mean)    ##########


p2 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = average_diff_26_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # 使用 Albers 投影
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # 将图例放在左下角
        legend.text = element_text(size = 40),  # 修改图例文本大小
        legend.title = element_text(size = 42),  # 修改图例标题大小
        legend.direction = "horizontal",  # 设置图例方向为横向
        axis.text.x = element_text(size = 42, color = "gray20",margin = margin(t = 20)),  # 设置x轴标签字体大小
        axis.text.y = element_text(size = 42, color = "gray20",margin = margin(r = 10)),  # 设置y轴标签字体大小
        axis.ticks.length = unit(-8, "pt"),     # 设置刻度线长度
        axis.ticks = element_line(size = 2),  # 设置刻度线宽度
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(colours = c("#003366", "#3399CC","#66CCFF", "#FFFFFF",
                                    "#FF9900", "#990000"),
                        na.value = "transparent",
                        name = "ΔLST (℃)",
                        values = scales::rescale(c(-6,-4, -2, 0, 2, 4)),
                        breaks = c(-6,-4, -2, 0, 2, 4), 
                        limits = c(-6, 4),
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      ####宽度
                                               barheight = 3,     ####高度
                                               title.vjust = 0,  # 调整标题与图例的垂直位置
                                               ticks = T,
                                               ticks.colour = "white",
                                               ticks.linewidth = 3.0/.pt,
                                               nrow = 1))

# 显示图形
print(p2)


ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_2.tiff",
  plot = p2,  width = 20,  height = 15,  units = "in",  dpi = 300)

####################  03 绘制Ameriflux站点地图3 cumulative   ######################

df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

df$sum_Diff_16_mean[df$sum_Diff_16_mean > 800] <- 1000


# 加载美国地图和州界线数据 

us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
#如果是调用的sf中的
#usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# 将 df 转换为 sf 对象
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # 转换坐标系统
df_sf$sum_Diff_16_mean <- as.numeric(df_sf$sum_Diff_16_mean)  


p3 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = sum_Diff_16_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # 使用 Albers 投影
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # 将图例放在左下角
        legend.text = element_text(size = 40),  # 修改图例文本大小
        legend.title = element_text(size = 42),  # 修改图例标题大小
        legend.direction = "horizontal",  # 设置图例方向为横向
        axis.text.x = element_text(size = 42, color = "gray20",margin = margin(t = 20)),  # 设置x轴标签字体大小
        axis.text.y = element_text(size = 42, color = "gray20",margin = margin(r = 10)),  # 设置y轴标签字体大小
        axis.ticks.length = unit(-8, "pt"),     # 设置刻度线长度
        axis.ticks = element_line(size = 2),  # 设置刻度线宽度
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(colours = c("#003366","#003366","#006699","#3399CC","#66CCFF",
                                    "#FFFFFF","#FF9900"),
                      na.value = "transparent",
                      name = "Cumulative ΔLST (℃·day)",
                      values = scales::rescale(c(-1000, -800,-600,-400,-200, 0, 200)),
                      limits = c(-1000, 200),
                      breaks = c(-800,-600,-400,-200, 0, 200),  # Specify the breaks for labels
                      labels = c("≤-800 ","-600","-400","-200","0","200"),  # Specify the corresponding labels
                      
                      guide = guide_colorbar(title.position = "top",
                                             title.hjust = 0.5,
                                             barwidth = 28,      ####宽度
                                             barheight = 3,     ####高度
                                             title.vjust = 0,  # 调整标题与图例的垂直位置
                                             ticks = T,
                                             ticks.colour = "white",
                                             ticks.linewidth = 3.0/.pt,
                                             nrow = 1))

# 显示图形
print(p3)


ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_3.tiff",
  plot = p3,  width = 20,  height = 15,  units = "in",  dpi = 300)

####################  04 绘制Ameriflux站点地图4 DOYs   ######################


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

df$sum_Diff_16_mean[df$days_16_mean > 240] <- 260

# 加载美国地图和州界线数据
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
#如果是调用的sf中的
#usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# 将 df 转换为 sf 对象
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # 转换坐标系统


p4 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = days_16_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # 使用 Albers 投影
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # 将图例放在左下角
        legend.text = element_text(size = 40),  # 修改图例文本大小
        legend.title = element_text(size = 42),  # 修改图例标题大小
        legend.direction = "horizontal",  # 设置图例方向为横向
        axis.text.x = element_text(size = 42, color = "gray20",margin = margin(t = 20)),  # 设置x轴标签字体大小
        axis.text.y = element_text(size = 42, color = "gray20",margin = margin(r = 10)),  # 设置y轴标签字体大小
        axis.ticks.length = unit(-8, "pt"),     # 设置刻度线长度
        axis.ticks = element_line(size = 2),  # 设置刻度线宽度
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(colours = c("#FFCC00", "#FF9900",
                                    "#FF6633","#CC3333","#990000", "#330000"),
                        na.value = "transparent",
                        name = "Duration (days)",
                        values = scales::rescale(c(140,160,180,200,220,240,260)),
                        limits = c(140, 260),
                        breaks = c(140,160,180, 200,220, 240),  # Specify the breaks for labels
                        labels = c("140"," 160"," 180", "  200","   220","       ≥240"),  # Specify the corresponding labels
                        
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      ####宽度
                                               barheight = 3,     ####高度
                                               title.vjust = 0,  # 调整标题与图例的垂直位置
                                               ticks = T,
                                               ticks.colour = "white",
                                               ticks.linewidth = 3.0/.pt,
                                               nrow = 1))

# 显示图形
print(p4)



ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_4.tiff",
  plot = p4,  width = 20,  height = 15,  units = "in",  dpi = 300)





##########################   z 绘制生长季温度气泡图   #########################3
# 
# selected_df_1 <- selected_df %>%
#   mutate(color = ifelse(mean_Diff_16_mean > 0, "#FFA07A", "lightblue"),
#          size_category = cut(mean_Diff_16_mean,
#                              breaks = c(-2.5, 0, 2.5, 5, 7.5, 10,12.5),
#                              labels = c("-2.5-0", "0.0-2.5", "2.5-5.0", "5.0-7.5", "7.5-10", "\u003E 10")))
# # 
# # 
# 
# # 绘图
# p2 <- ggplot(selected_df_1, aes(x = MAT, y = MAP)) +
#   geom_point(aes(size = size_category, color = color), alpha = 0.8) +  # 调整透明度
#   scale_color_identity() +  # 使用指定的颜色
#   scale_size_manual(values = c(7, 7, 9, 11, 13, 15), name = "Average ") +  # 自定义大小
#   labs(x = "Mean Annual Temperature (℃))", y = "Mean Annual Precipitation (℃))",
#        title = "") +
#   geom_text(aes(label = substr(site_id, nchar(site_id) - 2, nchar(site_id))), 
#             vjust = -0.5, size = 5)+  # 只添加站点名称的后三位
#   guides(color = guide_legend(ncol = 1,keywidth = 2),
#          fill = guide_legend(byrow = TRUE) ) +
#   theme_bw() +
#   ylim(750, 1400) +
#   xlim(0, 20) +
#   theme(axis.text.y =  element_text(size = 14,color = "gray20"),
#         axis.text.x =  element_text(size = 14,color = "gray20"),
#         axis.title = element_blank(),
#         panel.grid = element_blank(),
#         legend.position = "none") +
#   theme(legend.position = "right")
# p2
# 
# # ggsave(filename =  "./0.figure/Fig.2-Median_1.tiff", 
# #        plot = p11, width = 4, height = 3 , units = "in", dpi = 300)
