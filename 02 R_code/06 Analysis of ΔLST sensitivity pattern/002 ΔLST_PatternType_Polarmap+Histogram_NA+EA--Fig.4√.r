##########################     0.加载包和数据      ############################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library("scales")

# library(ggspatial)
setwd("D:/VegetationImpact")


############################### 1-1 SOS极地图  ##################################
######## (a) SOS
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r01 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")
df01 <- as.data.frame(r01, xy = TRUE, na.rm = TRUE)  
colnames(df01) <- c("long", "lat","k_value") 
df01$k_value <- as.numeric(as.character(df01$k_value))                    ################################

r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_1.tif")   #NA
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_1.tif")  #EA

merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))             ###############################
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





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df1, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
         panel.background = element_blank(),
         axis.ticks=element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1

ggsave(
  filename = "./0.figure/Fig.4-polar_1.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)

############################### 1-2 MGP极地图  ##################################
######## (b) MGP
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r02 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif")
df02 <- as.data.frame(r02, xy = TRUE, na.rm = TRUE)  
colnames(df02) <- c("long", "lat","k_value") 
df02$k_value <- as.numeric(as.character(df02$k_value))                    ################################

r2 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_2.tif")   #NA
r22 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_2.tif")  #EA

merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_diff") 
df2$average_diff <- as.numeric(as.character(df2$average_diff))             ###############################
df2$group <- "MGP"


# 提取共有的像元点
common_pixels <- intersect(paste(df02$long, df02$lat, sep="_"), 
                           paste(df2$long, df2$lat, sep="_"))

# 根据共有的像元点筛选数据
df02_common <- df02[paste(df02$long, df02$lat, sep="_") %in% common_pixels, ]
df2_common <- df2[paste(df2$long, df2$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df2 <- merge(df02_common, df2_common, by=c("long", "lat"))

# 创建新的列 type
merged_df2$type <- ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value >= 0, "Type1",
                         ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value < 0, "Type2",
                                ifelse(merged_df2$average_diff < 0 & merged_df2$k_value >= 0, "Type3",
                                       ifelse(merged_df2$average_diff < 0 & merged_df2$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df2, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p2

ggsave(
  filename = "./0.figure/Fig.4-polar_2.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)

############################### 1-3 GMO极地图  ##################################
######## (c) GMO
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r03 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_3.tif")
df03 <- as.data.frame(r03, xy = TRUE, na.rm = TRUE)  
colnames(df03) <- c("long", "lat","k_value") 
df03$k_value <- as.numeric(as.character(df03$k_value))                    ################################

r3 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_3.tif")   #NA
r33 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_3.tif")  #EA

merged_diff_raster_3 <- merge(r3, r33)
df3 <- as.data.frame(merged_diff_raster_3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat","average_diff") 
df3$average_diff <- as.numeric(as.character(df3$average_diff))             ###############################
df3$group <- "GMO"


# 提取共有的像元点
common_pixels <- intersect(paste(df03$long, df03$lat, sep="_"), 
                           paste(df3$long, df3$lat, sep="_"))

# 根据共有的像元点筛选数据
df03_common <- df03[paste(df03$long, df03$lat, sep="_") %in% common_pixels, ]
df3_common <- df3[paste(df3$long, df3$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df3 <- merge(df03_common, df3_common, by=c("long", "lat"))

# 创建新的列 type
merged_df3$type <- ifelse(merged_df3$average_diff >= 0 & merged_df3$k_value >= 0, "Type1",
                          ifelse(merged_df3$average_diff >= 0 & merged_df3$k_value < 0, "Type2",
                                 ifelse(merged_df3$average_diff < 0 & merged_df3$k_value >= 0, "Type3",
                                        ifelse(merged_df3$average_diff < 0 & merged_df3$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p3 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df3, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p3

ggsave(
  filename = "./0.figure/Fig.4-polar_3.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)

############################### 1-4 GDO极地图  ##################################
######## (d) GDO
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r04 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif")
df04 <- as.data.frame(r04, xy = TRUE, na.rm = TRUE)  
colnames(df04) <- c("long", "lat","k_value") 
df04$k_value <- as.numeric(as.character(df04$k_value))                    ################################

r4 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_4.tif")   #NA
r44 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_4.tif")  #EA

merged_diff_raster_4 <- merge(r4, r44)
df4<- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","average_diff") 
df4$average_diff <- as.numeric(as.character(df4$average_diff))             ###############################
df4$group <- "GDO"


# 提取共有的像元点
common_pixels <- intersect(paste(df04$long, df04$lat, sep="_"), 
                           paste(df4$long, df4$lat, sep="_"))

# 根据共有的像元点筛选数据
df04_common <- df04[paste(df04$long, df04$lat, sep="_") %in% common_pixels, ]
df4_common <- df4[paste(df4$long, df4$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df4 <- merge(df04_common, df4_common, by=c("long", "lat"))

# 创建新的列 type
merged_df4$type <- ifelse(merged_df4$average_diff >= 0 & merged_df4$k_value >= 0, "Type1",
                          ifelse(merged_df4$average_diff >= 0 & merged_df4$k_value < 0, "Type2",
                                 ifelse(merged_df4$average_diff < 0 & merged_df4$k_value >= 0, "Type3",
                                        ifelse(merged_df4$average_diff < 0 & merged_df4$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p4 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df4, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p4

ggsave(
  filename = "./0.figure/Fig.4-polar_4.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-5 MSP极地图  ##################################
######## (e) MSP
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r05<- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif")
df05 <- as.data.frame(r05, xy = TRUE, na.rm = TRUE)  
colnames(df05) <- c("long", "lat","k_value") 
df05$k_value <- as.numeric(as.character(df05$k_value))

r5<- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_5.tif")   #NA
r55 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_5.tif")  #EA

merged_diff_raster_5 <- merge(r5, r55)
df5<- as.data.frame(merged_diff_raster_5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","average_diff") 
df5$average_diff <- as.numeric(as.character(df5$average_diff))             ###############################
df5$group <- "MSP"

# 提取共有的像元点
common_pixels <- intersect(paste(df05$long, df05$lat, sep="_"), 
                           paste(df5$long, df5$lat, sep="_"))

# 根据共有的像元点筛选数据
df05_common <- df05[paste(df05$long, df05$lat, sep="_") %in% common_pixels, ]
df5_common <- df5[paste(df5$long, df5$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df5 <- merge(df05_common, df5_common, by=c("long", "lat"))

# 创建新的列 type
merged_df5$type <- ifelse(merged_df5$average_diff >= 0 & merged_df5$k_value >= 0, "Type1",
                          ifelse(merged_df5$average_diff >= 0 & merged_df5$k_value < 0, "Type2",
                                 ifelse(merged_df5$average_diff < 0 & merged_df5$k_value >= 0, "Type3",
                                        ifelse(merged_df5$average_diff < 0 & merged_df5$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p5 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df5, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p5

ggsave(
  filename = "./0.figure/Fig.4-polar_5.tiff",
  plot = p5,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-6 EOS极地图  ##################################
######## (f) EOS

library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r06<- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")
df06 <- as.data.frame(r06, xy = TRUE, na.rm = TRUE)  
colnames(df06) <- c("long", "lat","k_value") 
df06$k_value <- as.numeric(as.character(df06$k_value))

r6<- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   #NA
r66<- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  #EA

merged_diff_raster_6 <- merge(r6, r66)
df6<- as.data.frame(merged_diff_raster_6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat","average_diff") 
df6$average_diff <- as.numeric(as.character(df6$average_diff))             
df6$group <- "EOS"


# 提取共有的像元点
common_pixels <- intersect(paste(df06$long, df06$lat, sep="_"), 
                           paste(df6$long, df6$lat, sep="_"))

# 根据共有的像元点筛选数据
df06_common <- df06[paste(df06$long, df06$lat, sep="_") %in% common_pixels, ]
df6_common <- df6[paste(df6$long, df6$lat, sep="_") %in% common_pixels, ]

# 将两个数据框按照像元进行合并
merged_df6 <- merge(df06_common, df6_common, by=c("long", "lat"))

# 创建新的列 type
merged_df6$type <- ifelse(merged_df6$average_diff >= 0 & merged_df6$k_value >= 0, "Type1",
                          ifelse(merged_df6$average_diff >= 0 & merged_df6$k_value < 0, "Type2",
                                 ifelse(merged_df6$average_diff < 0 & merged_df6$k_value >= 0, "Type3",
                                        ifelse(merged_df6$average_diff < 0 & merged_df6$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p6 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = merged_df6, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p6

ggsave(
  filename = "./0.figure/Fig.4-polar_6.tiff",
  plot = p6,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 7  分布直方图 ##################################
###############################
library(dplyr)

# 定义数据框列表
merged_dfs <- list(merged_df1, merged_df2, merged_df3, merged_df4, merged_df5, merged_df6)
group_names <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")  # 对应的组名
# merged_dfs$Type <- factor(merged_dfs$Type, levels = c("Type1", "Type2","Type3","Type4"))


# 初始化一个空数据框，用于存储结果
final_result <- data.frame()

# 循环处理每个数据框
for (i in seq_along(merged_dfs)) {
  # 计算每个 type 的数量
  type_counts <- merged_dfs[[i]] %>%
    group_by(type) %>%
    summarize(count = n())
  
  # 计算占比
  type_counts <- type_counts %>%
    mutate(percentage = round(count / sum(count) * 100))
  
  # 添加组名
  type_counts$group <- group_names[i]
  
  # 确保每组总和等于100%
  total_percentage <- sum(type_counts$percentage)
  percentage_diff <- 100 - total_percentage
  if (percentage_diff != 0) {
    # 找到百分比最大的组
    max_percentage_type <- which.max(type_counts$percentage)
    # 调整该组的百分比，使总和为100%
    type_counts$percentage[max_percentage_type] <- type_counts$percentage[max_percentage_type] + percentage_diff
  }
  type_counts$LabelPos <-  c(100,
                             100-type_counts$percentage[1]-type_counts$percentage[2]/2-3,
                             100-type_counts$percentage[1]-type_counts$percentage[2]-type_counts$percentage[3]/2,
                             12)
  # 将结果添加到 final_result 中
  final_result <- rbind(final_result, type_counts)
}

# 重新设置组名的因子顺序
final_result$group <- factor(final_result$group, levels = group_names)
final_result$type <- factor(final_result$type, levels = c("Type1", "Type2","Type3","Type4"))

# 打印最终结果
print(final_result)


p7 <- ggplot(final_result, aes(x = group, y = percentage, fill = type)) +
  geom_col(position = "stack", width = 0.5) +  # 调整柱宽度为0.8
  labs(x = "Phenological event", y = "Percentage (%)") +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    # name = "Type",  # 设置图例标题
                    labels = c("W-P: Warming-Positive", "W-N: Warming-Negative","C-P: Cooling-Positive","C-N: Cooling-Negative")) +  # 设置图例标签)+
  theme_minimal() +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%"), y = LabelPos), size = 9)+
  theme(axis.line.y = element_line(),   # 显示x坐标轴线
        axis.line.x = element_line(),   # 显示y坐标轴线
        axis.ticks.x = element_line(),  # 显示坐标轴刻度
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_text(size = 31),
        axis.text = element_text(size = 28,color = "gray20"),
        axis.title.x = element_text(margin = margin(t = 15)),  # 向下调整横坐标标题位置
        # legend.title = element_text(size = 28) ,  
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 28),
        legend.spacing.y = unit(0.7, "cm"),                    #图例标题与图例距离
        legend.position = "right",
        panel.grid = element_line( linetype = "blank"))+
  guides(fill = guide_legend(byrow = TRUE))+                   #图例内部距离
  coord_fixed(ratio = 1/70) +
  ylim(0,100)
p7
ggsave(filename =  "./0.figure/Fig.4-percent_7.tiff", 
       plot = p7, width = 19, height = 5 , units = "in", dpi = 300)


###########################  1-2 SOS频数分布直方图   ##################################


# library(dplyr)
# library(ggplot2)
# 
# # 计算每个 type 分组的数量
# type_counts1 <- merged_df1 %>%
#   group_by(type) %>%
#   summarize(count = n())
# # 计算占比
# type_counts1 <- type_counts1 %>%
#   mutate(percentage = count / sum(count) * 100)
# print(type_counts1)
# type_counts1$group <- "SOS"
# 
# 
# 
# p11 <- ggplot(type_counts, aes(x = type, y = percentage, fill = type)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = paste0(round(percentage, 0), "%")), 
#             vjust = -0.5,  # 调整文本位置
#             size = 9,                   
#             color = "black") +   
#   scale_fill_manual(values = c(Type1 = "#FF6633", Type2 = "#FFCC33", 
#                                Type3 = "#99CC00", Type4 = "#66CCFF")) +
#   theme_bw() +
#   coord_fixed(ratio = 1/18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(size = 22), 
#         axis.text.y = element_text(size = 22),
#         axis.title = element_blank(),
#         panel.grid = element_line( linetype = "blank"))+
#         # axis.text.x = element_blank(),  # 隐藏 x 轴文字
#         # axis.ticks.x = element_blank()
#   # ylab("Percentage")  
#   ylim(0, 45)  # 设置纵坐标范围为0到100，表示百分比
# print(p11)
# 
# ggsave(filename = "./0.figure/Fig.4-chart_1.tiff", 
#        plot = p11, width = 6, height = 4 , units = "in", dpi = 300)




