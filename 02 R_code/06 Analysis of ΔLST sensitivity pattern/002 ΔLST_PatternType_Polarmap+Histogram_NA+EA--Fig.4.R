##########################     0. Load Packages and Data      ############################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library("scales")

# library(ggspatial)
setwd("D:/VegetationImpact")


############################### 1-1 SOS Polar Map  ##################################
######## (a) SOS
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r01 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")
df01 <- as.data.frame(r01, xy = TRUE, na.rm = TRUE)  
colnames(df01) <- c("long", "lat","k_value") 
df01$k_value <- as.numeric(as.character(df01$k_value))

r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_1.tif")   # NA
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_1.tif")  # EA

merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))             
df1$group <- "SOS"


# Extract common pixel points
common_pixels <- intersect(paste(df01$long, df01$lat, sep="_"), paste(df1$long, df1$lat, sep="_"))

# Filter data based on common pixel points
df01_common <- df01[paste(df01$long, df01$lat, sep="_") %in% common_pixels, ]
df1_common <- df1[paste(df1$long, df1$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames by pixels
merged_df1 <- merge(df01_common, df1_common, by=c("long", "lat"))

# Create a new column type
merged_df1$type <- ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value >= 0, "Type1",
                          ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value < 0, "Type2",
                                 ifelse(merged_df1$average_diff < 0 & merged_df1$k_value >= 0, "Type3",
                                        ifelse(merged_df1$average_diff < 0 & merged_df1$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)  # Defines the x axes required
# class(merged_diff_raster_1)
# merged_diff_raster_1<- terra::rast(merged_diff_raster_1)
p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # Fill with light gray
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
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40", linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the Arctic projection
p1

ggsave(
  filename = "./0.figure/Fig.4-polar_1.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-2 MGP Polar Map  ##################################
######## (b) MGP
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

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


# Extract common pixel points
common_pixels <- intersect(paste(df02$long, df02$lat, sep="_"), 
                           paste(df2$long, df2$lat, sep="_"))

# Filter data based on common pixel points
df02_common <- df02[paste(df02$long, df02$lat, sep="_") %in% common_pixels, ]
df2_common <- df2[paste(df2$long, df2$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames based on pixel coordinates
merged_df2 <- merge(df02_common, df2_common, by=c("long", "lat"))

# Create a new column 'type'
merged_df2$type <- ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value >= 0, "Type1",
                          ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value < 0, "Type2",
                                 ifelse(merged_df2$average_diff < 0 & merged_df2$k_value >= 0, "Type3",
                                        ifelse(merged_df2$average_diff < 0 & merged_df2$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
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
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 30°
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 50°
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 70°
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the Arctic projection
p2

ggsave(
  filename = "./0.figure/Fig.4-polar_2.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-2 MGP Polar Map  ##################################
######## (b) MGP
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

r02 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif")
df02 <- as.data.frame(r02, xy = TRUE, na.rm = TRUE)  
colnames(df02) <- c("long", "lat","k_value") 
df02$k_value <- as.numeric(as.character(df02$k_value))                    

r2 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_2.tif")   #NA
r22 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_2.tif")  #EA

merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_diff") 
df2$average_diff <- as.numeric(as.character(df2$average_diff))            
df2$group <- "MGP"


# Extract common pixel points
common_pixels <- intersect(paste(df02$long, df02$lat, sep="_"), 
                           paste(df2$long, df2$lat, sep="_"))

# Filter data based on common pixel points
df02_common <- df02[paste(df02$long, df02$lat, sep="_") %in% common_pixels, ]
df2_common <- df2[paste(df2$long, df2$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames based on pixel coordinates
merged_df2 <- merge(df02_common, df2_common, by=c("long", "lat"))

# Create a new column 'type'
merged_df2$type <- ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value >= 0, "Type1",
                          ifelse(merged_df2$average_diff >= 0 & merged_df2$k_value < 0, "Type2",
                                 ifelse(merged_df2$average_diff < 0 & merged_df2$k_value >= 0, "Type3",
                                        ifelse(merged_df2$average_diff < 0 & merged_df2$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60)# Defines the x axes required

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
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
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 30°
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 50°
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 70°
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the Arctic projection
p2

ggsave(
  filename = "./0.figure/Fig.4-polar_2.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-4 GDO Polar Map ##################################
######## (d) GDO
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

r04 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif")
df04 <- as.data.frame(r04, xy = TRUE, na.rm = TRUE)  
colnames(df04) <- c("long", "lat","k_value") 
df04$k_value <- as.numeric(as.character(df04$k_value))                   

r4 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_4.tif")   #NA
r44 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_4.tif")  #EA

merged_diff_raster_4 <- merge(r4, r44)
df4<- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","average_diff") 
df4$average_diff <- as.numeric(as.character(df4$average_diff))             
df4$group <- "GDO"


# Extract common pixels
common_pixels <- intersect(paste(df04$long, df04$lat, sep="_"), 
                           paste(df4$long, df4$lat, sep="_"))

# Filter data based on common pixels
df04_common <- df04[paste(df04$long, df04$lat, sep="_") %in% common_pixels, ]
df4_common <- df4[paste(df4$long, df4$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames by pixel
merged_df4 <- merge(df04_common, df4_common, by=c("long", "lat"))

# Create a new column 'type'
merged_df4$type <- ifelse(merged_df4$average_diff >= 0 & merged_df4$k_value >= 0, "Type1",
                          ifelse(merged_df4$average_diff >= 0 & merged_df4$k_value < 0, "Type2",
                                 ifelse(merged_df4$average_diff < 0 & merged_df4$k_value >= 0, "Type3",
                                        ifelse(merged_df4$average_diff < 0 & merged_df4$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60) # Defines the x axes required

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
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40", linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 30°
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 50°
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 70°
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If showing the Arctic projection
p4

ggsave(
  filename = "./0.figure/Fig.4-polar_4.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)



############################### 1-5 MSP Polar Map  ##################################
######## (e) MSP
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

r05<- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif")
df05 <- as.data.frame(r05, xy = TRUE, na.rm = TRUE)  
colnames(df05) <- c("long", "lat","k_value") 
df05$k_value <- as.numeric(as.character(df05$k_value))

r5<- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_5.tif")   #NA
r55 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_5.tif")  #EA

merged_diff_raster_5 <- merge(r5, r55)
df5<- as.data.frame(merged_diff_raster_5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","average_diff") 
df5$average_diff <- as.numeric(as.character(df5$average_diff))             
df5$group <- "MSP"

# Extract common pixel points
common_pixels <- intersect(paste(df05$long, df05$lat, sep="_"), 
                           paste(df5$long, df5$lat, sep="_"))

# Filter data based on common pixel points
df05_common <- df05[paste(df05$long, df05$lat, sep="_") %in% common_pixels, ]
df5_common <- df5[paste(df5$long, df5$lat, sep="_") %in% common_pixels, ]

# Merge the two dataframes by pixels
merged_df5 <- merge(df05_common, df5_common, by=c("long", "lat"))

# Create a new column 'type'
merged_df5$type <- ifelse(merged_df5$average_diff >= 0 & merged_df5$k_value >= 0, "Type1",
                          ifelse(merged_df5$average_diff >= 0 & merged_df5$k_value < 0, "Type2",
                                 ifelse(merged_df5$average_diff < 0 & merged_df5$k_value >= 0, "Type3",
                                        ifelse(merged_df5$average_diff < 0 & merged_df5$k_value < 0, "Type4", NA))))





x_lines <- seq(-120,180, by = 60) # Defines the x axes required

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
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the polar stereographic projection
p5

ggsave(
  filename = "./0.figure/Fig.4-polar_5.tiff",
  plot = p5,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 1-6 EOS Polar Map ##################################
######## (f) EOS

library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)

r06 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")
df06 <- as.data.frame(r06, xy = TRUE, na.rm = TRUE)  
colnames(df06) <- c("long", "lat", "k_value") 
df06$k_value <- as.numeric(as.character(df06$k_value))

r6 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   # NA
r66 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  # EA

merged_diff_raster_6 <- merge(r6, r66)
df6 <- as.data.frame(merged_diff_raster_6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat", "average_diff") 
df6$average_diff <- as.numeric(as.character(df6$average_diff))             
df6$group <- "EOS"

# Extract common pixels
common_pixels <- intersect(paste(df06$long, df06$lat, sep="_"), 
                           paste(df6$long, df6$lat, sep="_"))

# Filter data based on common pixels
df06_common <- df06[paste(df06$long, df06$lat, sep="_") %in% common_pixels, ]
df6_common <- df6[paste(df6$long, df6$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames based on the pixel
merged_df6 <- merge(df06_common, df6_common, by=c("long", "lat"))

# Create a new column 'type'
merged_df6$type <- ifelse(merged_df6$average_diff >= 0 & merged_df6$k_value >= 0, "Type1",
                          ifelse(merged_df6$average_diff >= 0 & merged_df6$k_value < 0, "Type2",
                                 ifelse(merged_df6$average_diff < 0 & merged_df6$k_value >= 0, "Type3",
                                        ifelse(merged_df6$average_diff < 0 & merged_df6$k_value < 0, "Type4", NA))))

x_lines <- seq(-120, 180, by = 60)  # Defines the x axes required

p6 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
  geom_tile(data = merged_df6, aes(x = long, y = lat, fill = type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 = "#FF9933", Type3 = "#66CCFF", Type4 = "#006699"),
                    na.value = "transparent",
                    name = "Type",
                    guide = "colorbar") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°N
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30°N
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50°N
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70°N
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the polar stereographic projection
p6

ggsave(
  filename = "./0.figure/Fig.4-polar_6.tiff",
  plot = p6, width = 15, height = 15, units = "in", dpi = 300)



############################### 7 Distribution Histogram ##################################
###############################
library(dplyr)

# Define a list of data frames
merged_dfs <- list(merged_df1, merged_df2, merged_df3, merged_df4, merged_df5, merged_df6)
group_names <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")  # Corresponding group names


# Initialize an empty data frame to store results
final_result <- data.frame()

# Loop through each data frame
for (i in seq_along(merged_dfs)) {
  # Calculate the count for each type
  type_counts <- merged_dfs[[i]] %>%
    group_by(type) %>%
    summarize(count = n())
  
  # Calculate the percentage
  type_counts <- type_counts %>%
    mutate(percentage = round(count / sum(count) * 100))
  
  # Add group name
  type_counts$group <- group_names[i]
  
  # Ensure that the total percentage equals 100%
  total_percentage <- sum(type_counts$percentage)
  percentage_diff <- 100 - total_percentage
  if (percentage_diff != 0) {
    # Find the group with the highest percentage
    max_percentage_type <- which.max(type_counts$percentage)
    # Adjust the percentage for that group so the total equals 100%
    type_counts$percentage[max_percentage_type] <- type_counts$percentage[max_percentage_type] + percentage_diff
  }
  type_counts$LabelPos <-  c(100,
                             100-type_counts$percentage[1]-type_counts$percentage[2]/2-3,
                             100-type_counts$percentage[1]-type_counts$percentage[2]-type_counts$percentage[3]/2,
                             12)
  # Add the result to final_result
  final_result <- rbind(final_result, type_counts)
}

# Reset the factor levels for group names
final_result$group <- factor(final_result$group, levels = group_names)
final_result$type <- factor(final_result$type, levels = c("Type1", "Type2","Type3","Type4"))

# Print the final result
print(final_result)


p7 <- ggplot(final_result, aes(x = group, y = percentage, fill = type)) +
  geom_col(position = "stack", width = 0.5) +  # Adjust the bar width to 0.8
  labs(x = "Phenological event", y = "Percentage (%)") +
  scale_fill_manual(values = c(Type1 = "#CC3333", Type2 ="#FF9933" , Type3 ="#66CCFF" , Type4 = "#006699"),
                    # name = "Type",  # Set the legend title
                    labels = c("W-P: Warming-Positive", "W-N: Warming-Negative","C-P: Cooling-Positive","C-N: Cooling-Negative")) +  # Set legend labels
  theme_minimal() +
  geom_text(aes(label = paste0(sprintf("%.0f", percentage), "%"), y = LabelPos), size = 9)+
  theme(axis.line.y = element_line(),   # Display x-axis line
        axis.line.x = element_line(),   # Display y-axis line
        axis.ticks.x = element_line(),  # Display x-axis ticks
        axis.ticks.y = element_line(size = 1),  # Display y-axis ticks width
        axis.ticks.length.y = unit(0.2, "cm"),   # Display y-axis tick length
        axis.title = element_text(size = 31),
        axis.text = element_text(size = 28,color = "gray20"),
        axis.title.x = element_text(margin = margin(t = 15)),  # Adjust the x-axis title position downward
        legend.title = element_blank(),  # Hide legend title
        legend.text = element_text(size = 28),
        legend.spacing.y = unit(0.7, "cm"),                    # Distance between legend title and legend
        legend.position = "right",
        panel.grid = element_line( linetype = "blank"))+
  guides(fill = guide_legend(byrow = TRUE))+                   # Internal distance within the legend
  coord_fixed(ratio = 1/70) +
  ylim(0,100)
p7
ggsave(filename =  "./0.figure/Fig.4-percent_7.tiff", 
       plot = p7, width = 19, height = 5 , units = "in", dpi = 300)
