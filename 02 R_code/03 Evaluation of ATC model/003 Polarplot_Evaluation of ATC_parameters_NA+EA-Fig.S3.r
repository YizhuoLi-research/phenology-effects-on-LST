######################      0. Load packages and data      ######################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library("scales")
library(dplyr)

setwd("D:/VegetationImpact")

#########################  1-1 First Fitting RMSE Polar Map  ########################

r1 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/rmse.tif")
r11 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/rmse.tif")

# Analysis for NH_1st_fitting_RMSE
merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_rmse") 
df1$average_rmse <- as.numeric(as.character(df1$average_rmse))
avg_NH <- round(mean(df1$average_rmse, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df1$average_rmse, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #0.51
cat("0.15std_NH:", std_NH, "\n")  #0.21

# Analysis for NA_1st_fitting_RMSE
df1_NA <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1_NA) <- c("long", "lat","average_rmse") 
avg_NA <- round(mean(df1_NA$average_rmse, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df1_NA$average_rmse, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #4.72
cat("0.15std_NA:", std_NA, "\n")  #0.13

# Analysis for EA_1st_fitting_RMSE
df1_EA <- as.data.frame(r11, xy = TRUE, na.rm = TRUE)  
colnames(df1_EA) <- c("long", "lat","average_rmse") 
avg_EA <- round(mean(df1_EA$average_rmse, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df1_EA$average_rmse, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #5.11
cat("0.15std_EA:", std_EA, "\n")  #0.23

summary(df1$average_rmse)
above_10_count <- sum(df1$average_rmse > 10, na.rm = TRUE)
total_count <- length(df1$average_rmse)
above_10_percentage <- above_10_count /  length(df1$average_rmse) * 100
# cat("Percentage of values greater than 10:", above_10_percentage, "%\n")    # count for: 1.190774 %
# df1$group <- "average_rmse"


wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
# Defines the required x-axis lines
x_lines <- seq(-120,180, by = 60)

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df1, aes(x = long, y = lat, fill = average_rmse)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # Specify longitude and latitude
  
  scale_fill_gradientn(colours = c("#006699", "#66CCFF", "#99CC00","#FFCC00","#FF9900", "#CC3300"), 
                       na.value = "transparent",
                       name = "RMSE (℃)",
                       values = rescale(c(0,3,6,9,12,15)),
                       breaks = c(0,3,6,9,12,15), 
                       limits=c(0,15),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,       # Adjust length
                                              barheight = 1.5,
                                              title.vjust = 0.5,   # Adjust the vertical alignment of title with the legend
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))  +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Display polar projection if needed
p1

ggsave(
  filename = "./0.figure/Fig.S3-polar_rmse_1.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)

#########################  1-2 First Fitting R Polar Map ########################

r2 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge//rr.tif")
r22 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge//rr.tif")

# Analysis for NH_1st_fitting_R
merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_rr") 
df2$average_rr <- as.numeric(as.character(df2$average_rr))
avg_NH <- round(mean(df2$average_rr, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df2$average_rr, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  # 0.93
cat("0.15std_NH:", std_NH, "\n")  # 0.00

# Analysis for NA_1st_fitting_R, which is rr--Pearson cor
df2_NA <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2_NA) <- c("long", "lat","average_rr") 
avg_NA <- round(mean(df2_NA$average_rr, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df2_NA$average_rr, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  # 0.91
cat("0.15std_NA:", std_NA, "\n")  # 0.01

# Analysis for EA_1st_fitting_R
df2_EA <- as.data.frame(r22, xy = TRUE, na.rm = TRUE)  
colnames(df2_EA) <- c("long", "lat","average_rr") 
avg_EA <- round(mean(df2_EA$average_rr, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df2_EA$average_rr, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  # 0.94
cat("0.15std_EA:", std_EA, "\n")  # 0.00

summary(df2$average_rr)
below_0.5_count <- sum(df2$average_rr < 0.5, na.rm = TRUE)
total_count <- length(df2$average_rr)
below_0.5_percentage <- below_0.5_count / length(df2$average_rr) * 100
cat("Percentage of values below 0.5:", below_0.5_percentage, "%\n")    # Percentage: 0.0 %

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60) # Defines the required x-axis lines

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +
  geom_tile(data = df2, aes(x = long, y = lat, fill = average_rr)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +
  scale_fill_gradientn(colours = c("#006699","#66CCFF","#99CC00","#FFCC00","#FF9900","#CC3300"), 
                       na.value = "transparent",
                       name <- expression("R"),
                       values = rescale(c( 0.5, 1.0)),
                       breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                       labels = format(c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), nsmall = 1),
                       limits = c(0.5, 1.0),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 1.5,
                                              title.vjust = 0.5,  
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))  +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds latitude and longitude grid lines
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Adds latitude and longitude labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Displays the polar projection
p2

ggsave(
  filename = "./0.figure/Fig.S3-polar_r_1-legend.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)


#########################  2-1 Second Fitting RMSE Polar Map ########################

r3 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/rmse_2.tif")
r33 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge//rmse_2.tif")

# Analysis for NH_2nd_fitting_RMSE
merged_diff_raster_3 <- merge(r3, r33)
df3 <- as.data.frame(merged_diff_raster_3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat","average_rmse_2")  
df3$average_rmse_2 <- as.numeric(as.character(df3$average_rmse_2))
avg_NH <- round(mean(df3$average_rmse_2, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df3$average_rmse_2, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #5.12
cat("0.15std_NH:", std_NH, "\n")  #0.21

# Analysis for NA_2nd_fitting_RMSE
df3_NA <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3_NA) <- c("long", "lat","average_rmse_2") 
avg_NA <- round(mean(df3_NA$average_rmse_2, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df3_NA$average_rmse_2, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #5.18
cat("0.15std_NA:", std_NA, "\n")  #0.15

# Analysis for EA_2nd_fitting_RMSE
df3_EA <- as.data.frame(r33, xy = TRUE, na.rm = TRUE)  
colnames(df3_EA) <- c("long", "lat","average_rmse_2") 
avg_EA <- round(mean(df3_EA$average_rmse_2, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df3_EA$average_rmse_2, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #5.10
cat("0.15std_EA:", std_EA, "\n")  #0.23

summary(df3$average_rmse_2)
above_10_count <- sum(df3$average_rmse_2 > 10, na.rm = TRUE)
total_count <- length(df3$average_rmse_2)
above_10_percentage <- above_10_count / length(df3$average_rmse_2) * 100
cat("Percentage of values above 10:", above_10_percentage, "%\n")    # Percentage: 0.79 %

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60) # Defines the x axes required

p3 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df3, aes(x = long, y = lat, fill = average_rmse_2)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # Set latitude and longitude limits
  
  scale_fill_gradientn(colours = c("#006699", "#66CCFF", "#99CC00","#FFCC00","#FF9900", "#CC3300"), 
                       na.value = "transparent",
                       name = "RMSE (℃)",
                       values = rescale(c(0,3,6,9,12,15)),
                       breaks = c(0,3,6,9,12,15), 
                       limits=c(0,15),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,       # Adjust length
                                              barheight = 1.5,
                                              title.vjust = 0.5,   # Center align title vertically
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))  +
  theme_minimal() +
  theme( 
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # For polar projection display
p3

ggsave(
  filename = "./0.figure/Fig.S3-polar_rmse_2.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)


ggsave(
  filename = "./0.figure/Fig.S3-polar_rmse_2.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)


#########################  2-2 Second Fitting R Polar Map  ########################

r4 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/rr_2.tif")
r44 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/rr_2.tif")

# Analysis for NH_2nd_fitting_R
merged_diff_raster_4 <- merge(r4, r44)
df4 <- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","average_rr_2") 
df4$average_rr_2 <- as.numeric(as.character(df4$average_rr_2))
avg_NH <- round(mean(df4$average_rr_2, na.rm = TRUE), 2)
std_NH <- round(0.15 * sd(df4$average_rr_2, na.rm = TRUE), 2)
cat("average_NH:", avg_NH, "\n")  # 0.82
cat("0.15std_NH:", std_NH, "\n")  # 0.02

# Analysis for NA_2nd_fitting_R, which is rr--Pearson correlation
df4_NA <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4_NA) <- c("long", "lat","average_rr_2") 
avg_NA <- round(mean(df4_NA$average_rr, na.rm = TRUE), 2)
std_NA <- round(0.15 * sd(df4_NA$average_rr, na.rm = TRUE), 2)
cat("average_NA:", avg_NA, "\n")  # 0.77
cat("0.15std_NA:", std_NA, "\n")  # 0.02

# Analysis for EA_2nd_fitting_R, which is rr--Pearson correlation
df4_EA <- as.data.frame(r44, xy = TRUE, na.rm = TRUE)  
colnames(df4_EA) <- c("long", "lat","average_rr_2") 
avg_EA <- round(mean(df4_EA$average_rr, na.rm = TRUE), 2)
std_EA <- round(0.15 * sd(df4_EA$average_rr, na.rm = TRUE), 2)
cat("average_EA:", avg_EA, "\n")  # 0.84
cat("0.15std_EA:", std_EA, "\n")  # 0.02

summary(df4$average_rr_2)
below_0.5_count <- sum(df4$average_rr_2 < 0, na.rm = TRUE)
total_count <- length(df4$average_rr_2)
below_0.5_percentage <- below_0.5_count / total_count * 100
cat("Percentage of values below 0.5:", below_0.5_percentage, "%\n")    # Percentage: 0.0%

wr <- map_data("world") %>%
  filter(lat > 20)
x_lines <- seq(-120, 180, by = 60)  # Defines the x-axes required

p4 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
  geom_tile(data = df4, aes(x = long, y = lat, fill = average_rr_2)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # Specify latitude and longitude
  scale_fill_gradientn(colours = c("#006699", "#66CCFF", "#99CC00", "#FFCC00", "#FF9900", "#CC3300"), 
                       na.value = "transparent",
                       name <- expression("R"),
                       values = rescale(c(0.5, 1.0)),
                       breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                       labels = format(c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), nsmall = 1),
                       limits = c(0.5, 1.0),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 1.5,
                                              title.vjust = 0.5,  # Adjust vertical position of the title
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))  +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 32),
    legend.text = element_text(size = 32),
    legend.position = "bottom") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Meridians
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 30
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 50
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 70
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0)) # Display in polar projection
p4

ggsave(
  filename = "./0.figure/Fig.S3-polar_r_2-legend.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)


# ###########################  3-1 First Fitting pvalue_1 #################################

# P-value of the first fitting

r5 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/pvalue.tif")
r55 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/pvalue.tif")
merged_diff_raster_5 <- merge(r5, r55)
df5 <- as.data.frame(merged_diff_raster_5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat", "average_pvalue") 
df5$average_pvalue <- as.numeric(as.character(df5$average_pvalue))
summary(df5$average_pvalue)
above_001_count <- sum(df5$average_pvalue > 0.01, na.rm = TRUE)
cat("Number of pixels with average_pvalue greater than 0.01:", above_001_count, "\n")
# Number of pixels with average_pvalue greater than 0.01: 0 

# ###########################  3-2 Second Fitting pvalue_2 ##################################

# P-value of the second fitting

r6 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/pvalue_2.tif")
r66 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/pvalue_2.tif")
merged_diff_raster_6 <- merge(r6, r66)
df6 <- as.data.frame(merged_diff_raster_6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat", "average_pvalue_2") 
df6$average_pvalue_2 <- as.numeric(as.character(df6$average_pvalue_2))
summary(df6$average_pvalue_2)
above_005_count <- sum(df6$average_pvalue > 0.05, na.rm = TRUE)
cat("Number of pixels with average_pvalue greater than 0.05:", above_005_count, "\n")    #458