##########################     0. Load Packages and Data      ############################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)

setwd("D:/VegetationImpact")

#########################  1-1 First Fitting: Mean Error Polar Map  ########################

r1 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/me.tif")
r11 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/me.tif")

# Analysis for NH First Fitting ME
merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_me") 
df1$average_me <- as.numeric(as.character(df1$average_me))
avg_NH <- round(mean(df1$average_me, na.rm = TRUE), 2)
std_NH <- round(0.15 * sd(df1$average_me, na.rm = TRUE), 2)
cat("average_NH:", avg_NH, "\n")  
cat("0.15std_NH:", std_NH, "\n")

# Calculate range
summary(df1$average_me)

# Calculate percentage outside [-5, 5]
outside_range_count <- sum(df1$average_me < -5 | df1$average_me > 5, na.rm = TRUE)
total_count <- sum(!is.na(df1$average_me))
outside_range_percentage <- outside_range_count / total_count * 100
cat("outside [-5, 5] percentage:", round(outside_range_percentage, 2), "%\n")

# Analysis for NA
... (rest remains unchanged for brevity)

# Visualization
wr <- map_data("world") %>% filter(lat > 20)
x_lines <- seq(-120, 180, by = 60)
df1$average_me <- ifelse(df1$average_me < -1.5, -1.5, ifelse(df1$average_me > 1.5, 1.5, df1$average_me))

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +
  geom_tile(data = df1, aes(x = long, y = lat, fill = average_me)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_gradientn(colours = c("#006699", "#66CCFF", "#99CC00", "#FFCC00", "#FF9900", "#FF6633", "#CC3300"), 
                       na.value = "transparent",
                       name = "Mean error (℃)",
                       values = rescale(c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5)),
                       breaks = c(-1.0, -0.5, 0, 0.5, 1.0), 
                       labels = c("-1.0", "-0.5", "0", "0.5", "1.0"),  
                       limits = c(-1.5, 1.5),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 1.5,
                                              title.vjust = 0.5,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
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
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))

p1

ggsave(
  filename = "./0.figure/Fig.S3-polar_me_1.tiff",
  plot = p1, width = 15, height = 15, units = "in", dpi = 300)

#########################  1-2 First fitting: R polar map  ########################

r2 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge//rr.tif")
r22 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge//rr.tif")

# Analysis for NH_1st_fitting_R
merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_rr") 
df2$average_rr <- as.numeric(as.character(df2$average_rr))
avg_NH <- round(mean(df2$average_rr, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df2$average_rr, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")
cat("0.15std_NH:", std_NH, "\n")

# Analysis for NA_1st_fitting_R (Pearson correlation)
df2_NA <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2_NA) <- c("long", "lat","average_rr") 
avg_NA <- round(mean(df2_NA$average_rr, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df2_NA$average_rr, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")
cat("0.15std_NA:", std_NA, "\n")

# Analysis for EA_1st_fitting_R
df2_EA <- as.data.frame(r22, xy = TRUE, na.rm = TRUE)  
colnames(df2_EA) <- c("long", "lat","average_rr") 
avg_EA <- round(mean(df2_EA$average_rr, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df2_EA$average_rr, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")
cat("0.15std_EA:", std_EA, "\n")

# Summary and proportion below 0.5
summary(df2$average_rr)
below_0.5_count <- sum(df2$average_rr < 0.5, na.rm = TRUE)
total_count <- length(df2$average_rr)
below_0.5_percentage <- below_0.5_count / total_count * 100
cat("Proportion of values < 0.5:", below_0.5_percentage, "%\n")

# World map and polar plot setup
wr <- map_data("world") %>% filter(lat > 20)
x_lines <- seq(-120,180, by = 60)

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +
  geom_tile(data = df2, aes(x = long, y = lat, fill = average_rr)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +
  scale_fill_gradientn(colours = c("#006699","#66CCFF","#99CC00","#FFCC00","#FF9900","#CC3300"), 
                       na.value = "transparent",
                       name = expression("R"),
                       values = rescale(c(0.5, 0.6, 0.7, 0.8, 0.9,1.0)),
                       breaks = c(0.6, 0.7, 0.8, 0.9, 1.0),
                       labels = c("0.6", "0.7", "0.8", "0.9", "1.0"),  
                       limits = c(0.5, 1.0),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 45,
                                              barheight = 1.5,
                                              title.vjust = 0.5,
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
    legend.position = "bottom") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13)+ 
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))
p2

ggsave(
  filename = "./0.figure/Fig.S3-polar_r_1.tiff",
  plot = p2, width = 15, height = 15, units = "in", dpi = 300)



#########################  2-1 Second fitting: me_2 polar map  ########################

r3 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/me_2.tif")
r33 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/me_2.tif")

# Analysis for NH second fitting: me_2
merged_diff_raster_3 <- merge(r3, r33)
df3 <- as.data.frame(merged_diff_raster_3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat", "average_me_2") 
df3$average_me_2 <- as.numeric(as.character(df3$average_me_2))
avg_NH <- round(mean(df3$average_me_2, na.rm = TRUE), 2)
std_NH <- round(0.15 * sd(df3$average_me_2, na.rm = TRUE), 2)
cat("average_NH:", avg_NH, "\n")
cat("0.15std_NH:", std_NH, "\n")
summary(df3$average_me_2)

# Percentage of values outside [-5, 5]
outside_range_count <- sum(df3$average_me_2 < -5 | df3$average_me_2 > 5, na.rm = TRUE)
total_count <- sum(!is.na(df3$average_me_2))
outside_range_percentage <- outside_range_count / total_count * 100
cat("outside [-5, 5] percentage:", round(outside_range_percentage, 2), "%\n")

# NA region
df3_NA <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3_NA) <- c("long", "lat", "average_me_2") 
avg_NA <- round(mean(df3_NA$average_me_2, na.rm = TRUE), 2)
std_NA <- round(0.15 * sd(df3_NA$average_me_2, na.rm = TRUE), 2)
cat("average_NA:", avg_NA, "\n")
cat("0.15std_NA:", std_NA, "\n")
outside_range_count <- sum(df3_NA$average_me_2 < -5 | df3_NA$average_me_2 > 5, na.rm = TRUE)
total_count <- sum(!is.na(df3_NA$average_me_2))
outside_range_percentage <- outside_range_count / total_count * 100
cat("outside [-5, 5] percentage:", round(outside_range_percentage, 2), "%\n")

# EA region
df3_EA <- as.data.frame(r33, xy = TRUE, na.rm = TRUE)  
colnames(df3_EA) <- c("long", "lat", "average_me_2") 
avg_EA <- round(mean(df3_EA$average_me_2, na.rm = TRUE), 2)
std_EA <- round(0.15 * sd(df3_EA$average_me_2, na.rm = TRUE), 2)
cat("average_EA:", avg_EA, "\n")
cat("0.15std_EA:", std_EA, "\n")
outside_range_count <- sum(df3_EA$average_me_2 < -5 | df3_EA$average_me_2 > 5, na.rm = TRUE)
total_count <- sum(!is.na(df3_EA$average_me_2))
outside_range_percentage <- outside_range_count / total_count * 100
cat("outside [-5, 5] percentage:", round(outside_range_percentage, 2), "%\n")

# Map
wr <- map_data("world") %>%
  filter(lat > 20)
x_lines <- seq(-120, 180, by = 60)

# Truncate values outside [-1.5, 1.5]
df3$average_me_2 <- ifelse(df3$average_me_2 < -1.5, -1.5,
                           ifelse(df3$average_me_2 > 1.5, 1.5, df3$average_me_2))

p3 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +
  geom_tile(data = df3, aes(x = long, y = lat, fill = df3$average_me_2)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  scale_fill_gradientn(colours = c("#006699", "#66CCFF", "#99CC00", "#FFCC00", "#FF9900", "#FF6633", "#CC3300"), 
                       na.value = "transparent",
                       name = "Mean error (℃)",
                       values = rescale(c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5)),
                       breaks = c(-1.0, -0.5, 0, 0.5, 1.0),
                       labels = c("-1.0", "-0.5", "0", "0.5", "1.0"),
                       limits = c(-1.5, 1.5),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 1.5,
                                              title.vjust = 0.5,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
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
  # Add lat/lon grid lines
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Add latitude/longitude labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # North polar projection

p3

# Save plot
ggsave(filename = "./0.figure/Fig.S3-polar_me_2.tiff",
       plot = p3, width = 15, height = 15, units = "in", dpi = 300)

#########################  2-2 Second fitting: rr_2 polar map  ########################

r4 <- raster("./NA_Results/0.atc_evaluation/NA_para_9yearAverge/rr_2.tif")
r44 <- raster("./EA_Results/0.atc_evaluation/EA_para_9yearAverge/rr_2.tif")

# Analysis for NH second fitting: rr_2
merged_diff_raster_4 <- merge(r4, r44)
df4 <- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","average_rr_2") 
df4$average_rr_2 <- as.numeric(as.character(df4$average_rr_2))
avg_NH <- round(mean(df4$average_rr_2, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df4$average_rr_2, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")
cat("0.15std_NH:", std_NH, "\n")

# Analysis for NA: rr_2 (Pearson correlation)
df4_NA <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4_NA) <- c("long", "lat","average_rr_2") 
avg_NA <- round(mean(df4_NA$average_rr_2, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df4_NA$average_rr_2, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")
cat("0.15std_NA:", std_NA, "\n")

# Analysis for EA: rr_2 (Pearson correlation)
df4_EA <- as.data.frame(r44, xy = TRUE, na.rm = TRUE)  
colnames(df4_EA) <- c("long", "lat","average_rr_2") 
avg_EA <- round(mean(df4_EA$average_rr_2, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df4_EA$average_rr_2, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")
cat("0.15std_EA:", std_EA, "\n")

summary(df4$average_rr_2)
below_0.5_count <- sum(df4$average_rr_2 < 0, na.rm = TRUE)
total_count <- length(df4$average_rr_2)
below_0.5_percentage <- below_0.5_count / total_count * 100
cat("Percentage of values < 0:", below_0.5_percentage, "%\n")

# Map preparation
wr <- map_data("world") %>%
  filter(lat > 20)
x_lines <- seq(-120,180, by = 60)

p4 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +
  geom_tile(data = df4, aes(x = long, y = lat, fill = average_rr_2)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +
  scale_fill_gradientn(colours = c("#006699","#66CCFF","#99CC00","#FFCC00","#FF9900","#CC3300"), 
                       na.value = "transparent",
                       name = expression("R"),  # Pearson correlation
                       values = rescale(c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)),
                       breaks = c(0.6, 0.7, 0.8, 0.9, 1.0),
                       labels = c("0.6", "0.7", "0.8", "0.9", "1.0"),  
                       limits = c(0.5, 1.0),
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 1.5,
                                              title.vjust = 0.5,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt)) +
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
  # Add lat/lon grid lines
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Add latitude/longitude labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 13) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 13) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # North polar projection

p4

ggsave(
  filename = "./0.figure/Fig.S3-polar_r_2.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)