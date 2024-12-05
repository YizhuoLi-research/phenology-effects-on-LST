###### 0. Load libraries and data ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(scales)

setwd("D:/VegetationImpact")


############################### 01 Cumulative temperature Polar Map ##################################

r <- raster("./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif")   
plot(r)
df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)  # Convert raster to dataframe with x, y coordinates and values
colnames(df) <- c("long", "lat","sum_diff")  # Rename columns
df$sum_diff <- as.numeric(as.character(df$sum_diff))  # Convert the sum_diff column to numeric
summary(df$sum_diff)  # Get summary statistics of the sum_diff values


# Count the number of rows where sum_diff is greater than 3000
count_greater_than_3000 <- sum(df$sum_diff > 3000)
# Print the result
cat("There are", count_greater_than_3000, "rows where sum_diff is greater than 3000.")
# Set values less than -3500 to -3500, and greater than 3500 to 3500
df$sum_diff <- ifelse(df$sum_diff < -3500, -3500, 
                      ifelse(df$sum_diff > 3500, 3500, df$sum_diff))


library(dplyr)
# library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)  # Filter world map data to keep only latitudes greater than 20

x_lines <- seq(-120,180, by = 60)  # Define the x axis lines (longitude)

p <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # Plot the world map with gray fill
  geom_tile(data = df, aes(x = long, y = lat, fill = sum_diff)) +  # Plot the cumulative sum_diff data as a tile layer
  # geom_raster(data = r, aes(x = x, y = y, fill = sum_diff)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +  # Add map borders with black color
  expand_limits(x = wr$long, y = wr$lat)  +  # Set the x and y limits to match the world map
  
  scale_fill_gradientn(colours = c("#000303","#003366","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9933","#FF6633","#CC3333","#990000"),
                       na.value = "transparent",  # Set transparent for NAs
                       name = "Cumulative ΔLST (℃·day)",  # Legend title
                       values = scales::rescale(c(-3500, -3000, -2000, -1000, 0, 1000, 2000, 3000, 3500)), 
                       limits = c(-3500, 3500),  # Set the value range for color scale
                       breaks = c(-3000,-2000,-1000, 0,1000,2000,3000),  # Define breaks for legend labels
                       labels = c("≤-3000","-2000","-1000", "0","1000","2000","≥3000"),  # Label the breaks
                       guide = guide_colorbar(title.position = "left",  # Customize color bar appearance
                                              title.hjust = 0.5,
                                              barwidth = 60,
                                              title.vjust = 1,
                                              barheight = 1.8,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt)) +  # Adjust tick marks on color bar
  
  theme_minimal() +
  theme( 
    panel.background = element_blank(),  # Set background to transparent
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),  # Remove axis text
    legend.title = element_text(size = 38),  # Set the legend title font size
    legend.text = element_text(size = 38),  # Set the legend text font size
    legend.position = "bottom") +  # Place the legend at the bottom
  scale_x_continuous(breaks = NULL) +  # Remove x-axis labels
  scale_y_continuous(breaks = NULL) +  # Remove y-axis labels
  xlab("") +  # Remove x-axis label
  ylab("") +  # Remove y-axis label
  # Add horizontal and vertical grid lines at specific latitudes and longitudes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Add labels for latitudes and longitudes
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # Add labels for longitude lines at specific intervals
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to show a polar projection
p


# Save the plot as a TIFF image
ggsave(
  filename = "./0.figure/Fig.3-polar_mean.tiff",  # Output file path
  plot = p,  # Plot to save
  width = 22,  # Width of the output image
  height = 15,  # Height of the output image
  units = "in",  # Units for width and height
  dpi = 300)  # Resolution (DPI)
