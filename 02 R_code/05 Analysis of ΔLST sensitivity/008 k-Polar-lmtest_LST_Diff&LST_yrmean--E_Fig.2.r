# Purpose: Map visualization for temperature sensitivity (DT) in polar regions

###### 0. Load libraries ####
library(terra)
library(tidyverse)
library(ggplot2)
library("scales")

setwd("D:/VegetationImpact")

########################################   1-1 DT-SOS-polar map   #########################################################
# DT: Temperature Sensitivity (the effect of temperature on vegetation)


wr <- map_data("world") %>%
  filter(lat > 20)  # Filter the map data to include only latitudes above 20
x_lines <- seq(-120,180, by = 60)  # Defines the x-axis lines for the map

# Load raster data for the temperature sensitivity (DT)
r1 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")  # Note: Modify 1, 2, 3, 4, 5, 6
max_value <- max(r1[], na.rm = TRUE)  # Find the maximum value in the raster
min_value <- min(r1[], na.rm = TRUE)  # Find the minimum value in the raster

# Convert raster data into a dataframe for plotting
df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat", "k_value")  # Assign column names
df1$k_value <- as.numeric(as.character(df1$k_value))  # Convert k_value to numeric
df1$k_value[df1$k_value > 10] <- 10  # Replace values greater than 10 with 10
df1$k_value[df1$k_value < -10] <- -10  # Replace values less than -10 with -10

# Create the plot with ggplot
p1 <- ggplot() +
  # Plot the temperature sensitivity data
  geom_tile(data = df1, aes(x = long, y = lat, fill = k_value)) +
  # Add the world map borders
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # Expand the limits of the plot
  expand_limits(x = wr$long, y = wr$lat) + 
  # Customize the color scale for temperature sensitivity
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF",
                               "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0, 2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0, 2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),
                    guide = guide_colorbar(title.position = "none")) +
  theme(
    panel.background = element_blank(),  # Remove background
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),  # Remove axis text
    legend.position = "none") +  # Remove the legend
  # Customize the grid lines and labels for the map
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines),
               colour = "gray40", linetype = "dashed") +  # Longitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20 line
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 30 line
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 50 line
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 70 line
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Add latitude and longitude labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # Add longitude labels
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  # Apply stereographic projection for the polar map
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # North Pole projection

# Display the plot
p1

# Save the plot as a high-resolution TIFF file
ggsave(
  filename = "./0.figure/E_Fig.2-polar_1.tiff",                        
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################################   1-2 DT-SOS - Frequency Distribution Map   #########################################################
# This section of the code is used to create a frequency distribution of the temperature sensitivity (DT) values,
# grouped into specified intervals, and then plot a bar chart to show the percentage of each segment.


library(dplyr)
library(ggplot2)

# Define break points and corresponding labels for categorizing k_value
my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)", "[-2,-1)", "[-1,0)",
               "[0,1)", "[1,2)", "[2,5)", "＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

# Categorize k_value into segments, filter out NA values, and calculate frequency and percentage
df1 <- df1 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)

# Round percentage to the nearest integer
df1$percentage <- round(df1$percentage)

# Correct the last percentage value to ensure the sum of all percentages equals 100%
error <- 100 - sum(df1$percentage)
df1$percentage[nrow(df1)] <- df1$percentage[nrow(df1)] + error

# Create a bar plot to show the percentage distribution
p11 <- ggplot(df1, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # Adjust text position
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # Show axis tick width
        axis.ticks.length.y = unit(0.2, "cm"),   # Show axis tick length
        axis.title = element_blank(),
        panel.grid = element_line(linetype = "blank"),
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # Set the range of the y-axis to 0-30, indicating percentages
print(p11)

# Save the plot as a TIFF file
ggsave(filename = "./0.figure/E_Fig.2-chart_1.tiff", 
       plot = p11, width = 7.6, height = 4 , units = "in", dpi = 300)


########################################   2-1 DT-MGP-polar map   #########################################################


r2 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif")
max_value <- max(r2[], na.rm = TRUE)  # Get the maximum value in the raster
min_value <- min(r2[], na.rm = TRUE)  # Get the minimum value in the raster

# Convert the raster data to a data frame
df2 <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat", "k_value")  # Rename columns for longitude, latitude, and k_value
df2$k_value <- as.numeric(as.character(df2$k_value))  # Convert k_value to numeric
df2$k_value[df2$k_value > 10] <- 10   # Replace all values greater than 10 with 10
df2$k_value[df2$k_value < -10] <- -10 # Replace all values less than -10 with -10

# Create the polar map plot using ggplot
p2 <- ggplot() +
  # geom_spatraster(data=r)  # Optional: This can be used to plot raster directly
  geom_tile(data = df2, aes(x = long, y = lat, fill = k_value)) +  # Create a tile map for the k_value data
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +  # Add map boundaries
  # labs(title = "(a) SOS", hjust = 1)  # Optional title
  expand_limits(x = wr$long, y = wr$lat) +  # Expand limits for map
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),  # Customize the color scale
                    na.value = "transparent",  # Handle missing values
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels for color scale
                    guide = guide_colorbar(title.position = "none")) +  # Hide color bar title
  theme_minimal() +  # Apply minimal theme
  theme(panel.background = element_blank(),  # Remove background grid
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),   # Remove axis text
        legend.position = "none") +  # Remove legend
  scale_x_continuous(breaks = NULL) +  # Remove x-axis labels
  scale_y_continuous(breaks = NULL) +  # Remove y-axis labels
  xlab("") +  # Remove x-axis label
  ylab("") +  # Remove y-axis label
  # Add horizontal and vertical lines for latitude and longitude
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Add meridian lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Add latitude 20° lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30° latitude
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50° latitude
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70° latitude
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +  # Label the latitude
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +  # Label the longitude
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Use stereographic projection to show the Arctic

# Display the plot
p2

# Save the plot to a file
ggsave(
  filename = "./0.figure/E_Fig.2-polar_2.tiff",
  plot = p2, width = 15, height = 15, units = "in", dpi = 300)


#################################   2-2 DT-MGP-Frequency distribution map   #########################################################


my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)  # Defining the breakpoints for the k_value intervals
my_labels <- c("＜-5",  "[-5,-2)", "[-2,-1)", "[-1,0)",
               "[0,1)", "[1,2)", "[2,5)", "＞5")  # Defining the labels for each interval
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")  # Specifying the color palette for the intervals

# Creating a new column 'segment' that categorizes 'k_value' into predefined intervals,
# and calculating the frequency and percentage of each interval.
df2 <- df2 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%  # Categorizing k_value into segments
  filter(!is.na(segment)) %>%  # Removing rows with NA segments
  group_by(segment) %>%  # Grouping the data by segments
  summarise(frequency = n()) %>%  # Counting the frequency of each segment
  ungroup() %>%  # Ungrouping to allow further manipulation
  mutate(percentage = frequency / sum(frequency) * 100)  # Calculating the percentage of each segment

# Rounding the percentages to the nearest integer
df2$percentage <- round(df2$percentage)

# Correcting the last percentage value to ensure the total sums to 100%
error <- 100 - sum(df2$percentage)
df2$percentage[nrow(df2)] <- df2$percentage[nrow(df2)] + error  # Adjusting the last percentage

# Creating a bar plot to visualize the frequency distribution of k_value intervals
p22 <- ggplot(df2, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +  # Plotting the bar chart
  geom_text(aes(label = paste0(round(percentage, 0), "%")),  # Adding labels with percentage values to the bars
            vjust = -0.5,  # Adjusting label position
            size = 10,  # Label text size
            color = "black") +  # Label text color
  scale_fill_manual(values = my_colors, drop = FALSE) +  # Setting the color palette
  theme_bw() +  # Using a white background theme
  coord_fixed(ratio = 1/8) +  # Setting a fixed aspect ratio for the plot
  theme(legend.position = "none",  # Hiding the legend
        axis.text.x = element_text(size = 25),  # Adjusting x-axis text size
        axis.text.y = element_text(size = 25),  # Adjusting y-axis text size
        axis.ticks.y = element_line(size = 1),  # Adjusting the width of y-axis ticks
        axis.ticks.length.y = unit(0.2, "cm"),  # Adjusting the length of y-axis ticks
        axis.title = element_blank(),  # Removing axis titles
        panel.grid = element_line(linetype = "blank"),  # Hiding grid lines
        axis.ticks.x = element_blank()) +  # Hiding x-axis ticks
  ylim(0, 30)  # Setting y-axis range to 0-30 to show percentage distribution
print(p22)  # Printing the plot

# Saving the plot as a .tiff file with high resolution
ggsave(filename = "./0.figure/E_Fig.2-chart_2.tiff", 
       plot = p22, width = 7.6, height = 4 , units = "in", dpi = 300)


########################################   3-1 DT-GMO-polar map   #########################################################


# Load world map data and filter for latitudes greater than 20
wr <- map_data("world") %>%
  filter(lat > 20)

# Define the x-axis lines at intervals of 60 degrees
x_lines <- seq(-120, 180, by = 60)

# Load the raster data for the temperature difference map
r3 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_3.tif")

# Find the maximum and minimum values of the raster data for scaling
max_value <- max(r3[], na.rm = TRUE)
min_value <- min(r3[], na.rm = TRUE)

# Convert the raster data to a data frame and rename the columns
df3 <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat", "k_value") 
df3$k_value <- as.numeric(as.character(df3$k_value))

# Limit the values of k_value to a range of [-10, 10] for visualization
df3$k_value[df3$k_value > 10] <- 10   # Replace values greater than 10 with 10
df3$k_value[df3$k_value < -10] <- -10 # Replace values less than -10 with -10

# Create the polar map plot
p3 <- ggplot() +
  # Fill the map with the k_value data
  geom_tile(data = df3, aes(x = long, y = lat, fill = k_value)) +
  # Add the world map boundary
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # Expand limits to ensure the map is fully displayed
  expand_limits(x = wr$long, y = wr$lat) + 
  # Define the color palette and breaks for the fill scale
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0, 2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0, 2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels for the breaks
                    guide = guide_colorbar(title.position = "none")) +
  # Minimal theme for the map
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  # Remove axis ticks and labels
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Add grid lines and labels for latitude and longitude
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°N
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 30°N
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 50°N
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line at 70°N
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Add labels for latitudes (30°N to 70°N) and longitudes (120°W to 180°W)
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  # Set the stereographic projection for the map
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # North Pole projection
p3  # Display the plot

# Save the plot as a .tiff file with high resolution
ggsave(
  filename = "./0.figure/E_Fig.2-polar_3.tiff",
  plot = p3, width = 15, height = 15, units = "in", dpi = 300)


#################################   3-2 DT-GMO-Frequency distribution map   #########################################################


my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

# Create a new column 'segment' based on the k_value and the specified breaks
df3 <- df3 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  # Filter out rows with NA values
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)

# Round the percentages to the nearest integer
df3$percentage <- round(df3$percentage)

# Adjust the last count to ensure the sum is exactly 100%
error <- 100 - sum(df3$percentage)
df3$percentage[nrow(df3)] <- df3$percentage[nrow(df3)] + error

# Generate the bar chart showing the frequency distribution as percentages
p33 <- ggplot(df3, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # Adjust text position
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # Show width of axis ticks
        axis.ticks.length.y = unit(0.2, "cm"),   # Set length of axis ticks
        axis.title = element_blank(),
        panel.grid = element_line(linetype = "blank"),
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # Set the y-axis range from 0 to 30 to show percentage
print(p33)

# Save the plot as a .tiff file
ggsave(filename = "./0.figure/E_Fig.2-chart_3.tiff", 
       plot = p33, width = 7.6, height = 4 , units = "in", dpi = 300)


########################################   4-1 DT-GDO-polar map   #########################################################


wr <- map_data("world") %>%
  filter(lat > 20)  # Filter to only include latitudes greater than 20 degrees
# head(wr)  # Optional: View the first few rows of the world map data
x_lines <- seq(-120,180, by = 60)  # Defines the x-axis lines required (longitude)

# Load the raster data and calculate the max and min values
r4 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif")
max_value <- max(r4[], na.rm = TRUE)
min_value <- min(r4[], na.rm = TRUE)

# Convert raster data to a data frame for plotting
df4 <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","k_value") 
df4$k_value <- as.numeric(as.character(df4$k_value))     

# Limit k_value to range [-10, 10]
df4$k_value[df4$k_value > 10] <- 10   # Replace values greater than 10 with 10
df4$k_value[df4$k_value < -10] <- -10 # Replace values smaller than -10 with -10

# Create the plot using ggplot2
p4 <- ggplot() +
  # Plot the k_value as a tile map (using long and lat as x and y axes)
  geom_tile(data = df4, aes(x = long, y = lat, fill = k_value)) +
  
  # Add country boundaries on the map
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  
  # Expand the map limits to include all longitudes and latitudes from the world map
  expand_limits(x = wr$long, y = wr$lat) + 
  
  # Define the color scale for k_value, with custom labels and breaks
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels
                    guide = guide_colorbar(title.position = "none")) +
  
  # Apply a minimal theme
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  
  # Remove axis ticks and labels
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  
  # Add latitude and longitude lines and labels
  geom_hline(aes(yintercept = 20), size = 0.1) +  # Add a horizontal line at 20°N
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Add dashed lines for longitudes
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Add a horizontal line at 20°N
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Add dashed lines at 30°N
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Add dashed lines at 50°N
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Add dashed lines at 70°N
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  
  # Add labels for latitudes and longitudes
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  
  # Set the map projection to stereographic for a polar view
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display a North Pole projection

# Display the map
p4

# Save the plot as a .tiff file
ggsave(
  filename = "./0.figure/E_Fig.2-polar_4.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)


#################################   4-2 DT-GDO-Frequency distribution map   #########################################################

#################################   4-2 DT-GDO-Frequency distribution map   #########################################################

# Purpose: Define the breaks for the k_value categories and their corresponding labels and colors
my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

# Purpose: Categorize k_value into segments based on predefined breaks, and calculate the frequency and percentage of each segment
df4 <- df4 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)

# Purpose: Round the percentage values to the nearest integer
df4$percentage <- round(df4$percentage)

# Purpose: Correct the last percentage value to ensure the sum is exactly 100%
error <- 100 - sum(df4$percentage)
df4$percentage[nrow(df4)] <- df4$percentage[nrow(df4)] + error

# Purpose: Create a bar plot to visualize the frequency distribution of the segments with percentage labels
p44 <- ggplot(df4, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # Adjust the position of the text labels
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # Display axis tick width
        axis.ticks.length.y = unit(0.2, "cm"),   # Display axis tick length
        axis.title = element_blank(),
        panel.grid = element_line(linetype = "blank"),
        # axis.text.x = element_blank(),  # Hide x axis text
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # Set the y-axis range from 0 to 30 for percentage display
print(p44)

# Purpose: Save the plot as a TIFF file
ggsave(filename = "./0.figure/E_Fig.2-chart_4.tiff", 
       plot = p44, width = 7.6, height = 4 , units = "in", dpi = 300)


########################################   5-1 DT-MSP-polar map   #########################################################


# Load world map data and filter for latitudes greater than 20.
wr <- map_data("world") %>%
  filter(lat > 20)
# Defines the x axes required for the map.
x_lines <- seq(-120, 180, by = 60)

# Load raster data from the specified file and get the max and min values.
r5 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif")
max_value <- max(r5[], na.rm = TRUE)
min_value <- min(r5[], na.rm = TRUE)

# Convert the raster data to a dataframe, renaming columns and adjusting k_value.
df5 <- as.data.frame(r5, xy = TRUE, na.rm = TRUE)
colnames(df5) <- c("long", "lat", "k_value")
df5$k_value <- as.numeric(as.character(df5$k_value))
df5$k_value[df5$k_value > 10] <- 10   # Replace values greater than 10 with 10.
df5$k_value[df5$k_value < -10] <- -10 # Replace values less than -10 with -10.

# Create a polar map using ggplot with custom color scales and map projections.
p5 <- ggplot() +
  # Add raster data as tiles
  geom_tile(data = df5, aes(x = long, y = lat, fill = k_value)) +
  # Overlay world map outline
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat) +
  # Customize color scale for the plot.
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF",
                               "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0, 2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels for the color scale.
                    guide = guide_colorbar(title.position = "none")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") +
  ylab("") +
  # Add horizontal and vertical lines for latitude and longitude.
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
  # Add text labels for latitude and longitude.
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15,
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Stereographic projection to show the North Pole.

# Print the polar map.
p5

# Save the plot as a high-resolution TIFF file.
ggsave(
  filename = "./0.figure/E_Fig.2-polar_5.tiff",
  plot = p5, width = 15, height = 15, units = "in", dpi = 300)


#################################   5-2 DT-MSP-Frequency distribution map   #########################################################


# Define break points for categorizing the k_value data.
my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
# Define the labels for the categories.
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
# Define colors for each category.
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

# Categorize the k_value into segments and calculate the frequency of each segment.
df5 <- df5 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%  # Categorize k_value
  filter(!is.na(segment)) %>%  # Remove NA values from the data
  group_by(segment) %>%
  summarise(frequency = n()) %>%  # Count the frequency of each category
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)  # Calculate percentage of each category

# Round the percentages to integers for better presentation.
df5$percentage <- round(df5$percentage)

# Correct the last count to ensure the total sum is exactly 100%.
error <- 100 - sum(df5$percentage)
df5$percentage[nrow(df5)] <- df5$percentage[nrow(df5)] + error

# Create a bar chart to display the percentage distribution of each segment.
p55 <- ggplot(df5, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +  # Create a bar for each segment
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # Adjust position of the text labels
            size = 10,                   
            color = "black") +   # Text color
  scale_fill_manual(values = my_colors, drop = FALSE) +  # Apply custom colors
  theme_bw() +  # Use a clean theme
  coord_fixed(ratio = 1/8) +  # Adjust the aspect ratio of the plot
  theme(legend.position = "none",  # Remove the legend
        axis.text.x = element_text(size = 25),  # Set size for x-axis labels
        axis.text.y = element_text(size = 25),  # Set size for y-axis labels
        axis.ticks.y = element_line(size = 1),  # Show y-axis tick marks
        axis.ticks.length.y = unit(0.2, "cm"),   # Set the length of y-axis tick marks
        axis.title = element_blank(),  # Remove axis titles
        panel.grid = element_line( linetype = "blank"),  # Remove grid lines
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  ylim(0, 30)  # Set the y-axis range from 0 to 30 for percentage display

# Display the plot.
print(p55)

# Save the plot as a high-resolution TIFF file.
ggsave(filename = "./0.figure/E_Fig.2-chart_5.tiff", 
       plot = p55, width = 7.6, height = 4 , units = "in", dpi = 300)


########################################   6-1 DT-EOS-polar map   #########################################################


# Load world map data and filter for latitudes above 20°.
wr <- map_data("world") %>%
  filter(lat > 20)
# Define the x-axis lines for the map (longitude).
x_lines <- seq(-120, 180, by = 60)

# Read raster data for the temperature difference between LST and actual LST.
r6 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")
# Find the maximum and minimum values in the raster.
max_value <- max(r6[], na.rm = TRUE)
min_value <- min(r6[], na.rm = TRUE)

# Convert the raster data to a data frame and clean the data.
df6 <- as.data.frame(r6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat","k_value") 
df6$k_value <- as.numeric(as.character(df6$k_value))

# Replace values greater than 10 with 10 and values less than -10 with -10.
df6$k_value[df6$k_value > 10] <- 10   
df6$k_value[df6$k_value < -10] <- -10 

# Create the plot using ggplot2.
p6 <- ggplot() +
  # Add the raster data as a tile map.
  geom_tile(data = df6, aes(x = long, y = lat, fill = k_value)) +
  # Add the world map outline with black borders.
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +  # Expand limits to fit the world map.
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),  # Custom color scale with labels.
                    na.value = "transparent",  # Set transparent for NA values.
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels for the color scale.
                    guide = guide_colorbar(title.position = "none")) +  # Remove the colorbar title.
  theme_minimal() +  # Apply minimal theme.
  theme(panel.background = element_blank(),  # Remove background panel.
        axis.ticks = element_blank(),  # Hide axis ticks.
        axis.title = element_blank(),  # Remove axis titles.
        axis.text = element_blank(),  # Hide axis labels.
        legend.position = "none") +  # Remove legend.
  scale_x_continuous(breaks = NULL) +  # Remove x-axis breaks.
  scale_y_continuous(breaks = NULL) +  # Remove y-axis breaks.
  xlab("") +  # Remove x-axis label.
  ylab("") +  # Remove y-axis label.
  # Add horizontal line at 20°N.
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  # Add dashed longitude lines at regular intervals.
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +
  # Add a solid latitude line at 20°N.
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  
  # Add dashed latitude lines at 30°, 50°, and 70°N.
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30°N
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50°N
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70°N
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Add text labels for latitude lines.
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # Add text labels for longitude lines.
  geom_text(aes( y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  # Apply stereographic projection to visualize the map.
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # North Pole projection

# Display the plot.
p6

# Save the plot as a high-resolution TIFF file.
ggsave(
  filename = "./0.figure/E_Fig.2-polar_6.tiff",
  plot = p6,  width = 15,  height = 15,  units = "in",  dpi = 300)



#################################   6-2 DT-EOS-Frequency distribution map   #########################################################


my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)  # Define the breakpoints for k_value bins
my_labels <- c("＜-5",  "[-5,-2)", "[-2,-1)", "[-1,0)",        # Labels corresponding to the breakpoints
               "[0,1)", "[1,2)", "[2,5)", "＞5")
my_colors <- c("#08519c", "#2171b5", "#6baed6", "#bdd7e7",      # Define the color palette for each bin
               "#fcae91", "#FC8D59", "#fb6a4a", "#cb181d")

# Mutate the data by cutting the k_value into the defined segments, calculate frequency and percentage of each bin
df6 <- df6 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%  # Categorize k_value into defined segments
  filter(!is.na(segment)) %>%  # Remove NA values
  group_by(segment) %>%  # Group by segments
  summarise(frequency = n()) %>%  # Calculate frequency of each segment
  ungroup() %>%  # Ungroup after summarization
  mutate(percentage = frequency / sum(frequency) * 100)  # Calculate percentage for each segment

# Round percentages to the nearest integer
df6$percentage <- round(df6$percentage)

# Adjust the last percentage value to ensure the total adds up to 100%
error <- 100 - sum(df6$percentage)
df6$percentage[nrow(df6)] <- df6$percentage[nrow(df6)] + error

# Purpose: The following ggplot code creates a bar chart visualizing the frequency distribution of k_value categories.

p66 <- ggplot(df6, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +  # Create the bar chart
  geom_text(aes(label = paste0(round(percentage, 0), "%")),  # Display percentage on top of each bar
            vjust = -0.5,  # Adjust text position
            size = 10,                   
            color = "black") +   # Set text color to black
  scale_fill_manual(values = my_colors, drop = FALSE) +  # Apply the custom color palette
  theme_bw() +  # Set black and white theme
  coord_fixed(ratio = 1/8) +  # Adjust aspect ratio for better visualization
  theme(legend.position = "none",  # Remove the legend
        axis.text.x = element_text(size = 25),  # Set font size for x-axis labels
        axis.text.y = element_text(size = 25),  # Set font size for y-axis labels
        axis.ticks.y = element_line(size = 1),  # Show y-axis ticks
        axis.ticks.length.y = unit(0.2, "cm"),  # Set the length of y-axis ticks
        axis.title = element_blank(),  # Remove axis titles
        panel.grid = element_line(linetype = "blank"),  # Remove gridlines
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  ylim(0, 30)  # Set y-axis range from 0 to 30 to represent percentage
print(p66)

# Save the plot as a high-quality TIFF file
ggsave(filename = "./0.figure/E_Fig.2-chart_6.tiff", 
       plot = p66, width = 7.6, height = 4 , units = "in", dpi = 300)
