##########################     0. Load packages and data      ############################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library("scales")

# library(ggspatial)
setwd("D:/VegetationImpact")

################     0. Get the average image of 9 years of difference (6 PHE images)   ####################
####################### Average image for EA region
file_list <- list.files("./EA_Results/0.diff_result/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)
#"./EA_Results/0.diff_result"
file_groups <- substr(basename(file_list), 1, 14)  # SOS -- 9 years; ... Extract the first x characters for grouping

file_groups_name <- unique(file_groups)

# Create a new folder
new_folder <- "./EA_Results/0.diff_result/EA_DIFF_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# Calculate the average image for each group and save it
for (group in file_groups_name) {
  # Select files belonging to the current group
  current_files <- file_list[file_groups == group]
  
  # Read the images of the current group and calculate the average
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images), na.rm = TRUE)
  
  # Generate the new file name and save the image
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name, overwrite = TRUE)
}


####################### Average image for NA region

file_list <- list.files("./NA_Results/0.diff_result/0common_pixel", pattern = "\\.tif$", 
                        full.names = TRUE)
#"./EA_Results/0.diff_result"
file_groups <- substr(basename(file_list), 1, 14)  # SOS -- 9 years; ... Extract the first x characters for grouping

file_groups_name <- unique(file_groups)

# Create a new folder
new_folder <- "./NA_Results/0.diff_result/NA_DIFF_9yearAverge"
dir.create(new_folder, showWarnings = FALSE)

# Calculate the average image for each group and save it
for (group in file_groups_name) {
  # Select files belonging to the current group
  current_files <- file_list[file_groups == group]
  
  # Read the images of the current group and calculate the average
  current_images <- lapply(current_files, raster::raster)
  average_image <- raster::mean(stack(current_images), na.rm = TRUE)
  
  # Generate the new file name and save the image
  new_file_name <- file.path(new_folder, paste0(group, ".tif"))
  raster::writeRaster(average_image, filename = new_file_name, overwrite = TRUE)
}


############################### 1-1 SOS Polar Map ##################################
######## (a) SOS
library(dplyr)
library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)  # Filter data for latitudes greater than 20°N

# Load raster data for North America (NA) and Europe-Asia (EA)
r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_1.tif")   # North America
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_1.tif")  # Europe-Asia

# Merge raster data
merged_diff_raster_1 <- merge(r1, r11)

# Convert raster data to a dataframe for plotting
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat", "average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))  # Ensure data is numeric
df1$group <- "SOS"  # Assign group label for "SOS"

# Define x-axis lines for longitude
x_lines <- seq(-120, 180, by = 60)

# Create the polar projection plot
p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +  # World map with light gray fill
  geom_tile(data = df1, aes(x = long, y = lat, fill = average_diff)) +    # Add raster tiles
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) + # Overlay country borders
  expand_limits(x = wr$long, y = wr$lat) +                               # Adjust axis limits
  scale_fill_gradientn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", "#FFFFFF",
                "#FF9900", "#CC3333"), 
    na.value = "transparent",
    name = expression(Delta * "LST (℃)"),  # Legend title for temperature difference
    values = rescale(c(-50, -40, -30, -20, -10, 0, 10, 20)),
    guide = "colorbar", 
    limits = c(-50, 20)
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),  # Blank background
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Longitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20°N
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 30°N
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 50°N
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 70°N
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 5) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 5) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Stereographic polar projection
p1

ggsave(
  filename = "./0.figure/Fig.2-polar_1.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)


########################### 1-2 SOS Frequency Distribution Histogram ##################################

# Optional: Get min, max, and median values
# min_value <- min(df1$average_diff, na.rm = TRUE)
# max_value <- max(df1$average_diff, na.rm = TRUE)
# median_value1 <- median(df1$average_diff, na.rm = TRUE)

df1$average_diff[df1$average_diff > 20] <- 20.5  # Cap values greater than 20
df1$average_diff[df1$average_diff < -20] <- -20.5  # Cap values less than -20

# Group the data into Positive and Negative categories based on average_diff
df1$group <- cut(df1$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)

# Calculate median for annotation
label_text <- sprintf("%.2f", round(median(df1$average_diff, na.rm = TRUE), 2))

# Plot the histogram
p11 <- ggplot(df1, aes(x = average_diff, fill = group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" = "lightblue")) +
  
  # Annotate the median on the plot
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 7200,
           label = label_text, color = "gray20", size = 11, hjust = -0.3) +
  
  # Customize plot appearance
  theme_bw() +
  theme(axis.text.y = element_text(size = 27, color = "gray20"),
        axis.text.x = element_text(size = 26, color = "gray20"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  
  # Set y-axis limit and x-axis formatting
  ylim(0, 8000) +
  coord_fixed(ratio = 1/270) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),  # Using Unicode for "greater than"
                     limits = c(-20, 22))

# Save the plot to file
ggsave(filename = "./0.figure/Fig.2-Median_1.tiff", 
       plot = p11, width = 4.5, height = 3, units = "in", dpi = 300)


############################### 2-1 MGP Polar Map ##################################

r2 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_2.tif")   # NA
r22 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_2.tif")  # EA
merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_diff") 
df2$average_diff <- (as.numeric(as.character(df2$average_diff)))     #################
df2$group <- "MGP"

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # Fill map with light gray
  geom_tile(data = df2, aes(x = long, y = lat, fill = average_diff)) +     # Create the map tiles for the average temperature difference
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +   # Add borders around regions
  expand_limits(x = wr$long, y = wr$lat)  +  # Set the longitude and latitude limits
  scale_fill_gradientn(colours = c("#000033","#003366","#006699","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9900","#CC3333" ), 
                       na.value = "transparent",   # Color gradient scale for the average temperature difference
                       name = "ΔLST (℃)",
                       values = rescale(c(-50,-40,-30,-20,-10,0,10,20)),
                       guide = "colorbar", limits=c(-50,20))+
  theme_minimal() +
  theme( 
    panel.background = element_blank(),   # Remove the panel background
    axis.ticks = element_blank(),          # Remove axis ticks
    axis.title = element_blank(),          # Remove axis titles
    axis.text = element_blank(),           # Remove axis text
    legend.position = "none") +            # Remove legend
  scale_x_continuous(breaks = NULL) +      # No x-axis breaks
  scale_y_continuous(breaks = NULL) +      # No y-axis breaks
  xlab("") +                               # Remove x-axis label
  ylab("") +                               # Remove y-axis label
  # Add lines for latitude and longitude
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20°N
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 30°N Latitude line
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 50°N Latitude line
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # 70°N Latitude line
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Add labels for latitude
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # Add labels for longitude
  geom_text(aes( y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) 
p2

ggsave(
  filename = "./0.figure/Fig.2-polar_2.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)

########################### 2-2 GMP Frequency Distribution Histogram ##################################

df2$average_diff[df2$average_diff > 20] <- 20.5
df2$average_diff[df2$average_diff < -20] <- -20.5
df2$group <- cut(df2$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df2$average_diff, na.rm = TRUE), 2))

p22 <- ggplot(df2, aes(x = df2$average_diff, fill = df2$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df2$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df2$average_diff, na.rm = TRUE), y = 7200,
           label = label_text,
           color = "gray20", size = 11, hjust = -0.3, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 27,color = "gray20"),   # Y-axis text size and color
        axis.text.x =  element_text(size = 26,color = "gray20"),   # X-axis text size and color
        axis.title = element_blank(),                             # Remove axis titles
        panel.grid = element_blank(),                             # Remove grid lines
        legend.position = "none") +                               # Remove legend
  ylim(0, 8000) +                                              # Set Y-axis limits
  coord_fixed(ratio = 1/270)+                                   # Fix the aspect ratio
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),   # Label the X-axis
                     limits = c(-20, 22))                          # Set X-axis limits
p22

ggsave(filename =  "./0.figure/Fig.2-Median_2.tiff", 
       plot = p22, width = 4.5, height = 3 , units = "in", dpi = 300)


############################### 3-1 GMO Polar Map ##################################

r3 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_3.tif")   # NA
r33 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_3.tif")  # EA
merged_diff_raster_3 <- merge(r3, r33)
df3 <- as.data.frame(merged_diff_raster_3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat", "average_diff") 
df3$average_diff <- (as.numeric(as.character(df3$average_diff)))    #################
df3$group <- "GMO"

p3 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # Fill the map with light gray color
  geom_tile(data = df3, aes(x = long, y = lat, fill = average_diff)) +   # Create tile layers based on data
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +  # Map borders in black color
  expand_limits(x = wr$long, y = wr$lat)  +                           # Specify longitude and latitude limits
  scale_fill_gradientn(colours = c("#000033","#003366","#006699","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9900","#CC3333" ), 
                       na.value = "transparent",                      # Set transparent for NA values
                       name = "ΔLST (℃)",                           # Color legend name
                       values = rescale(c(-50,-40,-30,-20,-10,0,10,20)), # Rescale color values
                       guide = "colorbar", limits = c(-50, 20)) +    # Set color scale limits
  theme_minimal() +
  theme(panel.background = element_blank(),                           # Remove panel background
        axis.ticks = element_blank(),                                 # Remove axis ticks
        axis.title = element_blank(),                                 # Remove axis titles
        axis.text = element_blank(),                                  # Remove axis text
        legend.position = "none") +                                   # Remove legend
  scale_x_continuous(breaks = NULL) +                                 # Remove X-axis breaks
  scale_y_continuous(breaks = NULL) +                                 # Remove Y-axis breaks
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +                      # Draw a horizontal line at 20° latitude
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Draw dashed meridian lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Draw the equator (latitude 20)
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Draw dashed lines at 30°N
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Draw dashed lines at 50°N
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Draw dashed lines at 70°N
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +  # Add latitude labels at 30°, 50°, 70°N
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +  # Add longitude labels
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # Display using a stereographic projection
p3

ggsave(
  filename = "./0.figure/Fig.2-polar_3.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)


###########################  3-2 GMO Frequency Distribution Histogram   ##################################

df3$average_diff[df3$average_diff > 20] <- 20.5
df3$average_diff[df3$average_diff < -20] <- -20.5
df3$group <- cut(df3$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df3$average_diff, na.rm = TRUE), 2))

p33 <- ggplot(df3, aes(x = df3$average_diff, fill = df3$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df3$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 7200,
           label = label_text,
           color = "gray20", size = 11, hjust = -0.3, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 27, color = "gray20"),
        axis.text.x =  element_text(size = 26, color = "gray20"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 8000) +
  coord_fixed(ratio = 1/270) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),   # "\u2264-20"
                     limits = c(-20, 22))
p33

ggsave(filename =  "./0.figure/Fig.2-Median_3.tiff", 
       plot = p33, width = 4.5, height = 3 , units = "in", dpi = 300)


############################### 4-1 GDO Polar Map  ##################################

r4 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_4.tif")   # NA
r44 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_4.tif")  # EA
merged_diff_raster_4 <- merge(r4, r44)
df4 <- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat", "average_diff") 
df4$average_diff <- (as.numeric(as.character(df4$average_diff)))    #################
df4$group <- "GDO"

p4 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
  geom_tile(data = df4, aes(x = long, y = lat, fill = average_diff)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # Specify longitude and latitude limits
  scale_fill_gradientn(colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", "#FFFFFF",
                                   "#FF9900", "#CC3333"), 
                       na.value = "transparent",
                       name = "ΔLST (℃)",
                       values = rescale(c(-50, -40, -30, -20, -10, 0, 10, 20)),
                       guide = "colorbar", limits = c(-50, 20)) +
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
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line at 20
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If displaying the polar projection
p4

ggsave(
  filename = "./0.figure/Fig.2-polar_4.tiff",
  plot = p4, width = 15, height = 15, units = "in", dpi = 300)


########################### 4-2 GDO Frequency Distribution Histogram  ##################################

df4$average_diff[df4$average_diff > 20] <- 20.5
df4$average_diff[df4$average_diff < -20] <- -20.5
df4$group <- cut(df4$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df4$average_diff, na.rm = TRUE), 2))

p44 <- ggplot(df4, aes(x = df4$average_diff, fill = df4$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df4$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 7200,
           label = label_text,
           color = "gray20", size = 11, hjust = -0.3, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 27,color = "gray20"),
        axis.text.x =  element_text(size = 26,color = "gray20"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 8000) +
  coord_fixed(ratio = 1/270) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),   # "\u2264-20"
                     limits = c(-20, 22))
p44

ggsave(filename =  "./0.figure/Fig.2-Median_4.tiff", 
       plot = p44, width = 4.5, height = 3 , units = "in", dpi = 300)


############################### 5-1 MSP Polar Map ##################################

r5 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_5.tif")   #NA
r55 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_5.tif")  #EA
merged_diff_raster_5 <- merge(r5, r55)
df5 <- as.data.frame(merged_diff_raster_5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","average_diff") 
df5$average_diff <- (as.numeric(as.character(df5$average_diff)))  #################
df5$group <- "MSP"

p5 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df5, aes(x = long, y = lat, fill = average_diff)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # Specify latitude and longitude
  scale_fill_gradientn(colours = c("#000033","#003366","#006699","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9900","#CC3333" ), 
                       na.value = "transparent",
                       name = "ΔLST (℃)",
                       values = rescale(c(-50,-40,-30,-20,-10,0,10,20)),
                       guide = "colorbar", limits=c(-50,20))+
  theme_minimal() +
  theme(panel.background = element_blank(),
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
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line 20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If displaying a polar stereographic projection

p5

ggsave(
  filename = "./0.figure/Fig.2-polar_5.tiff",
  plot = p5,  width = 15,  height = 15,  units = "in",  dpi = 300)


########################### 5-2 MSP Frequency Distribution Histogram ##################################

df5$average_diff[df5$average_diff > 20] <- 20.5
df5$average_diff[df5$average_diff < -20] <- -20.5
df5$group <- cut(df5$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df5$average_diff, na.rm = TRUE), 2))

p55 <- ggplot(df5, aes(x = df5$average_diff, fill = df5$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df5$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 7200,
           label = label_text,
           color = "gray20", size = 11, hjust = -0.3, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 27,color = "gray20"),
        axis.text.x =  element_text(size = 26,color = "gray20"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 8000) +
  coord_fixed(ratio = 1/270)+
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),   #"\u2264-20"
                     limits = c(-20, 22))
p55

ggsave(filename =  "./0.figure/Fig.2-Median_5.tiff", 
       plot = p55, width = 4.5, height = 3 , units = "in", dpi = 300)


############################### 6-1 EOS Polar Map ##################################

r6 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   # NA
r66 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  # EA
merged_diff_raster_6 <- merge(r6, r66)
df6 <- as.data.frame(merged_diff_raster_6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat", "average_diff") 
df6$average_diff <- (as.numeric(as.character(df6$average_diff)))  #################
df6$group <- "MSP"

p6 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
  geom_tile(data = df6, aes(x = long, y = lat, fill = average_diff)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           # specify latitude and longitude
  scale_fill_gradientn(colours = c("#000033","#003366","#006699","#3399CC","#66CCFF","#FFFFFF",
                                   "#FF9900","#CC3333" ), 
                       na.value = "transparent",
                       name = "ΔLST (℃)",
                       values = rescale(c(-50,-40,-30,-20,-10,0,10,20)),
                       guide = "colorbar", limits=c(-50,20)) +
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
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +     # Longitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude line 20
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line 30
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line 50
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude line 70
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  geom_text(aes( y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If showing the North Pole projection
p6

ggsave(
  filename = "./0.figure/Fig.2-polar_6.tiff",
  plot = p6,  width = 15,  height = 15,  units = "in",  dpi = 300)


########################### 6-2 EOS Frequency Distribution Histogram ##################################

df6$average_diff[df6$average_diff > 20] <- 20.5
df6$average_diff[df6$average_diff < -20] <- -20.5
df6$group <- cut(df6$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df6$average_diff, na.rm = TRUE), 2))

p66 <- ggplot(df6, aes(x = df6$average_diff, fill = df6$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df6$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 7200,
           label = label_text,
           color = "gray20", size = 11, hjust = -0.3, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 27, color = "gray20"),
        axis.text.x =  element_text(size = 26, color = "gray20"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 8000) +
  coord_fixed(ratio = 1/270) +
  scale_x_continuous(breaks = c(-20, -10, 0, 10, 20), 
                     labels = c("-20", "-10", "0", "10", "\u003E20"),   # "\u2264-20"
                     limits = c(-20, 22))
p66

ggsave(filename =  "./0.figure/Fig.2-Median_6.tiff", 
       plot = p66, width = 4.5, height = 3 , units = "in", dpi = 300)

############################### 7  Sum Positive and Negative Value Distribution Histogram ##################################

values1 <- df1$average_diff
values2 <- df2$average_diff
values3 <- df3$average_diff
values4 <- df4$average_diff
values5 <- df5$average_diff
values6 <- df6$average_diff

# Function to calculate the data frame for a list of values
calculate_data_merged <- function(values, group_name) {
  non_null_count <- sum(!is.na(values))
  negative_count <- sum(values < 0, na.rm = TRUE)
  positive_count <- sum(values > 0, na.rm = TRUE)
  
  negative_percentage <- round((negative_count / non_null_count) * 100, 2)
  positive_percentage <- round((positive_count / non_null_count) * 100, 2)
  
  total_percentage <- 100
  
  data_merged <- data.frame(
    ΔLST  = c("Negative", "Positive"),
    Percentage = c(negative_percentage, positive_percentage),
    LabelPos = c(25, 94), # Adjust label position
    group = group_name
  )
  
  return(data_merged)
}

# List containing all values
value_list <- list(values1, values2, values3, values4, values5, values6)
group_names <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")

# Apply function to each value in the list and store results in a list
result_list <- lapply(1:length(value_list), function(i) {
  calculate_data_merged(value_list[[i]], group_names[i])
})

# Merge all data_merged
final_data_merged <- do.call(rbind, result_list)
final_data_merged$group <- factor(final_data_merged$group, 
                                  c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")) # Control order
final_data_merged$ΔLST <- factor(final_data_merged$ΔLST, levels = c("Positive", "Negative"))
head(final_data_merged)

# Plot bar chart
p7 <- ggplot(final_data_merged, aes(x = group, y = Percentage, fill = ΔLST)) +
  geom_col(position = "stack", width = 0.53) +  # Adjust bar width to 0.53
  labs(x = "Phenological event", y = "Percentage (%)") +
  scale_fill_manual(values = c("Negative" ="lightblue", "Positive" = "orange")) +
  theme_bw() +
  geom_text(aes(label = paste0(sprintf("%.0f", Percentage), "%"), y = LabelPos), size = 10) +
  theme(axis.line.y = element_line(),   # Display x-axis line
        axis.line.x = element_line(),   # Display y-axis line
        axis.ticks.x = element_line(),  # Display axis ticks
        axis.ticks.y = element_line(size = 1),  # Display axis tick width
        axis.ticks.length.y = unit(0.2, "cm"),   # Display axis tick length
        axis.title = element_text(size = 32),
        axis.text = element_text(size = 29, color = "gray20"),
        axis.title.x = element_text(margin = margin(t = 15)),  # Adjust x-axis title position
        legend.title = element_text(size = 29, hjust = 0.5),
        legend.text = element_text(size = 29),
        legend.spacing.y = unit(0.7, "cm"),                    # Distance between legend title and legend
        legend.position = "right",
        panel.grid = element_line(linetype = "blank")) +
  geom_point(data = subset(final_data_merged, ΔLST == "Negative"),
             aes(x = as.numeric(group), y = Percentage),
             color = "red", size = 1.5, show.legend = FALSE) +  # Add red dots at corresponding positions
  geom_smooth(data = subset(final_data_merged, ΔLST == "Negative"),
              aes(x = as.numeric(group), y = Percentage),
              method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red", size = 1,
              show.legend = FALSE) +  # Use polynomial fitting of degree 2
  guides(fill = guide_legend(byrow = TRUE)) +                   # Internal spacing of legend
  coord_fixed(ratio = 1/90) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 130))   # Customize y-axis scale
p7

ggsave(filename =  "./0.figure/Fig.2-P&Npercent_7.tiff", 
       plot = p7, width = 25, height = 5 , units = "in", dpi = 300)



############################### 8  AmeriFlux Positive and Negative Value Distribution Histogram ##################################

df <- read.csv("./AmerifluxData_Analysis/1330_Noen&Normal_Results_1y_17 final-info.csv")
head(df)
values1 <- df$average_diff_21_mean 
values2 <- df$average_diff_26_mean 

# Function to calculate the data frame for a list of values
calculate_data_merged <- function(values, group_name) {
  non_null_count <- sum(!is.na(values))
  negative_count <- sum(values < 0, na.rm = TRUE)
  positive_count <- sum(values > 0, na.rm = TRUE)
  
  negative_percentage <- round((negative_count / non_null_count) * 100, 2)
  positive_percentage <- round((positive_count / non_null_count) * 100, 2)
  
  total_percentage <- 100
  
  data_merged <- data.frame(
    ΔLST  = c("Negative", "Positive"),
    Percentage = c(negative_percentage, positive_percentage),
    # Label position for the text
    LabelPos = c(25, 94),
    group = group_name
  )
  
  return(data_merged)
}

# List containing all the values
value_list <- list(values1, values2)
group_names <- c("OGU", "EGD")

# Apply function to each value in the list and store results in a list
result_list <- lapply(1:length(value_list), function(i) {
  calculate_data_merged(value_list[[i]], group_names[i])
})

# Merge all data_merged
final_data_merged <- do.call(rbind, result_list)
final_data_merged$group <- factor(final_data_merged$group, 
                                  c("OGU", "EGD")) # Control the order
final_data_merged$ΔLST <- factor(final_data_merged$ΔLST, levels = c("Positive", "Negative"))

# Plot bar chart
p8 <- ggplot(final_data_merged, aes(x = group, y = Percentage, fill = ΔLST)) +
  geom_col(position = "stack", width = 0.49) +  # Adjust bar width to 0.49
  labs(x = "Phenological index", y = "Percentage (%)") +
  scale_fill_manual(values = c("Negative" ="lightblue", "Positive" = "orange")) +
  theme_bw() +
  geom_text(aes(label = paste0(sprintf("%.0f", Percentage), "%"), y = LabelPos), size = 10) +
  theme(axis.line.y = element_line(),   # Display x-axis line
        axis.line.x = element_line(),   # Display y-axis line
        axis.ticks.x = element_line(),  # Display axis ticks
        axis.ticks.y = element_line(size = 1),  # Display axis tick width
        axis.ticks.length.y = unit(0.2, "cm"),   # Display axis tick length
        axis.title = element_text(size = 32),
        axis.text = element_text(size = 29, color = "gray20"),
        axis.title.x = element_text(margin = margin(t = 15)),  # Adjust x-axis title position
        legend.title = element_text(size = 29, hjust = 0.5),
        legend.text = element_text(size = 29),
        legend.spacing.y = unit(0.7, "cm"),                    # Distance between legend title and legend
        legend.position = "right",
        panel.grid = element_line(linetype = "blank")) +
  geom_point(data = subset(final_data_merged, ΔLST == "Negative"),
             aes(x = as.numeric(group), y = Percentage),
             color = "red", size = 1.5, show.legend = FALSE) +  # Add red dots at corresponding positions
  
  guides(fill = guide_legend(byrow = TRUE)) +                   # Internal spacing of legend
  coord_fixed(ratio = 1/90) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 130))   # Customize y-axis scale

p8
ggsave(filename =  "./0.figure/Fig.2-P&Npercent_8.tiff", 
       plot = p8, width = 20, height = 5 , units = "in", dpi = 300)


############################### 9  AmeriFlux Positive/Negative ΔTa Histogram ##################################

df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)
values1 <- df$AF_TA_average_diff_1_mean  
values2 <- df$AF_TA_average_diff_6_mean   

calculate_data_merged <- function(values, group_name) {
  non_null_count <- sum(!is.na(values))
  negative_count <- sum(values < 0, na.rm = TRUE)
  positive_count <- sum(values > 0, na.rm = TRUE)
  
  negative_percentage <- round((negative_count / non_null_count) * 100, 2)
  positive_percentage <- round((positive_count / non_null_count) * 100, 2)
  
  total_percentage <- 100
  
  data_merged <- data.frame(
    ΔTa = c("Negative", "Positive"),
    Percentage = c(negative_percentage, positive_percentage),
    # Label position for percentage text
    LabelPos = c(25, 94),
    group = group_name
  )
  
  return(data_merged)
}

# List containing all values
value_list <- list(values1, values2)
group_names <- c("Green-up", "Dormancy")

# Apply function to each value and store results in a list
result_list <- lapply(1:length(value_list), function(i) {
  calculate_data_merged(value_list[[i]], group_names[i])
})

# Merge all data_merged results
final_data_merged <- do.call(rbind, result_list)
final_data_merged$group <- factor(final_data_merged$group, 
                                  c("Green-up", "Dormancy"))  # Control factor order
final_data_merged$ΔTa <- factor(final_data_merged$ΔTa, levels = c("Positive", "Negative"))

# Plot stacked bar chart
p9 <- ggplot(final_data_merged, aes(x = group, y = Percentage, fill = ΔTa)) +
  geom_col(position = "stack", width = 0.49) +  # Adjust bar width
  labs(x = "Phenological index", y = "Percentage (%)") +
  scale_fill_manual(values = c("Negative" = "lightblue", "Positive" = "orange")) +
  theme_bw() +
  geom_text(aes(label = paste0(sprintf("%.0f", Percentage), "%"), y = LabelPos), size = 10) +
  theme(
    axis.line = element_blank(),  # Remove default axis lines
    axis.ticks.x = element_line(),  # Show x-axis ticks
    axis.ticks.y = element_line(size = 1),  # Y-axis tick width
    axis.ticks.length.y = unit(0.2, "cm"),  # Y-axis tick length
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 29, color = "gray20"),
    axis.title.x = element_text(margin = margin(t = 15)),  # Move x-axis label downward
    legend.title = element_blank(),
    legend.text = element_text(size = 29),
    legend.spacing.y = unit(0.7, "cm"),  # Space between legend title and items
    legend.position = "right",
    legend.direction = "horizontal",  # Horizontal legend layout
    legend.key.width = unit(1.0, "cm"),
    panel.border = element_rect(color = "gray40", size = 1.5),  # Gray border
    panel.grid = element_line(linetype = "blank")  # Remove grid lines
  ) +
  geom_point(
    data = subset(final_data_merged, ΔTa == "Negative"),
    aes(x = as.numeric(group), y = Percentage),
    color = "red", size = 1.5, show.legend = FALSE
  ) +  # Add red points for Negative values
  guides(fill = guide_legend(
    byrow = TRUE, nrow = 1,  # Arrange legend in one row
    label.theme = element_text(margin = margin(r = 10))  # Add space after legend labels
  )) +
  coord_fixed(ratio = 1/80) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 135))  # Custom y-axis

p9

# Save figure
ggsave(filename = "./0.figure/Fig.2-P&Npercent_8_TA.tiff", 
       plot = p9, width = 18, height = 5, units = "in", dpi = 300)
