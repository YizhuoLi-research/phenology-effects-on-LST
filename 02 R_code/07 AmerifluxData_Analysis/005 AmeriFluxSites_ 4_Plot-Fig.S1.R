###### 0. Load packages ####
library(tidyverse)
library(dplyr)

setwd("D:/VegetationImpact")

# Create maps
library(ggplot2)
library("scales")
library(sf)
library(ggrepel)


#################### 02 Plotting Ameriflux Site Map 1: Immediate Temperature Effect after Greenup ######################
# This section of the code visualizes the immediate temperature effect after greenup for Ameriflux sites.
# Purpose: The goal of this section is to create a map that visualizes the immediate temperature effect (ΔLST) 
# after greenup for Ameriflux sites, using their geographic coordinates and temperature data (ΔLST values).

df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")
head(df)

# Load the map of US states and boundaries # https://catalog.data.gov/
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
# If using sf from maps:
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))


# View the updated dataframe
head(df)
# Convert the df to an sf object (spatial data)
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))      # Transform to the coordinate system of lower_48
df_sf$average_diff_21_mean <- as.numeric(df_sf$average_diff_21_mean)    

# Plot the map
p1 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +  # Plot the base map of the US
  geom_sf(data = df_sf, aes(color = average_diff_21_mean), size = 12, alpha = 0.9, shape = 19) +  # Plot the Ameriflux site data
  coord_sf(crs = st_crs("ESRI:102003")) +  # Use Albers projection
  theme_bw() +  # Use a white background theme
  theme(legend.position = c(0.185, 0.12),  # Position the legend at the bottom left
        legend.text = element_text(size = 40),  # Set the font size for legend text
        legend.title = element_text(size = 42),  # Set the font size for legend title
        legend.direction = "horizontal",  # Set the legend direction to horizontal
        axis.text.x = element_text(size = 42, color = "gray20",margin = margin(t = 20)),  # Set the x-axis label font size
        axis.text.y = element_text(size = 42, color = "gray20",margin = margin(r = 10)),  # Set the y-axis label font size
        axis.ticks.length = unit(-8, "pt"),     # Set the length of axis ticks
        axis.ticks = element_line(size = 2),  # Set the thickness of axis ticks
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +  # Add border to the panel
  xlab("") + 
  ylab("") +
  
  # Customize x and y axes
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  
  # Set color scale for the temperature difference
  scale_color_gradientn(colours = c("#003366", "#3399CC","#66CCFF", "#FFFFFF",
                                    "#FF9900", "#990000"),
                        na.value = "transparent",  # Handle missing values
                        name = "ΔLST (℃)",  # Color bar title
                        values = scales::rescale(c(-6,-4, -2, 0, 2, 4)),  # Rescale the values for color mapping
                        breaks = c(-6,-4, -2, 0, 2, 4),  # Set breaks in the color scale
                        limits = c(-6, 4),  # Set the limits of the color scale
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      # Set color bar width
                                               barheight = 3,     # Set color bar height
                                               title.vjust = 0,  # Adjust the vertical position of the title
                                               ticks = T,  # Show ticks on the color bar
                                               ticks.colour = "white",  # Set tick color
                                               ticks.linewidth = 3.0/.pt,  # Set tick line width
                                               nrow = 1))  # Set number of rows for the color bar

# Display the plot
print(p1)

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_1.tiff",
  plot = p1,  width = 20,  height = 15,  units = "in",  dpi = 300)


####################  02 Plot Ameriflux Station Map 2: Immediate Temperature Effect After Dormancy ######################
# This section of the code visualizes the immediate temperature effect after doemancy for Ameriflux sites.
# Purpose: The goal of this section is to create a map that visualizes the immediate temperature effect (ΔLST)
# after dormancy for Ameriflux sites, using their geographic coordinates and temperature data (ΔLST values).


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

# Load US map and state boundary data
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
# If using sf, we could alternatively call:
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# Convert the dataframe df to sf object using "long" and "lat" columns
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # Transform coordinate system
df_sf$average_diff_26_mean <- as.numeric(df_sf$average_diff_26_mean)    # Convert the specific temperature difference to numeric

# Plotting the map with temperature differences
p2 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +  # Plot the map of the lower 48 states
  geom_sf(data = df_sf, aes(color = average_diff_26_mean), size = 12, alpha = 0.9, shape = 19) +  # Plot the Ameriflux stations with temperature differences
  coord_sf(crs = st_crs("ESRI:102003")) +  # Use Albers projection
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # Position the legend in the bottom left
        legend.text = element_text(size = 40),  # Set the font size for the legend text
        legend.title = element_text(size = 42),  # Set the font size for the legend title
        legend.direction = "horizontal",  # Set the legend orientation to horizontal
        axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),  # Set x-axis label font size
        axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),  # Set y-axis label font size
        axis.ticks.length = unit(-8, "pt"),     # Set the length of the axis ticks
        axis.ticks = element_line(size = 2),  # Set the width of the axis ticks
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +  # Set the border of the panel
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +  # Set x-axis labels
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +  # Set y-axis labels
  scale_color_gradientn(colours = c("#003366", "#3399CC", "#66CCFF", "#FFFFFF", 
                                    "#FF9900", "#990000"),
                        na.value = "transparent",  # Set the color gradient for ΔLST (℃)
                        name = "ΔLST (℃)",
                        values = scales::rescale(c(-6, -4, -2, 0, 2, 4)),  # Rescale the values
                        breaks = c(-6, -4, -2, 0, 2, 4),  # Set the breaks for the color scale
                        limits = c(-6, 4),  # Set the limits for the color scale
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      # Set the width of the color bar
                                               barheight = 3,     # Set the height of the color bar
                                               title.vjust = 0,  # Adjust the vertical position of the title
                                               ticks = T,  # Display ticks
                                               ticks.colour = "white",  # Set the color of the ticks
                                               ticks.linewidth = 3.0/.pt,  # Set the thickness of the tick lines
                                               nrow = 1))  # Set the number of rows for the color bar

# Display the plot
print(p2)

# Save the plot to file
ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_2.tiff",  # Save the figure as a TIFF file
  plot = p2,  width = 20,  height = 15,  units = "in",  dpi = 300)  # Set the output dimensions and resolution


#################  03 Plot Ameriflux Station Map 3: Cumulative Temperature Across Entire Growing Season ######################
# Purpose: Read data for Ameriflux stations, prepare it, and plot the cumulative temperature effect (ΔLST) 
# across the entire growing season.


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

# Set temperature differences greater than 800 to 1000
df$sum_Diff_16_mean[df$sum_Diff_16_mean > 800] <- 1000

# Load US map and state boundary data
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
# If using sf, we could alternatively call:
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# Convert the dataframe df to an sf object using "long" and "lat" columns
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # Transform the coordinate system
df_sf$sum_Diff_16_mean <- as.numeric(df_sf$sum_Diff_16_mean)  # Convert the cumulative temperature difference to numeric

# Plotting the map with cumulative temperature differences across the growing season
p3 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +  # Plot the map of the lower 48 states
  geom_sf(data = df_sf, aes(color = sum_Diff_16_mean), size = 12, alpha = 0.9, shape = 19) +  # Plot the Ameriflux stations with cumulative temperature differences
  coord_sf(crs = st_crs("ESRI:102003")) +  # Use Albers projection
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # Position the legend in the bottom left
        legend.text = element_text(size = 40),  # Set the font size for the legend text
        legend.title = element_text(size = 42),  # Set the font size for the legend title
        legend.direction = "horizontal",  # Set the legend orientation to horizontal
        axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),  # Set x-axis label font size
        axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),  # Set y-axis label font size
        axis.ticks.length = unit(-8, "pt"),     # Set the length of the axis ticks
        axis.ticks = element_line(size = 2),  # Set the width of the axis ticks
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +  # Set the border of the panel
  xlab("") + 
  ylab("") +
  
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +  # Set x-axis labels
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +  # Set y-axis labels
  scale_color_gradientn(colours = c("#003366","#003366","#006699","#3399CC","#66CCFF", 
                                    "#FFFFFF","#FF9900"),  # Set the color gradient for Cumulative ΔLST
                        na.value = "transparent",  # Set transparent color for missing values
                        name = "Cumulative ΔLST (℃·day)",  # Name the color scale
                        values = scales::rescale(c(-1000, -800, -600, -400, -200, 0, 200)),  # Rescale the values
                        limits = c(-1000, 200),  # Set the limits for the color scale
                        breaks = c(-800, -600, -400, -200, 0, 200),  # Specify the breaks for labels
                        labels = c("≤-800 ","-600","-400","-200","0","200"),  # Specify the corresponding labels
                        
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 28,      # Set the width of the color bar
                                               barheight = 3,     # Set the height of the color bar
                                               title.vjust = 0,  # Adjust the vertical position of the title
                                               ticks = T,  # Display ticks
                                               ticks.colour = "white",  # Set the color of the ticks
                                               ticks.linewidth = 3.0/.pt,  # Set the thickness of the tick lines
                                               nrow = 1))  # Set the number of rows for the color bar

# Display the plot
print(p3)

# Save the plot to file
ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_3.tiff",  # Save the figure as a TIFF file
  plot = p3,  width = 20,  height = 15,  units = "in",  dpi = 300)  # Set the output dimensions and resolution


#################  03 Plot Ameriflux Site Map 4: Growing Season Length ######################
# Purpose: Read the dataset, preprocess the data, and plot the growing season length at Ameriflux sites


df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info")
head(df)

# Adjust outlier values: replace days_16_mean > 240 with 260
df$sum_Diff_16_mean[df$days_16_mean > 240] <- 260

# Load US map and state boundaries
us_states <- read_sf("./01 Download/cb_2022_us_state_20m/cb_2022_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
# Alternatively, we can use the sf package's built-in map:
# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

# Convert the df to a spatial object (sf)
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # Transform coordinate system

# Plot the growing season length at each site
p4 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = days_16_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # Use Albers projection
  theme_bw() +
  theme(legend.position = c(0.185, 0.12),  # Position legend at the lower left
        legend.text = element_text(size = 40),  # Change legend text size
        legend.title = element_text(size = 42),  # Change legend title size
        legend.direction = "horizontal",  # Set legend direction to horizontal
        axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),  # Set x-axis label font size
        axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),  # Set y-axis label font size
        axis.ticks.length = unit(-8, "pt"),     # Set tick length
        axis.ticks = element_line(size = 2),  # Set tick width
        
        panel.border = element_rect(color = "grey20", fill = NA, size = 2)) +  # Set border around the plot
  xlab("") + 
  ylab("") +
  
  # Set x and y axis labels with specified breaks and labels
  scale_x_continuous(breaks = c(-120, -100, -80),
                     labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50),
                     labels = c("30°N", "40°N", "50°N")) +
  
  # Set color scale for growing season duration, with specified limits and labels
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
                                               barwidth = 28,      #### Width
                                               barheight = 3,     #### Height
                                               title.vjust = 0,  # Adjust title's vertical position
                                               ticks = T,
                                               ticks.colour = "white",
                                               ticks.linewidth = 3.0/.pt,
                                               nrow = 1))

# Display the plot
print(p4)

# Save the plot as a high-resolution TIFF image
ggsave(
  filename = "./0.figure/Fig.S10-AmeriFlux_4.tiff",
  plot = p4,  width = 20,  height = 15,  units = "in",  dpi = 300)




