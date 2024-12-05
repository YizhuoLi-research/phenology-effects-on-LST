library(raster)
library(rasterVis)
setwd("D:/VegetationImpact")

################################  01. Data Matching and Plotting  ################################################

# Load the raster data from the file
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Assigning sequential values as placeholders (if necessary)
r[1:30] <- seq(1,30,1)
r0 <- r[1:30]

# Convert the raster to categorical data (ratified)
r <- ratify(r)
rat <- levels(r)[[1]]

# Define climate categories and assign them to the raster levels
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Update raster levels with climate categories
levels(r) <- rat

# Convert the raster data to a dataframe for ggplot visualization
rdf1 <- as.data.frame(r, xy = TRUE)
names(rdf1)[3] <- "climate"

# Match the raster with the 4 types of pixels from another file
type_r <- raster("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")
rdf2 <- as.data.frame(type_r, xy = TRUE)
# Remove NA values which result from the mask layer
rdf2 <- na.omit(rdf2)

# Merge the two dataframes by "x" and "y" coordinates
common_rdf <- merge(rdf1, rdf2, by = c("x", "y"))

# Check the unique values in the 'climate' column of the common_rdf dataframe
unique_climates <- unique(common_rdf$climate)
print(unique_climates)
common_rdf <- na.omit(common_rdf)  # Remove rows with NA values
head(common_rdf)

library(dplyr)

# Data processing to categorize the climate types
# The common_rdf dataframe is being processed to assign climate types and filter out rows with "Other" category
common_rdf <- common_rdf %>%
  mutate(Type = case_when(
    climate %in% c('Csa', 'Cfa', 'Cwa') ~ "CXa",
    climate %in% c('Csb', 'Cfb', 'Cwb') ~ "CXb",
    climate %in% c('Csc', 'Cfc', 'Cwc') ~ "CXc",
    climate %in% c('Dsa', 'Dfa', 'Dwa') ~ "DXa",
    climate %in% c('Dsb', 'Dfb', 'Dwb') ~ "DXb",
    climate %in% c('Dsc', 'Dfc', 'Dwc') ~ "DXc",
    climate %in% c('Dsd', 'Dfd', 'Dwd') ~ "DXd",
    TRUE ~ "Other"  # If no match, classify as "Other"
  )) %>%
  filter(Type != "Other")  # Remove rows classified as "Other"

# Check the result and view the filtered dataframe
head(common_rdf)

# Define climate labels, descriptions, and associated colors for visualization
climate_labels <- c("CXa", "CXb", "CXc", "DXa", "DXb", "DXc", "DXd") 
climate_descriptions <- c("Warm temperate climate with hot summer",
                          "Warm temperate climate with warm summer",
                          "Warm temperate climate with cool summer", 
                          "Cold climate with hot summer", 
                          "Cold climate with warm summer", 
                          "Cold climate with cool summer", 
                          "Cold climate with extremely continental") 
climate_colors <- c(
  "#663300", "#996600", "#FFCC00",
  "#820082", "#C800C8", "#C89BFA", "#C8C8FF")

# Create a mapping table for climate categories, descriptions, and colors
color_map <- data.frame(climate = climate_labels, descriptions = climate_descriptions, colors = climate_colors)

# Filter the color map to include only those climates present in the common_rdf dataframe
filtered_color_map <- color_map[color_map$climate %in% common_rdf$Type, ]

# Load required libraries for plotting and data manipulation
library("scales")
library(dplyr)
library(ggplot2)

# Purpose: Load world map data and filter it for latitudes greater than 20 degrees
wr <- map_data("world") %>%
  filter(lat > 20)

# Purpose: Define the x-axis lines required for the plot (every 60 degrees)
x_lines <- seq(-120,180, by = 60)

# Purpose: Create the plot
p1 <- ggplot() +
  
  # Purpose: Plot the world map with regions colored in light gray
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +  # fill = "lightgray"
  
  # Purpose: Plot the climate data as colored tiles based on the 'Type' variable
  geom_tile(data = common_rdf, aes(x = x, y = y, fill = Type)) +
  
  # Purpose: Overlay the world map boundary with black lines
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  
  # Purpose: Expand the plot limits to include the map boundaries
  expand_limits(x = wr$long, y = wr$lat) +  # Define longitude and latitude limits
  
  # Purpose: Set the color scale and custom labels for the climate types
  scale_fill_manual(values = setNames(filtered_color_map$colors, filtered_color_map$climate),
                    labels = c("Warm temperate climate with hot summer (CXa)", 
                               "Warm temperate climate with warm summer (CXb)",
                               "Warm temperate climate with cool summer (CXc)", 
                               "Cold climate with hot summer (DXa)", 
                               "Cold climate with warm summer (DXb)", 
                               "Cold climate with cool summer (DXc)", 
                               "Cold climate with extremely continental conditions (DXd)")) +  # Use predefined color palette
  
  # Purpose: Set the title for the legend
  labs(fill = "Major climate type reclassified by temperature") +
  
  # Purpose: Use minimal theme and customize plot appearance
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        # legend.position = "none") +
        legend.title = element_text(size = 38),  # Set legend title size
        legend.text = element_text(size = 38),   # Set legend text size
        legend.position = "right",               # Position legend to the right
        panel.grid = element_line(linetype = "blank"),  # Remove grid lines
        legend.spacing.y = unit(0.5, "cm")) +   # Adjust spacing between legend rows
  
  # Purpose: Customize legend position and text arrangement
  guides(fill = guide_legend(title.position = "top", title.hjust = 0 , byrow = TRUE )) +
  
  # Purpose: Remove x and y axis labels and ticks
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  
  # Purpose: Add horizontal and vertical grid lines (latitudes and longitudes)
  geom_hline(aes(yintercept = 20), size = 0.1) +  # Add horizontal line at 20°N
  
  # Purpose: Add dashed lines for latitude and longitude every 30° and 50° N
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Longitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20°N
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 30°N
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 50°N
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 70°N
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  
  # Purpose: Add latitude and longitude labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  
  # Purpose: Set map projection (stereographic projection centered on the North Pole)
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If displaying the North Pole projection

# Purpose: Display the plot
p1

# Purpose: Save the plot as a high-resolution image file
ggsave(
  filename = "./0.figure/Fig.S6_Koppen_Climate_7_T.tiff",
  plot = p1,  width = 30,  height = 15,  units = "in",  dpi = 300)
