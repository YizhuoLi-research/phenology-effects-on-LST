library(raster)
library(rasterVis)
setwd("D:/01Rawdata")

################################  01. Data Matching + Plotting  ################################################

# Purpose: Load a raster file containing climate data
r <- raster("./0.Results/EA_NA_koppen_30km_addClimate.tif")

# Assigning sequential values as placeholders (if necessary)
r[1:30] <- seq(1, 30, 1)
r0 <- r[1:30]

# Convert the raster to categorical data
r <- ratify(r)  # Convert raster to a factor type
rat <- levels(r)[[1]]

# Defining climate categories with their respective labels
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb', 'Cfc', 
                 'Csa', 'Csb', 'Csc', 
                 'Cwa', 'Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc', 'Dfd', 
                 'Dsa', 'Dsb', 'Dsc', 'Dsd',
                 'Dwa', 'Dwb', 'Dwc', 'Dwd', 
                 'EF',  'ET')

# Update raster levels with climate and colors
levels(r) <- rat

# Convert the raster to a dataframe for ggplot
rdf1 <- as.data.frame(r, xy = TRUE)
names(rdf1)[3] <- "climate"

# Match with the 4-types pixels data
type_r <- raster("./0.Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")
rdf2 <- as.data.frame(type_r, xy = TRUE)

# Removing NA values which result from the mask layer
rdf2 <- na.omit(rdf2)

# Merge the climate data and type data by x and y coordinates
common_rdf <- merge(rdf1, rdf2, by = c("x", "y"))

# Check unique values in the "climate" column of the common_rdf dataframe
unique_climates <- unique(common_rdf$climate)
print(unique_climates)

# Remove rows with missing data
common_rdf <- na.omit(common_rdf)
head(common_rdf)

library(dplyr)

# Purpose: Assign climate type categories to the merged dataframe
common_rdf <- common_rdf %>%
  mutate(Type = case_when(
    climate %in% c('Cfa', 'Cfb', 'Cfc') ~ "CfX",  # Warm temperate climate with fully humid
    climate %in% c('Csa', 'Csb', 'Csc') ~ "CsX",  # Warm temperate climate with summer dry
    climate %in% c('Cwa', 'Cwb', 'Cwc') ~ "CwX",  # Warm temperate climate with winter dry
    climate %in% c('Dfa', 'Dfb', 'Dfc', 'Dfd') ~ "DfX",  # Cold climate with fully humid
    climate %in% c('Dsa', 'Dsb', 'Dsc', 'Dsd') ~ "DsX",  # Cold climate with summer dry
    climate %in% c('Dwa', 'Dwb', 'Dwc', 'Dwd') ~ "DwX",  # Cold climate with winter dry
    TRUE ~ "Other"  # If no match, assign as "Other"
  )) %>%
  filter(Type != "Other")  # Filter out "Other" type rows

# Check the filtered dataframe to ensure correct classification
head(common_rdf)

# Define climate labels, descriptions, and corresponding colors
climate_labels <- c('CfX', 'CsX', 'CwX', 
                    'DfX', 'DsX', 'DwX')
climate_descriptions <- c("Warm temperate climate with fully humid",
                          "Warm temperate climate with summer dry",
                          "Warm temperate climate with winter dry", 
                          "Cold climate with fully humid", 
                          "Cold climate with summer dry", 
                          "Cold climate with winter dry") 
climate_colors <- c(
  "#005000", "#00AA00", "#96FF00",  # Colors for CfX, CsX, CwX
  "#003366", "#006699", "#66CCFF")  # Colors for DfX, DsX, DwX

# Purpose: Create a color mapping table for the climate types
color_map <- data.frame(climate = climate_labels, descriptions = climate_descriptions, colors = climate_colors)

# Filter the color map to only include the climate types present in common_rdf
filtered_color_map <- color_map[color_map$climate %in% common_rdf$Type, ]


library("scales")
library(dplyr)
library(ggplot2)

# Purpose: Prepare the world map data with a filter for latitude > 20
wr <- map_data("world") %>%
  filter(lat > 20)

# Defines the x axes required (longitude lines for the map)
x_lines <- seq(-120, 180, by = 60)

# Purpose: Create a plot using ggplot with the reclassified climate data
p1 <- ggplot() +
  # Purpose: Fill the world map with light gray color
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # fill = "lightgray"
  
  # Purpose: Add climate type data to the map using the specified color types
  geom_tile(data = common_rdf, aes(x = x, y = y, fill = Type)) +
  
  # Purpose: Add climate type data again to ensure it is plotted on top of the previous map
  geom_tile(data = common_rdf, aes(x = x, y = y, fill = Type)) +
  
  # Purpose: Overlay the world map borders with black color
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  
  # Purpose: Expand the limits of the map to cover the specified latitudes and longitudes
  expand_limits(x = wr$long, y = wr$lat)  +  # specify latitude and longitude limits
  
  # Purpose: Set a custom color palette for the climate types
  scale_fill_manual(values = setNames(filtered_color_map$colors, filtered_color_map$climate),
                    labels = c("Warm temperate climate with fully humid (CfX)",
                               "Warm temperate climate with summer dry (CsX)",
                               "Warm temperate climate with winter dry (CwX)", 
                               "Cold climate with fully humid (DfX)", 
                               "Cold climate with summer dry (DsX)", 
                               "Cold climate with winter dry (DwX)")) +  # Use predefined color palette
  
  # Purpose: Label the legend with a title
  labs(fill = "Major climate type reclassified by precipitation") +
  
  # Purpose: Use a minimal theme and adjust background, axis, and grid settings
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 38),
        legend.text = element_text(size = 38),
        legend.position = "right",
        panel.grid = element_line(linetype = "blank"),
        legend.spacing.y = unit(0.5, "cm")) +  # Adjust the spacing between legend rows
  
  # Purpose: Customize the legend's appearance
  guides(fill = guide_legend(title.position = "top", title.hjust = 0, byrow = TRUE)) +
  
  # Purpose: Remove ticks on both x and y axes
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  
  # Purpose: Add horizontal and vertical grid lines for better visualization of the map
  geom_hline(aes(yintercept = 20), size = 0.1) +  # Adds a horizontal line at latitude 20°N
  
  # Purpose: Add dashed meridian lines at specified longitudes
  geom_segment(size = 0.4, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # Longitude lines
  
  # Purpose: Add a solid line at the equator and other latitude lines
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20°N
  geom_segment(size = 0.8, aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 30°N
  geom_segment(size = 0.4, aes(y = 30, yend = 30, x = 180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 50°N
  geom_segment(size = 0.4, aes(y = 50, yend = 50, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # Latitude 70°N
  geom_segment(size = 0.4, aes(y = 70, yend = 70, x = 180, xend = 0), colour = "gray40", linetype = "dashed") +
  
  # Purpose: Add labels to the map at specific latitudes and longitudes
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes(y = 15 + c(-2, -2, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  
  # Purpose: Use stereographic projection for a polar view
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # If you want to display the map with a polar projection (North Pole)

# Purpose: Display the plot
p1

# Purpose: Save the plot as a high-resolution TIFF file
ggsave(
  filename = "D:/01Rawdata/0.figure/Fig.S7_Koppen_Climate_6_P.tiff",
  plot = p1, width = 38, height = 15, units = "in", dpi = 300)
