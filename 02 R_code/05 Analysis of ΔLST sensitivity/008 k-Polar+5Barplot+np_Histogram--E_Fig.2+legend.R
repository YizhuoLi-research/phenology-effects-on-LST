# Purpose: Data visualization for polar region with customized map and legend
# Load libraries
library(terra)
library(tidyterra)
library(ggplot2)
library("scales")
setwd("D:/VegetationImpact")

# Load world map data, filter to keep only latitudes above 20
wr <- map_data("world") %>%
  filter(lat > 20)

# Define the x-axis break lines
x_lines <- seq(-120,180, by = 60)

# Read raster data for the 6 LST difference layers
r1 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_1.tif")
max_value <- max(r1[], na.rm = TRUE)
min_value <- min(r1[], na.rm = TRUE)

# Convert raster to dataframe for plotting
df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","k_value") 
df1$k_value <- as.numeric(as.character(df1$k_value))         
df1$k_value[df1$k_value > 10] <- 10   # Replace values greater than 10 with 10
df1$k_value[df1$k_value < -10] <- -10 # Replace values less than -10 with -10

# Create the map plot with ggplot
p1<-ggplot() +
  # Add the raster data as a tile layer
  geom_tile(data = df1, aes(x = long, y = lat, fill = k_value)) +
  # Add the world map borders
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # Set the map limits and minimal theme
  expand_limits(x = wr$long, y = wr$lat)  + 
  theme_minimal() +
  # Customize legend and axis styles
  legend.title = element_text(size = 33),
legend.text = element_text(size = 33),
legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") +
  ylab("") +
  # Add gridlines and labels for latitudes and longitudes
  geom_hline(aes(yintercept = 20), size = 0.1)+
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") + # Longitude lines
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # Latitude 20 line
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  # Latitude 70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Add labels for latitude lines
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15,
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # Add labels for longitude lines
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  # Set color scale for the fill (temperature difference)
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # Custom labels
                    guide = guide_colorbar(title.position = "left",
                                           title.hjust = 0.5,
                                           barwidth = 60,
                                           title.vjust = 1,
                                           barheight = 1.8))

# Display the plot
p1

# Save the plot to a file
ggsave(
  filename = "./0.figure/E_Fig.2-polar_legend.tiff",
  plot = p1,  width = 18,  height = 15,  units = "in",  dpi = 300)
