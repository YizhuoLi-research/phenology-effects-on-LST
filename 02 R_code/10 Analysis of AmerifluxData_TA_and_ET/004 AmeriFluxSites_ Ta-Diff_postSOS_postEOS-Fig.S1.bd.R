###### 0. Load Required Packages ######
library(tidyverse)
library(dplyr)

setwd("D:/VegetationImpact")

# Create base map
library(ggplot2)
library("scales")
library(sf)
library(ggrepel)


#################### 01. Plot AmeriFlux site map – Panel 1 ######################

df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)

# Load US states shapefile (e.g., from https://catalog.data.gov/)
us_states <- read_sf("./01 Download/cb_2023_us_state_20m/cb_2023_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))

# Convert site data to sf object
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))  # Transform to match map CRS
df_sf$AF_TA_average_diff_1_mean <- as.numeric(df_sf$AF_TA_average_diff_1_mean)    
summary(df_sf$AF_TA_average_diff_1_mean )   #-3~6

p1 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = AF_TA_average_diff_1_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +  # Use Albers equal area projection
  theme_bw() +
  theme(
    legend.position = c(0.185, 0.12),  
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 42),
    legend.direction = "horizontal",  
    axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),
    axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),
    axis.ticks.length = unit(-8, "pt"),
    axis.ticks = element_line(size = 2),
    panel.border = element_rect(color = "grey20", fill = NA, size = 2)
  ) +
  xlab("") + 
  ylab("") +
  scale_x_continuous(breaks = c(-120, -100, -80), labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50), labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(
    colours = c("#003366", "#66CCFF", "#FFFFFF", "#FF9900", "#CC3333", "#990000"),
    na.value = "transparent",
    name = expression(Delta * "Ta (℃)"),
    values = scales::rescale(c(-4, -2, 0, 2, 4, 6)),
    breaks = c(-4, -2, 0, 2, 4, 6),
    limits = c(-4, 6),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 28,
      barheight = 3,
      title.vjust = 0,
      ticks = TRUE,
      ticks.colour = "white",
      ticks.linewidth = 3.0/.pt,
      nrow = 1
    )
  )

print(p1)

ggsave(
  filename = "./0.figure/Fig.S1-AmeriFlux_1_Ta.tiff",
  plot = p1, width = 20, height = 15, units = "in", dpi = 300
)


#################### 02. Plot AmeriFlux site map – Panel 2 ######################

df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)

# Load US states map
us_states <- read_sf("./01 Download/cb_2023_us_state_20m/cb_2023_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))

# Convert site data to sf object
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))
df_sf$AF_TA_average_diff_6_mean <- as.numeric(df_sf$AF_TA_average_diff_6_mean)
summary(df_sf$AF_TA_average_diff_6_mean)

p2 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = AF_TA_average_diff_6_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +
  theme_bw() +
  theme(
    legend.position = c(0.185, 0.12),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 42),
    legend.direction = "horizontal",
    axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),
    axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),
    axis.ticks.length = unit(-8, "pt"),
    axis.ticks = element_line(size = 2),
    panel.border = element_rect(color = "grey20", fill = NA, size = 2)
  ) +
  xlab("") + 
  ylab("") +
  scale_x_continuous(breaks = c(-120, -100, -80), labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50), labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(
    colours = c("#003366", "#66CCFF", "#FFFFFF", "#FF9900", "#CC3333", "#990000"),
    na.value = "transparent",
    name = expression(Delta * "Ta (℃)"),
    values = scales::rescale(c(-4, -2, 0, 2, 4, 6)),
    breaks = c(-4, -2, 0, 2, 4, 6),
    limits = c(-4, 6),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 28,
      barheight = 3,
      title.vjust = 0,
      ticks = TRUE,
      ticks.colour = "white",
      ticks.linewidth = 3.0/.pt,
      nrow = 1
    )
  )

print(p2)

ggsave(
  filename = "./0.figure/Fig.S1-AmeriFlux_2_Ta.tiff",
  plot = p2, width = 20, height = 15, units = "in", dpi = 300
)


#################### 03. Plot AmeriFlux site map – Panel 3 (cumulative ΔTa) ######################

df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)

# Cap extreme values
df$AF_TA_sum_diff_16_mean[df$AF_TA_sum_diff_16_mean > 800] <- 1000

# Load map data
us_states <- read_sf("./01 Download/cb_2023_us_state_20m/cb_2023_us_state_20m.shp")
lower_48 <- us_states %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))

# Convert to sf object
df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, st_crs(lower_48))
df_sf$AF_TA_sum_diff_16_mean <- as.numeric(df_sf$AF_TA_sum_diff_16_mean)

p3 <- ggplot() +
  geom_sf(data = lower_48, fill = "grey95", color = "black", size = 1) +
  geom_sf(data = df_sf, aes(color = AF_TA_sum_diff_16_mean), size = 12, alpha = 0.9, shape = 19) +
  coord_sf(crs = st_crs("ESRI:102003")) +
  theme_bw() +
  theme(
    legend.position = c(0.185, 0.12),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 36),
    legend.direction = "horizontal",
    axis.text.x = element_text(size = 42, color = "gray20", margin = margin(t = 20)),
    axis.text.y = element_text(size = 42, color = "gray20", margin = margin(r = 10)),
    axis.ticks.length = unit(-8, "pt"),
    axis.ticks = element_line(size = 2),
    panel.border = element_rect(color = "grey20", fill = NA, size = 2)
  ) +
  xlab("") + 
  ylab("") +
  scale_x_continuous(breaks = c(-120, -100, -80), labels = c("120°W", "100°W", "80°W")) +
  scale_y_continuous(breaks = c(30, 40, 50), labels = c("30°N", "40°N", "50°N")) +
  scale_color_gradientn(
    colours = c("#003366","#003366","#006699","#3399CC","#66CCFF","#FFFFFF","#FF9900"),
    na.value = "transparent",
    name = "Cumulative ΔTa (℃·day)",
    values = scales::rescale(c(-1000, -800, -600, -400, -200, 0, 200)),
    limits = c(-1000, 200),
    breaks = c(-800, -600, -400, -200, 0, 200),
    labels = c("≤ -800", "-600", "-400", "-200", "0", "200"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 28,
      barheight = 3,
      title.vjust = 0.4,
      ticks = TRUE,
      ticks.colour = "white",
      ticks.linewidth = 3.0/.pt,
      nrow = 1
    )
  )

print(p3)

ggsave(
  filename = "./0.figure/Fig.S1-AmeriFlux_3_Ta.tiff",
  plot = p3, width = 20, height = 15, units = "in", dpi = 300
)
