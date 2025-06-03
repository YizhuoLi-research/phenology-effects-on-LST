###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

setwd("D:/VegetationImpact")

##########################   01 FigS13: Compute DOY-mean ET and Tdiff across sites and years   ################################################

df_ET_Tdiff <- read.csv("./AmerifluxData_Analysis/Test_for_ET--6.AmeriFlux_ET_Tdiff_ALLResults_for_Calculation.csv")
head(df_ET_Tdiff)

df_ET_Tdiff_DOY_mean <- df_ET_Tdiff %>%
  filter(DOY >= 1 & DOY <= 365) %>%
  group_by(DOY) %>%
  summarise(
    mean_AF_TA = round(mean(AF_TA, na.rm = TRUE), 2),
    mean_AF_LST = round(mean(AF_LST, na.rm = TRUE), 2),
    mean_ET_calculated = round(mean(ET_calculated, na.rm = TRUE), 2),
    mean_ET_smoothed = round(mean(ET_smoothed, na.rm = TRUE), 2),
    mean_AF_TA_diff = round(mean(AF_TA_diff, na.rm = TRUE), 2),
    mean_AF_LST_diff = round(mean(AF_LST_diff, na.rm = TRUE), 2),
    
    mean_SOS_DOY = round(mean(SOS_DOY, na.rm = TRUE), 0),
    mean_MGP_DOY = round(mean(MGP_DOY, na.rm = TRUE), 0),
    mean_GMO_DOY = round(mean(GMO_DOY, na.rm = TRUE), 0),
    mean_GDO_DOY = round(mean(GDO_DOY, na.rm = TRUE), 0),
    mean_MSP_DOY = round(mean(MSP_DOY, na.rm = TRUE), 0),
    mean_EOS_DOY = round(mean(EOS_DOY, na.rm = TRUE), 0)
  ) %>% ungroup()

# Set axis ranges
et_y_min <- 0
et_y_max <- 16

temp_y_min <- -6
temp_y_max <- 6

# Calculate scale factor for dual axis alignment
scale_factor <- (et_y_max - et_y_min) / (temp_y_max - temp_y_min)
offset <- et_y_min - temp_y_min * scale_factor

##########################   01-1 ET_plot-1   ################################################

mean_SOS_DOY = round(mean(df_ET_Tdiff_DOY_mean$mean_SOS_DOY, na.rm = TRUE), 0)
mean_EOS_DOY = round(mean(df_ET_Tdiff_DOY_mean$mean_EOS_DOY, na.rm = TRUE), 0)

# Plot
p1 <- ggplot(df_ET_Tdiff_DOY_mean, aes(x = DOY)) +
  # Temperature-related variables (left Y axis, no rescaling)
  geom_line(aes(y = mean_AF_TA_diff, color = "AF_TA_diff"), size = 2) +
  geom_line(aes(y = mean_AF_LST_diff, color = "AF_LST_diff"), size = 2) +
  # ET (right Y axis, needs rescaling)
  geom_line(aes(y = mean_ET_smoothed * (temp_y_max - temp_y_min) / (et_y_max - et_y_min) + temp_y_min,
                color = "ET_smoothed"), size = 2) +
  scale_color_manual(
    values = c("ET_smoothed" = "#339900", "AF_TA_diff" = "#3399CC", "AF_LST_diff" = "#003366"),
    labels = c("ET_smoothed" = "ET", "AF_TA_diff" = expression("ΔTa"), "AF_LST_diff" = expression("ΔLST")),
    breaks = c("ET_smoothed", "AF_TA_diff", "AF_LST_diff")) +
  # Main Y axis: Temperature
  scale_y_continuous(
    name = "Vegetation effect on temperature (°C)",
    breaks = c(-6, -4, -2, 0, 2, 4),
    limits = c(temp_y_min, temp_y_max),
    expand = c(0, 0),
    # Secondary Y axis: ET
    sec.axis = sec_axis(~ (. - temp_y_min) * (et_y_max - et_y_min) / (temp_y_max - temp_y_min),
                        name = "Evapotranspiration (mm/day)",
                        breaks = c(0, 4, 8, 12))) +
  # Add vertical dashed lines for SOS and EOS, and horizontal line at temperature = 0
  geom_vline(xintercept = mean_SOS_DOY, color = "#99CC00", linetype = "dashed", size = 1.5) +
  geom_vline(xintercept = mean_EOS_DOY, color = "#663300", linetype = "dashed", size = 1.5) +
  # Add phase labels near the right temperature axis
  geom_text(aes(x = mean_SOS_DOY, y = offset, label = "Green-up"), vjust = 13.65, hjust = -0.03,
            size = 12.5, color = "#99CC00") +
  geom_text(aes(x = mean_EOS_DOY, y = offset, label = "Dormancy"), vjust = 13.65, hjust = -0.03, 
            size = 12.5, color = "#663300") +
  # Add horizontal dashed line at temperature = 0
  geom_hline(yintercept = offset, color = "#666666", linetype = "dashed", size = 0.5) +
  
  theme_minimal() +
  labs(x = "Day of year (DOY)", color = "") +
  theme(
    axis.title = element_text(size = 42),
    axis.text.y.left = element_text(size = 40, color = "black"),
    axis.text.y.right = element_text(size = 40, color = "black"),
    axis.text.x = element_text(size = 40, margin = margin(t = 14), color = "black"),
    axis.title.y.left = element_text(size = 40, margin = margin(r = 9)),
    axis.title.y.right = element_text(size = 40, margin = margin(l = 12)),
    axis.title.x = element_text(size = 40, margin = margin(t = 17)),
    axis.line = element_line(size = 2),
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid = element_line(linetype = "blank"),
    legend.position = c(0.08, 0.88),
    legend.key.height = unit(2.4, "cm"),
    legend.key.width = unit(3, "cm"),
    legend.direction = "vertical",
    legend.text = element_text(size = 38.5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_x_continuous(breaks = seq(1, 365, by = 50)) 

p1

ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFlux—ET_1.tiff",
       plot = p1, width = 30, height = 12, units = "in", dpi = 300)



######################   02 Calculate growing-season mean ET and Tdiff   ################################################

df_ET_Tdiff <- read.csv("./AmerifluxData_Analysis/Test_for_ET--6.AmeriFlux_ET_Tdiff_ALLResults_for_Calculation.csv")
head(df_ET_Tdiff)

# Group by site and year, then calculate the mean of all numeric columns (excluding NA)
df_mean_ET_Tdiff_GrowthSeason_summary <- df_ET_Tdiff %>%
  filter(DOY >= 1 & DOY <= 365) %>%  # Ensure only DOY 1–365 is used
  group_by(SiteID, year) %>%
  summarise(
    SOS_DOY = nth(SOS_DOY, 3),  # Use the 3rd available SOS_DOY value
    EOS_DOY = nth(EOS_DOY, 3),  # Use the 3rd available EOS_DOY value
    
    # Calculate the mean of each variable during the growing season (from SOS to EOS)
    # [DOY >= SOS_DOY & DOY <= EOS_DOY] is equivalent to [day_1:(day_6)]
    mean_AF_TA_GrowthSeason = round(mean(AF_TA[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    mean_AF_LST_GrowthSeason = round(mean(AF_LST[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    mean_AF_TA_diff_GrowthSeason = round(mean(AF_TA_diff[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    mean_AF_LST_diff_GrowthSeason = round(mean(AF_LST_diff[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    mean_ET_calculated_GrowthSeason = round(mean(ET_calculated[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    mean_ET_smoothed_GrowthSeason = round(mean(ET_smoothed[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    Veg = nth(Veg, 3),  # Use the 3rd available Veg value
    Clim = nth(Clim, 3)  # Use the 3rd available Clim value
  ) %>%
  ungroup()

summary(df_mean_ET_Tdiff_GrowthSeason_summary)
df <- na.omit(df_mean_ET_Tdiff_GrowthSeason_summary)

write.csv(
  df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--7.AF_mean_ET_Tdiff_SOS-EOS_eachSite_Year.csv",
  row.names = FALSE
)

