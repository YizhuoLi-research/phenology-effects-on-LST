###### 0. Load Required Packages ######

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(broom)
library(tibble)

setwd("D:/VegetationImpact")


######################  01. Divide the growing season into 5 equal stages per site-year and calculate mean ET and LST_diff  ######################

# Load full dataset with daily ET and LST_diff values
df_ET_Tdiff <- read.csv("./AmerifluxData_Analysis/Test_for_ET--6.AmeriFlux_ET_Tdiff_ALLResults_for_Calculation.csv")
head(df_ET_Tdiff)

# Step 1: Calculate 5 evenly spaced stages between SOS and EOS for each site-year
df_stage_info <- df_ET_Tdiff %>%
  filter(DOY >= 1 & DOY <= 365) %>%
  group_by(SiteID, year) %>%
  summarise(
    n = round((nth(EOS_DOY, 3) - nth(SOS_DOY, 3)) / 5),  # Interval size
    stage1_start = nth(SOS_DOY, 3),
    stage2_start = stage1_start + n,
    stage3_start = stage1_start + 2 * n,
    stage4_start = stage1_start + 3 * n,
    stage5_start = stage1_start + 4 * n,
    .groups = "drop"
  )

# Step 2: Add stage boundaries to main dataset
df_ET_Tdiff_with_stage <- df_ET_Tdiff %>%
  inner_join(df_stage_info, by = c("SiteID", "year"))

# Step 3: For each stage, compute the mean ET and LST_diff
df_mean_ET_Tdiff_5Stages_summary <- df_ET_Tdiff_with_stage %>%
  group_by(SiteID, year) %>%
  summarise(
    EOS_DOY = nth(EOS_DOY, 3),
    mean_AF_LST_diff_GrowthSeason = round(mean(AF_LST_diff[DOY >= stage1_start & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    mean_AF_LST_diff_stage1 = round(mean(AF_LST_diff[DOY >= stage1_start & DOY < stage2_start - 1], na.rm = TRUE), 2),
    mean_AF_LST_diff_stage2 = round(mean(AF_LST_diff[DOY >= stage2_start & DOY < stage3_start - 1], na.rm = TRUE), 2),
    mean_AF_LST_diff_stage3 = round(mean(AF_LST_diff[DOY >= stage3_start & DOY < stage4_start - 1], na.rm = TRUE), 2),
    mean_AF_LST_diff_stage4 = round(mean(AF_LST_diff[DOY >= stage4_start & DOY < stage5_start - 1], na.rm = TRUE), 2),
    mean_AF_LST_diff_stage5 = round(mean(AF_LST_diff[DOY >= stage5_start & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    mean_ET_smoothed_stage1 = round(mean(ET_smoothed[DOY >= stage1_start & DOY < stage2_start - 1], na.rm = TRUE), 2),
    mean_ET_smoothed_stage2 = round(mean(ET_smoothed[DOY >= stage2_start & DOY < stage3_start - 1], na.rm = TRUE), 2),
    mean_ET_smoothed_stage3 = round(mean(ET_smoothed[DOY >= stage3_start & DOY < stage4_start - 1], na.rm = TRUE), 2),
    mean_ET_smoothed_stage4 = round(mean(ET_smoothed[DOY >= stage4_start & DOY < stage5_start - 1], na.rm = TRUE), 2),
    mean_ET_smoothed_stage5 = round(mean(ET_smoothed[DOY >= stage5_start & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    Veg = nth(Veg, 3),
    Clim = nth(Clim, 3),
    .groups = "drop"
  )

df <- na.omit(df_mean_ET_Tdiff_5Stages_summary)

# Export stage-wise summaries per site-year
write.csv(
  df,
  file = "./AmerifluxData_Analysis/Test_for_ET--7-4.AF_mean_ET_Tdiff_5Stages_eachSite_Year.csv",
  row.names = FALSE
)


##################  Extract site-years with net growing-season cooling (mean ΔLST < 0)  ##################

df <- read.csv("./AmerifluxData_Analysis/Test_for_ET--7-4.AF_mean_ET_Tdiff_5Stages_eachSite_Year.csv")
df_cooling <- subset(df, df$mean_AF_LST_diff_GrowthSeason < 0)
head(df_cooling)


###################  02-1. Regression analysis (for FigS13b): ΔLST ~ ET across 5 phenophases  ###################

# Step 1: Convert wide-format data to long format (stage-wise observations)
df_cooling_long <- df_cooling %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = c(".value", "stage"),
    names_pattern = "mean_(.*)_stage(\\d+)"
  ) %>%
  rename(
    ET = ET_smoothed,
    LST_diff = AF_LST_diff
  ) %>%
  select(SiteID, year, stage, ET, LST_diff)

# Step 2: Compute correlation
cor_result <- cor.test(df_cooling_long$ET, df_cooling_long$LST_diff, use = "complete.obs")
r_value <- round(cor_result$estimate, 2)
p_value <- cor_result$p.value

# Step 3: Linear regression model
fit <- lm(LST_diff ~ ET, data = df_cooling_long)
slope <- round(coef(fit)[["ET"]], 2)
intercept <- round(coef(fit)[["(Intercept)"]], 2)
summary_fit <- summary(fit)

# Print result
cat("Cor_r =", r_value, ", p =", p_value, "\n")
cat("lm: LST_diff =", slope, "* ET +", intercept)

# Define regression and equation labels
equation_label <- paste("p < 0.001")
regression_label <- paste("y = ", round(intercept, 2), " - ", round(-slope, 2), " * x,", sep = "")


###################  Plot: ET vs ΔLST across five stages (cooling sites only)  ###################

# Define axis ticks
x_ticks <- c(0, 4, 8, 12, 16)
y_ticks <- c(-20, -10, 0, 10)

p2 <- ggplot(df_cooling_long, aes(x = ET, y = LST_diff)) +
  geom_smooth(method = "lm", se = FALSE, color = "#003366", size = 2) +
  geom_point(size = 7.6, color = "#003366", alpha = 0.8) +
  annotate("text", x = 8.8, y = 10.1, label = equation_label, size = 14, color = "#003366") +
  annotate("text", x = 3.8, y = 10.1, label = regression_label, size = 14, color = "#003366") +
  labs(
    x = expression("Average ET across stages (mm/day)"),
    y = expression("Average ΔLST across stages (℃)"),
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.line.y = element_line(size = 2),
    axis.ticks.y = element_line(size = 2, color = "black"),
    axis.ticks.length.y = unit(0.3, "cm"),
    axis.title.x = element_text(margin = margin(t = 16)),
    axis.title.y = element_text(margin = margin(t = 16)),
    panel.grid = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = 0, linewidth = 2) +
  geom_segment(
    data = data.frame(x = x_ticks),
    aes(x = x, xend = x, y = 0, yend = -0.32),
    inherit.aes = FALSE, size = 2
  ) +
  geom_text(
    data = data.frame(x = x_ticks),
    aes(x = x, y = -1.2, label = c("", "4", "8", "12", "16")),
    inherit.aes = FALSE, size = 13
  ) +
  scale_x_continuous(limits = c(0, 17), breaks = x_ticks, expand = c(0, 0),
                     labels = c("0", "4", "8", "12", "16")) +
  scale_y_continuous(limits = c(-22, 11), breaks = y_ticks, expand = c(0, 0),
                     labels = c("-20", "-10", "0", "10"))

p2 <- p2 + theme(axis.text.y = element_text(margin = margin(r = 0)))
p2

# Save figure to TIFF
ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFlux—ET(b).tiff",
       plot = p2, width = 15, height = 12, units = "in", dpi = 300)


###################   02-2 (for FigS13c).  ET_plot: regression between growing-season ET and annual mean LST   ###############################


df_ET_LST <- read.csv("./AmerifluxData_Analysis/Test_for_ET--6.AmeriFlux_ET_Tdiff_ALLResults_for_Calculation.csv")
head(df_ET_LST)

# Group by site and year, compute mean values excluding NA
df_mean_ET_Tdiff_GrowthSeason_summary <- df_ET_LST %>%
  filter(DOY >= 1 & DOY <= 365) %>%
  group_by(SiteID, year) %>%
  summarise(
    SOS_DOY = nth(SOS_DOY, 3),
    EOS_DOY = nth(EOS_DOY, 3),
    
    mean_AF_LST_year = round(mean(AF_LST[DOY >= 1 & DOY <= 365], na.rm = TRUE), 2),
    mean_AF_LST_diff_GrowthSeason = round(mean(AF_LST_diff[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    mean_ET_smoothed_GrowthSeason = round(mean(ET_smoothed[DOY >= SOS_DOY & DOY <= EOS_DOY], na.rm = TRUE), 2),
    
    Veg = nth(Veg, 3),
    Clim = nth(Clim, 3)
  ) %>%
  ungroup()

summary(df_mean_ET_Tdiff_GrowthSeason_summary)
df <- na.omit(df_mean_ET_Tdiff_GrowthSeason_summary)

write.csv(
  df,
  file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--7-5.AF_mean_ET_actLST_SOS-EOS_eachSite_Year.csv",
  row.names = FALSE
)



######################   Extract rows with overall cooling effect   ######################

df <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/Test_for_ET--7-5.AF_mean_ET_actLST_SOS-EOS_eachSite_Year.csv")

# Filter for sites with net cooling effect during the growing season 
# (LST_diff < 0 and actual LST > 0 to ensure physical relevance)
df_cooling <- subset(df, df$mean_AF_LST_diff_GrowthSeason < 0 & df$mean_AF_LST_year > 0)
head(df_cooling)


######################   Extract slope (k) and p-value from regression: ET ~ LST   ######################

# Step 1: Select relevant columns and keep other site/year metadata
df_cooling_long <- df_cooling %>%
  select(SiteID, year, 
         ET_mean = mean_ET_smoothed_GrowthSeason, 
         actLST = mean_AF_LST_year,
         everything())

head(df_cooling_long)

# Step 2: Correlation analysis
cor_result <- cor.test(df_cooling_long$ET_mean, df_cooling_long$actLST, use = "complete.obs")
r_value <- round(cor_result$estimate, 2)
p_value <- cor_result$p.value

# Step 3: Linear regression (ET ~ LST)
fit <- lm(ET_mean ~ actLST, data = df_cooling_long)
slope <- round(coef(fit)[["actLST"]], 2)
intercept <- round(coef(fit)[["(Intercept)"]], 2)
summary_fit <- summary(fit)

# Output correlation and regression results
cat("Cor_r =", r_value, ", p =", p_value, "\n")
cat("lm: ET =", slope, "* actLST +", intercept)

# Optional annotation for plot
equation_label <- paste("p < 0.05", sep = "")
regression_label <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), " * x,", sep = "")

summary(df_cooling_long)  # x range: 3.38–11.74; y range: 8.15–24.19


######################   Plot: Growing-season ET ~ Annual mean LST   ######################

# Define custom axis tick positions
x_ticks <- c(6, 12, 18, 24)
y_ticks <- c(0, 4, 8, 12, 16)

p3 <- ggplot(df_cooling_long, aes(x = actLST, y = ET_mean)) +
  geom_smooth(method = "lm", se = FALSE, color = "#003366", size = 2) +  # regression line
  geom_point(size = 7.6, color = "#003366", alpha = 0.8) +               # scatter points
  annotate("text", x = 16.2, y = 16.5, label = equation_label, size = 14, color = "#003366") +
  annotate("text", x = 10.0, y = 16.5, label = regression_label, size = 14, color = "#003366") +
  labs(
    x = expression("Annual mean LST (℃)"),
    y = expression("Average growing-season ET (mm/day)"),
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.line = element_line(size = 2, color = "black"),
    axis.ticks = element_line(size = 2, color = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(margin = margin(t = 16)),
    axis.title.y = element_text(margin = margin(r = 16)),
    panel.grid = element_blank()
  ) +
  # Add custom x-axis tick marks (short segments)
  geom_segment(
    data = data.frame(x = x_ticks),
    aes(x = x, xend = x, y = 0, yend = -0.32),
    inherit.aes = FALSE, size = 2
  ) +
  # Add custom x-axis tick labels
  geom_text(
    data = data.frame(x = x_ticks),
    aes(x = x, y = -1.2, label = c("6", "12", "18", "24")),
    inherit.aes = FALSE, size = 13
  ) +
  # Add y-axis "0" label manually
  geom_text(
    aes(x = -1.2, y = -1.2, label = "0"),
    inherit.aes = FALSE, size = 13
  ) +
  scale_x_continuous(limits = c(6, 26), breaks = x_ticks) +
  scale_y_continuous(
    limits = c(0, 17),
    breaks = y_ticks,
    expand = c(0, 0),
    labels = c("0", "4", "8", "12", "16")
  ) +
  theme(axis.text.y = element_text(margin = margin(r = 0)))

# Display plot
p3

# Save figure
ggsave(
  filename = "./0.figure/Reponse_review/FigS.AmeriFlux—ET(c).tiff",
  plot = p3,
  width = 15, height = 12, units = "in", dpi = 300
)
