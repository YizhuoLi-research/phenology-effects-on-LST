################## 00 Load required packages ##########################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

################## 01 Extract data by climate subtype — add attributes to climate raster ###################

# Load the raster containing Köppen climate types with added classifications
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Temporarily assign sequential values for the first 30 categories
r[1:30] <- seq(1, 30, 1)
r0 <- r[1:30]

# Convert raster to categorical raster with attribute table
r <- ratify(r)
rat <- levels(r)[[1]]

# Assign climate subtype labels in alphabetical order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc',
                 'Csa', 'Csb','Csc',
                 'Cwa','Cwb', 'Cwc',
                 'Dfa', 'Dfb', 'Dfc','Dfd',
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd',
                 'EF',  'ET')

# Restore the original cell values
r[1:30] <- r0

# Assign the updated attribute table back to the raster
levels(r) <- rat

# Convert each climate type to polygon boundaries
classify_border <- as.polygons(rast(r))

# Create list of all climate subtypes
climate_types <- rat$climate

# Define groups of climate subtypes by custom climate categories
filter_value_CXa <- c('Csa', 'Cfa', 'Cwa')
filter_value_CXb <- c('Csb', 'Cfb', 'Cwb')
filter_value_CXc <- c('Csc', 'Cfc', 'Cwc')
filter_value_DXa <- c('Dsa', 'Dfa', 'Dwa')
filter_value_DXb <- c('Dsb', 'Dfb', 'Dwb')
filter_value_DXc <- c('Dsc', 'Dfc', 'Dwc')
filter_value_DXd <- c('Dsd', 'Dfd', 'Dwd')

# Combine into a list
filter_values_list <- list(filter_value_CXa, filter_value_CXb, filter_value_CXc,
                           filter_value_DXa, filter_value_DXb, filter_value_DXc, filter_value_DXd)

# Named list for referencing specific climate groups
filter_values_named_list <- list(
  CXa = c('Csa', 'Cfa', 'Cwa'),
  CXb = c('Csb', 'Cfb', 'Cwb'),
  CXc = c('Csc', 'Cfc', 'Cwc'),
  DXa = c('Dsa', 'Dfa', 'Dwa'),
  DXb = c('Dsb', 'Dfb', 'Dwb'),
  DXc = c('Dsc', 'Dfc', 'Dwc'),
  DXd = c('Dsd', 'Dfd', 'Dwd')
)

############################# 01. Filter three forest types ####################

## 1. Load forest type raster
r <- rast("./EA+NA_Results/EA+NA_LandCover_30km.tif")
forest_types <- c(1, 2, 3)  # 1: Coniferous forest; 2: Broadleaf forest; 3: Mixed forest

################## 02. Extract values for 6 phenological phases by climate subtype for plotting ###########################

# File paths and phase labels
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
)

labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS", "SOS-EOS")

# Main function to extract and summarize by forest and climate groups
perform_analysis <- function(file_paths, labels, forest_raster, forest_types, climate_group_list) {
  list_of_results <- list()
  
  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    LST_sumdiff <- rast(file_path)
    
    for (group_name in names(climate_group_list)) {
      climate_classes <- climate_group_list[[group_name]]
      cat("Processing Climate Group:", group_name, "\n")
      
      # Get the polygon index for the climate group
      order_vals <- which(classify_border$EA_koppen_30km_addClimate %in% climate_classes)
      if (length(order_vals) == 0) next
      
      selected_polys <- classify_border[order_vals, ]
      selected_border_val <- terra::union(selected_polys)
      
      # Mask LST data and forest type data for this climate region
      LST_sumdiff_masked_zone <- mask(crop(LST_sumdiff, selected_border_val), selected_border_val)
      forest_masked <- mask(crop(forest_raster, selected_border_val), selected_border_val)
      
      # Loop over each forest type
      for (ft in forest_types) {
        forest_mask <- mask(LST_sumdiff_masked_zone, forest_masked, maskvalues = setdiff(unique(values(forest_masked)), ft))
        values_extracted <- values(forest_mask)
        values_extracted <- values_extracted[!is.na(values_extracted)]
        
        results_df <- data.frame(
          Forest_Type = ft,
          mean_value = round(mean(values_extracted, na.rm = TRUE), 2),
          sd_value = round(sd(values_extracted, na.rm = TRUE), 2),
          Phe_phase = labels[i],
          Climate_Group = group_name
        )
        
        list_of_results[[length(list_of_results) + 1]] <- results_df
      }
    }
  }
  
  final_results <- do.call(rbind, list_of_results)
  
  final_results$Forest_Label <- factor(final_results$Forest_Type,
                                       levels = c(1, 2, 3),
                                       labels = c("DNF", "DBF", "MF"))  # DNF = coniferous; DBF = broadleaf; MF = mixed
  
  return(final_results)
}

# Run analysis and save outputs
final_results <- perform_analysis(file_paths, labels, r, forest_types, filter_values_named_list)
print(head(final_results))

# Add error bars and clean NA rows
final_results$error_bars <- round(final_results$sd_value * 0.15, 2)
final_results <- na.omit(final_results)

write.csv(final_results, "./EA+NA_Results/EA_NA_CoolingResults_ByClimate_Forest_16.csv", row.names = FALSE)

# Remove full-season SOS–EOS rows and clean again
final_results <- final_results[final_results$Phe_phase != "SOS-EOS", ]
final_results <- na.omit(final_results)

# Print max and min values
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)
print(paste("Max value:", max_value))
print(paste("Min value:", min_value))

write.csv(final_results, "./EA+NA_Results/EA_NA_CoolingResults_ByClimate_Forest.csv", row.names = FALSE)


########################### 03. Barplot for CXa - Landcover Group #####################################################################

final_results <- read.csv("./EA+NA_Results/EA_NA_CoolingResults_ByClimate_Forest.csv")
library(dplyr)

#### CXa_DBF ####

df_CXa_DBF <- final_results %>%
  filter(Climate_Group == "CXa", Forest_Label == "DBF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXa_DBF$Phe_phase <- factor(df_CXa_DBF$Phe_phase, levels = names(colors)) 
df_CXa_DBF$mean_value <- as.numeric(df_CXa_DBF$mean_value)

# Plot barplot with error bars
p1 <- ggplot(df_CXa_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y = "Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p1

# Save plot
filename <- "./0.figure/Climate&forest_type"
if (!dir.exists(filename)) { dir.create(filename, recursive = TRUE) }

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_Cxa_DBF.tiff",
  plot = p1, width = 14.5, height = 12, units = "in", dpi = 300)

#### CXa_MF ####

df_CXa_MF <- final_results %>%
  filter(Climate_Group == "CXa", Forest_Label == "MF")

df_CXa_MF$Phe_phase <- factor(df_CXa_MF$Phe_phase, levels = names(colors)) 
df_CXa_MF$mean_value <- as.numeric(df_CXa_MF$mean_value)

# Plot barplot with error bars
p2 <- ggplot(df_CXa_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y = "Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p2

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_Cxa_MF.tiff",
  plot = p2, width = 14.5, height = 12, units = "in", dpi = 300)



########################### 03. Barplot for CXb - Landcover Group #####################################################################

#### CXb_DBF ####

df_CXb_DBF <- final_results %>%
  filter(Climate_Group == "CXb", Forest_Label == "DBF")

# Define phase colors
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXb_DBF$Phe_phase <- factor(df_CXb_DBF$Phe_phase, levels = names(colors)) 
df_CXb_DBF$mean_value <- as.numeric(df_CXb_DBF$mean_value)

# Plot bar chart with error bars (refined lines)
p3 <- ggplot(df_CXb_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bars
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),   # Hide major x-grid
        panel.grid.minor.x = element_blank(),   # Hide minor x-grid
        panel.border = element_rect(size = 2.2),  # Outer border size
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),  # Y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),  # Y-axis tick length
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p3

# Save the plot
ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_CXb_DBF.tiff",
  plot = p3, width = 14.5, height = 12, units = "in", dpi = 300)

#### CXb_MF ####

df_CXb_MF <- final_results %>%
  filter(Climate_Group == "CXb", Forest_Label == "MF")

df_CXb_MF$Phe_phase <- factor(df_CXb_MF$Phe_phase, levels = names(colors)) 
df_CXb_MF$mean_value <- as.numeric(df_CXb_MF$mean_value)

# Plot bar chart with error bars (refined lines)
p4 <- ggplot(df_CXb_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y = "Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p4

# Save the plot
ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_CXb_MF.tiff",
  plot = p4, width = 14.5, height = 12, units = "in", dpi = 300)


########################### 03. Barplot for CXc - Landcover Group #####################################################################

#### CXc_DBF ####

df_CXc_DBF <- final_results %>%
  filter(Climate_Group == "CXc", Forest_Label == "DBF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXc_DBF$Phe_phase <- factor(df_CXc_DBF$Phe_phase, levels = names(colors)) 

df_CXc_DBF$mean_value <- as.numeric(df_CXc_DBF$mean_value)

# Plot bar chart with error bars (fine-tuned line width)
p5 <- ggplot(df_CXc_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") ,
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0, 400), breaks = c(0,100,200,300,400))

p5

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_CXc_DBF.tiff",
  plot = p5,  width = 14.5,  height = 12,  units = "in",  dpi = 300)


#### CXc_MF ####

df_CXc_MF <- final_results %>%
  filter(Climate_Group == "CXc", Forest_Label == "MF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXc_MF$Phe_phase <- factor(df_CXc_MF$Phe_phase, levels = names(colors)) 

df_CXc_MF$mean_value <- as.numeric(df_CXc_MF$mean_value)

# Plot bar chart with error bars (fine-tuned line width)
p6 <- ggplot(df_CXc_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") ,
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0, 400), breaks = c(0,100,200,300,400))

p6

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_CXc_MF.tiff",
  plot = p6,  width = 14.5,  height = 12,  units = "in",  dpi = 300)


########################### 03. Barplot for DXa - Landcover Group #####################################################################
#### DXa_DBF ####

df_DXa_DBF <- final_results %>%
  filter(Climate_Group == "DXa", Forest_Label == "DBF")

df_DXa_DBF$Phe_phase <- factor(df_DXa_DBF$Phe_phase, levels = names(colors)) 

df_DXa_DBF$mean_value <- as.numeric(df_DXa_DBF$mean_value)

# Plot bar chart with error bars (fine-tuned line width)
p7 <- ggplot(df_DXa_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") ,
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200,-900,-600,-300,0,100))

p7

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXa_DBF.tiff",
  plot = p7,  width = 14.5,  height = 12,  units = "in",  dpi = 300)


#### DXa_MF ####


df_DXa_MF <- final_results %>%
  filter(Climate_Group == "DXa", Forest_Label == "MF")

df_DXa_MF$Phe_phase <- factor(df_DXa_MF$Phe_phase, levels = names(colors)) 

df_DXa_MF$mean_value <- as.numeric(df_DXa_MF$mean_value)

# Plot bar chart with error bars (fine-tuned line width)
p8 <- ggplot(df_DXa_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") ,
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200,-900,-600,-300,0,100))

p8

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXa_MF.tiff",
  plot = p8,  width = 14.5,  height = 12,  units = "in",  dpi = 300)



########################### 03. Barplot for DXb - Landcover Group #####################################################################

#### DXb_DBF ####

df_DXb_DBF <- final_results %>%
  filter(Climate_Group == "DXb", Forest_Label == "DBF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXb_DBF$Phe_phase <- factor(df_DXb_DBF$Phe_phase, levels = names(colors)) 

df_DXb_DBF$mean_value <- as.numeric(df_DXb_DBF$mean_value)

# Plot bar chart with error bars (refined line style)
p9 <- ggplot(df_DXb_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +  # Set theme
  theme(panel.grid.major.x = element_blank(),     # Hide major grid lines on x-axis
        panel.grid.minor.x = element_blank(),     # Hide minor grid lines on x-axis
        panel.border = element_rect(size = 2.2),  # Set panel border width
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),    # Set tick width on y-axis
        axis.ticks.length.y = unit(0.3, "cm"),    # Set tick length on y-axis
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),    # Hide x-axis title
        plot.background = element_rect(size = 100),  # Set background frame size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), # Adjust plot margins
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),           # Hide x-axis text
        axis.ticks.x = element_blank()) +        # Hide x-axis ticks
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p9

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXb_DBF.tiff",
  plot = p9, width = 14.5, height = 12, units = "in", dpi = 300)


#### DXb_DNF ####

df_DXb_DNF <- final_results %>%
  filter(Climate_Group == "DXb", Forest_Label == "DNF")

# Reuse same color scheme
df_DXb_DNF$Phe_phase <- factor(df_DXb_DNF$Phe_phase, levels = names(colors)) 
df_DXb_DNF$mean_value <- as.numeric(df_DXb_DNF$mean_value)

# Bar plot with error bars
p10 <- ggplot(df_DXb_DNF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"),
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74),
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p10

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXb_DNF.tiff",
  plot = p10, width = 14.5, height = 12, units = "in", dpi = 300)


#### DXb_MF ####

df_DXb_MF <- final_results %>%
  filter(Climate_Group == "DXb", Forest_Label == "MF")

df_DXb_MF$Phe_phase <- factor(df_DXb_MF$Phe_phase, levels = names(colors)) 
df_DXb_MF$mean_value <- as.numeric(df_DXb_MF$mean_value)

# Bar plot with error bars
p11 <- ggplot(df_DXb_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +
  labs(y ="Cumulative ΔLST (℃·day)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.2),
        axis.text.y  = element_text(size = 65, color = "black"),
        axis.ticks.y = element_line(size = 3),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 74),
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p11

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXb_MF.tiff",
  plot = p11, width = 14.5, height = 12, units = "in", dpi = 300)



########################### 03. Barplot for DXc - Landcover Group #####################################################################

#### DXc_DBF ####

df_DXc_DBF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "DBF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_DBF$Phe_phase <- factor(df_DXc_DBF$Phe_phase, levels = names(colors)) 

df_DXc_DBF$mean_value <- as.numeric(df_DXc_DBF$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p12 <- ggplot(df_DXc_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2.2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200,-900,-600,-300,0,100))

p12

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_DBF.tiff",
  plot = p12,  width = 14.5,  height = 12,  units = "in",  dpi = 300)


#### DXc_DNF ####

df_DXc_DNF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "DNF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_DNF$Phe_phase <- factor(df_DXc_DNF$Phe_phase, levels = names(colors)) 

df_DXc_DNF$mean_value <- as.numeric(df_DXc_DNF$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p13<- ggplot(df_DXc_DNF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2.2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200,-900,-600,-300,0,100))

p13

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_DNF.tiff",
  plot = p13,  width = 14.5,  height = 12,  units = "in",  dpi = 300)


#### DXc_MF ####

df_DXc_MF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "MF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_MF$Phe_phase <- factor(df_DXc_MF$Phe_phase, levels = names(colors)) 

df_DXc_MF$mean_value <- as.numeric(df_DXc_MF$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p14 <- ggplot(df_DXc_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2.2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200,-900,-600,-300,0,100))

p14


ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_MF.tiff",
  plot = p14,  width = 14.5,  height = 12,  units = "in",  dpi = 300)




########################### 03. Barplot for DXd - Landcover Group #####################################################################
#### DXc_DBF ####

df_DXc_DBF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "DBF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_DBF$Phe_phase <- factor(df_DXc_DBF$Phe_phase, levels = names(colors)) 

df_DXc_DBF$mean_value <- as.numeric(df_DXc_DBF$mean_value)

# Draw bar chart and add error bars (refined line style)
p12 <- ggplot(df_DXc_DBF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar chart
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +  # Apply theme
  theme(panel.grid.major.x = element_blank(),       # Hide major x-axis grid
        panel.grid.minor.x = element_blank(),       # Hide minor x-axis grid
        panel.border = element_rect(size = 2.2),    # Set border thickness
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),      # Set y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),      # Set y-axis tick length
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),      # Hide x-axis title
        plot.background = element_rect(size = 100), # Set plot background frame
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), # Set margins
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),              # Hide x-axis text
        axis.ticks.x = element_blank()) +           # Hide x-axis ticks
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p12

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_DBF.tiff",
  plot = p12, width = 14.5, height = 12, units = "in", dpi = 300)


#### DXc_DNF ####

df_DXc_DNF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "DNF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_DNF$Phe_phase <- factor(df_DXc_DNF$Phe_phase, levels = names(colors)) 

df_DXc_DNF$mean_value <- as.numeric(df_DXc_DNF$mean_value)

# Draw bar chart and add error bars (refined line style)
p13 <- ggplot(df_DXc_DNF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar chart
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +  # Apply theme
  theme(panel.grid.major.x = element_blank(),       
        panel.grid.minor.x = element_blank(),       
        panel.border = element_rect(size = 2.2),    
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),      
        axis.ticks.length.y = unit(0.3, "cm"),      
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),      
        plot.background = element_rect(size = 100), 
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), 
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),              
        axis.ticks.x = element_blank()) +           
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p13

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_DNF.tiff",
  plot = p13, width = 14.5, height = 12, units = "in", dpi = 300)


#### DXc_MF ####

df_DXc_MF <- final_results %>%
  filter(Climate_Group == "DXc", Forest_Label == "MF")

colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc_MF$Phe_phase <- factor(df_DXc_MF$Phe_phase, levels = names(colors)) 

df_DXc_MF$mean_value <- as.numeric(df_DXc_MF$mean_value)

# Draw bar chart and add error bars (refined line style)
p14 <- ggplot(df_DXc_MF, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar chart
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 4, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  theme_bw() +  # Apply theme
  theme(panel.grid.major.x = element_blank(),       
        panel.grid.minor.x = element_blank(),       
        panel.border = element_rect(size = 2.2),    
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),      
        axis.ticks.length.y = unit(0.3, "cm"),      
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),      
        plot.background = element_rect(size = 100), 
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), 
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),              
        axis.ticks.x = element_blank()) +           
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p14

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-1-barplot_DXc_MF.tiff",
  plot = p14, width = 14.5, height = 12, units = "in", dpi = 300)



###########################  04 - Barplot for All & Subtypes of AmeriFlux Sites   ###########################

df <- read.csv("D:/VegetationImpact/AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")
head(df)

# Calculate mean and standard deviation for all data
mean_all <- round(mean(df$sum_Diff_16_mean, na.rm = TRUE), 2)
sd_all <- round(sd(df$sum_Diff_16_mean, na.rm = TRUE), 2)

# Calculate mean and standard deviation for the DBF group
mean_DBF <- round(mean(df$sum_Diff_16_mean[df$Veg == "DBF"], na.rm = TRUE), 2)
sd_DBF <- round(sd(df$sum_Diff_16_mean[df$Veg == "DBF"], na.rm = TRUE), 2)

# Calculate mean and standard deviation for the MF group
mean_MF <- round(mean(df$sum_Diff_16_mean[df$Veg == "MF"], na.rm = TRUE), 2)
sd_val <- sd(df$sum_Diff_16_mean[df$Veg == "MF"], na.rm = TRUE)
sd_MF <- round(ifelse(is.na(sd_val), 0, sd_val), 2)

# Create a summary dataframe
summary_df <- data.frame(
  mean_value = c(mean_all, mean_DBF, mean_MF),
  sd_value = c(sd_all, sd_DBF, sd_MF),
  Group = c("All", "DBF", "MF"),
  error_bars = round(c(0.15 * sd_all, 0.15 * sd_DBF, 0.15 * sd_MF), 2)
)

# View the results
print(summary_df)
# mean_value sd_value Group error_bars
# 1    -558.74   633.22   All      94.98
# 2    -596.84   633.55   DBF      95.03
# 3      50.73     0.00    MF       0.00

colors <- c("All" = "#CC9900",  "DBF" ="#54a708",  "MF" ="#086a10")
summary_df$Group <- factor(summary_df$Group, levels = names(colors)) 

summary_df$mean_value <- as.numeric(summary_df$mean_value)

# Plot the bar chart and add error bars (refined style)
p16 <- ggplot(summary_df, aes(x = Group, y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", color = alpha("black", 0)) +  # Correct: only use fill=Group
  # Draw error bars only for rows with error_bar > 0 (MF has only one value so no error bar)
  geom_errorbar(
    data = subset(summary_df, error_bars > 0),
    aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars),
    size = 4, width = 0.5, color = "gray30"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +
  scale_fill_manual(values = colors) +  # Map color
  labs(y = "Cumulative ΔLST (℃·day)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),       # Hide major x grid
        panel.grid.minor.x = element_blank(),       # Hide minor x grid
        panel.border = element_rect(size = 2.2),    # Set border size
        axis.text.y  = element_text(size = 65, color = "black"), 
        axis.ticks.y = element_line(size = 3),      # Set y tick line width
        axis.ticks.length.y = unit(0.3, "cm"),      # Set y tick length
        axis.title.y = element_text(size = 74), 
        axis.title.x = element_text(size = 0),      # Hide x-axis title
        plot.background = element_rect(size = 100), # Set plot background thickness
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),  # Adjust panel margin
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),              # Hide x-axis text
        axis.ticks.x = element_blank()) +           # Hide x-axis ticks
  scale_y_continuous(limits = c(-1250, 100), breaks = c(-1200, -900, -600, -300, 0, 100))

p16

ggsave(
  filename = "./0.figure/Climate&forest_type/Fig.S-4-barplot_All_AmeriFlux.tiff",
  plot = p16, width = 14.5, height = 12, units = "in", dpi = 300)
