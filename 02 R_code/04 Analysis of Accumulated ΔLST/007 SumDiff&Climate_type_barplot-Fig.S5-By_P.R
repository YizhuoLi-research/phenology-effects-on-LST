##################  00 Load libraries ##########################################################################
# Load necessary libraries for raster manipulation and data handling
library(terra)
library(tidyverse)
library(raster)

# Set the working directory to the specified folder
setwd("D:/VegetationImpact")

##################  01 Extract data by climate subtype and add attributes to climate type data ###################

# Load the raster file for climate data
r <- raster("./EA+NA_Results/EA_NA_koppen_30km_addClimate.tif")

# Set values for the first 30 cells of the raster
r[1:30] <- seq(1,30,1)

# Create a subset raster of the first 30 cells
r0 <- r[1:30]

# Convert the raster to categorical data (ratified) for easier handling
r <- ratify(r)

# Get the attribute table of the raster
rat <- levels(r)[[1]]

# Assign climate types to the attribute table in alphabetical order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Restore the initial values in the first 30 cells of the raster
r[1:30] <- r0

# Reassign the modified attribute table back to the raster
levels(r) <- rat

# Optional: Use rasterVis library for visualizing the raster (commented out)
# library(rasterVis); levelplot(r)

# Define borders of each climate subtype region based on the raster
classify_border <- as.polygons(rast(r))

# Create a list of climate types
climate_types <- rat$climate 

# Define climate subtypes for filtering
filter_value_CfX <- c( 'Cfa','Cfb','Cfc')
filter_value_CsX <- c( 'Csa','Csb','Csc')
filter_value_CwX <- c( 'Cwa','Cwb','Cwc')
filter_value_DfX <- c( 'Dfa','Dfa','Dfc','Dfd')
filter_value_DsX <- c( 'Dsa','Dsb','Dsc','Dsd')
filter_value_DwX <- c( 'Dwa','Dwb','Dwc','Dwd')

# Create a list of filter values for different climate types
filter_values_list <- list(filter_value_CfX,filter_value_CsX,filter_value_CwX,
                           filter_value_DfX,filter_value_DsX,filter_value_DwX)

##################  02 Calculate regression line slope by climate subtype type and calculate for 6 diff files ###########################

# File paths and label list for the different diff files
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
  
)

# Labels corresponding to each diff file
labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS","SOS-EOS")

# Function to perform the analysis on the provided file paths and labels
perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()  # List to store results for each file
  
  for (i in seq_along(file_paths)) {
    
    # For each file path, perform analysis
    file_path <- file_paths[i]
    
    LST_sumdiff <- rast(file_path)  # Load the raster data for the current file
    
    # Perform the analysis for climate subtypes
    perform_analysis <- function(classify_border, filter_values) {
      results <- list()  # List to store the results of the subtype analysis
      
      # For each climate filter value (subtype), extract data and compute statistics
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)  # Match filter value with classification
        selected_border_val <- classify_border[order_val, ]  # Select corresponding region based on filter value
        df_val <- raster::extract(LST_sumdiff, selected_border_val)  # Extract values from raster
        df_val <- df_val[, -1]  # Remove the first column (likely an index)
        df_val <- data.frame(df_val)  # Convert to data frame
        
        colnames(df_val) <- c("LST_sumdiff")  # Rename the column for clarity
        
        # Extract LST_sumdiff values
        sumdiff_cols_val <- grep("LST_sumdiff", colnames(df_val), value = TRUE)
        LST_sumdiff_values_val <- unlist(df_val[, sumdiff_cols_val])
        
        # Create a new data frame with the LST values
        new_df_val <- data.frame(LST_sumdiff = LST_sumdiff_values_val)
        
        # Store the results for the current filter value (climate subtype)
        results[[length(results) + 1]] <- list(
          Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
          mean_value =  mean(new_df_val$LST_sumdiff, na.rm = TRUE),  # Mean value of LST_sumdiff
          sd_value = sd(new_df_val$LST_sumdiff, na.rm = TRUE)  # Standard deviation of LST_sumdiff
        )
        
      }
      return(do.call(rbind, results))  # Combine results for each filter value into one data frame
    }
    
    # Perform analysis for each climate subtype (filter values) and store results
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)  # Convert results to data frame
    results_df$type <- paste0(substr(results_df$Filter_Value, 2, 2),  # Extract and combine specific characters from the filter value
                              substr(results_df$Filter_Value, 3, 3),"X")
    results_df$type <- as.factor(results_df$type)  # Convert to factor for further analysis
    results_df$mean_value <- round(as.numeric(results_df$mean_value),2)  # Round the mean value to 2 decimal places
    results_df$sd_value <- round(as.numeric(results_df$sd_value),2)  # Round the standard deviation to 2 decimal places
    
    # Add phase labels (from the 'labels' list) to the results data frame
    results_df$Phe_phase <- labels[i]
    
    # Store results for the current file in the results list
    list_of_results[[i]] <- results_df
  }
  
  # Combine all results from different files into one data frame
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)  # Return the combined results
}


# Execute the analysis and get the final results
final_results <- perform_analysis(file_paths, labels)

# Convert 'Filter_Value' to character and calculate error bars based on standard deviation
final_results$Filter_Value <- as.character(final_results$Filter_Value)
final_results$error_bars <- round(final_results$sd_value* 0.15, 2)   

# Find and print the maximum and minimum values in the results
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)
print(paste("Maximum value:", max_value))
print(paste("Minimum value:", min_value))





########################### 03- Create barplot for 6 subtype —— CfX ###########################


# Filter the data to include only the "CfX" type and remove "SOS-EOS"
df_CfX <- final_results[final_results$type == "CfX", ]  # Filter data for CfX type
# > df_CfX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 1  "Cfa", "Cfb", "cfc"      -42.12    45.71  CfX   SOS-MGP       6.86
# 7  "Cfa", "Cfb", "cfc"     -154.32    92.74  CfX   MGP-GMO      13.91
# 13 "Cfa", "Cfb", "cfc"     -774.03   473.53  CfX   GMO-GDO      71.03
# 19 "Cfa", "Cfb", "cfc"     -298.70   231.20  CfX   GDO-MSP      34.68
# 25 "Cfa", "Cfb", "cfc"      -91.66    84.93  CfX   MSP-EOS      12.74
# 31 "Cfa", "Cfb", "cfc"    -1360.83   875.77  CfX   SOS-EOS     131.37
df_CfX <- df_CfX[df_CfX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" from the data

# Define custom colors for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Reorder the factor levels of 'Phe_phase' according to the color scheme
df_CfX$Phe_phase <- factor(df_CfX$Phe_phase, levels = names(colors))

# Convert 'mean_value' to numeric for proper plotting
df_CfX$mean_value <- as.numeric(df_CfX$mean_value)

# Plotting the barplot with error bars (Refining the lines)
p1 <- ggplot(df_CfX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create bar plot with custom fill color
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars to the plot
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add y-axis label
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1.5) +  # Add dashed horizontal line at y = 0
  theme_bw() +  # Set the theme to 'black and white'
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set border size for the entire plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Customize y-axis text appearance
        axis.ticks.y = element_line(size = 2.5),  # Customize y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Customize y-axis tick length
        axis.title.y = element_text(size = 70),  # Customize y-axis title size
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background size for the plot
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), # Adjust plot margins
        legend.position = "none",  # Hide legend
        panel.grid = element_line(linetype = "blank"),  # Remove gridlines
        axis.text.x = element_blank(),  # Hide x-axis labels
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p1  # Display the plot

# Save the plot as a high-resolution TIFF image
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CfX.tiff",
  plot = p1,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot with specified dimensions and resolution


########################### 03- Create barplot for 6 subtype —— CsX ###########################


# This code filters and plots the data for "CsX" subtype, visualizing the cumulative ΔLST over different phenological phases.
df_CsX <- final_results[final_results$type == "CsX", ]  # View data to create S_table6
# > df_CsX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 2  "Csa", "Csb", "Csc"      2.12    62.39  CsX   SOS-MGP       9.36
# 8  "Csa", "Csb", "Csc"      -52.08   125.04  CsX   MGP-GMO      18.76
# 14 "Csa", "Csb", "Csc"     -300.57   541.16  CsX   GMO-GDO      81.17
# 20 "Csa", "Csb", "Csc"      -65.96   309.16  CsX   GDO-MSP      46.37
# 26 "Csa", "Csb", "Csc"      -42.58   146.85  CsX   MSP-EOS      22.03
# 32 "Csa", "Csb", "Csc"     -459.08  1143.60  CsX   SOS-EOS     171.54
df_CsX <- df_CsX[df_CsX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" phase
df_CsX$Phe_phase <- factor(df_CsX$Phe_phase, levels = names(colors))  # Set the factor levels for Phe_phase
df_CsX$mean_value <- as.numeric(df_CsX$mean_value)  # Ensure mean_value is numeric

# Create a bar plot with error bars
p2 <- ggplot(df_CsX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create the bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add label to y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +  # Add horizontal line at y=0
  theme_bw() +  # Apply black and white theme
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set the border size of the plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Set the y-axis text size and color
        axis.ticks.y = element_line(size = 2.5),  # Set the y-axis ticks width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set the y-axis ticks length
        axis.title.y = element_text(size = 70),  # Set the y-axis title size
        axis.title.x = element_text(size = 0),  # Hide the x-axis title
        plot.background = element_rect(size = 100),      # Set the plot background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # Adjust the plot margins
        legend.position = "none",  # Hide the legend
        panel.grid = element_line(linetype = "blank"),  # Remove the grid
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p2

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CsX.tiff",
  plot = p2,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot as a high-resolution TIFF file


########################### 03- Create barplot for 6 subtype —— CwX ###########################


# This code filters and plots the data for "CwX" subtype, visualizing the cumulative ΔLST over different phenological phases.
df_CwX <- final_results[final_results$type == "CwX", ]  # View data to create S_table6
# > df_CwX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 3  "Cwa", "Cwb", "Cwc"      -90.20    61.23  CwX   SOS-MGP       9.18
# 9  "Cwa", "Cwb", "Cwc"     -227.52   115.38  CwX   MGP-GMO      17.31
# 15 "Cwa", "Cwb", "Cwc"     -918.51   436.43  CwX   GMO-GDO      65.46
# 21 "Cwa", "Cwb", "Cwc"     -287.50   169.73  CwX   GDO-MSP      25.46
# 27 "Cwa", "Cwb", "Cwc"      -56.30    74.61  CwX   MSP-EOS      11.19
# 33 "Cwa", "Cwb", "Cwc"    -1580.02   758.82  CwX   SOS-EOS     113.82
df_CwX <- df_CwX[df_CwX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" phase
df_CwX$Phe_phase <- factor(df_CwX$Phe_phase, levels = names(colors))  # Set the factor levels for Phe_phase
df_CwX$mean_value <- as.numeric(df_CwX$mean_value)  # Ensure mean_value is numeric

# Create a bar plot with error bars
p3 <- ggplot(df_CwX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create the bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add label to y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +  # Add horizontal line at y=0
  theme_bw() +  # Apply black and white theme
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set the border size of the plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Set the y-axis text size and color
        axis.ticks.y = element_line(size = 2.5),  # Set the y-axis ticks width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set the y-axis ticks length
        axis.title.y = element_text(size = 70),  # Set the y-axis title size
        axis.title.x = element_text(size = 0),  # Hide the x-axis title
        plot.background = element_rect(size = 100),      # Set the plot background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # Adjust the plot margins
        legend.position = "none",  # Hide the legend
        panel.grid = element_line(linetype = "blank"),  # Remove the grid
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p3

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CwX.tiff",
  plot = p3,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot as a high-resolution TIFF file


########################### 03- Create barplot for 6 subtype —— DfX ###########################


# This code filters and plots the data for "DfX" subtype, visualizing the cumulative ΔLST over different phenological phases.
df_DfX <- final_results[final_results$type == "DfX", ]  # View data to create S_table6
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 4  "Dfa", "Dfa", "Dfc", "Dfd"      25.64    67.29  DfX   SOS-MGP      10.09
# 10 "Dfa", "Dfa", "Dfc", "Dfd"      56.79    75.18  DfX   MGP-GMO      11.28
# 16 "Dfa", "Dfa", "Dfc", "Dfd"     245.38   301.41  DfX   GMO-GDO      45.21
# 22 "Dfa", "Dfa", "Dfc", "Dfd"      72.05   115.63  DfX   GDO-MSP      17.34
# 28 "Dfa", "Dfa", "Dfc", "Dfd"      24.46    60.60  DfX   MSP-EOS       9.09
# 34 "Dfa", "Dfa", "Dfc", "Dfd"     424.33   572.27  DfX   SOS-EOS      85.84
df_DfX <- df_DfX[df_DfX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" phase
df_DfX$Phe_phase <- factor(df_DfX$Phe_phase, levels = names(colors))  # Set the factor levels for Phe_phase
df_DfX$mean_value <- as.numeric(df_DfX$mean_value)  # Ensure mean_value is numeric

# Create a bar plot with error bars
p4 <- ggplot(df_DfX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create the bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add label to y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +  # Add horizontal line at y=0
  theme_bw() +  # Apply black and white theme
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set the border size of the plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Set the y-axis text size and color
        axis.ticks.y = element_line(size = 2.5),  # Set the y-axis ticks width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set the y-axis ticks length
        axis.title.y = element_text(size = 70),  # Set the y-axis title size
        axis.title.x = element_text(size = 0),  # Hide the x-axis title
        plot.background = element_rect(size = 100),      # Set the plot background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # Adjust the plot margins
        legend.position = "none",  # Hide the legend
        panel.grid = element_line(linetype = "blank"),  # Remove the grid
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p4

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DfX.tiff",
  plot = p4,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot as a high-resolution TIFF file


########################### 03- Create barplot for 6 subtype —— DsX ###########################


# This code filters and plots the data for "DsX" subtype, visualizing the cumulative ΔLST over different phenological phases.
df_DsX <- final_results[final_results$type == "DsX", ]  # View data to create S_table6
# > df_DsX                 Filter_Value mean_value sd_value type Phe_phase error_bars
# 5  "Dsa", "Dsb", "Dsc", "Dsd"    -  -7.52    66.81  DsX   SOS-MGP      10.02
# 11 "Dsa", "Dsb", "Dsc", "Dsd"      -42.76    68.62  DsX   MGP-GMO      10.29
# 17 "Dsa", "Dsb", "Dsc", "Dsd"     -196.48   278.32  DsX   GMO-GDO      41.75
# 23 "Dsa", "Dsb", "Dsc", "Dsd"   -  -74.89   134.02  DsX   GDO-MSP      20.10
# 29 "Dsa", "Dsb", "Dsc", "Dsd"      -39.80    82.10  DsX   MSP-EOS      12.31
# 35 "Dsa", "Dsb", "Dsc", "Dsd"     -361.46   576.89  DsX   SOS-EOS      86.53
df_DsX <- df_DsX[df_DsX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" phase
df_DsX$Phe_phase <- factor(df_DsX$Phe_phase, levels = names(colors))  # Set the factor levels for Phe_phase
df_DsX$mean_value <- as.numeric(df_DsX$mean_value)  # Ensure mean_value is numeric

# Create a bar plot with error bars
p5 <- ggplot(df_DsX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create the bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add label to y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +  # Add horizontal line at y=0
  theme_bw() +  # Apply black and white theme
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set the border size of the plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Set the y-axis text size and color
        axis.ticks.y = element_line(size = 2.5),  # Set the y-axis ticks width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set the y-axis ticks length
        axis.title.y = element_text(size = 70),  # Set the y-axis title size
        axis.title.x = element_text(size = 0),  # Hide the x-axis title
        plot.background = element_rect(size = 100),      # Set the plot background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # Adjust the plot margins
        legend.position = "none",  # Hide the legend
        panel.grid = element_line(linetype = "blank"),  # Remove the grid
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p5

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DsX.tiff",
  plot = p5,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot as a high-resolution TIFF file


########################### 03- Create barplot for 6 subtype —— DwX ###########################


# This code filters and plots the data for "DwX" subtype, visualizing the cumulative ΔLST over different phenological phases.
df_DwX <- final_results[final_results$type == "DwX", ]  # View data to create S_table6
# > df_DwX                 Filter_Value mean_value sd_value type Phe_phase error_bars
# 6  "Dwa", "Dwb", "Dwc", "Dwd"      -75.26    43.17  DwX   SOS-MGP       6.48
# 12 "Dwa", "Dwb", "Dwc", "Dwd"     -179.26    88.58  DwX   MGP-GMO      13.29
# 18 "Dwa", "Dwb", "Dwc", "Dwd"     -627.69   379.09  DwX   GMO-GDO      56.86
# 24 "Dwa", "Dwb", "Dwc", "Dwd"     -191.44   132.42  DwX   GDO-MSP      19.86
# 30 "Dwa", "Dwb", "Dwc", "Dwd"      -42.92    48.00  DwX   MSP-EOS       7.20
# 36 "Dwa", "Dwb", "Dwc", "Dwd"    -1116.57   628.32  DwX   SOS-EOS      94.25
df_DwX <- df_DwX[df_DwX$Phe_phase != "SOS-EOS", ]  # Remove "SOS-EOS" phase
df_DwX$Phe_phase <- factor(df_DwX$Phe_phase, levels = names(colors))  # Set the factor levels for Phe_phase
df_DwX$mean_value <- as.numeric(df_DwX$mean_value)  # Ensure mean_value is numeric

# Create a bar plot with error bars
p6 <- ggplot(df_DwX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Create the bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y ="Cumulative ΔLST (℃·day)") +  # Add label to y-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +  # Add horizontal line at y=0
  theme_bw() +  # Apply black and white theme
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis gridlines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis gridlines
        panel.border = element_rect(size = 2),        # Set the border size of the plot
        axis.text.y  = element_text(size = 60, color = "black"),  # Set the y-axis text size and color
        axis.ticks.y = element_line(size = 2.5),  # Set the y-axis ticks width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set the y-axis ticks length
        axis.title.y = element_text(size = 70),  # Set the y-axis title size
        axis.title.x = element_text(size = 0),  # Hide the x-axis title
        plot.background = element_rect(size = 100),      # Set the plot background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # Adjust the plot margins
        legend.position = "none",  # Hide the legend
        panel.grid = element_line(linetype = "blank"),  # Remove the grid
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))  # Set y-axis limits and breaks

p6

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DwX.tiff",
  plot = p6,  width = 17,  height = 13,  units = "in",  dpi = 300)  # Save the plot as a high-resolution TIFF file


