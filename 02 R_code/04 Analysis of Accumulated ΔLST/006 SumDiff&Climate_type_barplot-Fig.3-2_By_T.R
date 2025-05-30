##################  00 Load Required Packages   ##########################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 Extract Data by Climate Type and Subtype -- Add Attributes to Climate Type Data   ###################

# Load the climate raster data
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Modify the raster data with values from 1 to 30 for testing
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]

# Convert the raster data to categorical values (classify)
r <- ratify(r)  # Converts raster field to categorical data
rat <- levels(r)[[1]]

# Assign the climate categories in alphabetical order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Restore the modified values back into the raster object
r[1:30] <- r0

# Reassign the modified attribute table to the raster object
levels(r) <- rat
# library(rasterVis);levelplot(r)

# Create the borders for each climate region defined by 'filter_values_list'
classify_border <- as.polygons(rast(r))
# plot(classify_border)

# Create a list of climate types
climate_types <- rat$climate 

# Define the climate subtypes for each climate zone
filter_value_CXa <- c( 'Csa','Cfa','Cwa')
filter_value_CXb <- c( 'Csb','Cfb','Cwb')
filter_value_CXc <- c( 'Csc','Cfc','Cwc')
filter_value_DXa <- c( 'Dsa','Dfa','Dwa')
filter_value_DXb <- c( 'Dsb','Dfb','Dwb')
filter_value_DXc <- c( 'Dsc','Dfc','Dwc')
filter_value_DXd <- c( 'Dsd','Dfd','Dwd')

# Combine the subtypes into a list for filtering purposes
filter_values_list <- list(filter_value_CXa,filter_value_CXb,filter_value_CXc,
                           filter_value_DXa,filter_value_DXb,filter_value_DXc,filter_value_DXd)


##################  02 Extract Values for Six Phenological Stages by Climate Type and Subtype, and Plot the Results ###########################

# File paths and labels for the phenological stages
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
  
)

# Labels for the different phases of phenology
labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS","SOS-EOS")

# Function to perform the analysis on climate types and subtypes
perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()
  
  # Loop over each file path to extract data
  for (i in seq_along(file_paths)) {
    
    # Load each LST (Land Surface Temperature) difference file
    file_path <- file_paths[i]
    LST_sumdiff <- rast(file_path)
    
    # Perform analysis for each climate subtype (climate region)
    perform_analysis <- function(classify_border, filter_values) {
      results <- list() 
      
      # Loop over each climate region in the filter_values list
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)
        selected_border_val <- classify_border[order_val, ]
        
        # Extract the LST difference values for the selected climate region
        df_val <- raster::extract(LST_sumdiff, selected_border_val)
        df_val <- df_val[, -1]  # Remove the first column
        df_val <- data.frame(df_val)
        
        colnames(df_val) <- c("LST_sumdiff")
        
        # Extract the LST values
        sumdiff_cols_val <- grep("LST_sumdiff", colnames(df_val), value = TRUE)
        LST_sumdiff_values_val <- unlist(df_val[, sumdiff_cols_val])
        
        # Prepare the results dataframe
        new_df_val <- data.frame(LST_sumdiff = LST_sumdiff_values_val)
        
        # Append the results to the list
        results[[length(results) + 1]] <- list(
          Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
          mean_value =  mean(new_df_val$LST_sumdiff, na.rm = TRUE),
          sd_value = sd(new_df_val$LST_sumdiff, na.rm = TRUE)
        )
        
      }
      return(do.call(rbind, results))
    }
    
    # Perform analysis on the subtypes (regions)
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)
    
    # Add a new column 'type' based on Filter_Value
    results_df$type <- paste0(substr(results_df$Filter_Value, 2, 2),
                              "X",substr(results_df$Filter_Value, 4, 4))
    results_df$type <- as.factor(results_df$type)
    results_df$mean_value <- round(as.numeric(results_df$mean_value),2)
    results_df$sd_value <- round(as.numeric(results_df$sd_value),2)
    
    # Add the phenological stage label to the dataframe
    results_df$Phe_phase <- labels[i]
    
    # Store the results for this phenological stage
    list_of_results[[i]] <- results_df
  }
  
  # Combine all the results into a single data frame
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}


# Perform the analysis and obtain the final results
final_results <- perform_analysis(file_paths, labels)
final_results$Filter_Value <- as.character(final_results$Filter_Value) 

# Calculate error bars as 15% of the standard deviation
final_results$error_bars <- round(final_results$sd_value* 0.15, 2)

# Calculate the max and min values for the mean values
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)

# Print the maximum and minimum values
print(paste("Maximum value:", max_value))
print(paste("Minimum value:", min_value))




########################### 03-CXa Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "CXa" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_CXa <- final_results[final_results$type == "CXa", ]  
df_CXa <- df_CXa[df_CXa$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_CXa$Phe_phase <- factor(df_CXa$Phe_phase, levels = names(colors))  

# Convert mean_value to numeric format
df_CXa$mean_value <- as.numeric(df_CXa$mean_value)  

# Create the barplot with error bars (fine-tune line widths)
p1 <- ggplot(df_CXa, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p1

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_CXa.tiff",
  plot = p1,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-CXb Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "CXb" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_CXb <- final_results[final_results$type == "CXb", ]  
df_CXb <- df_CXb[df_CXb$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_CXb$Phe_phase <- factor(df_CXb$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_CXb$mean_value <- as.numeric(df_CXb$mean_value)


# Create the barplot with error bars (fine-tune line widths)
p2 <- ggplot(df_CXb, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p2

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_Cxb.tiff",
  plot = p2,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-CXc Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "CXc" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_CXc <- final_results[final_results$type == "CXc", ]  
df_CXc <- df_CXc[df_CXc$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_CXc$Phe_phase <- factor(df_CXc$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_CXc$mean_value <- as.numeric(df_CXc$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p3 <- ggplot(df_CXc, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78, margin = margin(r = 70)),  # Adjust margin for y-axis title
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(0, 200), breaks = c(0,50,100,150,200))  # Set y-axis limits and breaks

# Display the plot
p3

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_CXc.tiff",
  plot = p3,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-DXa Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "DXa" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_DXa <- final_results[final_results$type == "DXa", ]  
df_DXa <- df_DXa[df_DXa$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_DXa$Phe_phase <- factor(df_DXa$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_DXa$mean_value <- as.numeric(df_DXa$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p4 <- ggplot(df_DXa, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p4

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXa.tiff",
  plot = p4,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-DXb Plot Barplot for 7 Subtypes  #####################################################################

# Filter the data for "DXb" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_DXb <- final_results[final_results$type == "DXb", ]  
df_DXb <- df_DXb[df_DXb$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_DXb$Phe_phase <- factor(df_DXb$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_DXb$mean_value <- as.numeric(df_DXb$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p5 <- ggplot(df_DXb, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p5

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXb.tiff",
  plot = p5,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-DXc Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "DXc" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_DXc <- final_results[final_results$type == "DXc", ]  
df_DXc <- df_DXc[df_DXc$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_DXc$Phe_phase <- factor(df_DXc$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_DXc$mean_value <- as.numeric(df_DXc$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p6 <- ggplot(df_DXc, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p6

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXc.tiff",
  plot = p6,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


########################### 03-DXd Plot, Barplot for 7 Subtypes  #####################################################################

# Filter the data for "DXd" subtype and exclude "SOS-EOS" phase for analysis (S_table5 preparation)
df_DXd <- final_results[final_results$type == "DXd", ]  
df_DXd <- df_DXd[df_DXd$Phe_phase != "SOS-EOS", ]

# Define color palette for each phase
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
df_DXd$Phe_phase <- factor(df_DXd$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
df_DXd$mean_value <- as.numeric(df_DXd$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p7 <- ggplot(df_DXd, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p7

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXd.tiff",
  plot = p7,  width = 16.5,  height = 13,  units = "in",  dpi = 300)



############# 04. 5 phenological phases Average Cumulative Temperature Histogram ##################################


# File paths and labels list
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
)

labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS","SOS-EOS")

# Function to perform analysis on the raster files and summarize results
perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()  # Create an empty list to store results
  
  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    
    # Read raster data
    LST_sumdiff <- raster(file_path)
    
    # Perform analysis to calculate mean and standard deviation of values
    results_df <- data.frame(
      mean_value = mean(values(LST_sumdiff), na.rm = TRUE),
      sd_value = sd(values(LST_sumdiff), na.rm = TRUE)
    )
    
    # Round results to two decimal places
    results_df$mean_value <- round(results_df$mean_value, 2)
    results_df$sd_value <- round(results_df$sd_value, 2)
    
    # Add corresponding label for phase
    results_df$Phe_phase <- labels[i]
    
    list_of_results[[i]] <- results_df  # Add the result to the list
  }
  
  # Combine all results into a single data frame
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}

# Call the function to perform the analysis and get results
final_results <- perform_analysis(file_paths, labels)
print(final_results)

# Calculate error bars (15% of standard deviation)
final_results$error_bars <- round(final_results$sd_value * 0.15, 2)

# Find maximum and minimum values of the mean
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)

# Print the maximum and minimum values
print(paste("Maximum value:", max_value))
print(paste("Minimum value:", min_value))

# Filter out the "SOS-EOS" phase
final_results <- final_results[final_results$Phe_phase != "SOS-EOS", ]

# Define color palette for different phases
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")

# Set the order of the phases in the plot
final_results$Phe_phase <- factor(final_results$Phe_phase, levels = names(colors)) 

# Convert mean_value to numeric format
final_results$mean_value <- as.numeric(final_results$mean_value)

# Create the barplot with error bars (fine-tune line widths)
p_sum <- ggplot(final_results, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Draw bar plot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Add error bars
  labs(y = "Cumulative ΔLST (℃·day)") +  # Add y-axis label
  theme_bw() +  # Set the theme to black and white
  theme(panel.grid.major.x = element_blank(),         # Hide major x-axis grid lines
        panel.grid.minor.x = element_blank(),         # Hide minor x-axis grid lines
        panel.border = element_rect(size = 2),        # Set border size for the plot
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # Increase y-axis tick width
        axis.ticks.length.y = unit(0.3, "cm"),   # Set y-axis tick length
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # Hide x-axis title
        plot.background = element_rect(size = 100),      # Set background border size
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"), # Adjust panel margins
        legend.position = "none",  # Remove legend
        panel.grid = element_line(linetype = "blank"),  # Hide grid lines
        axis.text.x = element_blank(),  # Hide x-axis text
        axis.ticks.x = element_blank()) +  # Hide x-axis ticks
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))  # Set y-axis limits and breaks

# Display the plot
p_sum

# Save the plot as a TIFF file
ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_sum.tiff",
  plot = p_sum,  width = 16.5,  height = 13,  units = "in",  dpi = 300)



############################  05 - Barplot of All & Subtypes of AmeriFlux Sites (Cumulative ΔLST)   ###########################

df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv")
head(df)

# Calculate overall mean and standard deviation
mean_all <- round(mean(df$sum_Diff_16_mean, na.rm = TRUE), 2)
sd_all <- round(sd(df$sum_Diff_16_mean, na.rm = TRUE), 2)

# Calculate mean and standard deviation for group Cfa
mean_cfa <- round(mean(df$sum_Diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)
sd_cfa <- round(sd(df$sum_Diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)

# Calculate mean and standard deviation for group Dfb
mean_dfb <- round(mean(df$sum_Diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)
sd_dfb <- round(sd(df$sum_Diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)

# Create summary data frame
summary_df <- data.frame(
  mean_value = c(mean_all, mean_cfa, mean_dfb),
  sd_value = c(sd_all, sd_cfa, sd_dfb),
  Group = c("All", "CXa", "DXb"),
  error_bars = round(c(0.15 * sd_all, 0.15 * sd_cfa, 0.15 * sd_dfb), 2)
)

# Print summary
print(summary_df)

colors <- c("All" = "#663300",  "CXa" = "#CC9900", "DXb" = "#FFCC00")
summary_df$Group <- factor(summary_df$Group, levels = names(colors)) 
summary_df$mean_value <- as.numeric(summary_df$mean_value)

# Plot barplot with error bars
p8 <- ggplot(summary_df, aes(x = Group, y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # Barplot
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # Error bars
  labs(y = "Cumulative ΔLST (℃·day)") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2),
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0), 
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p8

ggsave(
  filename = "./0.figure/Fig.3-barplot_All_AmeriFlux.tiff",
  plot = p8,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


###########################  06 - Barplot of All & Subtypes of AmeriFlux Sites (Cumulative ΔTa)   ###########################

df <- read.csv("./AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_average_TAdiff_all-info.csv")
head(df)

# Calculate overall mean and standard deviation
mean_all <- round(mean(df$AF_TA_sum_diff_16_mean, na.rm = TRUE), 2)
sd_all <- round(sd(df$AF_TA_sum_diff_16_mean, na.rm = TRUE), 2)

# Calculate mean and standard deviation for group Cfa
mean_cfa <- round(mean(df$AF_TA_sum_diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)
sd_cfa <- round(sd(df$AF_TA_sum_diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)

# Calculate mean and standard deviation for group Dfb
mean_dfb <- round(mean(df$AF_TA_sum_diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)
sd_dfb <- round(sd(df$AF_TA_sum_diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)

# Create summary data frame
summary_df <- data.frame(
  mean_value = c(mean_all, mean_cfa, mean_dfb),
  sd_value = c(sd_all, sd_cfa, sd_dfb),
  Group = c("All", "CXa", "DXb"),
  error_bars = round(c(0.15 * sd_all, 0.15 * sd_cfa, 0.15 * sd_dfb), 2)
)

# Print summary
print(summary_df)

colors <- c("All" = "#663300",  "CXa" = "#CC9900", "DXb" = "#FFCC00")
summary_df$Group <- factor(summary_df$Group, levels = names(colors)) 
summary_df$mean_value <- as.numeric(summary_df$mean_value)

# Plot barplot with error bars
p8 <- ggplot(summary_df, aes(x = Group, y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +
  labs(y = "Cumulative ΔTa (℃·day)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2),
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.title.y = element_text(size = 78, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 0),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 20, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(-900, 300), breaks = c(-900,-600,-300,0,300))

p8

ggsave(
  filename = "./0.figure/Fig.3-barplot_All_AmeriFlux_Ta.tiff",
  plot = p8,  width = 16.5,  height = 13,  units = "in",  dpi = 300)
