##################  00 Load Packages ##########################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 Calculate Slope of Regression Lines for Each Climate-Subtype Region and Add Attributes to Climate Type Data ###################

# Load the raster file with climate data
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Assign sequential values as placeholders (if necessary)
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]

# Convert raster to categorical data for classification
r <- ratify(r) # Converts raster field to categorical data
rat <- levels(r)[[1]]

# Assign climate types to the raster categories in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')

# Remove the placeholders assigned earlier
r[1:30] <- r0

# Reassign the modified attribute table back to the raster object
levels(r) <- rat

# Defining climate categories for further processing
climate_types <- rat$climate 

# Defining specific climate regions for filtering
filter_value_CfX <- c('Cfa', 'Cfb', 'Cfc')
filter_value_CsX <- c('Csa', 'Csb', 'Csc')
filter_value_CwX <- c('Cwa', 'Cwb', 'Cwc')
filter_value_DfX <- c('Dfa', 'Dfb', 'Dfc', 'Dfd')
filter_value_DsX <- c('Dsa', 'Dsb', 'Dsc', 'Dsd')
filter_value_DwX <- c('Dwa', 'Dwb', 'Dwc', 'Dwd')

# Creating a list of these filtered climate categories
filter_values_list <- list(filter_value_CfX, filter_value_CsX, filter_value_CwX,
                           filter_value_DfX, filter_value_DsX, filter_value_DwX)

##################  01 Calculate Slope of Regression Lines for Each Climate-Subtype Region and Compute for 6 Diff Files ###########################

# File paths and labels list for different climate data
file_paths <- c(
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_1/",  # SOS difference map
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_2/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_3/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_4/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_5/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_6/"
)

labels <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")

# Function to perform analysis on each climate region and compute regression slopes
perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()
  
  for (i in seq_along(file_paths)) {
    file_list_diff <- list.files(file_paths[i], pattern = "\\.tif$", full.names = TRUE)
    LST_diff <- rast(c(file_list_diff))
    
    file_list_act <- list.files("./EA+NA_Results/merged_actLSTmean_years/", pattern = "\\.tif$", full.names = TRUE)
    LST_act <- rast(c(file_list_act))
    
    # Process the data by adjusting values
    sample = LST_diff      
    sample[is.finite(sample)] = 1
    sample2 = LST_act
    sample2[is.finite(sample2)] = 1
    LST_diff = LST_diff * sample[[1]] * sample2[[1]]
    LST_act = LST_act * sample[[1]] * sample2[[1]] 
    
    # Perform analysis for each climate region subtype
    perform_analysis <- function(classify_border, filter_values) {
      results <- list()
      # Iterating through each climate subtype (e.g., Cf, Cs, Dw)
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)
        selected_border_val <- classify_border[order_val, ]
        
        # Extract values for both LST difference and actual LST data
        df1_val <-  terra::extract(LST_diff, selected_border_val)
        df2_val <-  terra::extract(LST_act, selected_border_val)
        df_val <- cbind(df1_val[, -1], df2_val[, -1])
        colnames(df_val) <- c(
          "LST_diff2013", "LST_diff2014", "LST_diff2015", "LST_diff2016", "LST_diff2017",
          "LST_diff2018", "LST_diff2019", "LST_diff2020", "LST_diff2021",
          "LST_act2013", "LST_act2014", "LST_act2015", "LST_act2016", "LST_act2017",
          "LST_act2018", "LST_act2019", "LST_act2020", "LST_act2021"
        )
        
        # Filter columns for differences and actual LST values
        diff_cols_val <- grep("LST_diff", colnames(df_val), value = TRUE)
        act_cols_val <- grep("LST_act", colnames(df_val), value = TRUE)
        
        # Unlist and merge the difference and actual LST values into one data frame
        LST_diff_values_val <- unlist(df_val[, diff_cols_val])
        LST_act_values_val <- unlist(df_val[, act_cols_val])
        
        new_df_val <- data.frame(LST_diff = LST_diff_values_val, LST_act = LST_act_values_val)
        new_df_val <- na.omit(new_df_val)
        
        # Check if there are enough data points for linear fitting (e.g., at least 500 data points)
        if (nrow(new_df_val) >= 500) {  # Minimum of 500 data points for linear fitting
          model_val <- lm(LST_diff ~ LST_act, data = new_df_val)
          
          results[[length(results) + 1]] <- list(
            Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
            Slope = as.numeric(coef(model_val)[2]),
            R_squared = summary(model_val)[["r.squared"]],
            p_value = summary(model_val)[["coefficients"]][2, 4]
          )
        } else {
          # If there are not enough data points (less than 500), skip this climate type
          cat("Insufficient data points for filter value:", filter_val, "\n")
        }
      }
      
      return(do.call(rbind, results))
    }
    
    # Execute analysis for all climate subtypes
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)
    
    # Extract the climate type based on filter value (second character in filter value)
    results_df$type <- substr(results_df$Filter_Value, 2, 2)  # Extract second character as type
    results_df$type <- paste("Type ", results_df$type, sep = "") # Convert character to type name
    results_df$type <- factor(results_df$type)
    
    # Add significance stars based on p-value thresholds
    results_df$stars <- ifelse(results_df$p_value <= 0.001, "***",
                               ifelse(results_df$p_value <= 0.01, "**",
                                      ifelse(results_df$p_value <= 0.05, "*", "")))
    
    # Round the slope and R-squared values for display
    results_df$stars <- factor(results_df$stars)
    results_df$Slope <- round(as.numeric(results_df$Slope), 4)
    results_df$R_squared <- round(as.numeric(results_df$R_squared), 4)
    
    # Add label for the specific analysis (e.g., SOS, MGP)
    results_df$PHE <- labels[i]
    
    list_of_results[[i]] <- results_df
  }
  
  # Combine all results into one data frame
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}

# Perform the analysis and get the final results
final_results <- perform_analysis(file_paths, labels)

# Find the maximum and minimum slope values
max_value <- max(final_results$Slope)
min_value <- min(final_results$Slope)

# Print the maximum and minimum slope values
print(paste("Maximum value:", max_value))
print(paste("Minimum value:", min_value))


##################    02 Plotting lineplot for C-type-precipitation  ###########################################################################

library(dplyr)
library(ggplot2)

# Clean the 'Filter_Value' by removing double quotes and convert to character type
final_results$Filter_Value <- lapply(final_results$Filter_Value, function(x) gsub("\"", "", x))
final_results$Filter_Value <- as.character(final_results$Filter_Value)

# Reorder the 'PHE' factor levels
final_results$PHE <- factor(final_results$PHE, levels = c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS"))
final_results$PHE_num <- as.numeric(final_results$PHE)

# Create a legend column combining the first two characters of 'Filter_Value'
final_results$legend <- paste0(substr(final_results$Filter_Value, 1, 1), substr(final_results$Filter_Value, 2, 2),"X")

# Check the data types
class(final_results$PHE_num)
class(final_results$Filter_Value)
class(final_results$Slope)

# Create ggplot chart labels data
labels_data <- final_results  # Use the main dataset for labels
labels_data$label_text <- paste("", final_results$stars)  # Create label text

###
# Filter data for C-type and D-type climates
k_df_C <- labels_data %>%
  filter(substr(legend, 1, 1) == "C")

k_df_D <- labels_data %>%
  filter(substr(legend, 1, 1) == "D")


###########################  04-1 Plotting lineplot for C-climate type k-value   #####################################################################

# Ensure 'Slope' is numeric
k_df_C$Slope <- as.numeric(k_df_C$Slope)     
k_df_D$Slope <- as.numeric(k_df_D$Slope)     

# Create a line plot for the C-type climate
p1 <- ggplot(k_df_C, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # Increase line thickness
  geom_text(data = k_df_C, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE) +             # Don't display text legend
  labs(x = "Phenological index", y = expression(paste(D[T]~"(℃/℃)"))) + # Axis labels
  theme_minimal() +                           # Use a minimal theme
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 2) + # Add a dashed horizontal line at y = 0
  scale_color_manual(
    values = c( "#005000", "#00AA00", "#96FF00"),  # Custom colors for the climate types
    labels = c("CfX", "CsX", "CwX"),
    name = "Climate type"
  ) +
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.5) + # Adjust the aspect ratio
  theme(
    legend.position = c(0.84, 0.88), 
    axis.text.x  = element_text(size = 36), 
    axis.text.y  = element_text(size = 42), 
    axis.line = element_line(size = 2),  # Adjust axis line thickness
    axis.ticks = element_line(size = 2), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_text(size = 37),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)), # Adjust the distance between x-axis and x-axis title
    panel.grid = element_line(linetype = "blank")
  ) +
  guides(color = guide_legend(ncol = 1, keywidth = 2),    # Adjust legend key length
         fill = guide_legend(byrow = TRUE)) + 
  scale_y_continuous(breaks = c(-1.0, -0.5, 0, 0.5, 1.0), limits = c(-1.0, 1.0))  # Set y-axis limits and breaks

p1

# Save the plot as a .tiff file
ggsave(
  filename = "./0.figure/Fig.S7-lineplot_C-P.tiff",
  plot = p1,  width = 15,  height = 11,  units = "in",  dpi = 300
)


###########################  04-2 Plotting lineplot for D-climate type k-value   #####################################################################

# Create a line plot for the D-type climate
p2 <- ggplot(k_df_D, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # Increase line thickness
  geom_text(data = k_df_D, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE) +             # Don't display text legend
  labs(x = "Phenological index", y = expression(paste(D[T]~"(℃/℃)"))) + # Axis labels
  theme_minimal() +                           # Use a minimal theme
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 2) + # Add a dashed horizontal line at y = 0
  guides(color = guide_legend(ncol = 1)) +    # Adjust the legend layout
  scale_color_manual(
    values = c( "#003366", "#006699", "#66CCFF"),  # Custom colors for the climate types
    labels = c("DfX", "DsX", "DwX"),
    name = "Climate type"
  ) +
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.5) + # Adjust the aspect ratio
  theme(
    legend.position = c(0.84, 0.88), 
    axis.text.x  = element_text(size = 36), 
    axis.text.y  = element_text(size = 42), 
    axis.line = element_line(size = 2),  # Adjust axis line thickness
    axis.ticks = element_line(size = 2), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_text(size = 37),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)), # Adjust the distance between x-axis and x-axis title
    panel.grid = element_line(linetype = "blank") # Remove grid lines
  ) +
  guides(color = guide_legend(keywidth = 2), fill = guide_legend(byrow = TRUE)) +  # Adjust legend key length
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1.0), limits = c(-0.6, 1.4))  # Set y-axis limits and breaks

# Display the plot
p2

# Save the plot as a .tiff file
ggsave(
  filename = "./0.figure/Fig.S7-lineplot_D-P.tiff",
  plot = p2,  width = 15,  height = 11,  units = "in",  dpi = 300
)
