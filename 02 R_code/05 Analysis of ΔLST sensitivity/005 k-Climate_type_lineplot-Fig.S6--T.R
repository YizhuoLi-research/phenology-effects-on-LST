#############################################  00 Load Packages  #################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

############  01 Calculate Regression Slope for Each Climate Zone by Climate-Subtype and Add Attributes to Climate Type Data ###################

# Read the climate type raster file
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Assign a sequence to the first 30 cells for demonstration purposes
r[1:30] <- seq(1, 30, 1)  
r0 <- r[1:30]

# Convert the raster into categorical data (factorize the raster)
r <- ratify(r) 

# Create a legend for the climate types in alphabetic order
rat <- levels(r)[[1]]
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb', 'Cfc', 
                 'Csa', 'Csb', 'Csc', 
                 'Cwa', 'Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc', 'Dfd', 
                 'Dsa', 'Dsb', 'Dsc', 'Dsd',
                 'Dwa', 'Dwb', 'Dwc', 'Dwd', 
                 'EF', 'ET')

# Remove the placeholder values
r[1:30] <- r0

# Reassign the modified attribute table to the raster object
levels(r) <- rat

# Define borders for each climate type from the raster
classify_border <- as.polygons(rast(r))

# Define climate type list
climate_types <- rat$climate 

# Define climate zone subtypes for filtering
filter_value_CXa <- c('Csa', 'Cfa', 'Cwa')
filter_value_CXb <- c('Csb', 'Cfb', 'Cwb')
filter_value_CXc <- c('Csc', 'Cfc', 'Cwc')
filter_value_DXa <- c('Dsa', 'Dfa', 'Dwa')
filter_value_DXb <- c('Dsb', 'Dfb', 'Dwb')
filter_value_DXc <- c('Dsc', 'Dfc', 'Dwc')
filter_value_DXd <- c('Dsd', 'Dfd', 'Dwd')

# Create a list of filter values for different climate zones
filter_values_list <- list(filter_value_CXa, filter_value_CXb, filter_value_CXc,    
                           filter_value_DXa, filter_value_DXb, filter_value_DXc, filter_value_DXd)

##################  02 Calculate Regression Slope for Each Pixel by Climate-Subtype Type -- Calculate Slope for 6 Diff Files ###########################

# Define file paths and label list
file_paths <- c(
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_1/",  # SOS difference map
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_2/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_3/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_4/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_5/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_6/"
)

labels <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")

# Define the main function to perform the analysis
perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()
  
  for (i in seq_along(file_paths)) {
    # i = 4
    
    # Read the difference files for each year in the current folder
    file_list_diff <- list.files(file_paths[i], pattern = "\\.tif$", full.names = TRUE)
    LST_diff <- rast(c(file_list_diff))
    
    # Read the actual LST files for each year
    file_list_act <- list.files("./EA+NA_Results/merged_actLSTmean_years/", pattern = "\\.tif$", full.names = TRUE)
    LST_act <- rast(c(file_list_act))
    
    # Data processing: mask out non-finite values and perform multiplication
    sample = LST_diff      
    sample[is.finite(sample)] = 1
    sample2 = LST_act
    sample2[is.finite(sample2)] = 1
    LST_diff = LST_diff * sample[[1]] * sample2[[1]]
    LST_act = LST_act * sample[[1]] * sample2[[1]] 
    
    # Define the nested analysis function for climate types
    perform_analysis <- function(classify_border, filter_values) {
      results <- list()
      
      # Loop through each filter value (climate type)
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)
        selected_border_val <- classify_border[order_val, ]
        
        # Extract the data for the current filter value
        df1_val <-  terra::extract(LST_diff, selected_border_val)
        df2_val <-  terra::extract(LST_act, selected_border_val)
        
        # Combine the extracted data
        df_val <- cbind(df1_val[, -1], df2_val[, -1])
        colnames(df_val) <- c(
          "LST_diff2013", "LST_diff2014", "LST_diff2015", "LST_diff2016", "LST_diff2017",
          "LST_diff2018", "LST_diff2019", "LST_diff2020", "LST_diff2021",
          "LST_act2013", "LST_act2014", "LST_act2015", "LST_act2016", "LST_act2017",
          "LST_act2018", "LST_act2019", "LST_act2020", "LST_act2021"
        )
        
        # Select the columns for LST_diff and LST_act
        diff_cols_val <- grep("LST_diff", colnames(df_val), value = TRUE)
        act_cols_val <- grep("LST_act", colnames(df_val), value = TRUE)
        
        # Unlist the values for analysis
        LST_diff_values_val <- unlist(df_val[, diff_cols_val])
        LST_act_values_val <- unlist(df_val[, act_cols_val])
        
        # Create a new data frame for regression analysis
        new_df_val <- data.frame(LST_diff = LST_diff_values_val, LST_act = LST_act_values_val)
        new_df_val <- na.omit(new_df_val)
        
        # Check if there are enough data points for linear regression (500 data points minimum)
        if (nrow(new_df_val) >= 500) {  # 500 data points threshold
          model_val <- lm(LST_diff ~ LST_act, data = new_df_val)
          
          # Store the regression results
          results[[length(results) + 1]] <- list(
            Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
            Slope = as.numeric(coef(model_val)[2]),
            R_squared = summary(model_val)[["r.squared"]],
            p_value = summary(model_val)[["coefficients"]][2, 4]
          )
        } else {
          # Skip this climate type if there are not enough data points for regression
          cat("Insufficient data points for filter value:", filter_val, "\n")
        }
      }
      
      return(do.call(rbind, results))
    }
    
    # Execute the analysis for all climate subtypes
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)
    
    # Extract and transform the climate type from the filter value
    results_df$type <- substr(results_df$Filter_Value, 2, 2)  # Extract the second character as type
    results_df$type <- paste("Type ", results_df$type, sep = "")  # Convert the type label to corresponding name
    results_df$type <- factor(results_df$type)
    
    # Assign star ratings based on p-values
    results_df$stars <- ifelse(results_df$p_value <= 0.001, "***",
                               ifelse(results_df$p_value <= 0.01, "**",
                                      ifelse(results_df$p_value <= 0.05, "*", "")))
    
    results_df$stars <- factor(results_df$stars)
    
    # Round the slope and R-squared values to 4 decimal places
    results_df$Slope <- round(as.numeric(results_df$Slope), 4)
    results_df$R_squared <- round(as.numeric(results_df$R_squared), 4)
    
    # Add the label corresponding to the current climate type
    results_df$PHE <- labels[i]
    
    list_of_results[[i]] <- results_df
  }
  
  # Combine all the results into one data frame
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}

# Run the analysis and obtain the results
final_results <- perform_analysis(file_paths, labels)

# Calculate and print the maximum and minimum slope values
max_value <- max(final_results$Slope)
min_value <- min(final_results$Slope)

# Print the maximum and minimum slope values
print(paste("Maximum Slope:", max_value))
print(paste("Minimum Slope:", min_value))


##################    02 Plot Lineplot (6) ###########################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Remove quotation marks from Filter_Value and convert it to character type
final_results$Filter_Value <- lapply(final_results$Filter_Value, function(x) gsub("\"", "", x))
final_results$Filter_Value <- as.character(final_results$Filter_Value)

# Set the PHE variable as a factor with a defined order
final_results$PHE <- factor(final_results$PHE, levels = c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS"))
final_results$PHE_num <- as.numeric(final_results$PHE)

# Create a new legend variable by extracting specific characters from Filter_Value
final_results$legend <- paste0(substr(final_results$Filter_Value, 1, 1), "X",substr(final_results$Filter_Value, 3, 3))

# Check the class of the key variables
class(final_results$PHE_num)  # Check class of PHE_num
class(final_results$Filter_Value)  # Check class of Filter_Value
class(final_results$Slope)  # Check class of Slope

# Prepare the data for the plot labels
labels_data <- final_results  # Assume using the same dataset for the labels
labels_data$label_text <- paste("", final_results$stars)  # Create the label text for each point

# Filter data for climate type "C" for separate analysis
k_df_C <- labels_data %>%
  filter(substr(legend, 1, 1) == "C")

# Filter data for climate type "D" for separate analysis
k_df_D <- labels_data %>%
  filter(substr(legend, 1, 1) == "D")


###########################  04-1 Plot Lineplot for C-climate type k values #####################################################################

# Convert Slope values to numeric for both C and D climate types
k_df_C$Slope <- as.numeric(k_df_C$Slope)    
k_df_D$Slope <- as.numeric(k_df_D$Slope)     

# Create a lineplot for C-climate type
p1 <- ggplot(k_df_C, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # Increase the line thickness
  geom_text(data = k_df_C, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE) +             # Do not show text labels in the legend
  # labs(x = "Phenological index", y = "β (℃/℃)") +  # Axis labels
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)"))) +  # Axis labels with expression for units
  theme_minimal() +                           # Use a minimal theme
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size=2) +  # Add a dashed horizontal line at y = 0
  scale_color_manual(
    values = c("#663300", "#996600", "#FFCC00"),
    labels = c("CXa", "CXb", "CXc"),
    name = "Climate type"
  ) +
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.5) +  # Set aspect ratio of plot
  theme(
    legend.position = c(0.84, 0.88), 
    axis.text.x  = element_text(size = 36), 
    axis.text.y  = element_text(size = 42), 
    axis.line = element_line(size = 2),  # Adjust axis line thickness
    axis.ticks = element_line(size = 2), 
    axis.ticks.length = unit(0.3, "cm"),
    axis.ticks.y = element_line(size = 2),  # Adjust axis tick width
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_text(size = 37),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis title distance
    panel.grid = element_line(linetype = "blank")  # Remove grid lines
  ) +
  guides(color = guide_legend(ncol = 1, keywidth = 2),
         fill = guide_legend(byrow = TRUE)) +
  ylim(-1.0, 1.0)  # Set y-axis limits
p1

# Save the plot as a .tiff file
ggsave(
  filename = "./0.figure/Fig.S6-lineplot_C-T.tiff",
  plot = p1, width = 15, height = 11, units = "in", dpi = 300
)

###########################  04-2 Plot Lineplot for D-climate type k values #####################################################################

# Create a lineplot for D-climate type
p2 <- ggplot(k_df_D, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # Increase the line thickness
  geom_text(data = k_df_D, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE) +             # Do not show text labels in the legend
  # labs(x = "Phenological index", y = "β (℃/℃)") +  # Axis labels
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)"))) +  # Axis labels with expression for units
  theme_minimal() +                           # Use a minimal theme
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size=2) +  # Add a dashed horizontal line at y = 0
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(
    values = c("#820082", "#C800C8", "#C89BFA", "#C8C8FF"),
    labels = c("DXa", "DXb", "DXc", "DXd"),
    name = "Climate type"
  ) +
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.7) +  # Set aspect ratio of plot
  theme(
    legend.position = c(0.84, 0.86), 
    axis.text.x  = element_text(size = 36), 
    axis.text.y  = element_text(size = 42), 
    axis.line = element_line(size = 2),  # Adjust axis line thickness
    axis.ticks = element_line(size = 2), 
    axis.ticks.length = unit(0.3, "cm"),
    axis.ticks.y = element_line(size = 2),  # Adjust axis tick width
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_text(size = 37),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis title distance
    panel.grid = element_line(linetype = "blank")  # Remove grid lines
  ) +
  guides(color = guide_legend(ncol = 1, keywidth = 2),
         fill = guide_legend(byrow = TRUE)) +
  # Set y-axis ticks and limits
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1.0, 1.5, 2.0), limits = c(-0.6, 2.25))  
p2

# Save the plot as a .tiff file
ggsave(
  filename = "./0.figure/Fig.S6-lineplot_D-T.tiff",
  plot = p2, width = 15, height = 11, units = "in", dpi = 300
)

