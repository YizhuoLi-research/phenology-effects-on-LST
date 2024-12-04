library(terra)
library(raster)
library(ggplot2)
library(dplyr)

setwd("D:/VegetationImpact")

####### 0 The data source is the difference file from the merged Northern Hemisphere data after running code 003 EA+NA_Results

################   01 9-Year SOS Time Series Line Plot   #########################

process_data <- function(folder_path, group_name) {
  # Set working directory
  setwd(folder_path)
  
  # Get the list of files
  file_list <- list.files(pattern = "average_diff_1_\\d{4}\\.tif")  ###Modified
  
  # Extract the year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read the file for the current year
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate the mean value (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate the standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a data frame
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}


# Call the function to process data for different regions
data_EA_1 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_1 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_1 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_1", "Northern Hemisphere")   

data_EA_1$mean_value <-  as.numeric(data_EA_1$mean_value)      
data_NA_1$mean_value <-  as.numeric(data_NA_1$mean_value)      
data_NH_1$mean_value <-  as.numeric(data_NH_1$mean_value)      
### Modified
# Combine the data
final_df_1 <- rbind(data_EA_1, data_NA_1, data_NH_1)
final_df_1$group <- factor(final_df_1$group, levels = c("Eurasia", "North America", "Northern Hemisphere")) 
# Set the order of the factor levels (for plotting)

# Calculate trend line data
trend_data <- data_NH_1 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])

# Create trend line equation labels
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   ### This is a negative value
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")

# Plot the line chart
p1 <- ggplot(final_df_1, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase the line thickness #1.5
  geom_point(size = 7) +  # Add points and set their size #5.5
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                #0.2 1.5
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_1$year), max(final_df_1$year), by = 2)) +  # Set x-axis breaks to integers
  scale_color_manual(
    values = c('#0886CC','#DA4E33',"#333333"),
    labels = c( "Eurasia", "North America", "Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  geom_smooth(data = subset(final_df_1, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.2) +
  theme(legend.position = "none",
        # legend.position = c(0.8, 0.85),
        axis.text.x  = element_text(size = 40),
        axis.text.y  = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show tick width for y-axis
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)), # Adjust the distance between x-axis title and x-axis (set to 20)
        # legend.margin = margin(20, 20, 20, 20),  # Adjust the margin of the legend
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust line spacing),    # Legend line length
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))  # Set y-axis breaks and limits

p1

ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-1.tiff",
       plot = p1, width = 15, height = 13, units = "in", dpi = 300)


################   02 9年MGP时间序列折线图   #########################

################   02 9-Year MGP Time Series Line Plot   #########################

process_data <- function(folder_path, group_name) {
  # Set working directory
  setwd(folder_path)
  
  # Get list of files
  file_list <- list.files(pattern = "average_diff_2_\\d{4}\\.tif")  #### Make sure to modify
  
  # Extract year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read the file for each year
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Compute mean (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Compute standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a dataframe
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}
data_EA_2 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_2 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_2 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_2", "Northern Hemisphere")   


data_EA_2$mean_value <-  as.numeric(data_EA_2$mean_value)
data_NA_2$mean_value <-  as.numeric(data_NA_2$mean_value)
data_NH_2$mean_value <-  as.numeric(data_NH_2$mean_value)     

# Merge data
final_df_2 <- rbind(data_EA_2, data_NA_2, data_NH_2)
final_df_2$group <- factor(final_df_2$group, levels = c("Eurasia", "North America", "Northern Hemisphere")) 
# Set the order of levels for plotting

# Calculate trend line data
trend_data <- data_NH_2 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])
# Create trend line equation labels
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   ### Here is the negative value
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")
# Plot line chart
p2 <- ggplot(final_df_2, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase the line thickness #1.5
  geom_point(size = 7) +  # Add points and set size #5.5
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                # 0.2 1.5
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_2$year), max(final_df_2$year), by = 2)) +  # Set x-axis ticks as integers
  scale_color_manual(
    values = c('#0886CC', '#DA4E33', "#333333"),
    labels = c("Eurasia", "North America", "Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  geom_smooth(data = subset(final_df_2, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.2) +
  theme(legend.position = "none",
        # legend.position = c(0.8, 0.85),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show axis tick width
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)), # Adjust the distance between x-axis title and x-axis (e.g., set to 20)
        # legend.margin = margin(20, 20, 20, 20),  # Adjust the outer margin of the legend
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust line spacing),    # Legend line length
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))  # Set limits in scale_y_continuous
p2

ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-2.tiff",
       plot = p2, width = 15, height = 13, units = "in", dpi = 300)



################   03 9-year GMO Time Series Line Chart   #########################

process_data <- function(folder_path, group_name) {
  # Set the working directory
  setwd(folder_path)
  
  # Get the list of files
  file_list <- list.files(pattern = "average_diff_3_\\d{4}\\.tif")  #### Please modify
  
  # Get the year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read the file for each year
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate the mean (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate the standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a data frame
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}

data_EA_3 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_3 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_3 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_3", "Northern Hemisphere")   

data_EA_3$mean_value <-  as.numeric(data_EA_3$mean_value)      
data_NA_3$mean_value <-  as.numeric(data_NA_3$mean_value)      
data_NH_3$mean_value <-  as.numeric(data_NH_3$mean_value)      

# Combine the data
final_df_3 <- rbind(data_EA_3, data_NA_3, data_NH_3)
final_df_3$group <- factor(final_df_3$group, levels = c("Eurasia", "North America", "Northern Hemisphere")) 
# Set the order of the levels (for plotting order)

# Calculate trend line data
trend_data <- data_NH_3 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])

# Create trend line equation label
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   ### This is a negative value
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")

# Plot the line chart
p3 <- ggplot(final_df_3, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase the line thickness #1.5
  geom_point(size = 7) +  # Add points and set their size #5.5
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                #0.2 1.5
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_3$year), max(final_df_3$year), by = 2)) +  # Set x-axis ticks as integers
  scale_color_manual(
    values = c('#0886CC', '#DA4E33', "#333333"),
    labels = c("Eurasia", "North America", "Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  geom_smooth(data = subset(final_df_3, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.2) +
  theme(legend.position = "none",
        # legend.position = c(0.8, 0.85),
        axis.text.x  = element_text(size = 40),
        axis.text.y  = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust the axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show the axis tick width
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)), # Adjust the x-axis title distance (e.g., set it to 20)
        # legend.margin = margin(20, 20, 20, 20),  # Adjust the legend's outer margin
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust line spacing),    # Legend line length
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))  # Set limits in scale_y_continuous
p3

ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-3.tiff",
       plot = p3,  width = 15,  height = 13,  units = "in",  dpi = 300)



################   04 9-year GDO Time Series Line Plot   #########################

process_data <- function(folder_path, group_name) {
  # Set working directory
  setwd(folder_path)
  
  # Get the list of files
  file_list <- list.files(pattern = "average_diff_4_\\d{4}\\.tif")  #### Please modify if needed
  
  # Extract the year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read the file for each year
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate the mean value (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate the standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a data frame
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}

data_EA_4 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_4 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_4 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_4", "Northern Hemisphere")   

data_EA_4$mean_value <-  as.numeric(data_EA_4$mean_value)    
data_NA_4$mean_value <-  as.numeric(data_NA_4$mean_value)      
data_NH_4$mean_value <-  as.numeric(data_NH_4$mean_value)     

# Combine the data
final_df_4 <- rbind(data_EA_4, data_NA_4, data_NH_4)
final_df_4$group <- factor(final_df_4$group, levels = c("Eurasia", "North America", "Northern Hemisphere")) 
# Set the order of the groups for plotting

# Calculate the trend line data
trend_data <- data_NH_4 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])

# Create the equation labels for the trend line
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")

# Plot the line graph
p4 <- ggplot(final_df_4, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase the line thickness (1.5)
  geom_point(size = 7) +  # Add points with size 7 (5.5)
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                # Error bar size (0.2, 1.5)
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_4$year), max(final_df_4$year), by = 2)) +  # Set x-axis ticks to integers
  scale_color_manual(
    values = c('#0886CC','#DA4E33', "#333333"),
    labels = c("Eurasia", "North America", "Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  geom_smooth(data = subset(final_df_4, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trend line for Northern Hemisphere
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  # theme(legend.position = "left") +
  coord_fixed(ratio = 1 / 3.2) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust the axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show axis tick width
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)),
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))  # Set the limits in scale_y_continuous
p4

# Save the plot to a file
ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-4.tiff",
       plot = p4, width = 15, height = 13, units = "in", dpi = 300)



################   05 9-year MSP Time Series Line Plot   #########################

process_data <- function(folder_path, group_name) {
  # Set working directory
  setwd(folder_path)
  
  # Get the file list
  file_list <- list.files(pattern = "average_diff_5_\\d{4}\\.tif")  #### Note: modify if necessary
  
  # Extract year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read each year's file
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate mean value (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create data frame
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}
data_EA_5 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_5 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_5 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_5", "Northern Hemisphere")   

data_EA_5$mean_value <-  as.numeric(data_EA_5$mean_value)      
data_NA_5$mean_value <-  as.numeric(data_NA_5$mean_value)      
data_NH_5$mean_value <-  as.numeric(data_NH_5$mean_value)      

# Combine data
final_df_5 <- rbind(data_EA_5, data_NA_5, data_NH_5)
final_df_5$group <- factor(final_df_5$group, levels = c("Eurasia", "North America", "Northern Hemisphere")) 
# Set the factor levels for plotting order

# Calculate trend line data
trend_data <- data_NH_5 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])

# Create trend line equation label
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   ### This is a negative value
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")

# Plot the line graph
p5 <- ggplot(final_df_5, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase the line thickness #1.5
  geom_point(size = 7) +  # Add points and set size #5.5
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                #0.2 1.5
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_5$year), max(final_df_5$year), by = 2)) +  # Set x-axis breaks as integers
  scale_color_manual(
    values = c('#0886CC', '#DA4E33', "#333333"),
    labels = c("Eurasia", "North America", "Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  geom_smooth(data = subset(final_df_5, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  coord_fixed(ratio = 1/3.2) +
  theme(legend.position = "none",
        # legend.position = c(0.8, 0.85),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show tick width
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust distance between x-axis title and axis
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust row spacing),    # Legend line length
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))  # Set limits in scale_y_continuous
p5

# Save the plot
ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-5.tiff",
       plot = p5, width = 15, height = 13, units = "in", dpi = 300)


################   06 9-year EOS time series line plot   #########################

process_data <- function(folder_path, group_name) {
  # Set the working directory
  setwd(folder_path)
  
  # Get the list of files
  file_list <- list.files(pattern = "average_diff_6_\\d{4}\\.tif")  ####Note to modify
  
  # Extract year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, 
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read each year's file
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate the mean (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate the standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a data frame
  data <- data.frame(year = years, mean_value = mean_values, 
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}
data_EA_6 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_6 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_6 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_6", "Northern Hemisphere")   

data_EA_6$mean_value <-  as.numeric(data_EA_6$mean_value)      
data_NA_6$mean_value <-  as.numeric(data_NA_6$mean_value)      
data_NH_6$mean_value <-  as.numeric(data_NH_6$mean_value)      

# Merge the data
final_df_6 <- rbind(data_EA_6, data_NA_6,data_NH_6)
final_df_6$group <- factor(final_df_6$group, levels = c("Eurasia", "North America","Northern Hemisphere")) 
# Set the order of levels for plotting

# Calculate trend line data
trend_data <- data_NH_6 %>%
  group_by(group) %>%
  summarize(intercept = coef(lm(mean_value ~ year))[1],
            slope = coef(lm(mean_value ~ year))[2],
            rsquared = summary(lm(mean_value ~ year))$r.squared,
            p_value = coef(summary(lm(mean_value ~ year)))[2, 4])
# Create the trendline equation label
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2),   ### This is a negative value
                          ifelse(trend_data$slope >= 0, " + ", " - "), 
                          abs(round(trend_data$slope, 2)), " * x", sep = "")
# Plot the line graph
p6 <- ggplot(final_df_6, aes(x = year, y = mean_value, group = group, color = group)) +
  geom_line(size = 2) +    # Increase line thickness #1.5
  geom_point(size = 7) +  # Add points and set size #5.5
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                # 0.2 1.5
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_6$year), max(final_df_6$year), by = 2)) +  # Set x-axis scale to integers
  scale_color_manual(
    values = c('#0886CC','#DA4E33',"#333333"),
    labels = c("Eurasia", "North America","Northern Hemisphere")) +
  # geom_smooth(method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trendline
  geom_smooth(data = subset(final_df_6, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  # Add trendline
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.2) +
  theme(legend.position = "none",
        # legend.position = c(0.8, 0.85),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 42),
        axis.line = element_line(size = 2),  # Adjust axis line thickness to 2
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.y = element_line(size = 2),  # Show axis tick width
        axis.title = element_text(size = 45, margin = margin(t = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 37),
        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis title margin (e.g., set to 20)
        # legend.margin = margin(20, 20, 20, 20),  # Adjust legend margin
        panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust line spacing),    # Set legend line length
         fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks = c(-15,-10,-5,0,5), limits = c(-15, 7))  # Set y-axis limits in scale_y_continuous
p6

ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-6.tiff",
       plot = p6, width = 15, height = 13, units = "in", dpi = 300)



################   07 legend-9-year time series line plot   #########################

process_data <- function(folder_path, group_name) {
  # Set the working directory
  setwd(folder_path)
  
  # Get the list of files
  file_list <- list.files(pattern = "average_diff_6_\\d{4}\\.tif")  #### Note to modify
  
  # Get the year information
  years <- unique(as.integer(substr(file_list, nchar(file_list) - 7,
                                    nchar(file_list) - 4)))
  
  # Initialize vectors to store the results
  mean_values <- numeric(length(years))
  std_errors <- numeric(length(years))
  
  # Loop through each year's file
  for (i in 1:length(years)) {
    # Read each year's file
    file <- rast(file_list[i])
    # Extract non-NA values
    non_na_values <- values(file)[!is.na(values(file))]
    # Calculate the mean value (ignoring NA values)
    mean_values[i] <- mean(non_na_values)
    # Calculate the standard deviation
    std_errors[i] <- sd(non_na_values)
  }
  
  # Create a data frame
  data <- data.frame(year = years, mean_value = mean_values,
                     std_error = std_errors,
                     std_error_cal = 0.15 * std_errors,
                     group = group_name)
  
  return(data)
}

data_EA_1 <- process_data("D:/VegetationImpact/EA_Results/0.diff_result/0common_pixel", "Eurasia")
data_NA_1 <- process_data("D:/VegetationImpact/NA_Results/0.diff_result/0common_pixel", "North America")
data_NH_1 <- process_data("D:/VegetationImpact/EA+NA_Results/merge_average_diff_years/merged_average_diff_1", "Northern Hemisphere")   

data_EA_6$mean_value <-  as.numeric(data_EA_6$mean_value)      
data_NA_6$mean_value <-  as.numeric(data_NA_6$mean_value)      
data_NH_6$mean_value <-  as.numeric(data_NH_6$mean_value)      

# Combine data
final_df_6 <- rbind(data_EA_6, data_NA_6, data_NH_6)
final_df_6$group <- factor(final_df_5$group, levels = c("Eurasia", "North America", "Northern Hemisphere"))
# Set the order of the groups for plotting

# Plot the line chart
p6 <- ggplot(final_df_6, aes(x = year, y = mean_value, group = group, color = factor(group, levels = c("Northern Hemisphere", "North America", "Eurasia")))) +
  geom_line(size = 2) +    
  geom_point(size = 7) +  
  geom_errorbar(aes(ymin = mean_value - std_error_cal, 
                    ymax = mean_value + std_error_cal), 
                width = 0.3, size = 3) +                
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size = 1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_df_6$year), max(final_df_6$year), by = 2)) +  
  scale_color_manual(
    values = c("#333333", "#DA4E33", "#0886CC"),  # Black, Red, Blue order
    labels = c("Northern Hemisphere", "North America", "Eurasia")
  ) +
  geom_smooth(data = subset(final_df_6, group == "Northern Hemisphere"), 
              method = "lm", se = FALSE, color = "#000000", size = 2, linetype = "dashed") +  
  geom_text(aes(label = equation_label), x = 2018.5, y = 6, size = 15, color = "#000000") +
  geom_text(aes(label = regression_label), x = 2014.5, y = 6, size = 15, color = "#000000") +
  coord_fixed(ratio = 1/3.2) +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, "cm"),  # Set the spacing between legend items
    axis.text.x = element_text(size = 40),
    axis.text.y = element_text(size = 42),
    axis.line = element_line(size = 2),  
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_blank(),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)),  
    panel.grid = element_line(linetype = "blank")
  ) +
  guides(
    color = guide_legend(nrow = 1, keywidth = 4, label.theme = element_text(size = 37, margin = margin(b = 10))) 
  ) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5), limits = c(-15, 7))

print(p6)

p6

ggsave(filename = "D:/VegetationImpact/0.figure/E_Fig.1-diff&years-commonlegend.tiff",
       plot = p6,  width = 15,  height = 13,  units = "in",  dpi = 300)
