# This code calculates the annual mean temperature for North America and visualizes the interannual variation along with a trend line.
###### 0. Load required packages ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

################################   01  Calculate Annual Mean Temperature - North America  ###########################
###############################################################################################         
# List all the .tif files in the directory
file_list  <- list.files("./NA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$",
                         full.names = TRUE)

# Initialize a vector to store the annual average temperatures
average_temperatures <- numeric(length(file_list))

# Loop through each .tif file and calculate the average temperature
for (i in 1:length(file_list)) {
  
  # Read the .tif file
  temperature_data <- raster::raster(file_list[i])
  
  # Calculate the mean temperature value
  average_temperature <- mean(temperature_data[], na.rm = TRUE)
  # Store the average temperature in the vector
  average_temperatures[i] <- average_temperature
}

# Print the annual mean temperature for each year
for (i in 1:length(file_list)) {
  year <- gsub("_actLSTmean.tif", "", basename(file_list[i]))
  cat(year, "annual mean temperature: ", average_temperatures[i], "\n")
}



library(ggplot2)

# Create a data frame
data_NA <- data.frame(Year = gsub("_actLSTmean.tif", "", basename(file_list)),
                      Average_Temperature = average_temperatures)

# Convert Year to numeric type
data_NA$Year <- as.numeric(data_NA$Year)


# Calculate the trend line data
trend_data <- data_NA %>%
  summarize(intercept = coef(lm(Average_Temperature ~ Year))[1],
            slope = coef(lm(Average_Temperature ~ Year))[2],
            rsquared = summary(lm(Average_Temperature ~ Year))$r.squared,
            p_value = coef(summary(lm(Average_Temperature ~ Year)))[2, 4])
# Create the trend line equation label
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2), " + ", round(trend_data$slope, 2), " * x", sep = "")


# Create the line plot showing the interannual variation
p1 <- ggplot(data_NA, aes(x = Year, y = Average_Temperature)) +
  geom_line(size = 2, color = "#DA4E33") +     # Increase line thickness
  geom_point(size = 7,color = "#DA4E33") +    # Add points with size 7
  geom_errorbar(aes(ymin = Average_Temperature - 0.15 * Average_Temperature, 
                    ymax = Average_Temperature + 0.15 * Average_Temperature),
                width = 0.2, size = 1.5, color = "#DA4E33") +  # Add error bars
  labs(x = "Year",
       y = "Annual mean temperature (℃)") +  # Label y-axis
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021)) +  # Set x-axis breaks for years
  coord_fixed(ratio = 1/1.2) +  # Set fixed aspect ratio
  theme_minimal() +  # Apply minimal theme
  geom_smooth(data = data_NA, method = "lm", se = FALSE, color = "#DA4E33", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018, y = 14.3, size = 14, color = "#000000") +  # Display R² and p-value on plot
  geom_text(aes(label = regression_label), x = 2014.5, y = 14.3, size = 14, color = "#000000") +  # Display regression equation
  theme( legend.position = "none",  # Hide legend
         axis.text.x  = element_text(size = 38),  # Set x-axis text size
         axis.text.y  = element_text(size = 40),  # Set y-axis text size
         axis.line = element_line(size = 2),  # Set axis line thickness
         axis.ticks = element_line(size = 2),  # Set axis ticks thickness
         axis.ticks.length = unit(0.3, "cm"),  # Set axis tick length
         axis.ticks.y = element_line(size = 2),  # Set y-axis ticks width
         axis.title = element_text(size = 45, margin = margin(t = 10)),  # Set axis title size and margin
         axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis title margin
         panel.grid = element_line(linetype = "blank")) +  # Remove grid lines
  guides(color = guide_legend(ncol = 1, keywidth = 2),    # Set legend line length
         fill = guide_legend(byrow = TRUE)) +  # Adjust legend fill direction
  ylim(8,15)  # Set y-axis limits

# Display the plot
print(p1)

# Save the plot as a high-resolution TIFF file
ggsave(filename = "./0.figure/FigS4.annual_mean_T_NA.tiff",
       plot = p1,  width = 15,  height = 12,  units = "in",  dpi = 300)  # Save plot as TIFF file



#####################################   02  Calculate Annual Mean Temperature - Eurasia  ###########################
###############################################################################################      
#### Eurasia
file_list  <- list.files("./EA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$",
                         full.names = TRUE)

# Initialize a vector to store the average temperature for each year
average_temperatures <- numeric(length(file_list))

# Loop through each .tif file and calculate the average temperature
for (i in 1:length(file_list)) {
  # Read the .tif file
  temperature_data <- raster::raster(file_list[i])
  # Calculate the average temperature
  average_temperature <- mean(temperature_data[], na.rm = TRUE)
  # Store the average temperature in the vector
  average_temperatures[i] <- average_temperature
}

# Print the average temperature for each year
for (i in 1:length(file_list)) {
  year <- gsub("_actLSTmean.tif", "", basename(file_list[i]))
  cat(year, "Annual Mean Temperature: ", average_temperatures[i], "\n")
}

library(ggplot2)

# Create a data frame
data_EA <- data.frame(Year = gsub("_actLSTmean.tif", "", basename(file_list)),
                      Average_Temperature = average_temperatures)

# Convert Year to numeric type
data_EA$Year <- as.numeric(data_EA$Year)

# Calculate trend line data
trend_data <- data_EA %>%
  summarize(intercept = coef(lm(Average_Temperature ~ Year))[1],
            slope = coef(lm(Average_Temperature ~ Year))[2],
            rsquared = summary(lm(Average_Temperature ~ Year))$r.squared,
            p_value = coef(summary(lm(Average_Temperature ~ Year)))[2, 4])

# Create equation label for the trend line
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2), " + ", round(trend_data$slope, 2), " * x", sep = "")

# Create a plot showing the annual variation of the temperature
p2 <- ggplot(data_EA, aes(x = Year, y = Average_Temperature)) +
  geom_line(size = 2, color = "#0886CC") +     # Increase the line width
  geom_point(size = 7, color = "#0886CC") +    # Add points and set their size to 7
  geom_errorbar(aes(ymin = Average_Temperature - 0.15 * Average_Temperature, 
                    ymax = Average_Temperature + 0.15 * Average_Temperature),
                width = 0.2, size = 1.5, color = "#0886CC") + # Add error bars with width and size
  labs(x = "Year",
       y = "Annual mean temperature (℃)") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021)) +  # Set the years to display on the x-axis
  coord_fixed(ratio = 1/1.2) + # Adjust the aspect ratio of the plot
  theme_minimal()+
  geom_smooth(data = data_EA, method = "lm", se = FALSE, color = "#0886CC", size = 2, linetype = "dashed") +  # Add trend line
  geom_text(aes(label = equation_label), x = 2018, y = 9.4, size = 14, color = "#000000") + # Add equation label
  geom_text(aes(label = regression_label), x = 2014.5, y = 9.4, size = 14, color = "#000000") + # Add regression line label
  theme( legend.position = "none",   # Hide the legend
         axis.text.x  = element_text(size = 38),
         axis.text.y  = element_text(size = 40),
         axis.line = element_line(size = 2),  # Adjust axis line width to 2
         axis.ticks = element_line(size = 2),
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # Display axis tick width
         axis.title = element_text(size = 45, margin = margin(t = 10)), # Adjust title spacing
         axis.title.x = element_text(margin = margin(t = 20)),  # Adjust the x-axis title margin
         panel.grid = element_line(linetype = "blank")) + # Remove grid lines
  guides(color = guide_legend(ncol = 1, keywidth = 2),    # Set legend line length
         fill = guide_legend(byrow = TRUE)) +
  ylim(3, 10)

# Display the plot
print(p2)

# Save the plot as a .tiff file
ggsave(filename = "./0.figure/FigS4.annual_mean_T_EA.tiff",
       plot = p2, width = 15, height = 12, units = "in", dpi = 300)
