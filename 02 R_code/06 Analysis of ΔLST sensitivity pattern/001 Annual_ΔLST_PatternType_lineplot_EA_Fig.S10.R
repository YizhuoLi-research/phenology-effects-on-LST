library(terra)
library(raster)
library(ggplot2)
library(dplyr)         #### This type is variable

setwd("D:/VegetationImpact")

####### 0 Data source is from the 02 Europe-America diff_result raster data merged file
####### Note: Type is not variable: the first value of type is the annual average △LST, and the second value is k  
####### Both are values generated over 9 years, essentially matching with the type pattern of different phenological events

################   01 9-year time series line chart ---Eurasia #########################
# Modify for 6 groups: 1-SOS 2-MGP 3-GMO 4-GDO 5-MSP 6-EOS, need to modify and run 6 times

library("scales")


r01 <- rast("././EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")         ## Note modification
df01 <- as.data.frame(r01, xy = TRUE, na.rm = TRUE)  
colnames(df01) <- c("long", "lat","k_value") 
df01$k_value <- as.numeric(as.character(df01$k_value))          

r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   #NA                    ## Note modification
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  #EA                    ## Note modification

merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))  

df1$group <- "SOS"


# Extract common pixels
common_pixels <- intersect(paste(df01$long, df01$lat, sep="_"), paste(df1$long, df1$lat, sep="_"))

# Filter data based on common pixels
df01_common <- df01[paste(df01$long, df01$lat, sep="_") %in% common_pixels, ]
df1_common <- df1[paste(df1$long, df1$lat, sep="_") %in% common_pixels, ]

# Merge the two data frames by pixel
merged_df1 <- merge(df01_common, df1_common, by=c("long", "lat"))

# Create a new column for type
merged_df1$type <- ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value >= 0, "Type1",
                          ifelse(merged_df1$average_diff >= 0 & merged_df1$k_value < 0, "Type2",
                                 ifelse(merged_df1$average_diff < 0 & merged_df1$k_value >= 0, "Type3",
                                        ifelse(merged_df1$average_diff < 0 & merged_df1$k_value < 0, "Type4", NA))))





###  0000. Create △LST time series for four different regions  
###   EA  6 groups    ----Create △LST time series for four different regions   ##########

file_list <- list.files(path = "./EA_Results/0.diff_result/0common_pixel/",
                        pattern = "^average_diff_6_.*\\.tif$", full.names = TRUE)                          ## Note modification

# Create an empty list to store the results
results_list <- list()
years <- unique(as.integer(substr(file_list, nchar(file_list) - 7, nchar(file_list) - 4)))

for (i in 1:length(file_list)) {
  file <- raster::raster(file_list[i])
  
  # Convert to data frame
  file_df <- as.data.frame(file, xy = TRUE)
  names(file_df) <- c("long", "lat", "average_diff")
  merged_df1_clean <- merged_df1[complete.cases(merged_df1), ]
  file_df_clean <- file_df[complete.cases(file_df), ]
  
  # Compare operations
  common_pixels <- merge(file_df_clean, merged_df1_clean[, c("long", "lat", "type")], 
                         by = c("long", "lat"))
  common_pixels <- na.omit(common_pixels)
  
  # Group by type and calculate mean and standard error, and add year column
  result <- common_pixels %>%
    group_by(type) %>%
    summarize(year = years[i],
              average_diff_mean =  mean(average_diff),
              average_diff_std = sd(average_diff),
              average_diff_std_cal = 0.15 * sd(average_diff))
  
  # Store the result in the list
  results_list[[i]] <- result
}

final_result <- bind_rows(results_list)
final_result$type <- factor(final_result$type, levels = c("Type1", "Type2","Type3","Type4")) 
final_result$year <- as.numeric(final_result$year)  



p1 <- ggplot(final_result, aes(x = year, y = average_diff_mean, group = type, color = type)) +
  geom_line(size = 1.5) +                       # Increase line thickness
  geom_point(size = 5.5) +  # Add points and set size to 2
  geom_errorbar(aes(ymin = average_diff_mean - average_diff_std_cal, 
                    ymax = average_diff_mean + average_diff_std_cal), 
                width = 0.2, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", size=1) +
  labs(x = "Year", y = "ΔLST (℃)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(final_result$year), max(final_result$year), by = 2)) +  # Set x-axis ticks as integers
  scale_color_manual(   values = c( "#CC3333","#FF9933","#66CCFF", "#006699" ),
                        labels = c( "Warming-Positive","Warming-Negative","Cooling-Positive", "Cooling-Negative")) +
  # geom_smooth(data = subset(final_df_1, group == "Northern Hemisphere"), method = "lm", se = FALSE, color = "#000000", size = 1.5, linetype = "dashed") +  # Add trend line
  # geom_text(aes(label = equation_label), x = 2018, y = 14, size = 13, color = "#000000") +
  # geom_text(aes(label = regression_label), x = 2014.5, y = 14, size = 13, color = "#000000") +
  # # theme(legend.position = "left") +
  coord_fixed(ratio = 1/3.2) +
  theme( 
    legend.position = "none",
    # legend.position = c(0.8, 0.85),
    # legend.position = c(0.8, 0.2),
    axis.text.x  = element_text(size = 40),
    axis.text.y  = element_text(size = 42),
    axis.line = element_line(size = 2),  # Adjust coordinate axis line thickness to 2
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(0.3, "cm"),
    axis.ticks.y = element_line(size = 2),  # Display coordinate axis tick width
    axis.title = element_text(size = 45, margin = margin(t = 10)),
    legend.title = element_blank(),
    legend.text = element_text(size = 37),
    axis.title.x = element_text(margin = margin(t = 20)),# Adjust distance between x-axis title and x-axis (e.g., set to 20)
    legend.margin = margin(20, 20, 20, 20),  # Adjust legend margin
    panel.grid = element_line(linetype = "blank")) +
  guides(color = guide_legend(ncol = 1, keywidth = 2,
                              label.theme = element_text(size = 37, 
                                                         margin = margin(b = 18))),  # Adjust line spacing),    # Legend line length
         fill = guide_legend(byrow = TRUE)) +
  ylim(-15, 7)
p1

ggsave(filename = "./0.figure/Fig.S10-test-diff&years-EA_6.tiff",         ## Note modification
       plot = p1,  width = 15,  height = 13,  units = "in",  dpi = 300)
