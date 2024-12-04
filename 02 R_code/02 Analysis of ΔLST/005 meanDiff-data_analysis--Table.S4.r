##########################     0. Load Packages and Data      ############################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library("scales")

setwd("D:/VegetationImpact")


############################### 1-1 SOS  ##################################

r1 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_1.tif")   #NA
r11 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_1.tif")  #EA

merged_diff_raster_1 <- merge(r1, r11)
df1 <- as.data.frame(merged_diff_raster_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))
df1$group <- "SOS"

#Analysis for NH_LST_diff
avg_NH <- round(mean(df1$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df1$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df1$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df1$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #-0.99
cat("0.15std_NH:", std_NH, "\n")  #-0.54
cat("min_NH:", min_NH, "\n")  #-23.50
cat("max_NH:", max_NH, "\n")  #16.92

#Analysis for NA_LST_diff
df1_NA <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df1_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df1_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df1_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df1_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #0.46
cat("0.15std_NA:", std_NA, "\n")  #0.37
cat("min_NA:", min_NA, "\n")  #-18.9 
cat("max_NA:", max_NA, "\n")  # 16.92

#Analysis for EA_LST_diff
df1_EA <- as.data.frame(r11, xy = TRUE, na.rm = TRUE)  
colnames(df1_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df1_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df1_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df1_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df1_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  # -1.48 
cat("0.15std_EA:", std_EA, "\n")  #0.57
cat("min_ EA:", min_EA, "\n")  #-23.5
cat("max_EA:", max_EA, "\n")  #15.91 


############################### 2-1 MGP  ##################################


r2 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_2.tif")   #NA
r22 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_2.tif")  #EA
merged_diff_raster_2 <- merge(r2, r22)
df2 <- as.data.frame(merged_diff_raster_2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","average_diff") 
df2$average_diff <- as.numeric(as.character(df2$average_diff))
df2$group <- "MGP"
summary(df2$average_diff)

#Analysis for NH_LST_diff
avg_NH <- round(mean(df2$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df2$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df2$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df2$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #-3.58
cat("0.15std_NH:", std_NH, "\n")  #0.62
cat("min_NH:", min_NH, "\n")      #18.59
cat("max_NH:", max_NH, "\n")      #-26.29

#Analysis for NA_LST_diff
df2_NA <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df2_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df2_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df2_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df2_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #2.87
cat("0.15std_NA:", std_NA, "\n")  #0.58
cat("min_NA:", min_NA, "\n")      #-16.34
cat("max_NA:", max_NA, "\n")      #26.49

#Analysis for EA_LST_diff
df2_EA <- as.data.frame(r22, xy = TRUE, na.rm = TRUE)  
colnames(df2_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df2_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df2_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df2_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df2_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #3.82
cat("0.15std_EA:", std_EA, "\n")  #0.63
cat("min_ EA:", min_EA, "\n")     #-18.59
cat("max_EA:", max_EA, "\n")      #26.40


############################### 3-1 GMO  ##################################

r3 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_3.tif")   #NA
r33 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_3.tif")  #EA
merged_diff_raster_3 <- merge(r3, r33)
df3 <- as.data.frame(merged_diff_raster_3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat","average_diff") 
df3$average_diff <- as.numeric(as.character(df3$average_diff))
df3$group <- "GMO"
summary(df3$average_diff)

#Analysis for NH_LST_diff
avg_NH <- round(mean(df3$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df3$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df3$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df3$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #6.06
cat("0.15std_NH:", std_NH, "\n")  #0.78
cat("min_NH:", min_NH, "\n")      #-41.72
cat("max_NH:", max_NH, "\n")      #18.85

#Analysis for NA_LST_diff
df3_NA <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df3_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df3_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df3_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df3_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #-5.65 
cat("0.15std_NA:", std_NA, "\n")  #0.77
cat("min_NA:", min_NA, "\n")      #-35.6
cat("max_NA:", max_NA, "\n")      #15.4

#Analysis for EA_LST_diff
df3_EA <- as.data.frame(r33, xy = TRUE, na.rm = TRUE)  
colnames(df3_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df3_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df3_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df3_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df3_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #-6.19
cat("0.15std_EA:", std_EA, "\n")  #0.78
cat("min_ EA:", min_EA, "\n")     #-41.72
cat("max_EA:", max_EA, "\n")      #18.85


############################### 4-1 GDO  ##################################

r4 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_4.tif")   #NA
r44 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_4.tif")  #EA
merged_diff_raster_4 <- merge(r4, r44)
df4 <- as.data.frame(merged_diff_raster_4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","average_diff") 
df4$average_diff <- as.numeric(as.character(df4$average_diff))
df4$group <- "GDO"
summary(df4$average_diff)

#Analysis for NH_LST_diff
avg_NH <- round(mean(df4$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df4$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df4$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df4$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #-5.44
cat("0.15std_NH:", std_NH, "\n")  #0.78
cat("min_NH:", min_NH, "\n")      #-47.32
cat("max_NH:", max_NH, "\n")      #19.59

#Analysis for NA_LST_diff
df4_NA <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df4_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df4_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df4_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df4_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #-5.78 
cat("0.15std_NA:", std_NA, "\n")  #0.90
cat("min_NA:", min_NA, "\n")      #-47.32
cat("max_NA:", max_NA, "\n")      #19.04

#Analysis for EA_LST_diff
df4_EA <- as.data.frame(r44, xy = TRUE, na.rm = TRUE)  
colnames(df4_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df4_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df4_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df4_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df4_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #-5.32
cat("0.15std_EA:", std_EA, "\n")  #0.74
cat("min_ EA:", min_EA, "\n")     #-45.99
cat("max_EA:", max_EA, "\n")      #19.59


############################### 5-1 MSP  ##################################

r5 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_5.tif")   #NA
r55 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_5.tif")  #EA
merged_diff_raster_5 <- merge(r5, r55)
df5 <- as.data.frame(merged_diff_raster_5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","average_diff") 
df5$average_diff <- as.numeric(as.character(df5$average_diff))
df5$group <- "MSP"
summary(df5$average_diff)

#Analysis for NH_LST_diff
avg_NH <- round(mean(df5$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df5$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df5$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df5$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #-2.63
cat("0.15std_NH:", std_NH, "\n")  #0.48
cat("min_NH:", min_NH, "\n")      #-28.84
cat("max_NH:", max_NH, "\n")      #19.11

#Analysis for NA_LST_diff
df5_NA <- as.data.frame(r5, xy = TRUE, na.rm = TRUE)  
colnames(df5_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df5_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df5_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df5_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df5_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #-1.88 
cat("0.15std_NA:", std_NA, "\n")  #0.45
cat("min_NA:", min_NA, "\n")      #-28.84
cat("max_NA:", max_NA, "\n")      #19.11

#Analysis for EA_LST_diff
df5_EA <- as.data.frame(r55, xy = TRUE, na.rm = TRUE)  
colnames(df5_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df5_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df5_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df5_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df5_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #-2.88
cat("0.15std_EA:", std_EA, "\n")  #0.48
cat("min_ EA:", min_EA, "\n")     #-27.81
cat("max_EA:", max_EA, "\n")      #12.77



############################### 6-1 EOS  ##################################

r6 <- raster("./NA_Results/0.diff_result/NA_DIFF_9yearAverge/average_diff_6.tif")   #NA
r66 <- raster("./EA_Results/0.diff_result/EA_DIFF_9yearAverge/average_diff_6.tif")  #EA
merged_diff_raster_6 <- merge(r6, r66)
df6 <- as.data.frame(merged_diff_raster_6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat","average_diff") 
df6$average_diff <- as.numeric(as.character(df6$average_diff))
df6$group <- "MSP"
summary(df6$average_diff)

#Analysis for NH_LST_diff
avg_NH <- round(mean(df6$average_diff, na.rm = TRUE),2)
std_NH <- round(0.15*sd(df6$average_diff, na.rm = TRUE),2)
max_NH <- round(max(df6$average_diff, na.rm = TRUE),2)
min_NH <- round(min(df6$average_diff, na.rm = TRUE),2)
cat("average_NH:", avg_NH, "\n")  #-0.17
cat("0.15std_NH:", std_NH, "\n")  #0.36
cat("min_NH:", min_NH, "\n")      #-23.33
cat("max_NH:", max_NH, "\n")      #13.51

#Analysis for NA_LST_diff
df6_NA <- as.data.frame(r6, xy = TRUE, na.rm = TRUE)  
colnames(df6_NA) <- c("long", "lat","average_diff") 
avg_NA <- round(mean(df6_NA$average_diff, na.rm = TRUE),2)
std_NA <- round(0.15*sd(df6_NA$average_diff, na.rm = TRUE),2)
max_NA <- round(max(df6_NA$average_diff, na.rm = TRUE),2)
min_NA <- round(min(df6_NA$average_diff, na.rm = TRUE),2)
cat("average_NA:", avg_NA, "\n")  #0.24 
cat("0.15std_NA:", std_NA, "\n")  #0.28
cat("min_NA:", min_NA, "\n")      #-23.33
cat("max_NA:", max_NA, "\n")      #13.51

#Analysis for EA_LST_diff
df6_EA <- as.data.frame(r66, xy = TRUE, na.rm = TRUE)  
colnames(df6_EA) <- c("long", "lat","average_diff") 
avg_EA <- round(mean(df6_EA$average_diff, na.rm = TRUE),2)
std_EA <- round(0.15*sd(df6_EA$average_diff, na.rm = TRUE),2)
max_EA <- round(max(df6_EA$average_diff, na.rm = TRUE),2)
min_EA <- round(min(df6_EA$average_diff, na.rm = TRUE),2)
cat("average_EA:", avg_EA, "\n")  #-0.31
cat("0.15std_EA:", std_EA, "\n")  #0.38
cat("min_ EA:", min_EA, "\n")     #-20.57
cat("max_EA:", max_EA, "\n")      #10.8

