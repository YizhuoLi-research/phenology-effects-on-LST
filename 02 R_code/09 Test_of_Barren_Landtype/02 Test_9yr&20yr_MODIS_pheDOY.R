library(terra)
library(ggplot2)
library(tidyverse)
library(dplyr)

setwd("D:/VegetationImpact/0.figure/Reponse_review")

############################### 1. MODIS 9yr VS 20yr SOS###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/1"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/1"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]


#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p1 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of SOS", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  # 添加 25% 和 75% 分位数的垂直线（虚线）
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(10, 160), breaks = c(30,60,90,120,150))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p1

# filename <- "./0.figure/Reponse_review"
# if (!dir.exists(filename)) {  dir.create(filename, recursive = TRUE)}
ggsave(
  filename = "./Fig.S-SOS.tiff",
  plot = p1,  width = 18,  height = 13,  units = "in",  dpi = 300)



############################### 2. MODIS 9yr VS 20yr MGP ###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/2"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/2"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]


#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p2 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of MGP", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  # 添加 25% 和 75% 分位数的垂直线（虚线）
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(70, 220), breaks = c(90,120,150,180,210))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p2

ggsave(
  filename = "./Fig.S-MGP.tiff",
  plot = p2,  width = 18,  height = 13,  units = "in",  dpi = 300)



############################### 3. MODIS 9yr VS 20yr GMO ###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/3"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/3"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]

#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p3 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of GMO", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  # 添加 25% 和 75% 分位数的垂直线（虚线）
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(100,260), breaks = c(120,150,180,210,240))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p3

ggsave(
  filename = "./Fig.S-GMO.tiff",
  plot = p3,  width = 18,  height = 13,  units = "in",  dpi = 300)



############################### 4. MODIS 9yr VS 20yr GMO ###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/4"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/4"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]

#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p4 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of GDO", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(130, 300), breaks = c(150,180,210,240,270))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p4

ggsave(
  filename = "./Fig.S-GDO.tiff",
  plot = p4,  width = 18,  height = 13,  units = "in",  dpi = 300)



############################### 5. MODIS 9yr VS 20yr MSP ###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/5"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/5"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]

#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p5 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of MSP", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  # 添加 25% 和 75% 分位数的垂直线（虚线）
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(180, 330), breaks = c(180,210,240,270,300))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p5

ggsave(
  filename = "./Fig.S-MSP.tiff",
  plot = p5,  width = 18,  height = 13,  units = "in",  dpi = 300)



############################### 6. MODIS 9yr VS 20yr EOS ###########################
####  01筛选预处理  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/6"

# 查看所有 .tif 影像文件
tif_files_MODIS <- list.files(path = folder_path_MODIS, pattern = "\\.tif$", full.names = TRUE)
processed_rasters <- list()

for (i in seq_along(tif_files_MODIS)) {
  raster_data <- rast(tif_files_MODIS[i])
  
  # 设置无效值为 NA（小于1或大于366）
  raster_data[raster_data < 1 | raster_data > 366] <- NA
  processed_rasters[[i]] <- raster_data
  
  # 构造新的文件名
  original_name <- basename(tif_files_MODIS[i])
  new_name <- sub("\\.tif$", "_filtered.tif", original_name)
  output_path <- file.path(folder_path_MODIS, new_name)
  
  # 写入新文件（不会覆盖原文件）
  writeRaster(raster_data, filename = output_path, overwrite = TRUE)
}


####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/6"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

# 计算 25% 和 75% 分位数
quantile_data <- doy_data %>%
  group_by(Period) %>%
  summarise(
    q25 = quantile(DOY, probs = 0.25, na.rm = TRUE),
    q75 = quantile(DOY, probs = 0.75, na.rm = TRUE)
  )

# 计算差值（9年 - 20年）
diff_q25 <- quantile_data$q25[quantile_data$Period == "9 years"] - quantile_data$q25[quantile_data$Period == "20 years"]
diff_q75 <- quantile_data$q75[quantile_data$Period == "9 years"] - quantile_data$q75[quantile_data$Period == "20 years"]

#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p6 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of EOS", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  # 添加 25% 和 75% 分位数的垂直线（虚线）
  geom_vline(data = quantile_data,aes(xintercept = q25, color = Period),
             linetype = "dashed",size = 1.5) +
  geom_vline(data = quantile_data,aes(xintercept = q75, color = Period),
             linetype = "dashed",size = 1.5) +
  annotate("text", x = Inf, y = Inf,
           label = bquote(atop(Delta[25~"%"] == .(round(diff_q25, 1)) ~ days,
                               Delta[75~"%"] == .(round(diff_q75, 1)) ~ days)),
           hjust = 1.1, vjust = 1.3, size = 19, color = "black", lineheight = 0.9) +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(210, 360), breaks = c(210,240,270,300,330))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p6

ggsave(
  filename = "./Fig.S-EOS.tiff",
  plot = p6,  width = 18,  height = 13,  units = "in",  dpi = 300)


############################### legend ###########################

####  02 重新读取  ####
folder_path_MODIS <- "./download_sample_MODIS_PHE/4"

# 仅查找以 "_filtered.tif" 结尾的文件
filtered_files <- list.files(path = folder_path_MODIS, pattern = "_filtered\\.tif$", full.names = TRUE)
# 筛选出2013-2021年的文件，存储为 phe_9yr
phe_9yr <- filtered_files[grepl("^20(13|14|15|16|17|18|19|20|21)", basename(filtered_files))]

# 筛选出2002-2021年的文件，存储为 phe_20yr
phe_20yr <- filtered_files[grepl("^20(02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21)", basename(filtered_files))]


#### 03 提取数据值 ####
# 读取数据并提取DOY值
extract_doy <- function(files) {
  doy_values <- numeric()
  for (file in files) {
    raster_data <- rast(file)
    doy_values <- c(doy_values, values(raster_data, na.rm = TRUE))
  }
  return(doy_values)
}

# 提取9年和20年数据的DOY值
doy_9yr <- extract_doy(phe_9yr)
doy_20yr <- extract_doy(phe_20yr)

# 创建数据框
doy_data <- data.frame(
  DOY = c(doy_9yr, doy_20yr),
  Period = rep(c("9 years", "20 years"), c(length(doy_9yr), length(doy_20yr)))
)

peak_doy <- doy_data %>%
  group_by(Period) %>%
  summarise(
    peak_doy = {
      dens <- density(DOY, na.rm = TRUE)
      dens$x[which.max(dens$y)]
    }
  )

# 计算差值
diff_value <- peak_doy$peak_doy[peak_doy$Period == "9 years"] - 
  peak_doy$peak_doy[peak_doy$Period == "20 years"]


#### 04 绘制频数分布图 ####


# 查看DOY的最大值和最小值
min_DOY <- min(doy_data$DOY, na.rm = TRUE)
max_DOY <- max(doy_data$DOY, na.rm = TRUE)


# 然后在ggplot中添加annotate
p4 <- ggplot(doy_data, aes(x = DOY, fill = Period)) +
  geom_density(size = 1.2, alpha = 0.3,adjust = 1.4) +  #,adjust = 1.5调整曲线平滑
  labs(x = "DOY of GDO", y = "Density") +
  theme_bw() +
  scale_fill_manual(values = c("9 years" = "blue", "20 years" = "red")) +
  geom_vline(data = peak_doy,
             aes(xintercept = peak_doy, color = Period),
             linetype = "dashed", size = 1.5) +
  annotate("text", x = Inf, y = Inf, 
           label = sprintf("Δ = %.1f day", diff_value),
           hjust = 1.2, vjust = 1.6, size = 19, color = "black") +
  # 其他theme设置保持不变
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(size = 2.0),
        axis.text.y = element_text(size = 60, color = "black"),
        axis.text.x = element_text(size = 60, color = "black"),
        axis.ticks.y = element_line(size = 2),
        axis.ticks.x = element_line(size = 2),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.title.y = element_text(size = 60,margin = margin(r = 22, unit = "pt") ),
        axis.title.x = element_text(size = 60,margin = margin(t = 18, unit = "pt") ),
        plot.background = element_rect(size = 100),
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "right",
        legend.title =element_blank(),# element_text(size = 17,vjust = 1),  # 图例标题 高度居中
        legend.text = element_text(size = 60), 
        legend.key.height = unit(1, "cm"),  # 图例色块/符号的高度
        legend.key.width = unit(2, "cm"),  # 加宽色块（默认通常为0.5-1cm）
        legend.spacing.y = unit(3, "cm"),   # 图例项间的垂直间距
        panel.grid = element_line(linetype = "blank")) +
  scale_x_continuous(limits = c(130, 300), breaks = c(150,180,210,240,270))+
  scale_y_continuous(limits = c(0.00, 0.10), breaks = c(0.00,0.02,0.04,0.06,0.08,0.10))

p4

ggsave(
  filename = "./Fig.S-legend.tiff",
  plot = p4,  width = 18,  height = 13,  units = "in",  dpi = 300)

