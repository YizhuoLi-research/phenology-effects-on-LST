library(tidyverse)
library(raster)
library(ggplot2)
library(patchwork)
library(dplyr)
library("scales")
library(maps)
# library(ggspatial)
setwd("D:/VegetationImpact")


############################### 1-1 Greenup 极地图  ##################################
######## (a) Green-up


# max_value <- max(df5$average_diff, na.rm = TRUE)
# min_value <- min(df5$average_diff, na.rm = TRUE)
# cat("最大值:", max_value, "\n")
# cat("最小值:", min_value, "\n")

r1 <- raster("./0.figure/Reponse_review/download_Sample_MODIS_2012/Result_2012/average_diff_1_common.tif")   
df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)
df1 <- df1 %>%   filter(x >= -87.2, x <= -86.8)    #36,36.4,-87.2,-86.8 1004个非空像元
# 统计非NA像元数量
non_na_count <- sum(!is.na(df1[[3]]))  # 如果第三列是值列
cat("非空像元数量为：", non_na_count, "\n")

colnames(df1) <- c("long", "lat","average_diff") 
df1$average_diff <- as.numeric(as.character(df1$average_diff))    #################
summary(df1)

p1 <- ggplot(df1, aes(x = long, y = lat, fill = average_diff)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#003366", "#66CCFF", "#FFFFFF", "#FF9900", "#CC3333"),
    values = rescale(c(-10, -5, 0, 5, 10)),
    limits = c(-10, 10),
    na.value = "transparent",
    name = "ΔLST post-green-up (℃)",       #  将标题拆成竖排
    guide = guide_colorbar(
      direction = "vertical",
      title.position = "left",              # 标题在左
      title.hjust = 0.5,                    # 居中
      barwidth = 0.5,
      barheight = 17,
      label.theme = element_text(size = 14),
      title.theme = element_text(size = 14, angle = 90)    ) ) + # 旋转标题
  # coord_fixed(ratio = 4/6) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_line(),
    panel.border = element_rect(size = 1),
    axis.text = element_text(size = 14),
    legend.position = "right"   # 图例放右边
  ) +
  scale_x_continuous(breaks = seq(floor(min(df1$long)), ceiling(max(df1$long)), by = 0.2)) +
  scale_y_continuous(breaks = seq(floor(min(df1$lat)), ceiling(max(df1$lat)), by = 0.2))

p1

ggsave(
  filename = "./0.figure//Reponse_review/Fig.Case_post_greenup_1.tiff",
  plot = p1,  width = 16,  height = 10,  units = "cm",  dpi = 300)


###########################  1-2 频数分布直方图   ##################################

# df1$average_diff[df1$average_diff > 20] <- 20.5
# df1$average_diff[df1$average_diff < -20] <- -20.5
df1$group <- cut(df1$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df1$average_diff, na.rm = TRUE), 2))

df1$group <- cut(df1$average_diff, 
                 breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), 
                 include.lowest = TRUE)
total_count <- sum(!is.na(df1$average_diff))
positive_count <- sum(df1$group == "Positive", na.rm = TRUE)
negative_count <- sum(df1$group == "Negative", na.rm = TRUE)
positive_percent <- round(100 * positive_count / total_count, 1)
negative_percent <- round(100 * negative_count / total_count, 1)
cat("Positive percentage:", positive_percent, "%\n")
cat("Negative percentage:", negative_percent, "%\n")

p11 <- ggplot(df1, aes(x = df1$average_diff, fill = df1$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df1$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df1$average_diff, na.rm = TRUE), y = 300,
           label = label_text,
           color = "gray20", size = 6, hjust = -0.25, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 14,color = "gray20"),
        axis.text.x =  element_text(size = 14,color = "gray20"),
        axis.title = element_blank(),
        panel.border = element_rect( size = 1),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 350) +
  coord_fixed(ratio = 4/100)+
  scale_x_continuous(breaks = c(-10, -5, 0, 5, 10), 
                     labels = c("-10", "-5", "0", "5", "10"),   #"\u2264-20"
                     limits = c(-10, 10))
p11

ggsave(filename =  "./0.figure//Reponse_review/Fig.Case_post_greenup_11.tiff", 
       plot = p11,  width = 16,  height = 10,  units = "cm",  dpi = 300)


############################### 2-1 Dormancy 极地图  ##################################


r2 <- raster("./0.figure/Reponse_review/download_Sample_MODIS_2012/Result_2012/average_diff_6_common.tif")   
df2 <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)
df2 <- df2 %>%   filter(x >= -87.2, x <= -86.8)    #36,36.4,-87.2,-86.8 1004个非空像元
# 统计非NA像元数量
non_na_count <- sum(!is.na(df1[[3]]))  # 如果第三列是值列
cat("非空像元数量为：", non_na_count, "\n")

colnames(df2) <- c("long", "lat","average_diff") 
df2$average_diff <- as.numeric(as.character(df2$average_diff))    #################
summary(df2)

p2 <- ggplot(df2, aes(x = long, y = lat, fill = average_diff)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#003366", "#66CCFF", "#FFFFFF", "#FF9900", "#CC3333"),
    values = rescale(c(-10, -5, 0, 5, 10)),
    limits = c(-10, 10),
    na.value = "transparent",
    name = "ΔLST post-dormancy (℃)",       #  将标题拆成竖排
    guide = guide_colorbar(
      direction = "vertical",
      title.position = "left",              # 标题在左
      title.hjust = 0.5,                    # 居中
      barwidth = 0.5,
      barheight = 17,
      label.theme = element_text(size = 14),
      title.theme = element_text(size = 14, angle = 90)    ) ) + # 旋转标题
  # coord_fixed(ratio = 4/6) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_line(),
    panel.border = element_rect(size = 1),
    axis.text = element_text(size = 14),
    legend.position = "right"   # 图例放右边
  ) +
  scale_x_continuous(breaks = seq(floor(min(df1$long)), ceiling(max(df1$long)), by = 0.2)) +
  scale_y_continuous(breaks = seq(floor(min(df1$lat)), ceiling(max(df1$lat)), by = 0.2))

p2

ggsave(
  filename = "./0.figure//Reponse_review/Fig.Case_post_dormancy_1.tiff",
  plot = p2,  width = 16,  height = 10,  units = "cm",  dpi = 300)



###########################  2-2 频数分布直方图   ##################################


df2$group <- cut(df2$average_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df2$average_diff, na.rm = TRUE), 2))

# 计算百分比
df2$group <- cut(df2$average_diff, 
                 breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), 
                 include.lowest = TRUE)
total_count <- sum(!is.na(df2$average_diff))
positive_count <- sum(df2$group == "Positive", na.rm = TRUE)
negative_count <- sum(df2$group == "Negative", na.rm = TRUE)
positive_percent <- round(100 * positive_count / total_count, 1)
negative_percent <- round(100 * negative_count / total_count, 1)
cat("Positive percentage:", positive_percent, "%\n")
cat("Negative percentage:", negative_percent, "%\n")

p22 <- ggplot(df2, aes(x = df2$average_diff, fill = df2$group)) +
  geom_histogram(binwidth = 1, color = "black", center = 0.5, position = "identity") +
  geom_vline(aes(xintercept = median(df2$average_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df2$average_diff, na.rm = TRUE), y = 300,
           label = label_text,
           color = "gray20", size = 6, hjust = -0.25, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 14,color = "gray20"),
        axis.text.x =  element_text(size = 14,color = "gray20"),
        axis.title = element_blank(),
        panel.border = element_rect( size = 1),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 350) +
  coord_fixed(ratio = 4/100)+
  scale_x_continuous(breaks = c(-10, -5, 0, 5, 10), 
                     labels = c("-10", "-5", "0", "5", "10"),   #"\u2264-20"
                     limits = c(-10, 10))
p22

ggsave(filename =  "./0.figure//Reponse_review/Fig.Case_post_dormancy_11.tiff", 
       plot = p22,  width = 16,  height = 10,  units = "cm",  dpi = 300)


############################### 3-1 Cumulative 极地图  ##################################


r3 <- raster("./0.figure/Reponse_review/download_Sample_MODIS_2012/Result_2012/sum_diff_16_common.tif")   
df3 <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)
df3 <- df3 %>%   filter(x >= -87.2, x <= -86.8)    #36,36.4,-87.2,-86.8 1004个非空像元
# 统计非NA像元数量
non_na_count <- sum(!is.na(df1[[3]]))  # 如果第三列是值列
cat("非空像元数量为：", non_na_count, "\n")

colnames(df3) <- c("long", "lat","sum_diff") 
df3$sum_diff <- as.numeric(as.character(df3$sum_diff))    #################
summary(df3)

df3 <- df3 %>%
  mutate(sum_diff = ifelse(sum_diff < -3500, -3500, sum_diff))

p3 <- ggplot(df3, aes(x = long, y = lat, fill = sum_diff)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#000303","#003366","#3399CC","#66CCFF","#FFFFFF"),
    values = scales::rescale(c(-3500, -3000, -2000, -1000, 0)),
    limits = c(-3500, 0),
    breaks = c(-3000,-2000,-1000, 0),  # Specify the breaks for labels
    labels = c("-3000","-2000","-1000", "0"),
    # values = scales::rescale(c(-4000, -3000, -2000, -1000, 0)),
    # limits = c(-4000, 0),
    # breaks = c(-4000,-3000,-2000,-1000, 0),  # Specify the breaks for labels
    # labels = c("-4000","-3000","-2000","-1000", "0"), 
    na.value = "transparent",
    name = "Cumulative ΔLST (℃·day)",       #  将标题拆成竖排
    guide = guide_colorbar(
      direction = "vertical",
      title.position = "left",              # 标题在左
      title.hjust = 0.5,                    # 居中
      barwidth = 0.5,
      barheight = 17,
      label.theme = element_text(size = 14),
      title.theme = element_text(size = 14, angle = 90)    ) ) + # 旋转标题
  # coord_fixed(ratio = 4/6) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_line(),
    panel.border = element_rect(size = 1),
    axis.text = element_text(size = 16),
    legend.position = "right"   # 图例放右边
  ) +
  scale_x_continuous(breaks = seq(floor(min(df1$long)), ceiling(max(df1$long)), by = 0.2)) +
  scale_y_continuous(breaks = seq(floor(min(df1$lat)), ceiling(max(df1$lat)), by = 0.2))

p3

ggsave(
  filename = "./0.figure//Reponse_review/Fig.Case_Cumulative_1.tiff",
  plot = p3,  width = 18,  height = 11,  units = "cm",  dpi = 300)



###########################  1-2 频数分布直方图   ##################################

r3 <- raster("./0.figure/Reponse_review/download_Sample_MODIS_2012/Result_2012/sum_diff_16_common.tif")   
df3 <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)
df3 <- df3 %>%   filter(x >= -87.2, x <= -86.8)    #36,36.4,-87.2,-86.8 1004个非空像元
colnames(df3) <- c("long", "lat","sum_diff") 
df3$sum_diff <- as.numeric(as.character(df3$sum_diff))    #################

df3$group <- cut(df3$sum_diff, breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), include.lowest = TRUE)
label_text <- sprintf("%.2f", round(median(df3$sum_diff, na.rm = TRUE), 2))

# 计算百分比
df3$group <- cut(df3$sum_diff, 
                 breaks = c(-Inf, 0, Inf), 
                 labels = c("Negative", "Positive"), 
                 include.lowest = TRUE)
total_count <- sum(!is.na(df3$sum_diff))
positive_count <- sum(df3$group == "Positive", na.rm = TRUE)
negative_count <- sum(df3$group == "Negative", na.rm = TRUE)
positive_percent <- round(100 * positive_count / total_count, 1)
negative_percent <- round(100 * negative_count / total_count, 1)
cat("Positive percentage:", positive_percent, "%\n")
cat("Negative percentage:", negative_percent, "%\n")


p33 <- ggplot(df3, aes(x = df3$sum_diff, fill = df3$group)) +
  geom_histogram(binwidth = 100, boundary = -3500,color = "black",  position = "identity") +
  geom_vline(aes(xintercept = median(df3$sum_diff, na.rm = TRUE), linetype = "Median"),
             size = 1.3, color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("Positive" = "orange", "Negative" ="lightblue")) +
  annotate("text", x = median(df3$sum_diff, na.rm = TRUE), y = 130,
           label = label_text,
           color = "gray20", size = 7, hjust = -0.16, coordinates = c("data", "plot")) +
  theme_bw() +
  theme(axis.text.y =  element_text(size = 16,color = "gray20"),
        axis.text.x =  element_text(size = 16,color = "gray20"),
        axis.title = element_blank(),
        panel.border = element_rect( size = 1),
        panel.grid = element_blank(),
        legend.position = "none") +
  ylim(0, 150) +
  # coord_fixed(ratio = 4/100)+
  # scale_x_continuous(breaks = c(-3000, -2000, -1000, 0), 
  #                    labels = c("-3000", "-2000", "-1000",  "0"),   #"\u2264-20"
  #                    limits = c(-3500, 0))
  scale_x_continuous(breaks = seq(-3500, 0, by = 500),
                     labels = c("-3500","-3000","-2500","-2000","-1500","-1000", "-500", "0"),   #"\u2264-20"
                     limits = c(-4000, 0))
p33

ggsave(filename =  "./0.figure//Reponse_review/Fig.Case_Cumulative_11.tiff", 
       plot = p33,  width = 16,  height = 11,  units = "cm",  dpi = 300)
