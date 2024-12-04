###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(ggplot2)
library("scales")

setwd("D:/VegetationImpact")

############################################   1-1 k-MAP极地图   #########################################################


wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r1 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")       #注意修改 1 2 3 4 5 6
max_value <- max(r1[], na.rm = TRUE)
min_value <- min(r1[], na.rm = TRUE)

df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","k_value") 
df1$k_value <- as.numeric(as.character(df1$k_value))       
df1$k_value[df1$k_value > 10] <- 10   # 将所有大于10的值替换为10
df1$k_value[df1$k_value < -10] <- -10 # 将所有小于-10的值替换为-10


p1<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df1, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    # panel.grid.major = element_line(size = 0.25, colour = "grey70",   #主网格线样式
    #                                 linetype = "dashed"), 
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")+
  # legend.title = element_text(size = 25) ,
  # legend.text = element_text(size = 20),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1
ggsave(
  filename = "./0.figure/E_Fig.2-polar_1.tiff",                        
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)

# ################################ （不要）02 系数 & 纬度#############################################
# 
# library(matrixStats)
# lat<-yFromRow(r1)  #获取纬度
# MAT_mat<-as.matrix(r1,wide=TRUE)  #获取栅格值的矩阵
# row_means <- rowMeans(MAT_mat,na.rm=TRUE)#计算每行的均值和标准差
# row_sds <- rowSds(MAT_mat,na.rm=TRUE)
# dat<-data.frame(lat,row_means, row_sds)#组合纬度和均值数据到数据框
# 
# p2<-ggplot(dat, aes(x=row_means, y=lat)) +
#   geom_path(size=0.5)+
#   labs(y="Latitude(°)",x="Coef")+
#   geom_ribbon(aes(y=lat, xmin=row_means-row_sds,xmax = row_means+row_sds),fill = "lightgrey", alpha=0.5)+
#   coord_fixed(ratio = 1)+ #调整y轴/x轴的比例
#   theme_classic()+
#   xlim(-15,15)
# p2
# 
# library(patchwork)
# p <- p1+p2
# p

##################################  1-2 频数分布图  ############################################
library(dplyr)
library(ggplot2)

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

df1 <- df1 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df1$percentage <- round(df1$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df1$percentage)
df1$percentage[nrow(df1)] <- df1$percentage[nrow(df1)] + error


p11 <- ggplot(df1, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p11)

ggsave(filename = "./0.figure/E_Fig.2-chart_1.tiff", 
       plot = p11, width = 7.6, height = 4 , units = "in", dpi = 300)

############################################   2-1 k-MAP极地图   #########################################################

r2 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_2.tif")
max_value <- max(r2[], na.rm = TRUE)
min_value <- min(r2[], na.rm = TRUE)

df2 <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","k_value") 
df2$k_value <-  as.numeric(as.character(df2$k_value))
df2$k_value[df2$k_value > 10] <- 10   # 将所有大于10的值替换为10
df2$k_value[df2$k_value < -10] <- -10 # 将所有小于-10的值替换为-10

p2<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df2, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p2
ggsave(
  filename = "./0.figure/E_Fig.2-polar_2.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)

##################################  2-2 频数分布图  ############################################

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

df2 <- df2 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df2$percentage <- round(df2$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df2$percentage)
df2$percentage[nrow(df2)] <- df2$percentage[nrow(df2)] + error


p22 <- ggplot(df2, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p22)

ggsave(filename = "./0.figure/E_Fig.2-chart_2.tiff", 
       plot = p22, width = 7.6, height = 4 , units = "in", dpi = 300)

############################################   3-1 k-MAP极地图   #########################################################

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r3 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_3.tif")
max_value <- max(r3[], na.rm = TRUE)
min_value <- min(r3[], na.rm = TRUE)

df3 <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat","k_value") 
df3$k_value <- as.numeric(as.character(df3$k_value))      
df3$k_value[df3$k_value > 10] <- 10   # 将所有大于10的值替换为10
df3$k_value[df3$k_value < -10] <- -10 # 将所有小于-10的值替换为-10

p3<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df3, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p3
ggsave(
  filename = "./0.figure/E_Fig.2-polar_3.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)

##################################  3-2 频数分布图  ############################################

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

df3 <- df3 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df3$percentage <- round(df3$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df3$percentage)
df3$percentage[nrow(df3)] <- df3$percentage[nrow(df3)] + error

p33 <- ggplot(df3, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p33)

ggsave(filename = "./0.figure/E_Fig.2-chart_3.tiff", 
       plot = p33, width = 7.6, height = 4 , units = "in", dpi = 300)

############################################   4-1 k-MAP极地图   #########################################################

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r4 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_4.tif")
max_value <- max(r4[], na.rm = TRUE)
min_value <- min(r4[], na.rm = TRUE)

df4 <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","k_value") 
df4$k_value <- as.numeric(as.character(df4$k_value))      ################################
df4$k_value[df4$k_value > 10] <- 10   # 将所有大于10的值替换为10
df4$k_value[df4$k_value < -10] <- -10 # 将所有小于-10的值替换为-10

p4<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df4, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p4
ggsave(
  filename = "./0.figure/E_Fig.2-polar_4.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)

##################################  4-2 频数分布图  ############################################

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

df4 <- df4 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df4$percentage <- round(df4$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df4$percentage)
df4$percentage[nrow(df4)] <- df4$percentage[nrow(df4)] + error

p44 <- ggplot(df4, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p44)

ggsave(filename = "./0.figure/E_Fig.2-chart_4.tiff", 
       plot = p44, width = 7.6, height = 4 , units = "in", dpi = 300)

############################################   5-1 k-MAP极地图   #########################################################

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r5 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_5.tif")
max_value <- max(r5[], na.rm = TRUE)
min_value <- min(r5[], na.rm = TRUE)

df5 <- as.data.frame(r5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","k_value") 
df5$k_value <- as.numeric(as.character(df5$k_value))       
df5$k_value[df5$k_value > 10] <- 10   # 将所有大于10的值替换为10
df5$k_value[df5$k_value < -10] <- -10 # 将所有小于-10的值替换为-10

p5<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df5, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p5
ggsave(
  filename = "./0.figure/E_Fig.2-polar_5.tiff",
  plot = p5,  width = 15,  height = 15,  units = "in",  dpi = 300)

##################################  5-2 频数分布图  ############################################

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")
df5 <- df5 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df5$percentage <- round(df5$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df5$percentage)
df5$percentage[nrow(df5)] <- df5$percentage[nrow(df5)] + error

p55 <- ggplot(df5, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p55)

ggsave(filename = "./0.figure/E_Fig.2-chart_5.tiff", 
       plot = p55, width = 7.6, height = 4 , units = "in", dpi = 300)

############################################   6-1 k-MAP极地图   #########################################################

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r6 <- rast("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_6.tif")
max_value <- max(r6[], na.rm = TRUE)
min_value <- min(r6[], na.rm = TRUE)

df6 <- as.data.frame(r6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat","k_value") 
df6$k_value <- as.numeric(as.character(df6$k_value))
df6$k_value[df6$k_value > 10] <- 10   # 将所有大于10的值替换为10
df6$k_value[df6$k_value < -10] <- -10 # 将所有小于-10的值替换为-10

p6<-ggplot() +
  # geom_spatraster(data=r)+
  geom_tile(data = df6, aes(x = long, y = lat, fill = k_value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  # labs(title = "(a) SOS",hjust = 1)+
  expand_limits(x = wr$long, y = wr$lat)  + 
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "none"))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p6
ggsave(
  filename = "./0.figure/E_Fig.2-polar_6.tiff",
  plot = p6,  width = 15,  height = 15,  units = "in",  dpi = 300)


##################################  6-2 频数分布图  ############################################

my_breaks <- c(max_value, -5, -2, -1, 0, 1, 2, 5, min_value)
my_labels <- c("＜-5",  "[-5,-2)","[-2,-1)","[-1,0)",
               "[0,1)","[1,2)","[2,5)","＞5")
my_colors <- c("#08519c","#2171b5","#6baed6","#bdd7e7",
               "#fcae91","#FC8D59","#fb6a4a","#cb181d")

df6 <- df6 %>%
  mutate(segment = cut(k_value, breaks = my_breaks, labels = my_labels)) %>%
  filter(!is.na(segment)) %>%  
  group_by(segment) %>%
  summarise(frequency = n()) %>%
  ungroup() %>%
  mutate(percentage = frequency / sum(frequency) * 100)
# 将百分比四舍五入到整数
df6$percentage <- round(df6$percentage)
# 将最后一个计数进行修正以确保总和为100%
error <- 100 - sum(df6$percentage)
df6$percentage[nrow(df6)] <- df6$percentage[nrow(df6)] + error

p66 <- ggplot(df6, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            vjust = -0.5,  # 调整文本位置
            size = 10,                   
            color = "black") +   
  scale_fill_manual(values = my_colors, drop = FALSE) +
  theme_bw() +
  coord_fixed(ratio = 1/8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.y = element_line(size = 1),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.2, "cm"),   # 显示坐标轴刻度长度
        axis.title = element_blank(),
        panel.grid = element_line( linetype = "blank"),
        # axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +
  # ylab("Percentage")  +
  ylim(0, 30)  # 设置纵坐标范围为0到100，表示百分比
print(p66)

ggsave(filename = "./0.figure/E_Fig.2-chart_6.tiff", 
       plot = p66, width = 7.6, height = 4 , units = "in", dpi = 300)
