#加载包
library(terra)
library(tidyterra)
library(ggplot2)
library("scales")
setwd("D:/VegetationImpact")

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

r1 <- rast("./EA+NA_Results/merged_diffLST&actLST/merged_diffLST&actLST_1.tif")
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
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    # panel.background = element_blank(),
    # # panel.grid.major = element_line(size = 0.25, colour = "grey70",   #主网格线样式
    # #                                 linetype = "dashed"), 
    # axis.ticks=element_blank(),
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    # legend.position = "none")+
legend.title = element_text(size = 33),
legend.text = element_text(size = 33),
legend.position = "bottom")+
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
  # coord_map(projection = "stereographic", orientation = c(90, 0, 0))+  # 如果要显示北极投影
  scale_fill_stepsn(name = expression(paste(D[T]~"(℃/℃)")),
                    na.value = "transparent",
                    colors = c("#000033","#003366","#006699","#3399CC","#66CCFF",
                               "#fcae91","#FC8D59","#FF6633","#CC3333","#990000"),
                    breaks = c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0),
                    limits = c(-10, 10),
                    values = rescale(c(-10.0, -5.0, -2.0, -1.0, 0, 1.0,  2.0, 5.0, 10.0)),
                    labels = c("", "-5", "-2", "-1", "0", "1", "2", "5", ""),  # 自定义显示的标签
                    guide = guide_colorbar(title.position = "left",
                                           title.hjust = 0.5,
                                           barwidth = 60,
                                           title.vjust = 1,
                                           barheight = 1.8))



p1
ggsave(
  filename = "./0.figure/E_Fig.2-polar_legend.tiff",
  plot = p1,  width = 18,  height = 15,  units = "in",  dpi = 300)
