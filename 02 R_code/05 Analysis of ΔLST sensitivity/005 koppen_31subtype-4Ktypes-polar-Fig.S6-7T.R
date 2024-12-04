library(raster)
library(rasterVis)
setwd("D:/VegetationImpact")

################################  01.数据匹配+作图  ################################################

# Load the raster
r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")

# Assigning sequential values as placeholders (if necessary)
r[1:30] <- seq(1,30,1)
r0 <- r[1:30]

# Convert the raster to categorical data
r <- ratify(r)
rat <- levels(r)[[1]]

# Defining climate categories
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')
climate_labels <- c('Af', 'Am', 'As', 'Aw',
                    'BSh', 'BSk', 'BWh', 'BWk',
                    'Cfa', 'Cfb','Cfc', 
                    'Csa', 'Csb','Csc', 
                    'Cwa','Cwb', 'Cwc', 
                    'Dfa', 'Dfb', 'Dfc','Dfd', 
                    'Dsa', 'Dsb', 'Dsc','Dsd',
                    'Dwa', 'Dwb', 'Dwc','Dwd', 
                    'EF',  'ET')
# # Color palette for climate classification (A-B-C-D-E)
# climate_colors <- c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", 
#                     "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", 
#                     "#003200", "#005000", "#007800", 
#                     "#00D700", "#00AA00", "#96FF00",
#                     "#BEBE00", "#8C8C00", "#5A5A00",
#                     "#550055", "#820082", "#C800C8", "#FF6EFF",
#                     "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6",
#                     "#C8C8FF", "#C89BFA", "#B464FA", "#6E28B4",
#                     "#6496FF", "#64FFFF")



# Update raster levels with climate and colors
levels(r) <- rat

# Convert the raster to a dataframe for ggplot
rdf1 <- as.data.frame(r, xy = TRUE)
names(rdf1)[3] <- "climate"

# match with the 4-types pixels
type_r <- raster("./EA+NA_Results/merged_diffLST&actLST/0common_pixel_k/merged_diffLST&actLST_1.tif")
rdf2 <- as.data.frame(type_r, xy = TRUE)
# Removing NA values which result from the masklayer
rdf2 <- na.omit(rdf2)

common_rdf <- merge(rdf1, rdf2, by = c("x", "y"))

# Check unique values in the Climate column of the common_rdf dataframe
unique_climates <- unique(common_rdf$climate)
print(unique_climates)
common_rdf <- na.omit(common_rdf)
head(common_rdf)

library(dplyr)


# 假设common_rdf已经是您的数据框，我们继续从上一步开始
common_rdf <- common_rdf %>%
  mutate(Type = case_when(
    climate %in% c('Csa', 'Cfa', 'Cwa') ~ "CXa",
    climate %in% c('Csb', 'Cfb', 'Cwb') ~ "CXb",
    climate %in% c('Csc', 'Cfc', 'Cwc') ~ "CXc",
    climate %in% c('Dsa', 'Dfa', 'Dwa') ~ "DXa",
    climate %in% c('Dsb', 'Dfb', 'Dwb') ~ "DXb",
    climate %in% c('Dsc', 'Dfc', 'Dwc') ~ "DXc",
    climate %in% c('Dsd', 'Dfd', 'Dwd') ~ "DXd",
    TRUE ~ "Other"  # 如果没有匹配到任何上述分类，则归为"Other"
  )) %>%
  filter(Type != "Other")  # 过滤掉分类为"Other"的行

# 检查结果，查看已过滤的数据框
head(common_rdf)

climate_labels <- c("CXa", "CXb", "CXc", "DXa", "DXb", "DXc", "DXd") 
climate_descriptions <- c("Warm temperate climate with hot summer",
                          "Warm temperate climate with warm summer",
                          "Warm temperate climate with cool summer", 
                          "Cold climate with hot summer", 
                          "Cold climate with warm summer", 
                          "Cold climate with cool summer", 
                          "Cold climate with extremely continental") 
climate_colors <- c(
  "#663300", "#996600", "#FFCC00",
  "#820082", "#C800C8", "#C89BFA", "#C8C8FF")
# 创建颜色映射表
color_map <- data.frame(climate = climate_labels, descriptions = climate_descriptions, colors = climate_colors)
# 筛选存在于common_rdf中的climate
filtered_color_map <- color_map[color_map$climate %in% common_rdf$Type, ]

# plot
library("scales")
library(dplyr)
library(ggplot2)

wr <- map_data("world") %>%
  filter(lat > 20)

# Defines the x axes required
x_lines <- seq(-120,180, by = 60)

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = common_rdf, aes(x = x, y = y, fill = Type)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_manual(values = setNames(filtered_color_map$colors, filtered_color_map$climate),
                    labels = c("Warm temperate climate with hot summer (CXa)", 
                               "Warm temperate climate with warm summer (CXb)",
                               "Warm temperate climate with cool summer (CXc)", 
                               "Cold climate with hot summer (DXa)", 
                               "Cold climate with warm summer (DXb)", 
                               "Cold climate with cool summer (DXc)", 
                               "Cold climate with extremely continential conditions (DXd)"))+  # Use predefined color palette
  labs(fill = "Major climate type reclassified by temperature") +

  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        # legend.position = "none")+
        legend.title = element_text(size = 38) ,
        legend.text = element_text(size = 38),
        legend.position = "right",
        panel.grid = element_line(linetype = "blank"),
        legend.spacing.y = unit(0.5, "cm") )+ # 调整图例行间距)+
  guides(fill = guide_legend( title.position = "top", title.hjust = 0 , byrow = TRUE )) +

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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 14) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 14) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1

ggsave(
  filename = "./0.figure/Fig.S6_Koppen_Climate_7_T.tiff",
  plot = p1,  width = 30,  height = 15,  units = "in",  dpi = 300)


#######################
# ## 计算Dfd, Dsd, Dwd在DXd中的占比
# 
# dx_climates <- c("Dfd", "Dsd", "Dwd")  # 定义需要计算的climate类型
# 
# # 获取DXd类别中的总数及Dfd, Dsd, Dwd的数量和占比
# dx_stats <- common_rdf %>%
#   filter(Type == "DXd") %>%
#   summarise(
#     Total = n(),
#     Count = sapply(dx_climates, function(x) sum(climate == x)),
#     Proportion = sapply(dx_climates, function(x) sum(climate == x) / Total)
#   ) %>%
#   pivot_longer(cols = starts_with("Count"), names_to = "Climate", values_to = "Count") %>%
#   pivot_longer(cols = starts_with("Proportion"), names_to = "Climate_Proportion", values_to = "Proportion")
# 
# # 打印结果
# print(dx_stats)
library(dplyr)

# 假设你的数据框叫做 df
# 计算Type为Cxc的比例
cxc_proportion <- common_rdf %>%
  summarise(
    Total = n(),  # 计算总行数
    Cxc_Count = sum(Type == "CXc"),  # 计算Type为Cxc的行数
    Proportion = Cxc_Count / Total  # 计算比例
  )

# 打印结果
print(cxc_proportion)


# map
library(maps)
library(ggplot2)
# 假设你的数据框名为 df
# 筛选出Type为Cxc的数据
CXc_data <- common_rdf[common_rdf$Type == "CXc",]

# 获取世界地图数据
world_map <- map_data("world")

# 绘制地图并添加Cxc的位置点
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray70", colour = "white") +
  geom_point(data = CXc_data, aes(x = x, y = y), color = "red", size = 3) +
  labs(title = "Geographical Distribution of Type Cxc",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12), title = element_text(size = 14))
