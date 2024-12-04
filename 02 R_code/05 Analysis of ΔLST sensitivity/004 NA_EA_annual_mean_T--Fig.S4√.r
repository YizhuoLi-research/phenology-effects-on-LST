###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

################################   01  计算年平均温度-NorthAmerica  ###########################
###############################################################################################         
# NA
file_list  <- list.files("./NA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$",
                        full.names = TRUE)

# 初始化一个向量来存储每年的平均温度
average_temperatures <- numeric(length(file_list))

# 循环读取每个.tif文件，并计算平均温度
for (i in 1:length(file_list)) {
  
     # 读取.tif文件
    temperature_data <- raster::raster(file_list[i])
    
  # 计算温度的平均值
  average_temperature <- mean(temperature_data[], na.rm = TRUE)
  # 存储平均温度到向量中
  average_temperatures[i] <- average_temperature
}

# 打印每年的平均温度
for (i in 1:length(file_list)) {
  year <- gsub("_actLSTmean.tif", "", basename(file_list[i]))
  cat(year, "年均温度：", average_temperatures[i], "\n")
}



library(ggplot2)

# 创建数据框
data_NA <- data.frame(Year = gsub("_actLSTmean.tif", "", basename(file_list)),
                   Average_Temperature = average_temperatures)

# 将年份转换为数值型
data_NA$Year <- as.numeric(data_NA$Year)


# 计算趋势线数据
trend_data <- data_NA %>%
  summarize(intercept = coef(lm(Average_Temperature ~ Year))[1],
            slope = coef(lm(Average_Temperature ~ Year))[2],
            rsquared = summary(lm(Average_Temperature ~ Year))$r.squared,
            p_value = coef(summary(lm(Average_Temperature ~ Year)))[2, 4])
# 创建趋势线方程式标签
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2), " + ", round(trend_data$slope, 2), " * x", sep = "")


# 创建年际变化的曲线图
p1 <- ggplot(data_NA, aes(x = Year, y = Average_Temperature)) +
  geom_line(size = 2, color = "#DA4E33") +     # 增加线条的粗细
  geom_point(size = 7,color = "#DA4E33") +    # 添加点，并设置大小为2
  geom_errorbar(aes(ymin = Average_Temperature - 0.15 * Average_Temperature, 
                    ymax = Average_Temperature + 0.15 * Average_Temperature),
                width = 0.2, size = 1.5, color = "#DA4E33") +
  labs(x = "Year",
       y = "Annual mean temperature (℃)") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021)) +  # 设置 x 轴显示的年份
  coord_fixed(ratio = 1/1.2) +
  theme_minimal()+
  geom_smooth(data = data_NA, method = "lm", se = FALSE, color = "#DA4E33", size = 2, linetype = "dashed") +  # 添加趋势线
  geom_text(aes(label = equation_label), x = 2018, y = 14.3, size = 14, color = "#000000")+
  geom_text(aes(label = regression_label), x = 2014.5, y = 14.3, size = 14, color = "#000000")+
  theme( legend.position = "none",
         axis.text.x  = element_text(size = 38),
         axis.text.y  = element_text(size = 40),
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2),
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         # legend.title = element_blank(),
         # legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),    #图例线长
         fill = guide_legend(byrow = TRUE))+
  ylim(8,15)

# 显示曲线图
print(p1)

ggsave(filename = "./0.figure/FigS4.annual_mean_T_NA.tiff",
       plot = p1,  width = 15,  height = 12,  units = "in",  dpi = 300)


#####################################   02  计算年平均温度-Euraisa  ###########################
###############################################################################################      
####Eurasia
file_list  <- list.files("./EA_Results/0.actLSTmean_yr/0common_pixel/", pattern = "\\.tif$",
                         full.names = TRUE)

# 初始化一个向量来存储每年的平均温度
average_temperatures <- numeric(length(file_list))

# 循环读取每个.tif文件，并计算平均温度
for (i in 1:length(file_list)) {
  # 读取.tif文件
  temperature_data <- raster::raster(file_list[i])
  # 计算温度的平均值
  average_temperature <- mean(temperature_data[], na.rm = TRUE)
  # 存储平均温度到向量中
  average_temperatures[i] <- average_temperature
}

# 打印每年的平均温度
for (i in 1:length(file_list)) {
  year <- gsub("_actLSTmean.tif", "", basename(file_list[i]))
  cat(year, "年均温度：", average_temperatures[i], "\n")
}



library(ggplot2)

# 创建数据框
data_EA <- data.frame(Year = gsub("_actLSTmean.tif", "", basename(file_list)),
                   Average_Temperature = average_temperatures)

# 将年份转换为数值型
data_EA$Year <- as.numeric(data_EA$Year)

# 计算趋势线数据
trend_data <- data_EA %>%
  summarize(intercept = coef(lm(Average_Temperature ~ Year))[1],
            slope = coef(lm(Average_Temperature ~ Year))[2],
            rsquared = summary(lm(Average_Temperature ~ Year))$r.squared,
            p_value = coef(summary(lm(Average_Temperature ~ Year)))[2, 4])
# 创建趋势线方程式标签
equation_label <- paste("R\u00b2 = ", round(trend_data$rsquared, 2), ", p = ", round(trend_data$p_value, 2), sep = "")
regression_label <- paste("y = ", round(trend_data$intercept, 2), " + ", round(trend_data$slope, 2), " * x", sep = "")

# 创建年际变化的曲线图
p2 <- ggplot(data_EA, aes(x = Year, y = Average_Temperature)) +
  geom_line(size = 2, color = "#0886CC") +     # 增加线条的粗细
  geom_point(size = 7, color = "#0886CC") +    # 添加点，并设置大小为2
  geom_errorbar(aes(ymin = Average_Temperature - 0.15 * Average_Temperature, 
                    ymax = Average_Temperature + 0.15 * Average_Temperature),
                    width = 0.2, size = 1.5, color = "#0886CC") +
  labs(x = "Year",
       y = "Annual mean temperature (℃)") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021)) +  # 设置 x 轴显示的年份
  coord_fixed(ratio = 1/1.2) +
  theme_minimal()+
  geom_smooth(data = data_EA, method = "lm", se = FALSE, color = "#0886CC", size = 2, linetype = "dashed") +  # 添加趋势线
  geom_text(aes(label = equation_label), x = 2018, y = 9.4, size = 14, color = "#000000")+
  geom_text(aes(label = regression_label), x = 2014.5, y = 9.4, size = 14, color = "#000000")+
  theme( legend.position = "none",
         axis.text.x  = element_text(size = 38),
         axis.text.y  = element_text(size = 40),
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2),
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         # legend.title = element_blank(),
         # legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),    #图例线长
         fill = guide_legend(byrow = TRUE))+
  ylim(3,10)

# 显示曲线图
print(p2)

ggsave(filename = "./0.figure/FigS4.annual_mean_T_EA.tiff",
       plot = p2,  width = 15,  height = 12,  units = "in",  dpi = 300)

