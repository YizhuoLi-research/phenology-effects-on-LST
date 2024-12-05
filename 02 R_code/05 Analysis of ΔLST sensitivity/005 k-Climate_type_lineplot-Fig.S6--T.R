#############################################  00 加载包   #################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

############  01 按气候--subtype类型取像元计算各个气候区内的回归线斜率--给气候类型数据添加属性   ###################

r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]
r <- ratify(r) # Converts raster field to categorical data
rat <- levels(r)[[1]]
# Legend in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc', 
                 'Csa', 'Csb','Csc', 
                 'Cwa','Cwb', 'Cwc', 
                 'Dfa', 'Dfb', 'Dfc','Dfd', 
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd', 
                 'EF',  'ET')
# Remove the placeholders
r[1:30] <- r0
#将修改后的属性表重新赋值给对象r
levels(r) <- rat
# library(rasterVis);levelplot(r)

#划定filter_values_list中每个类型区域的边界
classify_border <- as.polygons(rast(r))
# plot(classify_border)
# 创建气候类型列表
climate_types <-rat$climate 
# # 使用 expand.grid 创建所有气候类型的组合  ###31种subtypes
# all_climates <- expand.grid(climate_types, stringsAsFactors = FALSE)
# colnames(all_climates) <- "Climate_Type"
# # 将结果放入一个列表中
# filter_values_list <- split(all_climates$Climate_Type, 1:nrow(all_climates))


filter_value_CXa <- c( 'Csa','Cfa','Cwa')
filter_value_CXb <- c( 'Csb','Cfb','Cwb')
filter_value_CXc <- c( 'Csc','Cfc','Cwc')
filter_value_DXa <- c( 'Dsa','Dfa','Dwa')
filter_value_DXb <- c( 'Dsb','Dfb','Dwb')
filter_value_DXc <- c( 'Dsc','Dfc','Dwc')
filter_value_DXd <- c( 'Dsd','Dfd','Dwd')

filter_values_list <- list(filter_value_CXa,filter_value_CXb,filter_value_CXc,    
                           filter_value_DXa,filter_value_DXb,filter_value_DXc,filter_value_DXd)


##################  01 按气候--subtype类型取像元计算回归线斜率--计算6个diff文件的  ###########################


# 文件路径和标签列表
file_paths <- c(
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_1/",  #SOS的差值map
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_2/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_3/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_4/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_5/",
  "./EA+NA_Results/merge_average_diff_years/merged_average_diff_6/"
)

labels <- c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS")

perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()
  
  for (i in seq_along(file_paths)) {
    # i = 4
    
    file_list_diff <- list.files(file_paths[i], pattern = "\\.tif$", full.names = TRUE)
    LST_diff <- rast(c(file_list_diff))
    
    file_list_act <- list.files("./EA+NA_Results/merged_actLSTmean_years/", pattern = "\\.tif$", full.names = TRUE)
    LST_act <- rast(c(file_list_act))
    
    # 进行数据处理
    sample = LST_diff      
    sample[is.finite(sample)] = 1
    sample2 = LST_act
    sample2[is.finite(sample2)] = 1
    LST_diff = LST_diff * sample[[1]] * sample2[[1]]
    LST_act = LST_act * sample[[1]] * sample2[[1]] 
    
    # 进行分析
    perform_analysis <- function(classify_border, filter_values) {
      results <- list()
      # filter_val <- filter_value_CXa
      
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)
        selected_border_val <- classify_border[order_val, ]
        df1_val <-  terra::extract(LST_diff, selected_border_val)
        df2_val <-  terra::extract(LST_act, selected_border_val)
        df_val <- cbind(df1_val[, -1], df2_val[, -1])
        colnames(df_val) <- c(
          "LST_diff2013", "LST_diff2014", "LST_diff2015", "LST_diff2016", "LST_diff2017",
          "LST_diff2018", "LST_diff2019", "LST_diff2020", "LST_diff2021",
          "LST_act2013", "LST_act2014", "LST_act2015", "LST_act2016", "LST_act2017",
          "LST_act2018", "LST_act2019", "LST_act2020", "LST_act2021"
        )
        
        diff_cols_val <- grep("LST_diff", colnames(df_val), value = TRUE)
        act_cols_val <- grep("LST_act", colnames(df_val), value = TRUE)
        
        LST_diff_values_val <- unlist(df_val[, diff_cols_val])
        LST_act_values_val <- unlist(df_val[, act_cols_val])
        
        new_df_val <- data.frame(LST_diff = LST_diff_values_val, LST_act = LST_act_values_val)
        new_df_val <- na.omit(new_df_val)
        
        # 检查是否有足够的数据点进行线性拟合  取CD type
        if (nrow(new_df_val) >=500) {                  #500  3500
          model_val <- lm(LST_diff ~ LST_act, data = new_df_val)
          
          results[[length(results) + 1]] <- list(
            Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
            Slope = as.numeric(coef(model_val)[2]),
            R_squared = summary(model_val)[["r.squared"]],
            p_value = summary(model_val)[["coefficients"]][2, 4]
          )
        } else {
          # 不足0个数据点，无法进行线性拟合，跳过这个气候类型
          cat("Insufficient data points for filter value:", filter_val, "\n")
        }
      }
      
      return(do.call(rbind, results))
    }
    
    # 执行subtype的操作
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)
    results_df$type <- substr(results_df$Filter_Value, 2, 2)  # 提取第二个字符作为 type
    results_df$type <- paste("Type ", results_df$type, sep = "")# 将生成的type标签从字母转换为相应的类型名称
    results_df$type <- factor(results_df$type)
    results_df$stars <- ifelse(results_df$p_value <= 0.001, "***",
                               ifelse(results_df$p_value <= 0.01, "**",
                                      ifelse(results_df$p_value <= 0.05, "*", "")))
                                     
    results_df$stars <- factor(results_df$stars)
    results_df$Slope <- round(as.numeric(results_df$Slope),4)
    results_df$R_squared <- round(as.numeric(results_df$R_squared),4)
    
    # 添加标签
    results_df$PHE <- labels[i]
    
    list_of_results[[i]] <- results_df
  }
  
  # 将所有结果组合成一个数据框
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}


# 执行分析并获取结果
final_results <- perform_analysis(file_paths, labels)

max_value <- max(final_results$Slope)
min_value <- min(final_results$Slope)

# 打印最大和最小值
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))


##################    02 绘制lineplot （6）  ###########################################################################

library(dplyr)
library(ggplot2)
final_results$Filter_Value <- lapply(final_results$Filter_Value, function(x) gsub("\"", "", x))
final_results$Filter_Value <- as.character(final_results$Filter_Value)

final_results$PHE <- factor(final_results$PHE, levels = c("SOS", "MGP", "GMO", "GDO", "MSP", "EOS"))
final_results$PHE_num <- as.numeric(final_results$PHE)
final_results$legend <- paste0(substr(final_results$Filter_Value, 1, 1), "X",substr(final_results$Filter_Value, 3, 3))

class(final_results$PHE_num )
class(final_results$Filter_Value )
class(final_results$Slope)

# 创建 ggplot 图表
labels_data <- final_results  # 假设使用与主数据相同的数据来作为标签
labels_data$label_text <- paste("", final_results$stars)  # 创建标签文本

###
k_df_C <- labels_data %>%
  filter(substr(legend, 1, 1) == "C")

k_df_D <- labels_data %>%
  filter(substr(legend, 1, 1) == "D")


###########################  04-1 绘制C-climatetype的k值lineplot图   #####################################################################

k_df_C$Slope <-  as.numeric(k_df_C$Slope)    
k_df_D$Slope <-  as.numeric(k_df_D$Slope)     

p1 <- ggplot(k_df_C, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # 增加线条的粗细
  geom_text(data = k_df_C, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE)+             #不显示字标图例
  # labs(x = "Phenological index", y = "β (℃/℃  )") +  # 坐标轴标签
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)")))+ 
  theme_minimal() +                           # 使用简洁主题
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=2) +
  scale_color_manual(
    values = c("#663300", "#996600", "#FFCC00"),
    labels = c("CXa", "CXb", "CXc"),
    name = "Climate type"
  )+
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.5) +
  theme( legend.position = c(0.84, 0.88), 
         axis.text.x  = element_text(size = 36), 
         axis.text.y  = element_text(size = 42), 
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2), 
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         legend.title = element_text(size = 37),
         legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),
         fill = guide_legend(byrow = TRUE) ) +
  ylim(-1.0 ,1.0)
p1

ggsave(
  filename = "./0.figure/Fig.S6-lineplot_C-T.tiff",
  plot = p1,  width = 15,  height = 11,  units = "in",  dpi = 300)


###########################  04-2 绘制D-climatetype的k值lineplot图   #####################################################################


p2 <- ggplot(k_df_D, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # 增加线条的粗细
  geom_text(data = k_df_D, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE)+             #不显示字标图例
  # labs(x = "Phenological index", y = "β (℃/℃  )") +  # 坐标轴标签
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)")))+ 
  theme_minimal() +                           # 使用简洁主题
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=2) +
  guides(color = guide_legend(ncol = 1))+
  scale_color_manual(
    values = c("#820082", "#C800C8", "#C89BFA", "#C8C8FF"),
    labels = c("DXa", "DXb", "DXc","DXd"),
    name = "Climate type"
  )+
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.7) +
  theme( legend.position = c(0.84, 0.86), 
         axis.text.x  = element_text(size = 36), 
         axis.text.y  = element_text(size = 42), 
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2), 
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         legend.title = element_text(size = 37),
         legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),
         fill = guide_legend(byrow = TRUE) ) +
  # scale_y_continuous(breaks = c(-0.5, 1.0, 1.5)) +  
  # ylim(-0.6 , 1.9)
  scale_y_continuous(breaks = c(-0.5,0,0.5, 1.0, 1.5,2.0), limits = c(-0.6, 2.25))  # 在 scale_y_continuous 中设置 limits



p2

ggsave(
  filename = "./0.figure/Fig.S6-lineplot_D-T.tiff",
  plot = p2,  width = 15,  height = 11,  units = "in",  dpi = 300)
