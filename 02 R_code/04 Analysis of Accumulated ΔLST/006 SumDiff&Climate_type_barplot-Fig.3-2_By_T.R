##################  00 加载包   ##########################################################################
library(terra)
library(tidyverse)
library(raster)

setwd("D:/VegetationImpact")

##################  01 按气候--subtype类型提取数据--给气候类型数据添加属性   ###################

r <- raster("./EA+NA_Results/EA_NA_koppen_30km_addClimate.tif")
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


##################  02 按气候--subtype类型取6个物候阶段的数值作图  ###########################


# 文件路径和标签列表
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
  
)

labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS","SOS-EOS")

perform_analysis <- function(file_paths, labels) {
     list_of_results <- list()

  
     i = 1
 
   for (i in seq_along(file_paths)) {

    
     file_path <- file_paths[i]
     
     LST_sumdiff <- rast(file_path)

    # 进行分析
    perform_analysis <- function(classify_border, filter_values) {
      results <- list() 
      filter_val <- filter_value_CXa
      
      for (filter_val in filter_values) {
        order_val <- match(filter_val, classify_border$EA_koppen_30km_addClimate)
        selected_border_val <- classify_border[order_val, ]
        df_val <- raster::extract(LST_sumdiff, selected_border_val)
        df_val <- df_val[, -1]
        df_val <- data.frame(df_val)
        
        colnames(df_val) <- c(
          "LST_sumdiff"   )
        
        sumdiff_cols_val <- grep("LST_sumdiff", colnames(df_val), value = TRUE)

        LST_sumdiff_values_val <- unlist(df_val[, sumdiff_cols_val])

        new_df_val <- data.frame(LST_sumdiff = LST_sumdiff_values_val)

        results[[length(results) + 1]] <- list(
            Filter_Value = paste(sprintf('"%s"', filter_val), collapse = ", "), 
            mean_value =  mean(new_df_val$LST_sumdiff, na.rm = TRUE),
            sd_value = sd(new_df_val$LST_sumdiff, na.rm = TRUE)
          )
     
      }
      return(do.call(rbind, results))
    }
    
    # 执行subtype的操作
    filter_values <- filter_values_list
    results_df <- perform_analysis(classify_border, filter_values)
    results_df <- as.data.frame(results_df)
    results_df$type <- paste0(substr(results_df$Filter_Value, 2, 2),
                              "X",substr(results_df$Filter_Value, 4, 4))
    results_df$type <- as.factor(results_df$type)
    results_df$mean_value <- round(as.numeric(results_df$mean_value),2)
    results_df$sd_value <- round(as.numeric(results_df$sd_value),2)
   
    
    # 添加标签
    results_df$Phe_phase <- labels[i]
    
    list_of_results[[i]] <- results_df
  }
  
  # 将所有结果组合成一个数据框
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}


# 执行分析并获取结果
final_results <- perform_analysis(file_paths, labels)
final_results$Filter_Value <- as.character(final_results$Filter_Value) ##########在这里统一变负值-
final_results$error_bars <- round(final_results$sd_value* 0.15, 2)   
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)
# class(final_results$type)
# 打印最大和最小值
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))



###########################  03-CXa 绘制 7种 subtype的barplot图   #####################################################################


# df_CXa1 <- final_results %>%
#   filter(type == "CXa")
df_CXa <- final_results[final_results$type == "CXa", ]  #查看数据制作S_table5
df_CXa <- df_CXa[df_CXa$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXa$Phe_phase <- factor(df_CXa$Phe_phase, levels = names(colors))  

df_CXa$mean_value <- as.numeric(df_CXa$mean_value)  

# 绘制柱状图并添加误差线（细化线条）
p1 <- ggplot(df_CXa, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p1

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_CXa.tiff",
  plot = p1,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


###########################  03-CXb 绘制 7种 subtype的barplot图   #####################################################################

df_CXb <- final_results[final_results$type == "CXb", ]  #查看数据制作S_table5
df_CXb <- df_CXb[df_CXb$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXb$Phe_phase <- factor(df_CXb$Phe_phase, levels = names(colors)) 

df_CXb$mean_value <- as.numeric(df_CXb$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p2 <- ggplot(df_CXb, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p2

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_Cxb.tiff",
  plot = p2,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

###########################  03-CXc 绘制 7种 subtype的barplot图   #####################################################################

df_CXc <- final_results[final_results$type == "CXc", ]  #查看数据制作S_table5
df_CXc <- df_CXc[df_CXc$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CXc$Phe_phase <- factor(df_CXc$Phe_phase, levels = names(colors)) 

df_CXc$mean_value <- as.numeric(df_CXc$mean_value)

# 绘制柱状图并添加误差线（细化线条）
p3 <- ggplot(df_CXc, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78, margin = margin(r = 70)),  # 调整y轴标题的边距
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(0, 200), breaks = c(0,50,100,150,200))

p3

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_CXc.tiff",
  plot = p3,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

###########################  03-DXa 绘制 7种 subtype的barplot图   #####################################################################

df_DXa <- final_results[final_results$type == "DXa", ]  #查看数据制作S_table5
df_DXa <- df_DXa[df_DXa$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXa$Phe_phase <- factor(df_DXa$Phe_phase, levels = names(colors)) 

df_DXa$mean_value <- as.numeric(df_DXa$mean_value)

# 绘制柱状图并添加误差线（细化线条）
p4 <- ggplot(df_DXa, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p4

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXa.tiff",
  plot = p4,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

###########################  03-DXb 绘制 7种 subtype的barplot图   #####################################################################

df_DXb <- final_results[final_results$type == "DXb", ]  #查看数据制作S_table5
df_DXb <- df_DXb[df_DXb$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXb$Phe_phase <- factor(df_DXb$Phe_phase, levels = names(colors)) 

df_DXb$mean_value <- as.numeric(df_DXb$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p5 <- ggplot(df_DXb, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p5

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXb.tiff",
  plot = p5,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

###########################  03-DXc 绘制 7种 subtype的barplot图   #####################################################################

df_DXc <- final_results[final_results$type == "DXc", ]  #查看数据制作S_table5
df_DXc <- df_DXc[df_DXc$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXc$Phe_phase <- factor(df_DXc$Phe_phase, levels = names(colors)) 

df_DXc$mean_value <- as.numeric(df_DXc$mean_value)

# 绘制柱状图并添加误差线（细化线条）
p6 <- ggplot(df_DXc, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p6

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXc.tiff",
  plot = p6,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

###########################  03-DXd 绘制 7种 subtype的barplot图   #####################################################################

df_DXd <- final_results[final_results$type == "DXd", ]  #查看数据制作S_table5
df_DXd <- df_DXd[df_DXd$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_DXd$Phe_phase <- factor(df_DXd$Phe_phase, levels = names(colors)) 

df_DXd$mean_value <- as.numeric(df_DXd$mean_value)

# 绘制柱状图并添加误差线（细化线条）
p7 <- ggplot(df_DXd, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p7

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_DXd.tiff",
  plot = p7,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

################################# 04.sum  5段时间平均积温直方图 ##################################

# 文件路径和标签列表
file_paths <- c(
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_12.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_23.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_34.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_45.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_56.tif",
  "./EA+NA_Results/merged_sum_diff_average/merged_sum_diff_16.tif"
  
)

labels <- c("SOS-MGP", "MGP-GMO", "GMO-GDO", "GDO-MSP", "MSP-EOS","SOS-EOS")

perform_analysis <- function(file_paths, labels) {
  list_of_results <- list()
  
  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    
    # 读取栅格数据
    LST_sumdiff <- raster(file_path)
    
    # 进行分析
    results_df <- data.frame(
      mean_value = mean(values(LST_sumdiff), na.rm = TRUE),
      sd_value = sd(values(LST_sumdiff), na.rm = TRUE)
    )
    
    # 将结果保留两位小数
    results_df$mean_value <- round(results_df$mean_value, 2)
    results_df$sd_value <- round(results_df$sd_value, 2)
    
    # 添加标签
    results_df$Phe_phase <- labels[i]
    
    list_of_results[[i]] <- results_df
  }
  
  # 将所有结果组合成一个数据框
  all_results <- do.call(rbind, list_of_results)
  
  return(all_results)
}

# 调用函数，执行分析并获取结果
final_results <- perform_analysis(file_paths, labels)
print(final_results)

final_results$error_bars <- round(final_results$sd_value* 0.15, 2)   #查看数据制作S_table5
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)
# class(final_results$type)
# 打印最大和最小值
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))

final_results <- final_results[final_results$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
final_results$Phe_phase <- factor(final_results$Phe_phase, levels = names(colors)) 

final_results$mean_value <- as.numeric(final_results$mean_value)

# > final_results
# mean_value sd_value Phe_phase error_bars
# 1      31.80    59.82   SOS-MGP       8.97
# 2      98.43    94.82   MGP-GMO      14.22
# 3     437.49   407.79   GMO-GDO      61.17
# 4     145.39   171.28   GDO-MSP      25.69
# 5      39.48    70.11   MSP-EOS      10.52
# 6     752.59   746.38      <NA>     111.96



p_sum <- ggplot(final_results, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  # coord_fixed(ratio = 1/400) +
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))
p_sum

ggsave(
  filename = "./0.figure/Fig.3-barplot_mean_sum.tiff",
  plot = p_sum,  width = 16.5,  height = 13,  units = "in",  dpi = 300)


###########################  05 -AmeriFlux站点all&subtype的barplot图   #####################################################################

df <- read.csv("./AmerifluxData_Analysis/1330_Noen&Normal_Results_1y_17 final-info.csv")
head(df)
# 计算总体的平均值和标准差
mean_all <- round(mean(df$sum_Diff_16_mean, na.rm = TRUE), 2)
sd_all <- round(sd(df$sum_Diff_16_mean, na.rm = TRUE), 2)

# 计算 Cfa 组的平均值和标准差
mean_cfa <- round(mean(df$sum_Diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)
sd_cfa <- round(sd(df$sum_Diff_16_mean[df$Clim == "Cfa"], na.rm = TRUE), 2)

# 计算 Dfb 组的平均值和标准差
mean_dfb <- round(mean(df$sum_Diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)
sd_dfb <- round(sd(df$sum_Diff_16_mean[df$Clim == "Dfb"], na.rm = TRUE), 2)

# 创建数据框
summary_df <- data.frame(
  mean_value = c(mean_all, mean_cfa, mean_dfb),
  sd_value = c(sd_all, sd_cfa, sd_dfb),
  Group = c("All", "CXa", "DXb"),
  error_bars = round(c(0.15 * sd_all, 0.15 * sd_cfa, 0.15 * sd_dfb), 2)
)

# 查看结果
print(summary_df)
# mean_value sd_value Group error_bars
# 1     -558.74   633.22   All      94.98
# 2     -950.68   628.20   CXa      94.23
# 3     -210.35   408.01   DXb      61.20



colors <- c("All" = "#663300",  "CXa" = "#CC9900", "DXb" = "#FFCC00")
summary_df$Group <- factor(summary_df$Group, levels = names(colors)) 

summary_df$mean_value <- as.numeric(summary_df$mean_value)

# 绘制柱状图并添加误差线（细化线条）
p8 <- ggplot(summary_df, aes(x = Group, y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y = "Cumulative ΔLST (℃·day)") +  # 添加轴标签
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        axis.text.y  = element_text(size = 70, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 78), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
        legend.position = "none",
        panel.grid = element_line(linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 0), breaks = c(-1200,-900,-600,-300,0))

p8

ggsave(
  filename = "./0.figure/Fig.3-barplot_All_AmeriFlux.tiff",
  plot = p8,  width = 16.5,  height = 13,  units = "in",  dpi = 300)

