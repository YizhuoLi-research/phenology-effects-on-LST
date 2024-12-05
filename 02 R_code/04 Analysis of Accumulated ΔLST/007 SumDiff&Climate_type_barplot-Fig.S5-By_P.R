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


filter_value_CfX <- c( 'Cfa','Cfb','cfc')
filter_value_CsX <- c( 'Csa','Csb','Csc')
filter_value_CwX <- c( 'Cwa','Cwb','Cwc')
filter_value_DfX <- c( 'Dfa','Dfa','Dfc','Dfd')
filter_value_DsX <- c( 'Dsa','Dsb','Dsc','Dsd')
filter_value_DwX <- c( 'Dwa','Dwb','Dwc','Dwd')

filter_values_list <- list(filter_value_CfX,filter_value_CsX,filter_value_CwX,
                           filter_value_DfX,filter_value_DsX,filter_value_DwX)
##################  02 按气候--subtype类型取像元计算回归线斜率--计算6个diff文件的  ###########################


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

  
  # i = 1
 
   for (i in seq_along(file_paths)) {

    
     file_path <- file_paths[i]
     
     LST_sumdiff <- rast(file_path)

    # 进行分析
    perform_analysis <- function(classify_border, filter_values) {
      results <- list() 
      # filter_val <- filter_value_CXa
      
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
                              substr(results_df$Filter_Value, 3, 3),"X")
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
final_results$Filter_Value <- as.character(final_results$Filter_Value)
final_results$error_bars <- round(final_results$sd_value* 0.15, 2)   
max_value <- max(final_results$mean_value)
min_value <- min(final_results$mean_value)
# class(final_results$type)
# 打印最大和最小值
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))




###########################  03- Cf绘制 6种 subtype的barplot图   #####################################################################

df_CfX <- final_results[final_results$type == "CfX", ]  #查看数据制作S_table6
# > df_CfX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 1  "Cfa", "Cfb", "cfc"      -42.12    45.71  CfX   SOS-MGP       6.86
# 7  "Cfa", "Cfb", "cfc"     -154.32    92.74  CfX   MGP-GMO      13.91
# 13 "Cfa", "Cfb", "cfc"     -774.03   473.53  CfX   GMO-GDO      71.03
# 19 "Cfa", "Cfb", "cfc"     -298.70   231.20  CfX   GDO-MSP      34.68
# 25 "Cfa", "Cfb", "cfc"      -91.66    84.93  CfX   MSP-EOS      12.74
# 31 "Cfa", "Cfb", "cfc"    -1360.83   875.77  CfX   SOS-EOS     131.37
df_CfX <- df_CfX[df_CfX$Phe_phase != "SOS-EOS", ]
colors <- c("SOS-MGP" = "#99CC33", "MGP-GMO" = "#66CC00", "GMO-GDO" = "#006600",
            "GDO-MSP" = "#CCCC33", "MSP-EOS" = "#CC9900")
df_CfX$Phe_phase <- factor(df_CfX$Phe_phase, levels = names(colors))
df_CfX$mean_value <- as.numeric(df_CfX$mean_value)                          #####################


# 绘制柱状图并添加误差线（细化线条）
p1 <- ggplot(df_CfX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))

p1

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CfX.tiff",
  plot = p1,  width = 17,  height = 13,  units = "in",  dpi = 300)


###########################  03- Cs绘制 6种 subtype的barplot图   #####################################################################

df_CsX <- final_results[final_results$type == "CsX", ]  #查看数据制作S_table6
# > df_CsX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 2  "Csa", "Csb", "Csc"      2.12    62.39  CsX   SOS-MGP       9.36
# 8  "Csa", "Csb", "Csc"      -52.08   125.04  CsX   MGP-GMO      18.76
# 14 "Csa", "Csb", "Csc"     -300.57   541.16  CsX   GMO-GDO      81.17
# 20 "Csa", "Csb", "Csc"      -65.96   309.16  CsX   GDO-MSP      46.37
# 26 "Csa", "Csb", "Csc"      -42.58   146.85  CsX   MSP-EOS      22.03
# 32 "Csa", "Csb", "Csc"     -459.08  1143.60  CsX   SOS-EOS     171.54
df_CsX <- df_CsX[df_CsX$Phe_phase != "SOS-EOS", ]
df_CsX$Phe_phase <- factor(df_CsX$Phe_phase, levels = names(colors))
df_CsX$mean_value <- as.numeric(df_CsX$mean_value)


# 绘制柱状图并添加误差线（细化线条）
p2 <- ggplot(df_CsX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))

p2

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CsX.tiff",
  plot = p2,  width = 17,  height = 13,  units = "in",  dpi = 300)

###########################  03- Cw绘制 6种 subtype的barplot图   #####################################################################

df_CwX <- final_results[final_results$type == "CwX", ]  #查看数据制作S_table6
# > df_CwX
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 3  "Cwa", "Cwb", "Cwc"      -90.20    61.23  CwX   SOS-MGP       9.18
# 9  "Cwa", "Cwb", "Cwc"     -227.52   115.38  CwX   MGP-GMO      17.31
# 15 "Cwa", "Cwb", "Cwc"     -918.51   436.43  CwX   GMO-GDO      65.46
# 21 "Cwa", "Cwb", "Cwc"     -287.50   169.73  CwX   GDO-MSP      25.46
# 27 "Cwa", "Cwb", "Cwc"      -56.30    74.61  CwX   MSP-EOS      11.19
# 33 "Cwa", "Cwb", "Cwc"    -1580.02   758.82  CwX   SOS-EOS     113.82
df_CwX <- df_CwX[df_CwX$Phe_phase != "SOS-EOS", ]
df_CwX$Phe_phase <- factor(df_CwX$Phe_phase, levels = names(colors))
df_CwX$mean_value <- as.numeric(df_CwX$mean_value)  

# 绘制柱状图并添加误差线（细化线条）
p3 <- ggplot(df_CwX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))

p3

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_CwX.tiff",
  plot = p3,  width = 17,  height = 13,  units = "in",  dpi = 300)

###########################  03-Df 绘制 6种 subtype的barplot图   #####################################################################

df_DfX <- final_results[final_results$type == "DfX", ]  #查看数据制作S_table6
# Filter_Value mean_value sd_value type Phe_phase error_bars
# 4  "Dfa", "Dfa", "Dfc", "Dfd"      25.64    67.29  DfX   SOS-MGP      10.09
# 10 "Dfa", "Dfa", "Dfc", "Dfd"      56.79    75.18  DfX   MGP-GMO      11.28
# 16 "Dfa", "Dfa", "Dfc", "Dfd"     245.38   301.41  DfX   GMO-GDO      45.21
# 22 "Dfa", "Dfa", "Dfc", "Dfd"      72.05   115.63  DfX   GDO-MSP      17.34
# 28 "Dfa", "Dfa", "Dfc", "Dfd"      24.46    60.60  DfX   MSP-EOS       9.09
# 34 "Dfa", "Dfa", "Dfc", "Dfd"     424.33   572.27  DfX   SOS-EOS      85.84
df_DfX <- df_DfX[df_DfX$Phe_phase != "SOS-EOS", ]
df_DfX$Phe_phase <- factor(df_DfX$Phe_phase, levels = names(colors))
df_DfX$mean_value <- as.numeric(df_DfX$mean_value)      

# 绘制柱状图并添加误差线（细化线条）
p4 <- ggplot(df_DfX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))
p4

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DfX.tiff",
  plot = p4,  width = 17,  height = 13,  units = "in",  dpi = 300)

###########################  03-Ds 绘制 6种 subtype的barplot图   #####################################################################

df_DsX <- final_results[final_results$type == "DsX", ]  #查看数据制作S_table6
# > df_DsX                 Filter_Value mean_value sd_value type Phe_phase error_bars
# 5  "Dsa", "Dsb", "Dsc", "Dsd"    -  -7.52    66.81  DsX   SOS-MGP      10.02
# 11 "Dsa", "Dsb", "Dsc", "Dsd"      -42.76    68.62  DsX   MGP-GMO      10.29
# 17 "Dsa", "Dsb", "Dsc", "Dsd"     -196.48   278.32  DsX   GMO-GDO      41.75
# 23 "Dsa", "Dsb", "Dsc", "Dsd"   -  -74.89   134.02  DsX   GDO-MSP      20.10
# 29 "Dsa", "Dsb", "Dsc", "Dsd"      -39.80    82.10  DsX   MSP-EOS      12.31
# 35 "Dsa", "Dsb", "Dsc", "Dsd"     -361.46   576.89  DsX   SOS-EOS      86.53
df_DsX <- df_DsX[df_DsX$Phe_phase != "SOS-EOS", ]
df_DsX$Phe_phase <- factor(df_DsX$Phe_phase, levels = names(colors))
df_DsX$mean_value <- as.numeric(df_DsX$mean_value)                


# 绘制柱状图并添加误差线（细化线条）
p5 <- ggplot(df_DsX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))

p5

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DsX.tiff",
  plot = p5,  width = 17,  height = 13,  units = "in",  dpi = 300)

###########################  03-Dw 绘制 6种 subtype的barplot图   #####################################################################

df_DwX <- final_results[final_results$type == "DwX", ]  #查看数据制作S_table6
#> df_DwX                 Filter_Value mean_value sd_value type Phe_phase error_bars
# 6  "Dwa", "Dwb", "Dwc", "Dwd"      -75.26    43.17  DwX   SOS-MGP       6.48
# 12 "Dwa", "Dwb", "Dwc", "Dwd"     -179.26    88.58  DwX   MGP-GMO      13.29
# 18 "Dwa", "Dwb", "Dwc", "Dwd"     -627.69   379.09  DwX   GMO-GDO      56.86
# 24 "Dwa", "Dwb", "Dwc", "Dwd"     -191.44   132.42  DwX   GDO-MSP      19.86
# 30 "Dwa", "Dwb", "Dwc", "Dwd"      -42.92    48.00  DwX   MSP-EOS       7.20
# 36 "Dwa", "Dwb", "Dwc", "Dwd"    -1116.57   628.32  DwX   SOS-EOS      94.25
df_DwX <- df_DwX[df_DwX$Phe_phase != "SOS-EOS", ]
df_DwX$Phe_phase <- factor(df_DwX$Phe_phase, levels = names(colors))
df_DwX$mean_value <- as.numeric(df_DwX$mean_value)                     

# 绘制柱状图并添加误差线（细化线条）
p6 <- ggplot(df_DwX, aes(x = Phe_phase, y = mean_value, fill = Phe_phase)) +
  geom_bar(stat = "identity", color = alpha("black", 0), fill = colors) +  # 绘制柱状图
  geom_errorbar(aes(ymin = mean_value - error_bars, ymax = mean_value + error_bars), 
                size = 3, width = 0.5, color = "gray30") +  # 添加误差线
  labs(y ="Cumulative ΔLST (℃·day)")+  # 添加轴标签
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=1.5) +
  theme_bw() +  # 设定主题
  theme(panel.grid.major.x = element_blank(),         # 隐藏x轴网格线
        panel.grid.minor.x = element_blank(),         # 隐藏x轴次要网格线
        panel.border = element_rect(size = 2),        # 设置整个图的框线大小
        # axis.text.x  = element_text(size = 55, color = "black",angle = 15,
        #                             margin = margin(t = 25)), 
        axis.text.y  = element_text(size = 60, color = "black"), 
        axis.ticks.y = element_line(size = 2.5),  # 显示坐标轴刻度宽度
        axis.ticks.length.y = unit(0.3, "cm"),   # 显示坐标轴刻度长度
        axis.title.y = element_text(size = 70), 
        axis.title.x = element_text(size = 0),  # 隐藏 x 轴标题
        # axis.line.x = element_line(),         # 设置x轴颜色 color = "#999999",
        # axis.line.y = element_line(size = 1),         # 设置y轴颜色 color = "#999999",
        plot.background = element_rect(size = 100),      # 设置整个图的背景框线大小
        plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt") , # 调整面板边距
        legend.position = "none",
        panel.grid = element_line( linetype = "blank"),
        axis.text.x = element_blank(),  # 隐藏 x 轴文字
        axis.ticks.x = element_blank()) +  # 隐藏 x 轴刻度
  scale_y_continuous(limits = c(-1200, 100), breaks = c(-1200,-900,-600,-300,0,100))

p6

ggsave(
  filename = "./0.figure/Fig.S5-barplot_P_DwX.tiff",
  plot = p6,  width = 17,  height = 13,  units = "in",  dpi = 300)

