###### 0. 加载包 ####

library(terra)
library(tidyverse)
library(dplyr)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")



###################################   01 站点信息整合,筛选出DB植被站点  #################################33###



file1 <- "01 Download/05 AmerifluxData/LST_Ameriflux_Filtered.csv/LST_Ameriflux_Filtered.csv"
file2 <- "01 Download/05 AmerifluxData/TransitionDates.csv"

# 读取 CSV 文件内容
data1 <- read_csv(file1)
data2 <- read_csv(file2)

# 显示第一个文件的表头
header1 <- colnames(data1)
print("File 1 Columns:")
print(header1)

# 显示第二个文件的表头
header2 <- colnames(data2)
print("File 2 Columns:")
print(header2)


# 查看表2 veg_type列中的不同类别
unique_veg_types <- unique(data2$veg_type)
print(unique_veg_types)
# 查看不同类别的数量
num_veg_types <- length(unique_veg_types)
print(num_veg_types)                 
# [1] "DB" "SH" "EN" "TN" "AG" "GR" "XX" "WL" "UN" "EB"                        
#筛选出落叶阔叶林的站点

data2_DB <- subset(data2, veg_type == "DB")
unique_SiteID <- unique(data2_DB$SiteID)
print(unique_SiteID)


# 找到 data1 和 data2_DB 中重合的 SiteID 筛选出 data1_DB
common_sites <- intersect(unique(data1$SiteID), unique(data2_DB$SiteID))
# 输出重合的站点
print(common_sites)
# 计算重合站点的数量
num_common_sites <- length(common_sites)
print(num_common_sites)
# [1] "US-Ton" "US-xRN" "US-NC4" "US-xBL" "US-Slt" "US-SSH" "US-xLE" "US-Dk2" "US-xML"
# [10] "US-xUN" "US-xSC" "US-xSE" "US-xTR" "US-xHA" "US-xCL" "US-xBR" "US-MMS" "US-xUK"
# [19] "US-xST" "US-xDL" "US-xGR" "US-xJE" "US-UMB" "US-UMd" "US-MOz" "US-WCr" "US-Cwt"
# 27
data1_DB <- subset(data1, SiteID %in% common_sites)

xx_data <- data2[data2$veg_type == "XX", ]

# 查看过滤后的数据
print(xx_data)



###########################################    02 对Data2-PhenCam日期转日序   ################################################



# library(dplyr)
# library(lubridate)
# 筛选 transition_10 列中年份为13至21的行
data2_DB_DOY <- data2_DB %>%
  filter(as.numeric(substr(transition_10, nchar(transition_10) - 1, nchar(transition_10))) %in% 13:21)

# 提取年份并加上 "20" 前缀，生成完整的 20XX 年份
data2_DB_DOY <- data2_DB_DOY %>%
  mutate(year = paste0("20", substr(transition_10, nchar(transition_10) - 1, nchar(transition_10))))

# 转换为日期格式并计算日序数
data2_DB_DOY <- data2_DB_DOY %>%
  mutate(date_10 = mdy(transition_10),
         date_25 = mdy(transition_25),
         date_50 = mdy(transition_50),
         DOY_10 = yday(date_10),
         DOY_25 = yday(date_25),
         DOY_50 = yday(date_50))

# 查看结果
head(data2_DB_DOY)
header2 <- colnames(data2_DB_DOY)
print("File 2 Columns:")
print(header2)

data2_DB_DOY_selected <- data2_DB_DOY[, c("SiteID", "site", "veg_type", "direction",
                                          "year", "DOY_10", "DOY_25", "DOY_50")]

# 检查重复值、处理重复项
duplicates <- data2_DB_DOY_selected %>%
  group_by(SiteID, site, veg_type, year, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
print(duplicates)
# 查看重复项
data_US_Slt <- data2_DB_DOY_selected %>% 
  filter(SiteID == "US-Slt")
print(data_US_Slt)  
# SiteID site        veg_type direction year  DOY_10 DOY_25 DOY_50
# US-Slt silaslittle DB       rising    2016     119    126    133
# US-Slt silaslittle DB       rising    2016     321    325    351

#其中，原始数据中US-Slt站点2016rising两组数据-重复 我们应该选择较小值那组-符合趋势
data2_DB_DOY_selected <- data2_DB_DOY_selected %>%
  group_by(SiteID, site, veg_type, year, direction) %>%
  filter(DOY_10 == min(DOY_10) &  DOY_25 == min(DOY_25) & DOY_50 == min(DOY_50)
  ) %>%
  ungroup()
print(data2_DB_DOY_selected)

# 将数据框转换为宽格式，按照年份将rising和falling数据进行组合
# 分组并重命名列
rising_data <- data2_DB_DOY_selected %>%
  filter(direction == "rising") %>%
  rename(rising_DOY_10 = DOY_10, rising_DOY_25 = DOY_25, rising_DOY_50 = DOY_50) %>%
  dplyr::select(SiteID, site, veg_type, year, rising_DOY_10, rising_DOY_25, rising_DOY_50)

falling_data <- data2_DB_DOY_selected %>%
  filter(direction == "falling") %>%
  rename(falling_DOY_10 = DOY_10, falling_DOY_25 = DOY_25, falling_DOY_50 = DOY_50) %>%
  dplyr::select(SiteID, site, veg_type, year, falling_DOY_50, falling_DOY_25, falling_DOY_10)

# 合并两个数据框
combined_data <- full_join(rising_data, falling_data, by = c("SiteID", "site", "veg_type", "year"))
# 使用 na.omit 删除物候数据有NA 的行
data2_DB_DOY_combined <- na.omit(combined_data)


# 统计 SiteID 列中独特的站点数量
num_sites <- data2_DB_DOY_combined %>%
  distinct(SiteID) %>%
  nrow()
# 输出站点数量                
print(num_sites)    #27

#保存文件输出
if (!dir.exists("./AmerifluxData_Analysis")) {
  dir.create("./AmerifluxData_Analysis", recursive = TRUE)
}
write.csv(data2_DB_DOY_combined, "./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv", row.names = FALSE)



###########################################    03 对Data1-LST筛选13：30时刻数据与phe数据整合   ################################################




# 筛选 TIMESTAMP_START 以 “1330” 结尾且年份在 2013 到 2021 的行
data1_DB_selected <- data1_DB %>%
  filter(
    substr(as.character(TIMESTAMP_START), nchar(as.character(TIMESTAMP_START)) - 3, nchar(as.character(TIMESTAMP_START))) == "1330"
  ) %>%
  # 提取年份、日期和 DOY
  mutate(
    date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
    year = year(date),
    DOY = yday(date)
  ) %>%
  # 仅保留有年份为 2013 到 2021 的数据
  filter(year %in% 2013:2021)

# 查看结果
print(data1_DB_selected)


# 统计 SiteID 列中独特的站点数量
num_sites <- data1_DB_selected %>%
  distinct(SiteID) %>%
  nrow()
# 输出站点数量                23个
print(num_sites)

#保存文件输出
# 保存完整的 data1_DB_selected 到指定路径
write.csv(data1_DB_selected, "./AmerifluxData_Analysis/data1_DB_LST_selected_sum.csv", row.names = FALSE)



#################################    分noen站和非noen站进行分析，因为所选的温度列不同
# 保存 tau 列非空值部分到指定路径
data1_DB_selected_noen <- data1_DB_selected %>% filter(!is.na(tau))
write.csv(data1_DB_selected_noen, "./AmerifluxData_Analysis/data1_DB_LST_selected_noen.csv", row.names = FALSE)

# 保存 tau 列为空值的部分到指定路径
data1_DB_selected_normal <- data1_DB_selected %>% filter(is.na(tau))
write.csv(data1_DB_selected_normal, "./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv", row.names = FALSE)




