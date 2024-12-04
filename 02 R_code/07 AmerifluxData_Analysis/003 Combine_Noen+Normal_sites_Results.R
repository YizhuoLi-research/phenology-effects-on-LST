###### 0. 加载包 ####

library(terra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###################################   01 站点结果合并；信息整合  ####################################
########### 13:30
df1_noen <- read.csv("./AmerifluxData_Analysis/Noen_results.csv")
df2_norm <-  read.csv("./AmerifluxData_Analysis/Normal_results.csv")

########### 去掉SOS、EOS后存在空值的行
df1_noen <- df1_noen %>%
  filter(!is.na(average_diff_21) & 
           # !is.na(average_diff_22) & 
           # !is.na(average_diff_23) & 
           # !is.na(average_diff_24) & 
           # !is.na(average_diff_25) & 
           !is.na(average_diff_26))
df2_norm <- df2_norm %>%
  filter(!is.na(average_diff_21) & 
         !is.na(average_diff_26))

###########  将noen站和normal站组合

# 添加 site_type 列
df1_noen <- df1_noen %>% mutate(site_type = "noen")
df2_norm <- df2_norm %>% mutate(site_type = "norm")

# 合并数据框
combined_df_1330 <- bind_rows(df1_noen, df2_norm)

# 查看合并后的结果
head(combined_df_1330)

# 计算不同站点的数量
unique_sites <- unique(combined_df_1330$site_id)  # 替换为实际的站点列名
num_unique_sites <- length(unique_sites)
cat("去掉空值后的结果中有", num_unique_sites, "个不同的站点。\n")     #23个站点

write.csv(combined_df_1330, file = "./AmerifluxData_Analysis/1330_Noen+Normal_results.csv", row.names = FALSE)
aa= read.csv( "D:/01Rawdata/AmerifluxData_Analysis/1330_Noen&Normal_Results.csv")


###################################   (√)01-1-选出2013-2021期间全部年数据的站点  ####################################
#23个站点

# 统计每个站点的数据年份数量，并加入 site_type
site_year_counts <- combined_df_1330 %>%
  group_by(site_id, site_type) %>%
  summarise(year_count = n_distinct(Year), .groups = 'drop')

# 筛选出至少有1年数据的站点
sites_with_one_years <- site_year_counts %>%
  filter(year_count >= 1)

# 通过筛选得到至少有三年数据的站点的 ID                               
valid_sites <- sites_with_one_years$site_id

# 筛选出有效站点的数据并计算变量的年度平均值，同时保留 site_type 和 year_count
average_values <- combined_df_1330 %>%
  filter(site_id %in% valid_sites) %>%
  group_by(site_id, site_type) %>%
  summarise(
    year_count = n_distinct(Year),  # 重新计算年份数量
    RMSE_1_mean = round(mean(RMSE_1, na.rm = TRUE), 2),
    RMSE_1_sd = round(0.15*sd(RMSE_1, na.rm = TRUE), 2),
    R_1_mean = round(mean(R_1, na.rm = TRUE), 2),
    R_1_sd = round(0.15*sd(R_1, na.rm = TRUE), 2),
    RMSE_2_mean = round(mean(RMSE_2, na.rm = TRUE), 2),
    RMSE_2_sd = round(0.15*sd(RMSE_2, na.rm = TRUE), 2),
    R_2_mean = round(mean(R_2, na.rm = TRUE), 2),
    R_2_sd = round(0.15*sd(R_2, na.rm = TRUE), 2),
    days_16_mean = ceiling(mean(days_16, na.rm = TRUE)),
    days_16_sd = round(0.15*sd(days_16, na.rm = TRUE), 2),
    average_diff_21_mean = round(mean(average_diff_21, na.rm = TRUE), 2),
    average_diff_21_sd = round(0.15*sd(average_diff_21, na.rm = TRUE), 2),
    average_diff_26_mean = round(mean(average_diff_26, na.rm = TRUE), 2),
    average_diff_26_sd = round(0.15*sd(average_diff_26, na.rm = TRUE), 2),
    sum_Diff_16_mean = round(mean(sum_Diff_16, na.rm = TRUE), 2),
    sum_Diff_16_sd = round(0.15*sd(sum_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_mean = round(mean(mean_Diff_16, na.rm = TRUE), 2),
    mean_Diff_16_sd = round(0.15*sd(mean_Diff_16, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# 输出结果
print(average_values)


write.csv(average_values, file = "./AmerifluxData_Analysis/1330_Noen+Normal_Results_23.csv", row.names = T)


