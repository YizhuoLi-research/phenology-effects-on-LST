###### 0. 加载包 ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


###########################################    01 对Normal站点数据分析   ################################################


df1_LST_norm <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

head(df1_LST_norm)

# 存储最终结果的数据框
results <- data.frame(
  site_id = character(),
  Year = numeric(),
  RMSE_1 = numeric(), R_1 = numeric(),
  RMSE_2 = numeric(), R_2 = numeric(),LST_maxdiff = numeric(),
  average_diff_21 = numeric(), average_diff_22 = numeric(),
  average_diff_23 = numeric(), average_diff_24 = numeric(),
  average_diff_25 = numeric(), average_diff_26 = numeric(),
  days_12 = numeric(), days_23 = numeric(), days_34 = numeric(),
  days_45 = numeric(), days_56 = numeric(), days_16 = numeric(),
  sum_Diff_12 = numeric(), sum_Diff_23 = numeric(), sum_Diff_34 = numeric(),
  sum_Diff_45 = numeric(), sum_Diff_56 = numeric(), sum_Diff_16 = numeric(),
  mean_diff_12 = numeric(), mean_diff_23 = numeric(), mean_diff_34 = numeric(),
  mean_Diff_45 = numeric(), mean_Diff_56 = numeric(), mean_Diff_16 = numeric(),
  stringsAsFactors = FALSE
)


# 遍历每个站点和年份
site_ids <- unique(df1_LST_norm$SiteID)

for (site_id in site_ids) {

  # site_id <- 	"US-Ton"
  df1_LST_norm_site <- df1_LST_norm[df1_LST_norm$SiteID == site_id, ]
  df1_LST_norm_site$LST <- as.numeric(df1_LST_norm_site$corrected_surfaceTemp_97)
  df1_LST_norm_site$DOY <- as.numeric(df1_LST_norm_site$DOY)
  df1_LST_norm_site$year <- as.numeric(df1_LST_norm_site$year)

  df2_PHE_DOY_site <- df2_PHE_DOY[df2_PHE_DOY$SiteID == site_id, ]
  # df2_PHE_DOY_site$year <- as.numeric(df2_PHE_DOY$year)  # 将df2_PHE_DOY的year列转换为数值类型
  # str(df2_PHE_DOY)
years <- unique(df1_LST_norm_site$year)

  for (year in years) {

    # year <- 2021
    df1_LST_norm_site_year <- df1_LST_norm_site[df1_LST_norm_site$year == year, ]
    df2_PHE_DOY_site_year <- df2_PHE_DOY_site[ df2_PHE_DOY_site$year == year, ]

    # 如果该站点当年有温度没有DOY数据则跳过分析  比如"US-xUK"2017年有温度数据无DOY数据
    if (nrow(df2_PHE_DOY_site_year) == 0) {
      cat("跳过年份:", year, "因为 df2_PHE_DOY_site_year 没有数据\n")
      next  # 跳过该年份的分析
    }

    # 5天滑动平均
    df1_LST_norm_site_year <- df1_LST_norm_site_year %>%
      mutate(LST_smoothed = rollmean(LST, k = 5, fill = NA, align = "center")) %>%
      filter(DOY >= 3 & DOY <= 363) %>%
      select(DOY, LST_smoothed) %>%
      rename(LST = LST_smoothed)



    doy_values <- df2_PHE_DOY_site_year %>%
      filter(SiteID == site_id & year == year) %>%
      select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
             falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
      unlist()

    day_1 <- as.numeric(doy_values["rising_DOY_10"])
    day_2 <- as.numeric(doy_values["rising_DOY_25"])
    day_3 <- as.numeric(doy_values["rising_DOY_50"])
    day_4 <- as.numeric(doy_values["falling_DOY_50"])
    day_5 <- as.numeric(doy_values["falling_DOY_25"])
    day_6 <- as.numeric(doy_values["falling_DOY_10"])

    # 第一次拟合
    start <- list(tl = 20, da = 20, st = -0.5 * pi)
    lower <- c(0, 0, -pi)
    upper <- c(40, 40, pi)
    atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), data = df1_LST_norm_site_year,
                 start = start, lower = lower, upper = upper,
                 algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
    tl <- as.numeric(coef(atc)[1])
    da <- as.numeric(coef(atc)[2])
    st <- as.numeric(coef(atc)[3])
    fit_1 <- function(DOY, tl, da, st) { tl + da * sin(2 * pi * DOY / 365 + st) }

    # #计算第一次的RMSE、R
    lst_atc_1 <- na.omit(predict(atc))   # 计算拟合模型的预测值
    lst_raw_1 <- na.omit(df1_LST_norm_site_year$LST)
    RMSE_1=round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)),2)
    R_1=round(cor(lst_atc_1,lst_raw_1),2)

    # # 找到第一次拟合sin部分的最高值点# 计算最高点对应的LST值
    DOY_max <- (pi/2 - st) * 365 / (2 * pi)
    LST_max <- fit_1(DOY_max, tl, da, st)

    #######################################
    # 第二次拟合:将非生长季节的 LST 置为 NA
    df2 <- df1_LST_norm_site_year
    df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA


    if (all(is.na(df2$LST))) {
      cat("由于站点", site_id, "在年份", year, "的数据全为空值，无法进行进一步分析。\n")
    } else {
      # 继续后续分析代码  #比如 site_id <- 	"US-xML"在2017年

      start2 <- list(tl2 = 20, da2 = 20)
      lower2 <- c(0, 0)
      upper2 <- c(40, 40)
      atc2 <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2,
                    start = start2, lower = lower2, upper = upper2,
                    algorithm = "LM", control = nls.lm.control(maxiter = 10^5))
      tl2 <- as.numeric(coef(atc2)[1])
      da2 <- as.numeric(coef(atc2)[2])
      fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }

      # cat("第一次拟合参数: tl =", tl, ", da =", da, ", st =", st, "\n")
      # cat("第二次拟合参数: tl2 =", tl2, ", da2 =", da2, ", st =", st, "\n")

      # 计算差值和各阶段的累积、平均
      df1_LST_norm_site_year$fit_2_value <- fit_2(df1_LST_norm_site_year$DOY, tl2, da2, st)
      
      #计算差值用 实际值-拟合值
      df1_LST_norm_site_year$LST_diff <- df1_LST_norm_site_year$LST - df1_LST_norm_site_year$fit_2_value
      RMSE_2<- round(sqrt(mean((df2$LST- predict(atc,df2))^2, na.rm = TRUE)), 2)
      R_2 <- round(cor(predict(atc,df2), df2$LST, use = "complete.obs"), 2)

      #两次拟合最高值点计算
      # 第二次拟合的最高值点计算
      DOY_max_2 <- (pi/2 - st) * 365 / (2 * pi)
      # 计算第二次拟合的最高点对应的LST值
      LST_max_2 <- fit_2(DOY_max_2, tl2, da2,st)
      # 计算与第一次拟合的差值（用第二次减去第一次拟合）   #评估模型 用第二次拟合-第一次拟合
      LST_maxdiff <- round(LST_max_2 - LST_max, 2)


      # 计算差值和各阶段的累积、平均
      # 计算阶段平均差值
      average_diff_21 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_1 + 1 & df1_LST_norm_site_year$DOY <= day_1 + 5], na.rm = TRUE), 2)
      average_diff_22 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_2 + 1 & df1_LST_norm_site_year$DOY <= day_2 + 5], na.rm = TRUE), 2)
      average_diff_23 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_3 + 1 & df1_LST_norm_site_year$DOY <= day_3 + 5], na.rm = TRUE), 2)
      average_diff_24 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_4 + 1 & df1_LST_norm_site_year$DOY <= day_4 + 5], na.rm = TRUE), 2)
      average_diff_25 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_5 + 1 & df1_LST_norm_site_year$DOY <= day_5 + 5], na.rm = TRUE), 2)
      average_diff_26 <- round(mean(df1_LST_norm_site_year$LST_diff[df1_LST_norm_site_year$DOY >= day_6 + 1 & df1_LST_norm_site_year$DOY <= day_6 + 5], na.rm = TRUE), 2)

      # 计算阶段累积差值
      sum_diff_12 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_2 - 1)], na.rm = TRUE), 2)
      sum_diff_23 <- round(sum(df1_LST_norm_site_year$LST_diff[day_2:(day_3 - 1)], na.rm = TRUE), 2)
      sum_diff_34 <- round(sum(df1_LST_norm_site_year$LST_diff[day_3:(day_4 - 1)], na.rm = TRUE), 2)
      sum_diff_45 <- round(sum(df1_LST_norm_site_year$LST_diff[day_4:(day_5 - 1)], na.rm = TRUE), 2)
      sum_diff_56 <- round(sum(df1_LST_norm_site_year$LST_diff[day_5:(day_6)], na.rm = TRUE), 2)
      sum_diff_16 <- round(sum(df1_LST_norm_site_year$LST_diff[day_1:(day_6)], na.rm = TRUE), 2)

      # 计算阶段的天数
      days_12 <- day_2 - day_1
      days_23 <- day_3 - day_2
      days_34 <- day_4 - day_3
      days_45 <- day_5 - day_4
      days_56 <- day_6 - day_5 + 1
      days_16 <- day_6 - day_1 + 1

      # 计算阶段的平均差值 已检查
      mean_diff_12 <- round(sum_diff_12 / days_12, 2)
      mean_diff_23 <- round(sum_diff_23 / days_23, 2)
      mean_diff_34 <- round(sum_diff_34 / days_34, 2)
      mean_diff_45 <- round(sum_diff_45 / days_45, 2)
      mean_diff_56 <- round(sum_diff_56 / days_56, 2)
      mean_diff_16 <- round(sum_diff_16 / days_16, 2)
      # 返回结果

      # 保存结果
      results <- rbind(results, data.frame(site_id, Year = year,
                                           RMSE_1 = RMSE_1, R_1 = R_1, RMSE_2 = RMSE_2, R_2 = R_2, LST_maxdiff = LST_maxdiff,
                                           average_diff_21 = average_diff_21, average_diff_22 = average_diff_22, average_diff_23 = average_diff_23,
                                           average_diff_24 = average_diff_24, average_diff_25 = average_diff_25, average_diff_26 = average_diff_26,
                                           days_12 = days_12, days_23 = days_23, days_34 = days_34,
                                           days_45 = days_45, days_56 = days_56, days_16 = days_16,
                                           sum_Diff_12 = sum_diff_12, sum_Diff_23 = sum_diff_23, sum_Diff_34 = sum_diff_34,
                                           sum_Diff_45 = sum_diff_45, sum_Diff_56 = sum_diff_56, sum_Diff_16 = sum_diff_16,
                                           mean_diff_12 = mean_diff_12, mean_diff_23 = mean_diff_23, mean_diff_34 = mean_diff_34,
                                           mean_Diff_45 = mean_diff_45, mean_Diff_56 = mean_diff_56, mean_Diff_16 = mean_diff_16))
      print(results)
    }

  }
}



write.csv(results, file = "./AmerifluxData_Analysis/Normal_results.csv", row.names = FALSE)