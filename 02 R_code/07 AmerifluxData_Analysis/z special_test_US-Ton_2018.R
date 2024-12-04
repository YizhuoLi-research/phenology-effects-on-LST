###### 0. 加载包 ####

library(terra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")
########  单一站点调试 "US-Ton"  2018   #########

df1_LST_norm <- read.csv("./AmerifluxData_Analysis/data1_DB_LST_selected_normal.csv")
df2_PHE_DOY <-  read.csv("./AmerifluxData_Analysis/data2_DB_DOY_selected_sum.csv")

head(df1_LST_norm)
head(df2_PHE_DOY)

df1_LST_norm$LST <- as.numeric(df1_LST_norm$corrected_surfaceTemp_97)
df1_LST_norm$DOY <- as.numeric(df1_LST_norm$DOY)

# 筛选出 SiteID 为 "US-xRN" 且年份为 2017 的数据
df1_LST_norm_Site1 <- df1_LST_norm[df1_LST_norm$SiteID == "US-Ton" & df1_LST_norm$year == "2018", ]

# 5 天滑动平均
df1_LST_norm_Site1 <- df1_LST_norm_Site1 %>%
  arrange(DOY) %>%
  mutate(LST_smoothed = rollmean(LST, k = 5, fill = NA, align = "center"))
# 仅保留 DOY 3 到 363 的 LST 滑动平均数据
df1_LST_norm_Site1 <- df1_LST_norm_Site1 %>%
  filter(DOY >= 3 & DOY <= 363) %>%
  select(DOY, LST_smoothed) %>%
  rename(LST = LST_smoothed)

# 提取物候DOY值
doy_values <- df2_PHE_DOY %>%
  filter(SiteID == "US-Ton", year == 2018) %>%
  select(rising_DOY_10, rising_DOY_25, rising_DOY_50,
         falling_DOY_50, falling_DOY_25, falling_DOY_10) %>%
  unlist()

day_1 <- as.numeric(doy_values["rising_DOY_10"])
day_2 <- as.numeric(doy_values["rising_DOY_25"])
day_3 <- as.numeric(doy_values["rising_DOY_50"])
day_4 <- as.numeric(doy_values["falling_DOY_50"])
day_5 <- as.numeric(doy_values["falling_DOY_25"])
day_6 <- as.numeric(doy_values["falling_DOY_10"])

# 第一次拟合：
start <- list(tl = 20, da = 20, st = -0.5 * pi)
lower <- c(0, 0, -pi)
upper <- c(40, 40, pi)
atc <- nlsLM(LST~tl+da*sin(2*pi*DOY/365+st),data=df1_LST_norm_Site1,start=start,
             lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 10^5))
library(minpack.lm)

# 提取第一次拟合的参数
f1 <- coef(atc)[1]
f2 <- coef(atc)[2]
f3 <- coef(atc)[3]
tl <- as.numeric(f1)
da <- as.numeric(f2)
st <- as.numeric(f3)

# 打印第一次拟合结果
# print(atc)

# 定义第一次拟合的预测函数
fit_1 <- function(DOY, tl, da, st) { tl + da * sin(2 * pi * DOY / 365 + st) }


#计算第一次的RMSE、R
lst_atc_1 <- na.omit(predict(atc))   # 计算拟合模型的预测值
lst_raw_1 <- na.omit(df1_LST_norm_Site1$LST)
RMSE_1=round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)),2)
R_1=round(cor(lst_atc_1,lst_raw_1),2)

# 找到sin部分的最高值点# 计算最高点对应的LST值
DOY_max <- (pi/2 - st) * 365 / (2 * pi)
LST_max <- fit_1(DOY_max, tl, da, st)
# cat("DOY对应的最高点:", round(DOY_max, 2), "\n")
# cat("最高点的LST值:", round(LST_max, 2), "\n")

#########################################
# 第二次拟合：将非生长季节的 LST 置为 NA
df2 <- df1_LST_norm_Site1
df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA

# 第二次拟合参数设置
start <- list(tl2=20,da2=20)
lower=c(0,0)
upper=c(40,40)
atc <- nlsLM(LST~tl2+da2*sin(2*pi*DOY/365+st),data=df2,start=start,
             lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 10^5))
# 提取第二次拟合的参数
f1=coef(atc)[1]
f2=coef(atc)[2]
#f3=coef(atc)[3]
tl2 <- as.numeric(f1)
da2 <- as.numeric(f2)

# 打印第二次拟合结果
print(atc)

# 定义第二次拟合的预测函数
fit_2 <- function(DOY, tl2, da2, st) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }

# 输出拟合的最终参数结果
cat("第一次拟合参数: tl =", tl, ", da =", da, ", st =", st, "\n")
cat("第二次拟合参数: tl2 =", tl2, ", da2 =", da2, ", st =", st, "\n")

#计算第二次的RMSE和R
RMSE_2<- round(sqrt(mean((df2$LST- predict(atc,df2))^2, na.rm = TRUE)), 2)
R_2 <- round(cor(predict(atc,df2), df2$LST, use = "complete.obs"), 2)


#计算差值
df1_LST_norm_Site1$fit_2_value <- fit_2(df1_LST_norm_Site1$DOY, tl2, da2,st)
# df1_LST_norm_Site1$fit_1_value <- fit_1(df1_LST_norm_Site1$DOY, tl, da, st)
#与fit_2相比:  fit value-true value
df1_LST_norm_Site1$LST_diff <- df1_LST_norm_Site1$fit_2_value-df1_LST_norm_Site1$LST
#
# #phenological transition后5天
# average_diff_21 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_1+1 & df1_LST_norm_Site1$DOY <= day_1+5], na.rm = TRUE)
# average_diff_22 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_2+1 & df1_LST_norm_Site1$DOY <= day_2+5], na.rm = TRUE)
# average_diff_23 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_3+1 & df1_LST_norm_Site1$DOY <= day_3+5], na.rm = TRUE)
# average_diff_24 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_4+1 & df1_LST_norm_Site1$DOY <= day_4+5], na.rm = TRUE)
# average_diff_25 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_5+1 & df1_LST_norm_Site1$DOY <= day_5+5], na.rm = TRUE)
# average_diff_26 <- mean(df1_LST_norm_Site1$LST_diff[df1_LST_norm_Site1$DOY >= day_6+1 & df1_LST_norm_Site1$DOY <= day_6+5], na.rm = TRUE)
# #阶段累积
# sum_diff_12 <- sum(df1_LST_norm_Site1$LST_diff[day_1:(day_2-1)],na.rm = T)
# sum_diff_23 <- sum(df1_LST_norm_Site1$LST_diff[day_2:(day_3-1)],na.rm = T)
# sum_diff_34 <- sum(df1_LST_norm_Site1$LST_diff[day_3:(day_4-1)],na.rm = T)
# sum_diff_45 <- sum(df1_LST_norm_Site1$LST_diff[day_4:(day_5-1)],na.rm = T)
# sum_diff_56 <- sum(df1_LST_norm_Site1$LST_diff[day_5:day_6],na.rm = T)
# sum_diff_16 <- sum(df1_LST_norm_Site1$LST_diff[day_1:day_6],na.rm = T)
# #####阶段平均
# # 阶段天数
# days_12 <- day_2 - day_1
# days_23 <- day_3 - day_2
# days_34 <- day_4 - day_3
# days_45 <- day_5 - day_4
# days_56 <- day_6 - day_5
# days_16 <- day_6 - day_1
# # 均值
# if (days_12 > 0) { sum_diff_12_mean <- sum_diff_12 / days_12 } else { sum_diff_12_mean <- NA   }
# if (days_23 > 0) { sum_diff_23_mean <- sum_diff_23 / days_23 } else { sum_diff_23_mean <- NA   }
# if (days_34 > 0) { sum_diff_34_mean <- sum_diff_34 / days_34 } else { sum_diff_34_mean <- NA   }
# if (days_45 > 0) { sum_diff_45_mean <- sum_diff_45 / days_45 } else { sum_diff_45_mean <- NA   }
# if (days_56 > 0) { sum_diff_56_mean <- sum_diff_56 / days_56 } else { sum_diff_56_mean <- NA   }
# if (days_16 > 0) { sum_diff_16_mean <- sum_diff_16 / days_16 } else { sum_diff_16_mean <- NA   }
#
#
#
# # 输出平均值
# cat("sum_diff_12_mean:", sum_diff_12_mean, "\n")
# cat("sum_diff_23_mean:", sum_diff_23_mean, "\n")
# cat("sum_diff_34_mean:", sum_diff_34_mean, "\n")
# cat("sum_diff_45_mean:", sum_diff_45_mean, "\n")
# cat("sum_diff_56_mean:", sum_diff_56_mean, "\n")
# cat("sum_diff_16_mean:", sum_diff_16_mean, "\n")
#
#
# ################两次拟合的高值点的差值：（用第二次减去第一次）
# # 第二次拟合的最高值点计算
# DOY_max_2 <- (pi/2 - st) * 365 / (2 * pi)
#
# # 计算第二次拟合的最高点对应的LST值
# LST_max_2 <- fit_2(DOY_max_2, tl2, da2,st)
# # 计算与第一次拟合的差值（用第二次减去第一次）
# LST_diff <- round(LST_max_2 - LST_max, 2)
#
# # 输出结果
# cat("两次拟合最高点的LST差值:", LST_diff, "\n")
#
#
# # 创建数据框包含所有指定变量
# output_df <- data.frame(
#   SiteID = "US-xRN",
#   year = 2018,
#   average_diff_21 = average_diff_21,
#   average_diff_22 = average_diff_22,
#   average_diff_23 = average_diff_23,
#   average_diff_24 = average_diff_24,
#   average_diff_25 = average_diff_25,
#   average_diff_26 = average_diff_26,
#   days_12 = days_12,
#   days_23 = days_23,
#   days_34 = days_34,
#   days_45 = days_45,
#   days_56 = days_56,
#   days_16 = days_16,
#   RMSE_1 = RMSE_1,
#   R_1 = R_1,
#   RMSE_2 = RMSE_2,
#   R_2 = R_2,
#   LST_diff = sum(df1_LST_norm_Site1$LST_diff, na.rm = TRUE) # 累积LST差值
# )
#
# # 打印数据框
# print(output_df)


# ################################ 5.line+Point_plot #######################################
# library(ggplot2)
# library(gridExtra)
# ########
#
g1 <- ggplot(df1_LST_norm_Site1, aes(x=DOY)) +
  geom_point(aes(y=LST, color="LST"), size=0.7) +
  geom_line(aes(y=fit_2(DOY, tl2, da2,st), color="Fit2_LST"), size=1.2, linetype = "dashed") +
  geom_line(aes(y=fit_1(DOY,tl, da, st), color="Fit1_LST"), size=1.2, linetype=1) +
  
  geom_line(aes(y=LST, color="LST"), size=0.7) +
  ggtitle("SiteID: US-Ton,2018") +
  
  ylab("LST (\u2103)") +
  scale_x_continuous(breaks = seq(1, 365, by = 50))+
  geom_vline(xintercept=day_1, linetype="dashed",colour = "#99CC00",size=1.0)+  #SOS
  geom_vline(xintercept=day_2, linetype="dashed",colour = "#339900",size=1.0)+  #MGP
  geom_vline(xintercept=day_3, linetype="dashed",colour = "#336600",size=1.0)+  #GMO
  geom_vline(xintercept=day_4, linetype="dashed",colour = "#cc9933",size=1.0)+  #GDO
  geom_vline(xintercept=day_5, linetype="dashed",colour = "#663300",size=1.0)+  #MSP
  geom_vline(xintercept=day_6, linetype="dashed",colour = "#000000",size=1.0)+  #EOS
  geom_text(aes(x = day_1, y = -20, label = "SOS"), vjust = 1, hjust = -0.1, size = 4, color = "#99CC00") +
  geom_text(aes(x = day_2, y = -20, label = "Rising_DOY_25"), vjust = 1, hjust = -0.1, size = 4, color = "#339900") +
  geom_text(aes(x = day_3, y = -20, label = "Rising_DOY_50"), vjust = 1, hjust = -0.1, size = 4, color = "#336600") +
  geom_text(aes(x = day_4, y = -20, label = "falling_DOY_50"), vjust = 1, hjust = -0.1, size = 4, color = "#cc9933") +
  geom_text(aes(x = day_5, y = -20, label = "falling_DOY_25"), vjust = 1, hjust = -0.1, size = 4, color = "#663300") +
  geom_text(aes(x = day_6, y = -20, label = "EOS"), vjust = 1, hjust = -0.1, size = 4, color = "#000000") +
  ylim ( -22, 60 )+
  xlim ( 1, 365 )+
  theme_bw()+
  theme(strip.text = element_text(face="bold",size=13,lineheight=5.0), ##AFM
        plot.title = element_text(size = 16, face = "bold"),             ##AFM
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = c(0.02, 0.96),                 #图例位置
        legend.justification = c(0, 1),                  #图例对齐方式
        legend.margin = margin(-0.5, 0, 0, 0, "cm"),     #图例调整空白大小
        legend.key.size = unit(1.2, "lines"),            #图例线的长短
        legend.text = element_text(size = 14 , hjust=0),           #图例文字大小
        legend.key.height = unit(1, "cm"),               #图例调整行距
        legend.title = element_blank())+
  scale_color_manual(name="",
                     values=c("Fit2_LST"="#666666",
                              "Fit1_LST"="#FF9900",
                              "LST"="#FF9900"),  # 设置颜色
                     labels=c("Fit2_LST"=expression(ATC[p] ~ "LST"),
                              "Fit1_LST"=expression(ATC[s] ~ "LST"),
                              "LST"="LST"))

g1
