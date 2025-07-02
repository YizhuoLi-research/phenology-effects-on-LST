library(dplyr)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")

################################ 01 Forest #######################################
df <- read.csv("./01 Download/06 Robustness_Test_of_Barren_Landtype/001 Download_Data_From_GEE/2021-Barren_33.csv")

# Filter DOY between 1 and 365
df <- df[df$DOY >= 1 & df$DOY <= 365, ]
df$LST <- as.numeric(df$LST)

# Define phenological transition dates
day_1 = 97
day_2 = 118
day_3 = 138
day_4 = 231
day_5 = 265
day_6 = 299

# Select relevant columns and ensure numeric format
df = subset(df, select = c(DOY, LST))
df$year <- "2021"
df$LST = as.numeric(df$LST)
df$DOY = as.numeric(df$DOY)

####################### First fitting:
start <- list(tl = 20, da = 20, st = -0.5 * pi)
lower = c(0, 0, -pi)
upper = c(40, 40, pi)
atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), data = df, start = start,
             lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))

f1 = coef(atc)[1]
f2 = coef(atc)[2]
f3 = coef(atc)[3]
print(atc)

fit_1 <- function(DOY, tl, da, st) { tl + da * sin(2 * pi * DOY / 365 + st) }

# Extract parameters
tl <- as.numeric(f1)
da <- as.numeric(f2)
st <- as.numeric(f3)

# Calculate predicted values and residuals
lst_atc_1 <- na.omit(predict(atc))
lst_raw_1 <- na.omit(df$LST)
ME_1 = round(mean((lst_raw_1 - lst_atc_1)), 2)
R_1 = round(cor(lst_atc_1, lst_raw_1), 2)
print(ME_1); print(R_1)

# Identify the DOY with the maximum of the sine component
DOY_max <- (pi / 2 - st) * 365 / (2 * pi)
LST_max <- fit_1(DOY_max, tl, da, st)

# Print the maximum DOY and corresponding LST
cat("DOY of the LST peak:", round(DOY_max, 2), "\n")
cat("LST at the peak:", round(LST_max, 2), "\n")

# Normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize predicted and observed values
lst_atc_1_normalized <- normalize(lst_atc_1)
lst_raw_1_normalized <- normalize(lst_raw_1)

# Calculate mean error after normalization
ME_1_normalized = round((mean(lst_raw_1_normalized - lst_atc_1_normalized)), 2)
print(ME_1_normalized)

####################### Second fitting:
df2 <- df
df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA  # Set growing season LST to NA

# Fit model using only non-growing season
start <- list(tl2 = 20, da2 = 20)
lower = c(0, 0)
upper = c(40, 40)
atc <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2, start = start,
             lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))

f1 = coef(atc)[1]
f2 = coef(atc)[2]
f3 = coef(atc)[3]
print(atc)

fit_2 <- function(DOY, tl2, da2) { tl2 + da2 * sin(2 * pi * DOY / 365 + st) }

# Extract parameters
tl2 <- as.numeric(f1)
da2 <- as.numeric(f2)

# Calculate residuals and correlation
ME_2 <- format(round(mean(df2$LST - predict(atc, df2), na.rm = TRUE), 2), nsmall = 2)
R_2 <- round(cor(predict(atc, df2), df2$LST, use = "complete.obs"), 2)
print(ME_2); print(R_2)

################ Difference between the two fitted peak LSTs:
# Calculate the peak DOY from the second fitting
DOY_max_2 <- (pi / 2 - st) * 365 / (2 * pi)
LST_max_2 <- fit_2(DOY_max_2, tl2, da2)

# Calculate the difference in peak LST between two fittings
LST_diff <- format(round(LST_max_2 - LST_max, 2), nsmall = 2)

# Output the difference
cat("Difference between two fitted LST peaks:", LST_diff, "\n")


################################ 02 Barren #######################################

################################ 2. line + point plot #######################################
library(ggplot2)
library(gridExtra)

# Copy LST to LST_B
df$LST_B <- df$LST
# df <- df[, !names(df) %in% "LST"]  # Remove original LST column
# df <- df[, !names(df) %in% "LST2"] # Remove original B LST column

g1 <- ggplot(df, aes(x = DOY)) +
  geom_line(aes(y = fit_2(DOY, tl2, da2), color = "Fit2_LST_B"), size = 1.8, linetype = "solid") +
  geom_line(aes(y = fit_1(DOY, tl, da, st), color = "Fit1_LST_B"), size = 1.8, linetype = "solid") +
  geom_point(aes(y = LST_B, color = "LST_B"), size = 1.8, show.legend = FALSE) +  # Hide point legend
  ylab("LST (\u2103)") +
  scale_x_continuous(breaks = seq(1, 365, by = 50)) +
  geom_vline(xintercept = day_1, linetype = "dashed", colour = "#99CC00", size = 1.5) +  # SOS
  geom_vline(xintercept = day_2, linetype = "dashed", colour = "#339900", size = 1.5) +  # MGP
  geom_vline(xintercept = day_3, linetype = "dashed", colour = "#336600", size = 1.5) +  # GMO
  geom_vline(xintercept = day_4, linetype = "dashed", colour = "#cc9933", size = 1.5) +  # GDO
  geom_vline(xintercept = day_5, linetype = "dashed", colour = "#663300", size = 1.5) +  # MSP
  geom_vline(xintercept = day_6, linetype = "dashed", colour = "#000000", size = 1.5) +  # EOS
  # Uncomment below if you want to add phenophase labels
  # geom_text(aes(x = day_1, y = -20, label = "SOS"), vjust = 1, hjust = -0.1, size = 4, color = "#99CC00") +
  # geom_text(aes(x = day_2, y = -20, label = "MGP"), vjust = 1, hjust = -0.1, size = 4, color = "#339900") +
  # geom_text(aes(x = day_3, y = -20, label = "GMO"), vjust = 1, hjust = -0.1, size = 4, color = "#336600") +
  # geom_text(aes(x = day_4, y = -20, label = "GDO"), vjust = 1, hjust = -0.1, size = 4, color = "#cc9933") +
  # geom_text(aes(x = day_5, y = -20, label = "MSP"), vjust = 1, hjust = -0.1, size = 4, color = "#663300") +
  # geom_text(aes(x = day_6, y = -20, label = "EOS"), vjust = 1, hjust = -0.1, size = 4, color = "#000000") +
  ylim(-22, 60) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid")))) +  # Customize line style in legend
  theme(
    plot.title = element_blank(),             # Remove plot title
    axis.title = element_text(size = 29),
    axis.text.x = element_text(size = 29),
    axis.text.y = element_text(size = 29),
    # legend.position = c(0.55, 0.96),         # Uncomment to manually set legend position
    legend.position = "none",                 # Hide legend
    legend.justification = c(0, 1),
    legend.margin = margin(-0.5, 0, 0, 0, "cm"),
    legend.key.size = unit(2.7, "lines"),
    legend.text = element_text(size = 29 , hjust = 0),
    legend.key.height = unit(1, "cm"),
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank(),       # Remove minor grid lines
    legend.title = element_blank()
  ) +
  # Add ME and R annotations
  # annotate("text", x = 270, y = 55, label = paste("ME_1: ", ME_1), size = 5, hjust = 0) +
  # annotate("text", x = 270, y = 50, label = paste("R_1: ", R_1), size = 5, hjust = 0) +
  # annotate("text", x = 296, y = 60, label = sprintf("ME=%s", ME_2), size = 4, hjust = 0) +
  # annotate("text", x = 296, y = 53, label = sprintf("R=%s", R_2), size = 4, hjust = 0) +
  # annotate("text", x = 296, y = 46, label = bquote(Delta * LST[peak] * "= " * .(LST_diff)), size = 4, hjust = 0) +
  annotate("text", x = 1, y = 58, label = paste0("ME = ", ME_2, " ℃"), size = 9.5, hjust = 0) +
  annotate("text", x = 1, y = 49, label = paste0("R = ", R_2), size = 9.5, hjust = 0) +
  annotate("text", x = 1, y = 40, label = bquote(Delta * LST[max] * " = " * .(LST_diff) * " ℃"), size = 9.5, hjust = 0) +
  scale_color_manual(name = "",
                     values = c("Fit1_LST_B" = "#666666",
                                "Fit2_LST_B" = "#FF9900"),
                     labels = c("Fit1_LST_B" = expression(LST[Foliage]),
                                "Fit2_LST_B" = expression(LST[No-foliage]))) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid"))))
# ggtitle("p. Point (45.54, 42.60)")

g1

# Save plot to TIFF
output_path <- "./0.figure/test_only_Barren/Barren_landtype_12.tiff"
# output_path <- "./0.figure/test_only_Barren/Barren_landtype_1-legend.tiff"
ggsave(
  filename = output_path,
  plot = g1,
  width = 7.7,  # Width in inches
  height = 5.5,  # Height in inches
  units = "in",
  dpi = 300  # Resolution in DPI
)

# Print fit metrics
print(ME_1); print(R_1); print(ME_2); print(R_2); print(LST_diff)
