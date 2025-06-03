###### 0. Load required packages ####

library(dplyr)
library(zoo)
library(minpack.lm)
library(ggplot2)
library(ggpmisc)

setwd("D:/VegetationImpact")

df <- read.csv(file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_Cloud-LST-RESULTS_sites_years.csv")
head(df)

###################  01 Compare AmeriFlux LST vs. LST on cloud-free days  ################################################

# Select relevant columns
df1 <- df[, c("site_id", "Year", "LST_5mean_post_SOS", "LST_5mean_post_EOS",
              "LST_QC_5mean_post_SOS", "LST_QC_5mean_post_EOS")]
df1 <- na.omit(df1)

# Convert to long format for plotting aesthetics
df_long_1 <- data.frame(
  y = c(df1$LST_QC_5mean_post_SOS, df1$LST_QC_5mean_post_EOS),
  x = c(df1$LST_5mean_post_SOS, df1$LST_5mean_post_EOS),
  group = rep(c("Post-green-up", "Post-dormancy"), each = nrow(df1))
)
df_long_1 <- df_long_1 %>% 
  mutate(group = factor(group, levels = c("Post-green-up", "Post-dormancy")))  # Specify factor levels explicitly
summary(df_long_1)

# Define custom colors
color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")
line_color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")

ticks <- c(0, 10, 20, 30)

library(grid)
# Custom annotation grob for '0' label
zero_label <- annotation_custom(
  grob = textGrob("0", gp = gpar(fontsize = 39)),
  xmin = -1.2, xmax = -1.2,
  ymin = -1.2, ymax = -1.2
)

# Main plot with regression line and labels
p1 <- ggplot(df_long_1, aes(x = x, y = y, color = group)) +
  geom_point(size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2.4) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
    formula = y ~ x, parse = TRUE,
    size = 14, label.x = 0.05, label.y.npc = c(0.95, 0.85),
    coef.digits = 3, coef.keep.zeros = TRUE,
    decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
    f.digits = 2, rr.digits = 2, p.digits = 3,
    format = "f", format.args = list(nsmall = 2)
  ) +
  labs(
    x = expression("LST from full time series (℃)"),
    y = expression("LST on cloud-free days (℃)"),
    color = "",
    title = ""
  ) +
  scale_color_manual(values = line_color_map) +
  scale_fill_manual(values = color_map) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.line = element_line(size = 1.5),
    axis.ticks = element_line(size = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(margin = margin(t = 16)),
    axis.title.y = element_text(margin = margin(r = 16)),
    panel.grid = element_line(linetype = "blank"),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = c(0.81, 0.14),
    legend.title = element_blank(),
    legend.text = element_text(size = 39),
    legend.key.height = unit(2.4, "cm")
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +  # 1:1 line
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(0, 31), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 31), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  geom_segment(data = data.frame(x = ticks), aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_segment(data = data.frame(y = ticks), aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks[ticks != 0]), aes(x = x, y = -1.2, label = x),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(y = ticks[ticks != 0]), aes(x = -1.2, y = y, label = y),
            inherit.aes = FALSE, size = 13) +
  zero_label +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

p1
ggsave(filename = "./0.figure/Reponse_review/FigSS.AmeriFlux_LSTQC_1.tiff",
       plot = p1, width = 15, height = 15, units = "in", dpi = 300)

###################  01-1 (no-label version) ################################################

p11 <- ggplot(df_long_1, aes(x = x, y = y, color = group)) +
  geom_point(size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2.4) +
  # Labels removed for clean version
  labs(
    x = expression("LST from full time series (℃)"),
    y = expression("LST on cloud-free days (℃)"),
    color = "",
    title = ""
  ) +
  scale_color_manual(values = line_color_map) +
  scale_fill_manual(values = color_map) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.line = element_line(size = 1.5),
    axis.ticks = element_line(size = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(margin = margin(t = 16)),
    axis.title.y = element_text(margin = margin(r = 16)),
    panel.grid = element_line(linetype = "blank"),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = c(0.81, 0.14),
    legend.text = element_text(size = 39),
    legend.key.height = unit(2.4, "cm")
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(0, 31), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 31), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  geom_segment(data = data.frame(x = ticks), aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_segment(data = data.frame(y = ticks), aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks[ticks != 0]), aes(x = x, y = -1.2, label = x),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(y = ticks[ticks != 0]), aes(x = -1.2, y = y, label = y),
            inherit.aes = FALSE, size = 13) +
  zero_label +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

p11
ggsave(filename = "./0.figure/Reponse_review/FigSS.AmeriFlux_LSTQC_1-nolabel.tiff",
       plot = p11, width = 15, height = 15, units = "in", dpi = 300)


###################  02 Comparison between TA_meanDiff and LST_meanDiff from AmeriFlux ################################################

head(df)

# Extract relevant columns
df3 <- df[, c("site_id", "Year", "LST_average_diff_1", "LST_average_diff_6",
              "LST_QC_average_diff_1", "LST_QC_average_diff_6")]
df3 <- na.omit(df3)
summary(df3)

# Convert to long format for unified aesthetics
df_long_3 <- data.frame(
  y = c(df3$LST_average_diff_1, df3$LST_average_diff_6),
  x = c(df3$LST_QC_average_diff_1, df3$LST_QC_average_diff_6),
  group = rep(c("Post-green-up", "Post-dormancy"), each = nrow(df3))
)
df_long_3 <- df_long_3 %>% 
  mutate(group = factor(group, levels = c("Post-green-up", "Post-dormancy")))  # Force order
summary(df_long_3)

# Set custom colors
color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")
line_color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")

ticks <- c(-12, -6, 0, 6, 12)

p3 <- ggplot(df_long_3, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
    formula = y ~ x,
    parse = TRUE,
    size = 14,
    label.x = 0.05,
    coef.digits = 2) +  # Force 2 decimal places
  theme_minimal() +
  labs(x = expression("ΔLST from full time series (℃)"),
       y = expression("ΔLST on cloud-free days (℃)"),
       color = "",
       title = "") +
  scale_color_manual(values = line_color_map) +
  scale_fill_manual(values = color_map) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "black") +
  geom_vline(xintercept = 0, linewidth = 1.5, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 16)),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = "none"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +  # 1:1 reference line
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(-13, 13), breaks = ticks) +
  scale_x_continuous(limits = c(-13, 13), breaks = ticks) +
  coord_cartesian(clip = "off") +  # Use (0,0) as coordinate origin
  geom_segment(data = data.frame(x = ticks),
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +  # Custom x-axis ticks
  geom_text(data = data.frame(x = ticks),
            aes(x = x, y = -1.2, label = c("-12","-6","","6","12")),
            inherit.aes = FALSE, size = 13) +
  geom_segment(data = data.frame(y = ticks),
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +  # Custom y-axis ticks
  geom_text(data = data.frame(y = ticks),
            aes(x = -1.2, y = y, label = c("-12","-6","","","12")),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(x = 0.5, y = -1.2), 
            aes(x = x, y = y, label = "0"), 
            inherit.aes = FALSE, 
            size = 13) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

p3

ggsave(filename = "./0.figure/Reponse_review/FigSS.AmeriFlux_LSTQC_3.tiff",
       plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)

###################  02-1 (No-label version) Comparison of TA_meanDiff and LST_meanDiff ################################################

p33 <- ggplot(df_long_3, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  # stat_poly_eq(...)  # Equation and R² labels were disabled intentionally
  theme_minimal() +
  labs(x = expression("ΔLST from full time series (℃)"),
       y = expression("ΔLST on cloud-free days (℃)"),
       color = "",
       title = "") +
  scale_color_manual(values = line_color_map) +
  scale_fill_manual(values = color_map) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "black") +
  geom_vline(xintercept = 0, linewidth = 1.5, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text = element_blank(),  # Hide axis text
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = 53)),  # Distance between x-axis title and axis
    axis.title.y = element_text(margin = margin(r = 30)),  # Distance between y-axis title and axis
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = "none"  # Hide legend
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +  # 1:1 reference line
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(-13, 13), breaks = ticks) +
  scale_x_continuous(limits = c(-13, 13), breaks = ticks) +
  coord_cartesian(clip = "off") +  # Set (0,0) as axis origin, allow overflow
  geom_segment(data = data.frame(x = ticks),  # Custom tick lines along y=0
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks),  # Custom x-axis tick labels
            aes(x = x, y = -1.2, label = c("-12", "-6", "", "6", "12")),
            inherit.aes = FALSE, size = 14) +
  geom_segment(data = data.frame(y = ticks),  # Custom tick lines along x=0
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(y = ticks),  # Custom y-axis tick labels
            aes(x = -1.2, y = y, label = c("-12", "-6", "", "6", "12")),
            inherit.aes = FALSE, size = 14.5) +
  geom_text(data = data.frame(x = 0.5, y = -1.2),  # Add "0" label manually
            aes(x = x, y = y, label = "0"),
            inherit.aes = FALSE,
            size = 14.5) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))  # Set plot margins

p33

ggsave(filename = "./0.figure/Reponse_review/FigSS.AmeriFlux_LSTQC_3-nolabel.tiff",
       plot = p33, width = 15, height = 15, units = "in", dpi = 300)
