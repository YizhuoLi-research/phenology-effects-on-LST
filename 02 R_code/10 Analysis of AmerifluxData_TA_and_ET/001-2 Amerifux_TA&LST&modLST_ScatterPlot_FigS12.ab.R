###### 0. Load required packages ####

library(dplyr)
library(zoo)
library(minpack.lm)
library(ggplot2)
library(ggpmisc)

setwd("D:/VegetationImpact")

df <- read.csv(file = "D:/VegetationImpact/AmerifluxData_Analysis/Test_for_TA--RESULTS_sites_years.csv")
head(df)

###################  01 AmeriFlux TA VS AmeriFlux LST  ################################################

# Select relevant columns
df1 <- df[, c("site_id", "Year", "AF_5meanTA_post_SOS", "AF_5meanTA_post_EOS",
              "AF_5meanLST_post_SOS", "AF_5meanLST_post_EOS")]
df1 <- na.omit(df1)

# Create long-format data for unified aesthetic mapping
df_long_1 <- data.frame(
  y = c(df1$AF_5meanTA_post_SOS, df1$AF_5meanTA_post_EOS),
  x = c(df1$AF_5meanLST_post_SOS, df1$AF_5meanLST_post_EOS),
  group = rep(c("Post-green-up", "Post-dormancy"), each = nrow(df1))
)
df_long_1 <- df_long_1 %>% 
  mutate(group = factor(group, levels = c("Post-green-up", "Post-dormancy")))  # Force order

# Define custom colors
color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")
line_color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")

ticks <- c(0,10,20,30)

library(grid)
# Add a custom grob for the "0" label
zero_label <- annotation_custom(
  grob = textGrob("0", gp = gpar(fontsize = 39)),  # Adjust font size here
  xmin = -1.2, xmax = -1.2,
  ymin = -1.2, ymax = -1.2
)

p1 <- ggplot(df_long_1, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
    formula = y ~ x, parse = TRUE,
    size = 14, label.x = 0.05, label.y.npc = c(0.95, 0.85),
    coef.digits = 3, coef.keep.zeros = TRUE,
    decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
    f.digits = 2, rr.digits = 2, p.digits = 3,
    format = "f", format.args = list(nsmall = 2)) +
  labs(x = expression("5-day average LST from AmeriFlux (℃)"),
       y = expression("5-day average " ~ T[a] ~ " from AmeriFlux (℃)"),
       color = "",  # Legend title
       title = "") +
  scale_color_manual(values = line_color_map) +
  scale_fill_manual(values = color_map) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 40, face = "bold"),
    axis.title = element_text(size = 42),
    axis.text.x = element_text(size = 40, color = "black"),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.line = element_line(size = 1.5),  # Adjust axis line width
    axis.ticks = element_line(size = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(margin = margin(t = 16)),  # Set x-axis label margin
    axis.title.y = element_text(margin = margin(r = 16)),  # Set y-axis label margin
    panel.grid = element_line(linetype = "blank"),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = c(0.81, 0.14),  # Adjust legend position
    legend.title = element_blank(),
    legend.text = element_text(size = 39),
    legend.key.height = unit(2.4, "cm")  # Increase total legend item height
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +  # 1:1 line
  guides(color = guide_legend(override.aes = list(size = 10))) +  # Only show point colors in legend
  scale_y_continuous(limits = c(0, 30), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 30), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  geom_segment(data = data.frame(x = ticks),  # Add tick marks excluding 0
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_segment(data = data.frame(y = ticks),
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks[ticks != 0]),  # Add axis labels except 0
            aes(x = x, y = -1.2, label = x),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(y = ticks[ticks != 0]),
            aes(x = -1.2, y = y, label = y),
            inherit.aes = FALSE, size = 13) +
  zero_label +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))  # top, right, bottom, left

p1
ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFluxTA_a.tiff",
       plot = p1, width = 15, height = 15, units = "in", dpi = 300)


################### 01-1 (no-label) AmeriFlux TA VS AmeriFlux LST ################################################

p11 <- ggplot(df_long_1, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  # stat_poly_eq(...)  # Commented out: No equation/label on plot
  labs(x = expression("5-day average LST from AmeriFlux (℃)"),
       y = expression("5-day average " ~ T[a] ~ " from AmeriFlux (℃)"),
       color = "",  # Legend title
       title = "") +
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
  scale_y_continuous(limits = c(0, 30), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 30), breaks = ticks, labels = ifelse(ticks == 0, "", ticks), expand = c(0, 0)) +
  geom_segment(data = data.frame(x = ticks),
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_segment(data = data.frame(y = ticks),
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks[ticks != 0]),
            aes(x = x, y = -1.2, label = x),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(y = ticks[ticks != 0]),
            aes(x = -1.2, y = y, label = y),
            inherit.aes = FALSE, size = 13) +
  zero_label +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

p11
ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFluxTA_a-nolabel.tiff",
       plot = p11, width = 15, height = 15, units = "in", dpi = 300)


###################  02 AmeriFlux TA_meanDiff VS AmeriFlux LST_meanDiff ################################################

head(df)

# Extract relevant columns
df2 <- df[, c("site_id", "Year", "AF_TA_average_diff_1", "AF_TA_average_diff_6",
              "AF_LST_average_diff_1", "AF_LST_average_diff_6")]

df2 <- na.omit(df2)
summary(df2)

# Create long-format data for aesthetic mapping
df_long_2 <- data.frame(
  y = c(df2$AF_TA_average_diff_1, df2$AF_TA_average_diff_6),
  x = c(df2$AF_LST_average_diff_1, df2$AF_LST_average_diff_6),
  group = rep(c("Post-green-up", "Post-dormancy"), each = nrow(df2))
)
df_long_2 <- df_long_2 %>% 
  mutate(group = factor(group, levels = c("Post-green-up", "Post-dormancy")))  # Force specific order

# Set color scheme
color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")
line_color_map <- c("Post-green-up" = "#669944", "Post-dormancy" = "#663300")

ticks <- c(-14, -7, 0, 7, 14)

p2 <- ggplot(df_long_2, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")),
    formula = y ~ x,
    parse = TRUE,
    size = 14,
    label.x = 0.05,
    coef.digits = 2) +  # Force display of two decimal places
  theme_minimal() +
  labs(x = expression("ΔLST from AmeriFlux (℃)"),
       y = expression( ΔT[a] ~ " from AmeriFlux (℃)"),
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
    axis.text  = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = 40)),  # Adjust distance between x-axis and title
    axis.title.y = element_text(margin = margin(r = 16)),  # Adjust distance between y-axis and title
    panel.grid = element_line(linetype = "blank"),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = "none"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +  # 1:1 line
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(-14, 14), breaks = ticks) +
  scale_x_continuous(limits = c(-14, 14), breaks = ticks) +
  coord_cartesian(clip = "off") +  # Treat x = 0 and y = 0 as coordinate axes
  geom_segment(data = data.frame(x = ticks),  # Add x-axis ticks at y = 0
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks),  # Manually add x-axis tick labels
            aes(x = x, y = -1.2, label = c("-14","-7","","7","14")),
            inherit.aes = FALSE, size = 13) +
  geom_segment(data = data.frame(y = ticks),  # Add y-axis ticks at x = 0
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(y = ticks),  # Manually add y-axis tick labels
            aes(x = -1.2, y = y, label = c("-14","-7","","7","14")),
            inherit.aes = FALSE, size = 13) +
  geom_text(data = data.frame(x = 0.5, y = -1.2),  # Add "0" label for x = 0
            aes(x = x, y = y, label = "0"), 
            inherit.aes = FALSE, 
            size = 13) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))  # Margins: top, right, bottom, left

p2 

ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFluxTA_a.tiff",
       plot = p2, width = 15, height = 15, units = "in", dpi = 300)

###################  02-2 (no-label) AmeriFlux TA_meanDiff VS AmeriFlux LST_meanDiff ################################################

p22 <- ggplot(df_long_2, aes(x = x, y = y, color = group)) +
  geom_point(aes(color = group), size = 7.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, aes(color = group), linewidth = 2.4) +
  theme_minimal() +
  labs(x = expression("ΔLST from AmeriFlux (℃)"),
       y = expression( ΔT[a] ~ " from AmeriFlux (℃)"),
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
    axis.text  = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = 53)),
    axis.title.y = element_text(margin = margin(r = 30)),
    panel.grid = element_line(linetype = "blank"),
    panel.border = element_rect(color = "black", size = 1.5),
    legend.position = "none"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", linewidth = 1.5) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_y_continuous(limits = c(-14, 14), breaks = ticks) +
  scale_x_continuous(limits = c(-14, 14), breaks = ticks) +
  coord_cartesian(clip = "off") +
  geom_segment(data = data.frame(x = ticks),
               aes(x = x, xend = x, y = 0, yend = -0.4),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(x = ticks),
            aes(x = x, y = -1.2, label = c("-14","-7","","7","14")),
            inherit.aes = FALSE, size = 14) +
  geom_segment(data = data.frame(y = ticks),
               aes(x = 0, xend = -0.4, y = y, yend = y),
               inherit.aes = FALSE, size = 1.5) +
  geom_text(data = data.frame(y = ticks),
            aes(x = -1.2, y = y, label = c("-14","-7","","7","14")),
            inherit.aes = FALSE, size = 14.5) +
  geom_text(data = data.frame(x = 0.5, y = -1.2),
            aes(x = x, y = y, label = "0"), 
            inherit.aes = FALSE, 
            size = 14.5) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))

p22

ggsave(filename = "./0.figure/Reponse_review/FigS.AmeriFluxTA_b-nolabel.tiff",
       plot = p22, width = 15, height = 15, units = "in", dpi = 300)


