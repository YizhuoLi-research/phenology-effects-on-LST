library(dplyr)
library(zoo)
library(minpack.lm)

setwd("D:/VegetationImpact")


############### 1. ATC-estimated LST on top, observed LST on bottom, case of ET-dominated cooling ############
################################ 1-1. nls_analysis #######################################

# Reading the CSV file containing LST data
dataH_1 <- read.csv("./01 Download/For Hypothesis Test/Hypothesis_1.csv")

# Filtering data to include only the range of days from 1 to 365
dataH_1 <- dataH_1[dataH_1$DOY >= 1 & dataH_1$DOY <= 365, ]

# Converting LST to numeric type
dataH_1$LST <- as.numeric(dataH_1$LST)

# Specifying some key days for analysis (e.g., day 97, day 126, etc.)
day_1 = 97
day_2 = 126
day_3 = 160
day_4 = 223
day_5 = 276
day_6 = 329

# Selecting the necessary columns (DOY and LST)
dataH_1 = subset(dataH_1, select = c(DOY,LST))

# Adding a new column 'year' and assigning it the value "2013"
dataH_1$year <- "2013"

# Ensuring LST and DOY are numeric for modeling
dataH_1$LST = as.numeric(dataH_1$LST)
dataH_1$DOY = as.numeric(dataH_1$DOY)

# Defining initial values for the nonlinear least squares model
start <- list(tl=20, da=20, st=-0.5*pi)

# Setting lower and upper bounds for the parameters
lower = c(0, 0, -pi)
upper = c(40, 40, pi)

# Fitting a nonlinear model using the nlsLM function
atc <- nlsLM(LST ~ tl + da * sin(2 * pi * DOY / 365 + st), 
             data = dataH_1, start = start,
             lower = lower, upper = upper, algorithm = "LM",
             control = nls.lm.control(maxiter = 50))

# Extracting the fitted parameter values
f1 = coef(atc)[1]  # tl: baseline temperature
f2 = coef(atc)[2]  # da: amplitude of the temperature variation
f3 = coef(atc)[3]  # st: phase shift of the sine curve

# Printing the model output
print(atc)

# Defining the function to estimate LST based on the fitted parameters
fit_1 <- function(DOY, tl, da, st) {
  tl + da * sin(2 * pi * DOY / 365 + st)
}

# Extracting the coefficients from the model fit
co <- coef(atc)
tl <- as.numeric(f1)  # Assigning the baseline temperature
da <- as.numeric(f2)  # Assigning the amplitude of temperature variation
st <- as.numeric(f3)  # Assigning the phase shift

######################### 1-2.Non-growingSeason nls_analysis ##############################
# Prepare the data for analysis
df2 <- dataH_1
df2$LST[df2$DOY > day_1 & df2$DOY < day_6] <- NA  # Set LST values for the growing season to NA for non-growing season analysis

# Start values for the model parameters
start <- list(tl2=20, da2=20)

# Lower and upper bounds for the parameters
lower = c(0, 0)
upper = c(40, 40)

# Non-linear least squares model fitting (nlsLM)
# This model assumes the form: LST = tl2 + da2 * sin(2 * pi * DOY / 365 + st)
atc <- nlsLM(LST ~ tl2 + da2 * sin(2 * pi * DOY / 365 + st), data = df2, start = start,
             lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))

# Extract coefficients of the fitted model
f1 = coef(atc)[1]
f2 = coef(atc)[2]
f3 = coef(atc)[3]

# Print the model output
print(atc)

# Define the function for the fitted values based on the coefficients
fit_2 <- function(DOY, tl2, da2) {
  tl2 + da2 * sin(2 * pi * DOY / 365 + st)  # Model formula
}

# Extract the model coefficients
co <- coef(atc)
tl2 <- as.numeric(f1)
da2 <- as.numeric(f2)

################################ 1-3.line+Point_plot #######################################
library(ggplot2)
library(gridExtra)

# Calculate the difference between observed values and fitted values
dataH_1$fit_2_value <- fit_2(dataH_1$DOY, tl2, da2) * 0.82 + 1
dataH_1$fit_1_value <- fit_1(dataH_1$DOY, tl, da, st)

# Compute the difference between observed and fitted values
dataH_1$diff_2 <- dataH_1$LST - dataH_1$fit_2_value
dataH_1$diff_1 <- dataH_1$LST - dataH_1$fit_1_value

# Initialize the 'growingseason' column to NA
dataH_1$growingseason <- NA

# Set values in the 'growingseason' column based on the growing season period
dataH_1$growingseason[dataH_1$DOY >= day_1 & dataH_1$DOY <= day_6] <- dataH_1$DOY[dataH_1$DOY >= day_1 & dataH_1$DOY <= day_6]

# Print the updated dataframe
print(dataH_1)

# Plot the results: LST vs. Day of Year with fitted curves and shading for growing season
g1 <- ggplot(dataH_1, aes(x = DOY, y = LST)) +
  geom_line(mapping = aes(y = LST, color = "Original"), size = 0.7) +
  ggtitle("b. Cooling effect") +
  ylab("LST (\u2103)") +
  scale_x_continuous(breaks = seq(1, 365, by = 50)) +
  geom_line(mapping = aes(y = fit_2(DOY, tl2, da2) * 0.82 + 1, color = "Fit2"), size = 1.2, linetype = 1) +  # fit2, added label "Fit2"
  geom_ribbon(aes(x = growingseason,
                  ymin = fit_2(DOY, tl2, da2) * 0.82 + 1 + diff_2,
                  ymax = fit_2(DOY, tl2, da2) * 0.82 + 1,
                  fill = "Shaded Area"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("Shaded Area" = "#CCCCCC"), guide = "none") +
  # Adding vertical lines for different phenological stages
  geom_vline(xintercept = day_1, linetype = "dashed", colour = "#99CC00", size = 1.0) +  # SOS
  geom_vline(xintercept = day_2, linetype = "dashed", colour = "#339900", size = 1.0) +  # MGP
  geom_vline(xintercept = day_3, linetype = "dashed", colour = "#336600", size = 1.0) +  # GMO
  geom_vline(xintercept = day_4, linetype = "dashed", colour = "#cc9933", size = 1.0) +  # GDO
  geom_vline(xintercept = day_5, linetype = "dashed", colour = "#663300", size = 1.0) +  # MSP
  geom_vline(xintercept = day_6, linetype = "dashed", colour = "#000000", size = 1.0) +  # EOS
  
  # Adding text labels for each phenological event
  geom_text(aes(x = day_1, y = -4.5, label = "SOS"), vjust = 1, hjust = -0.1, size = 4, color = "#99CC00") +
  geom_text(aes(x = day_2, y = -4.5, label = "MGP"), vjust = 1, hjust = -0.1, size = 4, color = "#339900") +
  geom_text(aes(x = day_3, y = -4.5, label = "GMO"), vjust = 1, hjust = -0.1, size = 4, color = "#336600") +
  geom_text(aes(x = day_4, y = -4.5, label = "GDO"), vjust = 1, hjust = -0.1, size = 4, color = "#cc9933") +
  geom_text(aes(x = day_5, y = -4.5, label = "MSP"), vjust = 1, hjust = -0.1, size = 4, color = "#663300") +
  geom_text(aes(x = day_6, y = -4.5, label = "EOS"), vjust = 1, hjust = -0.1, size = 4, color = "#000000") +
  ylim(-5, 40) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 13, lineheight = 5.0),
        plot.title = element_text(size = 21, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.margin = margin(-0.5, 0, 0, 0, "cm"),
        legend.key.size = unit(1.2, "lines"),
        legend.text = element_text(size = 14),
        legend.text.align = 0,
        legend.key.height = unit(1, "cm"),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank()) +
  scale_color_manual(name = "",
                     values = c("Original" = "#FF9933", "Fit2" = "#0066CC", "Fit1" = "#CC3300"),
                     labels = c("Original" = expression(LST[Observation]),
                                "Fit2" = expression(LST[ATC])))

# Display the plot
g1


#calculate the difference between the true values and fitted values


g2 <- ggplot(dataH_1, aes(x = DOY, y = LST)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#666666", size = 0.5) +  
  geom_line(aes(y = diff_2, color = "diff_2"), size = 0.8, linetype = 1) +
  ggtitle("d") +
  ylab(expression("\u2206LST" ~ "(\u2103)")) +
  scale_x_continuous(breaks = seq(1, 365, by = 50)) +
  geom_ribbon(aes(x = growingseason,
                  ymin = 0,
                  ymax = diff_2,
                  fill = "Shaded Area"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("Shaded Area" = "#CCCCCC"), guide = "none") +
  geom_vline(xintercept = day_1, linetype = "dashed", colour = "#99CC00", size = 1.0) +  
  geom_vline(xintercept = day_2, linetype = "dashed", colour = "#339900", size = 1.0) +  
  geom_vline(xintercept = day_3, linetype = "dashed", colour = "#336600", size = 1.0) +  
  geom_vline(xintercept = day_4, linetype = "dashed", colour = "#cc9933", size = 1.0) +  
  geom_vline(xintercept = day_5, linetype = "dashed", colour = "#663300", size = 1.0) +  
  geom_vline(xintercept = day_6, linetype = "dashed", colour = "#000000", size = 1.0) +  
  geom_text(aes(x = day_1, y = -13.3, label = "SOS"), vjust = 2.2, hjust = -0.1, size = 4, color = "#99CC00") +
  geom_text(aes(x = day_2, y = -13.3, label = "MGP"), vjust = 2.2, hjust = -0.1, size = 4, color = "#339900") +
  geom_text(aes(x = day_3, y = -13.3, label = "GMO"), vjust = 2.2, hjust = -0.1, size = 4, color = "#336600") +
  geom_text(aes(x = day_4, y = -13.3, label = "GDO"), vjust = 2.2, hjust = -0.1, size = 4, color = "#cc9933") +
  geom_text(aes(x = day_5, y = -13.3, label = "MSP"), vjust = 2.2, hjust = -0.1, size = 4, color = "#663300") +
  geom_text(aes(x = day_6, y = -13.3, label = "EOS"), vjust = 2.2, hjust = -0.1, size = 4, color = "#000000") +
  ylim(-15, 15) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 13, lineheight = 5.0),
        plot.title = element_text(size = 21, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text.align = 0,
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_color_manual(name = "",  
                     values = c(diff_2 = "#0066CC"),
                     labels = c("diff_2" = expression("\u2206LST")))
g2


############### 2. ATC-estimated LST below, observed LST above, Albedo-driven warming situation ############

################################ 2-1. nls_analysis #######################################
# Load the data from a CSV file for hypothesis test 2
dataH_2 <- read.csv("./01 Download/For Hypothesis Test/Hypothesis_2.csv")

# Filter rows where DOY (day of year) is between 1 and 365 (valid days of the year)
dataH_2 <- dataH_2[dataH_2$DOY >= 1 & dataH_2$DOY <= 365, ]

# Ensure LST (Land Surface Temperature) column is numeric
dataH_2$LST <- as.numeric(dataH_2$LST)

# Select only DOY (day of year) and LST columns
dataH_2 = subset(dataH_2, select = c(DOY, LST))

# Set year to 2013
dataH_2$year <- "2013"

# Ensure LST and DOY are numeric
dataH_2$LST = as.numeric(dataH_2$LST)
dataH_2$DOY = as.numeric(dataH_2$DOY)

# Initialize the 'growingseason' column to NA
dataH_2$growingseason <- NA

# Assign values to the 'growingseason' column for days within the growing season
# The condition is that DOY should be between 'day_1' and 'day_6'
dataH_2$growingseason[dataH_2$DOY >= day_1 & dataH_2$DOY <= day_6] <- dataH_2$DOY[dataH_2$DOY >= day_1 & dataH_2$DOY <= day_6]

# Print the updated dataframe to check the changes
print(dataH_2)

# Set the starting parameters for the non-linear least squares (nls) fitting model
start <- list(a=20, b=20)

# Set lower and upper bounds for parameters
lower = c(0, 0)
upper = c(40, 40)

# Perform the nlsLM fitting with sine function to model LST
atc2 <- nlsLM(LST ~ a + b * sin(2 * pi * DOY / 365 + st), data = dataH_2, start = start,
              lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))

# Extract fitted coefficients
ff1 = coef(atc2)[1]
ff2 = coef(atc2)[2]
ff3 = coef(atc2)[3]

# Define the fitted model function
fit_11 <- function(DOY, a, b) { a + b * sin(2 * pi * DOY / 365 + st) }

# Assign the coefficients to variables a and b
a <- as.numeric(ff1)
b <- as.numeric(ff2)

######################### 2-2. Non-growingSeason nls_analysis ##############################
# Filter the data for the non-growing season by setting LST to NA for days outside the growing season
df3 <- dataH_2
df3$LST[df3$DOY > day_1 & df3$DOY < day_6] <- NA

# Set the starting parameters for the non-linear least squares (nls) fitting model for non-growing season
start <- list(a2 = 20, b2 = 20)

# Set lower and upper bounds for parameters for the non-growing season model
lower = c(0, 0)
upper = c(40, 40)

# Perform the nlsLM fitting with sine function to model LST during non-growing season
atc2 <- nlsLM(LST ~ a2 + b2 * sin(2 * pi * DOY / 365 + st), data = df3, start = start,
              lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))

# Extract fitted coefficients for non-growing season
ff1 = coef(atc2)[1]
ff2 = coef(atc2)[2]
ff3 = coef(atc2)[3]

# Print the model results for non-growing season fitting
print(atc2)

# Define the fitted model function for non-growing season
fit_22 <- function(DOY, a2, b2) { a2 + b2 * sin(2 * pi * DOY / 365 + st) }

# Assign the coefficients to variables a2 and b2 for non-growing season
a2 <- as.numeric(ff1)
b2 <- as.numeric(ff2)

################################ 2-3.line+Point_plot #######################################
################################ 2-3.line+Point_plot #######################################
library(ggplot2)
library(gridExtra)

# Calculate the fitted values using fit_2 and fit_11 models and store in new columns
dataH_2$fit_2_value <- fit_2(dataH_1$DOY, tl2, da2)*0.82+1
dataH_2$fit_1_value <- fit_11(dataH_2$DOY, a, b)

# Calculate the differences between observed LST and fitted values
dataH_2$diff_2 <- dataH_2$LST - dataH_2$fit_2_value
dataH_2$diff_1 <- dataH_2$LST - dataH_2$fit_1_value

# Plotting the LST with fitted values and shaded areas for the growing season
g3 <- ggplot(dataH_2, aes(x=DOY, y=LST)) +
  geom_line(mapping=aes(y=LST, color = "Original"), size = 0.7) + # Plot original LST
  ggtitle("a. Warming effect") + # Title of the plot
  ylab("LST (\u2103)") + # Label for the y-axis, with Celsius symbol
  scale_x_continuous(breaks = seq(1, 365, by = 50)) + # x-axis ticks for every 50 days
  geom_line(mapping=aes(y=fit_2(DOY, tl2, da2)*0.82+1 , color="Fit2"), size=1.2, linetype=1) + # Fitted LST curve using fit_2
  # geom_line(mapping=aes(y=fit_11(DOY, a, b), color="Fit1"), size=1.2, linetype=1) + # Fitted LST curve using fit_1 (commented out)
  
  # Add shaded area between observed LST and fit_2 for growing season
  geom_ribbon(aes(x = growingseason,
                  ymin = fit_2(DOY, tl2, da2) * 0.82 + 1 + diff_2,
                  ymax = fit_2(DOY, tl2, da2) * 0.82 + 1,
                  fill = "Shaded Area"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("Shaded Area" = "#CCCCCC"), guide = "none") + # Color for shaded area
  
  # Add vertical lines for phenological events (SOS, MGP, GMO, GDO, MSP, EOS)
  geom_vline(xintercept=day_1, linetype="dashed", colour = "#99CC00", size=1.0) +  # SOS (Start of Season)
  geom_vline(xintercept=day_2, linetype="dashed", colour = "#339900", size=1.0) +  # MGP (Mid-Growing Period)
  geom_vline(xintercept=day_3, linetype="dashed", colour = "#336600", size=1.0) +  # GMO (Growing Mid-Season Onset)
  geom_vline(xintercept=day_4, linetype="dashed", colour = "#cc9933", size=1.0) +  # GDO (Growing Decline Onset)
  geom_vline(xintercept=day_5, linetype="dashed", colour = "#663300", size=1.0) +  # MSP (Mid-Season Peak)
  geom_vline(xintercept=day_6, linetype="dashed", colour = "#000000", size=1.0) +  # EOS (End of Season)
  
  # Add labels for the vertical lines indicating the phenological events
  geom_text(aes(x = day_1, y = -4.5, label = "SOS"), vjust = 1, hjust = -0.1, size = 4, color = "#99CC00") + # SOS label
  geom_text(aes(x = day_2, y = -4.5, label = "MGP"), vjust = 1, hjust = -0.1, size = 4, color = "#339900") + # MGP label
  geom_text(aes(x = day_3, y = -4.5, label = "GMO"), vjust = 1, hjust = -0.1, size = 4, color = "#336600") + # GMO label
  geom_text(aes(x = day_4, y = -4.5, label = "GDO"), vjust = 1, hjust = -0.1, size = 4, color = "#cc9933") + # GDO label
  geom_text(aes(x = day_5, y = -4.5, label = "MSP"), vjust = 1, hjust = -0.1, size = 4, color = "#663300") + # MSP label
  geom_text(aes(x = day_6, y = -4.5, label = "EOS"), vjust = 1, hjust = -0.1, size = 4, color = "#000000") + # EOS label
  
  # Adjust the y-axis limits
  ylim(-5, 40) +
  
  # Apply a clean theme for the plot
  theme_bw() +
  theme(strip.text = element_text(face="bold", size=13, lineheight=5.0), # Bold title for each facet (if applicable)
        plot.title = element_text(size = 21, face = "bold"), # Title size and boldness
        axis.title = element_text(size = 15), # Axis title size
        axis.text.x = element_text(size=14), # x-axis text size
        axis.text.y = element_text(size=14), # y-axis text size
        legend.position = c(0.02, 0.98), # Legend position
        legend.justification = c(0, 1), # Legend alignment
        legend.margin = margin(-0.5, 0, 0, 0, "cm"), # Legend margin adjustment
        legend.key.size = unit(1.2, "lines"), # Legend line size
        legend.text = element_text(size = 14), # Legend text size
        legend.key.height = unit(1, "cm"), # Legend key height
        legend.background = element_rect(fill = "transparent", color = NA), # Transparent legend background
        legend.text.align = 0, # Left-align legend text
        panel.grid = element_blank()) + # Remove grid lines
  
  # Define custom colors for the lines and legend
  scale_color_manual(name="",
                     values=c("Original"="#FF9933", "Fit2"="#0066CC", "Fit1"="#CC3300"),  # Set colors
                     labels = c("Original" = expression(LST[Observation]),  # Label for original LST
                                "Fit2" = expression(LST[ATC])))  # Label for fit2 (model output)

# Print the plot
g3


# Calculate the difference between the true values and fitted values

g4 <- ggplot(dataH_2, aes(x = DOY, y = LST)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#666666", size = 0.5) +  
  geom_line(aes(y = diff_2, color = "diff_2"), size = 0.8, linetype = 1) +
  # geom_line(aes(y = diff_1, color = "diff_1"), size = 0.8, linetype = 1) # Optional second difference line
  
  ggtitle("c") +  # Title of the plot
  # ylab("ΔLST (°C)")  # Y-axis label for the plot (in degrees Celsius)
  ylab(expression("\u2206LST" ~ "(\u2103)")) +  # Y-axis label (in degrees Celsius)
  
  # Set the breaks on the x-axis (days of year)
  scale_x_continuous(breaks = seq(1, 365, by = 50)) +
  
  # Add shaded area for growing season
  geom_ribbon(aes(x = growingseason,
                  ymin = 0,
                  ymax = diff_2,
                  fill = "Shaded Area"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("Shaded Area" = "#CCCCCC"), guide = "none") +  # Customize shading
  
  # Add vertical lines indicating specific phenological events (SOS, MGP, etc.)
  geom_vline(xintercept = day_1, linetype = "dashed", colour = "#99CC00", size = 1.0) +  
  geom_vline(xintercept = day_2, linetype = "dashed", colour = "#339900", size = 1.0) +  
  geom_vline(xintercept = day_3, linetype = "dashed", colour = "#336600", size = 1.0) +  
  geom_vline(xintercept = day_4, linetype = "dashed", colour = "#cc9933", size = 1.0) +  
  geom_vline(xintercept = day_5, linetype = "dashed", colour = "#663300", size = 1.0) +  
  geom_vline(xintercept = day_6, linetype = "dashed", colour = "#000000", size = 1.0) +  
  
  # Add text labels for phenological events (SOS, MGP, etc.)
  geom_text(aes(x = day_1, y = -13.3, label = "SOS"), vjust = 2.2, hjust = -0.1, size = 4, color = "#99CC00") +
  geom_text(aes(x = day_2, y = -13.3, label = "MGP"), vjust = 2.2, hjust = -0.1, size = 4, color = "#339900") +
  geom_text(aes(x = day_3, y = -13.3, label = "GMO"), vjust = 2.2, hjust = -0.1, size = 4, color = "#336600") +
  geom_text(aes(x = day_4, y = -13.3, label = "GDO"), vjust = 2.2, hjust = -0.1, size = 4, color = "#cc9933") +
  geom_text(aes(x = day_5, y = -13.3, label = "MSP"), vjust = 2.2, hjust = -0.1, size = 4, color = "#663300") +
  geom_text(aes(x = day_6, y = -13.3, label = "EOS"), vjust = 2.2, hjust = -0.1, size = 4, color = "#000000") +
  
  # Set the y-axis limits
  ylim(-15, 15) +  
  
  # Customize the theme (background, title, axis text, etc.)
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 13, lineheight = 5.0),
        plot.title = element_text(size = 21, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text.align = 0,  # Align legend text to the left
        panel.grid = element_blank(),  # Remove grid lines
        legend.position = "none") +  # Hide legend
  
  # Customize the line colors and labels
  scale_color_manual(name = "",  
                     values = c("diff_2" = "#0066CC", "diff_1" = "#CC3300"),
                     labels = c("diff_2" = expression("\u2206LST"), 
                                "diff_1" = expression("\u2206LST"[s])))

# Display the plot
g4

# Combine multiple plots into a grid layout
p <- grid.arrange(g3, g1, g4, g2, ncol = 2, nrow = 2)

# Save the combined plot to a TIFF file
output_path <- "./0.figure/Fig.1.tiff"
ggsave(
  filename = output_path,
  plot = p,
  width = 12,  # Plot width in inches
  height = 10,  # Plot height in inches
  units = "in",  # Units of measurement (inches)
  dpi = 300  # Resolution (300 dpi)
)


