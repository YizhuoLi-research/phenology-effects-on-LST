###### 0. Load necessary libraries ####
library(terra)
library(zoo)
library(minpack.lm)
library(purrr)
library(rasterVis)
library(lattice)
library(raster)
setwd("D:/VegetationImpact")

###### 1. Import data ####
LST = rast( list.files("./01 Download/02 LandSurfaceTemperature_download/NA_LST/2013/",pattern=".tif$",full.names = T) )  # Load temperature data (3-363 images)

# Load phenological event data
PHE_q = rast( c("./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Onset_Greenness_Increas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Date_Mid_Greenup_Phase__merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Onset_Greenness_Maximum_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Onset_Greenness_Decreas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Date_Mid_Senescence_Pha_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2013/2013-Onset_Greenness_Minimum_merged.tif") )
# Load Phenological Events (PHE) data for various phases of vegetation growth

PHE_q[PHE_q>365] = NA   # Set any values greater than 365 as NA
PHE_q = ceiling(PHE_q)  # Round up phenological events since GEE can produce fractional values (e.g., 0.5), which needs to be rounded to the next day

# Function to check order of phenological events
check_order <- function(pixels) {
  if (any(is.na(pixels))) {            # Check if there are missing values in the input, if yes return NA vector
    return(rep(NA, 6))                 # If missing values exist, skip processing
  } else {
    # Check if the sequence of phenological events is in the correct order
    if (pixels[6] - pixels[5] < 0 | pixels[5] - pixels[4] < 0 | pixels[4] - pixels[3] < 0 | 
        pixels[3] - pixels[2] < 0 | pixels[2] - pixels[1] < 0) {
      return(rep(NA, 6)) # If the sequence is incorrect, return NA
    } else {
      return(pixels)    # Return the pixels if they are in the correct order
    }
  }
}

PHE <- app(PHE_q, check_order)  # Apply the order check function across the entire dataset
plot(PHE_q, breaks = seq(20, 360, length.out = 6))  # Plot phenological event data (raw)
plot(PHE, breaks = seq(20, 360, length.out = 6))    # Plot the cleaned phenological event data
summary(PHE_q)   # Summary of the raw phenological event data
summary(PHE)     # Summary of the cleaned phenological event data

##### 2. Data Processing ####
##### 2.1 Extract common pixels ####
sample = LST      
sample[is.finite(sample)] = 1  # Set finite values in LST as 1

sample2 = PHE
sample2[is.finite(sample2)] = 1  # Set finite values in PHE as 1

# Mask the LST and PHE datasets by the common sample (pixel mask)
LST = LST * sample[[1]] * sample2[[1]]
PHE = PHE * sample[[1]] * sample2[[1]]

##### 2.2 First Fit ####
# Construct an nlsLM model to fit LST data for each pixel
step_1 <-  function(pixels) {  # Function to process the time series for a specific pixel (3-365 DOY, 361 time steps)
  tryCatch(if (is.na(pixels[1]) | is.nan(pixels[1])) {     # If the first value in the time series is NA or NaN
    return(NA)  # If so, return NA for this pixel
  } else {  # Otherwise, create a data frame for the pixel's time series
    df =  data.frame(as.vector(unlist(pixels)))
    colnames(df) = "LST"  # Assign column name as 'LST'
    
    df$DOY = 2 + (1:length(pixels))  # Create a Day-of-Year (DOY) vector starting from 2
    start <- list(tl=20, da=20, st=-0.5*pi)  # Initial parameters for the model
    lower = c(0, 0, -pi)  # Lower bounds for the parameters
    upper = c(40, 40, pi)  # Upper bounds for the parameters
    
    # Fit the model using nlsLM for nonlinear least squares
    atc <- nlsLM(LST ~ tl + da*sin(2*pi*DOY/365 + st), data = df, start = start,
                 lower = lower, upper = upper, algorithm = "LM", 
                 control = nls.lm.control(maxiter = 50))
    
    f3 = coef(atc)[3]  # Extract the third parameter (st) from the model fit
    st <- as.numeric(f3)  # Store the value of st
    return(st)  # Return the fitted parameter value
  }, error = function(e){ return(NA) })  # If an error occurs, return NA
}

# Apply the model to all pixels in the LST data
st_1stmodel <- app(LST, step_1)
# plot(st_1stmodel)  # Optional: visualize the results
summary(st_1stmodel)  # Summary of the phase shift results
levelplot(st_1stmodel, contour = FALSE, margin = FALSE)  # Plot the phase shift results (without contour lines)

LST2 <- LST  # Initialize LST2 for further processing (can be modified later)

##### 2.3 Second Fitting ####
# Define a function to set specific time ranges to NA based on phenological events
NA_create <- function(pixels) {
  tryCatch(
    if (is.na(pixels[1]) | is.nan(pixels[1]) | is.na(pixels[6]) | is.nan(pixels[6]) | pixels[1] > 300 | pixels[6] > 365) {
      return(rep(NA, 361))  # Do not process out-of-boundary data
    } else {
      seq = rep(1, 361)  # Create a sequence of ones
      seq[(as.numeric(unlist(pixels[1])):as.numeric(unlist(pixels[6])))-2] = NA  # Set specific range to NA
      return(seq)
    },
    error = function(e) { return(rep(NA, 361)) }  # Handle errors by returning NA
  )
}

# Apply the NA_create function to the phenological events (PHE) data
NA_createRaster <- app(PHE, NA_create)
LST2 <- LST2 * NA_createRaster  # Mask LST2 values based on the generated NA raster
LST2

# Perform second fitting for each pixel
purrr::walk(1:ncell(LST2), function(x) {
  if (is.na(LST2[[1]][x])) {
    # Skip processing if the pixel is NA
  } else {
    df2 = data.frame(as.vector(unlist(LST2[x])))  # Extract LST2 values for the pixel
    colnames(df2) = "LST"  # Assign column name "LST"
    df2$DOY = 2 + (1:length(LST2[x]))  # Set Day of Year (DOY)
    st = as.vector(unlist(st_1stmodel[x]))  # Extract the "st" parameter from the first model
    start <- list(tl2 = 20, da2 = 20)  # Initial values for parameters
    lower = c(0, 0)  # Lower bounds for parameters
    upper = c(40, 40)  # Upper bounds for parameters
    
    # Perform nonlinear least squares fitting with the fixed "st" parameter
    atc <- nlsLM(LST ~ tl2 + da2*sin(2*pi*DOY/365 + st), data = df2, start = start,
                 lower = lower, upper = upper, algorithm = "LM", control = nls.lm.control(maxiter = 50))
    LST2[x] <<- predict(atc, df2)  # Replace the LST2 pixel with the fitted values
  }
}, .progress = T)


# Calculate the difference between the fitted values (LST2) and the true values (LST)
# LST_diff represents the difference between the case with leaf foliage and the case without leaf foliage
LST_diff <- LST - LST2   

plot(LST_diff)   # Visualize the daily differences from DOY 3 to 361 (different pixel counts each day)

# Replace NaN values with NA
LST_diff[is.nan(LST_diff)] = NA

## Generate a raster file with the difference data, adding it to the first LST image as the base
average_diff_21 <- LST[[1]]
average_diff_22 <- LST[[1]]
average_diff_23 <- LST[[1]]
average_diff_24 <- LST[[1]]
average_diff_25 <- LST[[1]]
average_diff_26 <- LST[[1]]

# Loop over all cells in the LST data to calculate the average differences for each phenological phase
for (n in 1:ncell(LST)) {
  cat(paste0(n, "/", ncell(LST), '\n'))
  if( is.na(LST[[1]][n]) ) {
    next
  }else{
    # Skip if any phenological event data is missing
    if (is.na(PHE[[1]][n]) | is.na(PHE[[2]][n]) | is.na(PHE[[3]][n]) | is.na(PHE[[4]][n]) | is.na(PHE[[5]][n]) | is.na(PHE[[6]][n])) {
      next
    } else {
      # Get the phenological event dates for each pixel
      phe_1 <- as.vector(unlist(PHE[[1]][n]))-2
      phe_2 <- as.vector(unlist(PHE[[2]][n]))-2
      phe_3 <- as.vector(unlist(PHE[[3]][n]))-2
      phe_4 <- as.vector(unlist(PHE[[4]][n]))-2
      phe_5 <- as.vector(unlist(PHE[[5]][n]))-2
      phe_6 <- as.vector(unlist(PHE[[6]][n]))-2
      
      # Adjust for boundary cases where the phenological event is too close to the end of the year
      if (phe_6 + 5 >= 361){ phe_6 =  361 - 5 }
      
      # Check for NA or NaN values in the phenological events
      if (any(is.na(c(phe_1, phe_2, phe_3, phe_4, phe_5, phe_6))) || any(is.nan(c(phe_1, phe_2, phe_3, phe_4, phe_5, phe_6)))) {        
        next     # Skip pixels with NA or NaN values for all phenological events
      } else {
        # Calculate the mean of the differences between LST and LST2 for the 5 days following each phenological event
        average_diff_21[n] <- mean(as.vector(unlist(LST_diff[n][(phe_1 + 1):(phe_1 + 5)])), na.rm = TRUE)
        average_diff_22[n] <- mean(as.vector(unlist(LST_diff[n][(phe_2 + 1):(phe_2 + 5)])), na.rm = TRUE)
        average_diff_23[n] <- mean(as.vector(unlist(LST_diff[n][(phe_3 + 1):(phe_3 + 5)])), na.rm = TRUE)
        average_diff_24[n] <- mean(as.vector(unlist(LST_diff[n][(phe_4 + 1):(phe_4 + 5)])), na.rm = TRUE)
        average_diff_25[n] <- mean(as.vector(unlist(LST_diff[n][(phe_5 + 1):(phe_5 + 5)])), na.rm = TRUE)
        average_diff_26[n] <- mean(as.vector(unlist(LST_diff[n][(phe_6 + 1):(phe_6 + 5)])), na.rm = TRUE)
      }
    }
  }
}

##### 3. Export the Results ####
# Save the difference results as raster files
writeRaster(average_diff_21,"./NA_Results/0.diff_result/average_diff_1_2013.tif",overwrite=T)
writeRaster(average_diff_22,"./NA_Results/0.diff_result/average_diff_2_2013.tif",overwrite=T)
writeRaster(average_diff_23,"./NA_Results/0.diff_result/average_diff_3_2013.tif",overwrite=T)
writeRaster(average_diff_24,"./NA_Results/0.diff_result/average_diff_4_2013.tif",overwrite=T)
writeRaster(average_diff_25,"./NA_Results/0.diff_result/average_diff_5_2013.tif",overwrite=T)
writeRaster(average_diff_26,"./NA_Results/0.diff_result/average_diff_6_2013.tif",overwrite=T)

##### 3.1 Visualize Results ####
plot(average_diff_21)
plot(average_diff_22)
plot(average_diff_23)
plot(average_diff_24)
plot(average_diff_25)
plot(average_diff_26)