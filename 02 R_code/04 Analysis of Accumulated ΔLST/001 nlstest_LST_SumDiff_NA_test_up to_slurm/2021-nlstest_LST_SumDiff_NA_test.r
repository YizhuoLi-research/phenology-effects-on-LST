###### 0. Load Packages ####
library(terra)
library(zoo)
library(minpack.lm)
library(purrr)
library(rasterVis)
library(lattice)
library(raster)
setwd("D:/VegetationImpact")


###### 1. Import Data ####
LST = rast( list.files("./01 Download/02 LandSurfaceTemperature_download/NA_LST/2021/", pattern=".tif$", full.names = T) )  # 3-363 images

PHE_q = rast( c("./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Increas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Date_Mid_Greenup_Phase__merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Maximum_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Decreas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Date_Mid_Senescence_Pha_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Minimum_merged.tif") )
PHE_q[PHE_q > 365] = NA
PHE_q = ceiling(PHE_q)  # If there are decimal places, round up by one day, as in GEE there may be cases with 0.5 due to averaging even numbers

check_order <- function(pixels) {
  if (any(is.na(pixels))) {            # Check if there are any missing values in the input; if yes, return an NA vector
    return(rep(NA, 6))                 # If no missing values, proceed with comparison
  } else {
    if (pixels[6] - pixels[5] < 0 | pixels[5] - pixels[4] < 0 | pixels[4] - pixels[3] < 0 | 
        pixels[3] - pixels[2] < 0 | pixels[2] - pixels[1] < 0) {
      return(rep(NA, 6)) # Exclude data outside the boundary
    } else {
      return(pixels)
    }
  }
}
PHE <- app(PHE_q, check_order)
plot(PHE_q, breaks = seq(20, 360, length.out = 6))
plot(PHE, breaks = seq(20, 360, length.out = 6))
summary(PHE_q)
summary(PHE)


##### Reduce data volume for trial run to save time ####
# LST = aggregate(LST,10)
# PHE = round(aggregate(PHE,10))

##### 2. Data Processing ####
##### 2.1 Extract common pixels ####
sample = LST      
sample[is.finite(sample)] = 1
# plot(sample)

sample2 = PHE
sample2[is.finite(sample2)] = 1

LST = LST * sample[[1]] * sample2[[1]]
PHE = PHE * sample[[1]] * sample2[[1]]

##### 2.2 First fitting ####
# Construct an nlstest to form a function for LST
step_1 <-  function(pixels){     # Represents the time series of a specific pixel from 3-365, a total of 361 specific pixels
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) ){     ## If the first pixel is NA or missing
    return( NA )                                             # Do not process this pixel
  }else{                                                     # Otherwise, create a dataframe for this pixel
    df =  data.frame(as.vector(unlist(pixels)))
    colnames(df) = "LST"                                     # Temperature of a specific pixel from 3-363
    
    df$DOY = 2 +( 1:length(pixels) )                         # DOY for a specific pixel from 3-363
    start <- list(tl=20,da=20,st=-0.5*pi) # Initial parameters
    lower=c(0,0,-pi) # Lower bounds of the model
    upper=c(40,40,pi) # Upper bounds of the model
    atc <- nlsLM(LST~tl+da*sin(2*pi*DOY/365+st),data=df,start=start,
                 lower=lower,upper=upper,algorithm = "LM",
                 control = nls.lm.control(maxiter = 50))
    # f1=coef(atc)[1]
    # f2=coef(atc)[2]
    f3=coef(atc)[3]
    st <- as.numeric(f3)
    return(st)
  },error =function(e){return( NA )})
}

st_1stmodel <- app(LST,step_1)
#plot(st_1stmodel)
levelplot(st_1stmodel, contour = FALSE, margin = FALSE)

LST2 <- LST
##### 2.3 Second fitting ####
# Create NA values within two specific time ranges to form a function for PHE
NA_create<-  function(pixels){
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) | is.na(pixels[6]) | is.nan( pixels[6] ) | pixels[1] > 300 |  pixels[6] > 365 ){
    return( rep(NA,361) ) # Do not process data outside the boundary
  }else{
    seq = rep(1,361)
    seq[ (as.numeric(unlist(pixels[1])) : as.numeric(unlist(pixels[6])) )-2 ] = NA
    return( seq )
  },error =function(e){return( rep(NA,361) )})
}
NA_createRaster <- app(PHE,NA_create)
LST2 = LST2 * NA_createRaster
LST2

purrr::walk(1:ncell(LST2),function(x){
  if( is.na(LST2[[1]][x]) ) {
  }else{
    df2 = data.frame(as.vector(unlist(LST2[x])))
    colnames(df2) = "LST"
    df2$DOY = 2 +( 1:length(LST2[x]) )   ### Setting, modify the very beginning
    st = as.vector(unlist(st_1stmodel[x]))
    start <- list(tl2=20,da2=20)
    lower=c(0,0)
    upper=c(40,40)
    atc <- nlsLM(LST~tl2+da2*sin(2*pi*DOY/365+st),data=df2,start=start,
                 lower=lower,upper=upper,algorithm = "LM",
                 control = nls.lm.control(maxiter = 50))
    LST2[x] <<- predict(atc,df2)
  }
},.progress = T)

# Calculate the difference between the fitted values and true values
LST_diff <- LST - LST2
## Create file, using the first LST image as the base layer to add data

sum_diff_12 <- LST[[1]]
sum_diff_23 <- LST[[1]]
sum_diff_34 <- LST[[1]]
sum_diff_45 <- LST[[1]]
sum_diff_56 <- LST[[1]]

sum_diff_16 <- LST[[1]]

for (n in 1:ncell(LST)) {
  cat(paste0(n, "/", ncell(LST), '\n'))
  if( is.na(LST[[1]][n]) ) {
    next
  }else{
    if (is.na(PHE[[1]][n]) | is.na(PHE[[2]][n]) | is.na(PHE[[3]][n]) | is.na(PHE[[4]][n]) |is.na(PHE[[5]][n]) | is.na(PHE[[6]][n])) {
      next
    } else {
      phe_1 <- as.vector(unlist(PHE[[1]][n]))-2
      phe_2 <- as.vector(unlist(PHE[[2]][n]))-2
      phe_3 <- as.vector(unlist(PHE[[3]][n]))-2
      phe_4 <- as.vector(unlist(PHE[[4]][n]))-2
      phe_5 <- as.vector(unlist(PHE[[5]][n]))-2
      phe_6 <- as.vector(unlist(PHE[[6]][n]))-2
      
      if (phe_6 + 5 >= 361){ phe_6 =  361 - 5   }
      
      # Check if indices contain NA
      if (any(is.na(c(phe_1, phe_2, phe_3, phe_4, phe_5)))) {
        next     # Skip calculation for pixels with NA in the five phenophases
      } else {
        
        # Calculate cumulative values
        sum_diff_12[n] <- sum( as.vector(unlist( LST_diff[n][phe_1:(phe_2-1)])),na.rm = T)
        sum_diff_23[n] <- sum( as.vector(unlist( LST_diff[n][phe_2:(phe_3-1)])),na.rm = T)
        sum_diff_34[n] <- sum( as.vector(unlist( LST_diff[n][phe_3:(phe_4-1)])),na.rm = T)
        sum_diff_45[n] <- sum( as.vector(unlist( LST_diff[n][phe_4:(phe_5-1)])),na.rm = T)
        sum_diff_56[n] <- sum( as.vector(unlist( LST_diff[n][phe_5:phe_6])),na.rm = T)
        sum_diff_16[n] <- sum( as.vector(unlist( LST_diff[n][phe_1:phe_6])),na.rm = T)
        
      }
    }
  }
}



##### Export Files ####
writeRaster(sum_diff_12,"./NA_Results/0diff_result/sum_diff_12_2021.tif",overwrite=T)
writeRaster(sum_diff_23,"./NA_Results/0diff_result/sum_diff_23_2021.tif",overwrite=T)
writeRaster(sum_diff_34,"./NA_Results/0diff_result/sum_diff_34_2021.tif",overwrite=T)
writeRaster(sum_diff_45,"./NA_Results/0diff_result/sum_diff_45_2021.tif",overwrite=T)
writeRaster(sum_diff_56,"./NA_Results/0diff_result/sum_diff_56_2021.tif",overwrite=T)
writeRaster(sum_diff_16,"./NA_Results/0diff_result/sum_diff_16_2021.tif",overwrite=T)






