###### 0. Load Packages ####
library(terra)
library(zoo)
library(minpack.lm)
library(purrr)
library(rasterVis)
library(lattice)
library(raster)
setwd("D:/VegetationImpact")


################### 1. Import Data  ###################

LST = rast( list.files("./01 Download/02 LandSurfaceTemperature_download/EA_LST/2013/",pattern=".tif$",full.names = T) )  # 3-363 images

PHE_q = rast( c("./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Increas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Date_Mid_Greenup_Phase__merged.tif",
                "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Maximum_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Decreas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Date_Mid_Senescence_Pha_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/2013/2013-Onset_Greenness_Minimum_merged.tif") )
PHE_q[PHE_q>365] = NA
PHE_q = ceiling(PHE_q)  # If there is a decimal point, round up by one day, as in GEE there might be cases where the median of even values averages to 0.5

check_order <- function(pixels) {
  if (any(is.na(pixels))) {            # Check for missing values; if present, return an NA vector
    return(rep(NA, 6))                 # If there are no missing values, proceed with comparison
  } else {
    if (pixels[6] - pixels[5] < 0 | pixels[5] - pixels[4] < 0 | pixels[4] - pixels[3] < 0 | 
        pixels[3] - pixels[2] < 0 | pixels[2] - pixels[1] < 0) {
      return(rep(NA, 6)) # Do not process data outside the boundary
    } else {
      return(pixels)
    }
  }
}
PHE<- app(PHE_q,check_order)
# plot(PHE_q, breaks = seq(20, 360, length.out = 6))
# plot(PHE, breaks = seq(20, 360, length.out = 6))
summary(PHE_q)
summary(PHE)

# ##### Reduce data size for a quick test run ####
# LST = aggregate(LST,10)
# PHE = round(aggregate(PHE,10),0)

##### 2. Data Processing ####
##### 2.1 Extract Common Pixels ####
sample = LST      
sample[is.finite(sample)] = 1
# plot(sample)

sample2 = PHE
sample2[is.finite(sample2)] = 1

LST = LST * sample[[1]] * sample2[[1]]
PHE = PHE * sample[[1]] * sample2[[1]]
# plot(LST)


################### 2.2 First Fitting ###################

# Build an nlstest and create a function for LST
step_1 <-  function(pixels){     # Represents a time series for a specific pixel, 3-365, a total of 361 specific pixels
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) ){     ## The first image of the pixel (DOY=3)
    return( c(NA,NA,NA,NA,NA,NA,NA) )                                             # If this pixel is NA or missing, skip processing
  }else{                                                     # Otherwise, form a dataframe for this pixel
    df =  data.frame(as.vector(unlist(pixels)))              ### Check LST[75*40]
    colnames(df) = "LST"                                     # Temperature of a pixel from DOY 3-363
    
    df$DOY = 2 +( 1:length(pixels) )                         # DOY of a pixel from 3-363
    start <- list(tl=20,da=20,st=200) # Initial parameter values
    lower=c(0,0,150) # Lower bounds for the model
    upper=c(40,40,250) # Upper bounds for the model
    atc <- nlsLM(LST~tl+da*sin(2*pi*DOY/365+st),data=df,start=start,
                 lower=lower,upper=upper,algorithm = "LM",
                 control = nls.lm.control(maxiter = 50))
    # f1=coef(atc)[1]
    # f2=coef(atc)[2]
    tl = summary(atc)$coef[1,1]
    da = summary(atc)$coef[2,1]
    # x3 = summary(atc)$coef[3,1]
    f3=coef(atc)[3]
    st <- as.numeric(f3)
    # p_tl = summary(atc)$coef[1,4]  # Extract the p-value
    # p_da = summary(atc)$coef[2,4]
    # p_st = summary(atc)$coef[3,4]
    # RMSE <- round(sqrt(mean((residuals(atc))^2)),2)
    lst_atc_1 <- na.omit(predict(atc))   # Calculate the predicted values of the fitted model
    lst_raw_1 <- na.omit(df$LST)
    rmse=round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)),2)
    me=round(mean(lst_raw_1 - lst_atc_1),2)
    rr=round(cor(lst_atc_1,lst_raw_1),2)
    pvalue=round(cor.test(lst_atc_1,lst_raw_1)$p.value,2)
    # slope=round(coef(lm(lst_atc_1~lst_raw_1+0)),2
    
    
    return(c(tl,da,st,rmse,me,rr,pvalue))
  },error =function(e){return( c(NA,NA,NA,NA,NA,NA,NA))})
}


st_1stmodel <- app(LST,step_1)   # 7 layers
# plot(st_1stmodel)
# summary(st_1stmodel)
# levelplot(st_1stmodel, contour = FALSE, margin = FALSE)

#### Save pr_test results
output_folder <- "./EA_Results/0.atc_evaluation/"  # Path to save the files
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}
names_vector <- c("tl","da","st","rmse","me","rr","pvalue")
for (i in 1:7) {
  output_path <- file.path(output_folder, paste0("2013_", names_vector[i], ".tif"))
  writeRaster(st_1stmodel[[i]], filename = output_path, overwrite = TRUE)
}


###### Retain only the st layer ####
st_1stmodel = st_1stmodel[[3]]   ###### This is the st value used for the second fitting
# plot(st_1stmodel)
LST2 <- LST

################### 2.3 Second Fitting ###################

# Filter values within two specific time ranges as NA, and create a function for PHE
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

# Choose the first layer of LST for writing results
tl2 <- LST[[1]]
da2 <- LST[[1]]
rmse_2 <- LST[[1]]
me_2 <- LST[[1]]
rr_2 <- LST[[1]]
pvalue_2 <- LST[[1]]


purrr::walk(1:ncell(LST2),function(x){
  if( is.na(LST2[[1]][x]) ) {
  }else{
    
    df2 = data.frame(as.vector(unlist(LST2[x])))
    colnames(df2) = "LST"
    df2$DOY = 2 +( 1:length(LST2[x]) )  ### Setting, modify the start DOY as needed
    
    st = as.vector(unlist(st_1stmodel[x]))
    start <- list(tl2=20,da2=20)
    lower=c(0,0)
    upper=c(40,40)
    
    atc <- nlsLM(LST~tl2+da2*sin(2*pi*DOY/365+st),data=df2,start=start,
                 lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 50))
    
    LST2[x] <<- predict(atc,df2)
    tl2[x] <<- summary(atc)$coef[1,1]
    da2[x] <<- summary(atc)$coef[2,1]
    rmse_2[x] <<- round(sqrt(mean((df2$LST- predict(atc,df2))^2, na.rm = TRUE)), 2)
    me_2[x] <<- round(mean(df2$LST - predict(atc,df2), na.rm = TRUE), 2)
    rr_2[x] <<- round(cor(predict(atc,df2), df2$LST, use = "complete.obs"), 2)
    pvalue_2[x] <<- round(cor.test(predict(atc,df2), df2$LST)$p.value, 2)
    
  }
},.progress = T)


writeRaster(tl2,"./EA_Results/0.atc_evaluation/2013_tl2.tif",overwrite=T)
writeRaster(da2,"./EA_Results/0.atc_evaluation/2013_da2.tif",overwrite=T)
writeRaster(rmse_2,"./EA_Results/0.atc_evaluation/2013_rmse_2.tif",overwrite=T)
writeRaster(me_2,"./EA_Results/0.atc_evaluation/2013_me_2.tif",overwrite=T)
writeRaster(rr_2,"./EA_Results/0.atc_evaluation/2013_rr_2.tif",overwrite=T)
writeRaster(pvalue_2,"./EA_Results/0.atc_evaluation/2013_pvalue_2.tif",overwrite=T)