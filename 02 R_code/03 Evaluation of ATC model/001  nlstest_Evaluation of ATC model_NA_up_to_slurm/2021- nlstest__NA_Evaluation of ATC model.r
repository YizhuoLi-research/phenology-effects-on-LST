###### 0. 加载包 ####
library(terra)
library(zoo)
library(minpack.lm)
library(purrr)
library(rasterVis)
library(lattice)
library(raster)
setwd("D:/VegetationImpact")


################### 1.导入数据  ###################

LST = rast( list.files("./01 Download/02 LandSurfaceTemperature_download/NA_LST/2021/",pattern=".tif$",full.names = T) )  #3-363张图

PHE_q = rast( c("./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Increas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Date_Mid_Greenup_Phase__merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Maximum_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Decreas_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Date_Mid_Senescence_Pha_merged.tif",
                "./01 Download/03 PhenologicalEvents_download/NA_PHE/NA_PHE_merged/2021/2021-Onset_Greenness_Minimum_merged.tif") )
PHE_q[PHE_q>365] = NA
PHE_q = ceiling(PHE_q)  #若有小数位进一天，因为在GEE中存在偶数qu中值平均有0.5的情况

check_order <- function(pixels) {
  if (any(is.na(pixels))) {            # 输入中是否有缺失值，如果有，则返回NA向量
    return(rep(NA, 6))                 # 如果没有缺失值再进行比较
  } else {
    if (pixels[6] - pixels[5] < 0 | pixels[5] - pixels[4] < 0 | pixels[4] - pixels[3] < 0 | 
        pixels[3] - pixels[2] < 0 | pixels[2] - pixels[1] < 0) {
      return(rep(NA, 6)) # 不处理边界外数据
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

# ##### 缩小数据量-节省时间的试运行####
# LST = aggregate(LST,10)
# PHE = round(aggregate(PHE,10),0)

##### 2. 数据处理 ####
##### 2.1 取共有像元 ####
sample = LST      
sample[is.finite(sample)] = 1
# plot(sample)

sample2 = PHE
sample2[is.finite(sample2)] = 1

LST = LST * sample[[1]] * sample2[[1]]
PHE = PHE * sample[[1]] * sample2[[1]]
# plot(LST)


################### 2.2 第一次拟合 ###################


#构建一个nlstest，对LST形成一个function
step_1 <-  function(pixels){     #表示某一特定像元的时间序列 3-365，一共361个特定像元
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) ){     ##像元的第一张图（DOY=3）
    return( c(NA,NA,NA,NA,NA,NA,NA) )                                             #这个像元如果是NA或缺省的情况不处理
  }else{                                                     #否则该像元处 形成一个dataframe
    df =  data.frame(as.vector(unlist(pixels)))              ###检查LST[75*40]
    colnames(df) = "LST"                                     #某一像元3-363像元的温度
    
    df$DOY = 2 +( 1:length(pixels) )                         #某一像元3-363的DOY
    start <- list(tl=20,da=20,st=-0.5*pi) #parm
    lower=c(0,0,-pi) #模型下界
    upper=c(40,40,pi) #模型上界
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
    # p_tl = summary(atc)$coef[1,4]  #提取的是p值
    # p_da = summary(atc)$coef[2,4]
    # p_st = summary(atc)$coef[3,4]
    # RMSE <- round(sqrt(mean((residuals(atc))^2)),2)
    lst_atc_1 <- na.omit(predict(atc))   # 计算拟合模型的预测值
    lst_raw_1 <- na.omit(df$LST)
    rmse=round(sqrt(mean((lst_raw_1 - lst_atc_1)^2)),2)
    me=round(mean(lst_raw_1 - lst_atc_1),2)
    rr=round(cor(lst_atc_1,lst_raw_1),2)
    pvalue=round(cor.test(lst_atc_1,lst_raw_1)$p.value,2)
    # slope=round(coef(lm(lst_atc_1~lst_raw_1+0)),2
    
    
    return(c(tl,da,st,rmse,me,rr,pvalue))
  },error =function(e){return( c(NA,NA,NA,NA,NA,NA,NA))})
}


st_1stmodel <- app(LST,step_1)   #7个lyr
# plot(st_1stmodel)
# summary(st_1stmodel)
# levelplot(st_1stmodel, contour = FALSE, margin = FALSE)

####保存pr_test结果
output_folder <- "./NA_Results/0.atc_evaluation/"  # 保存文件的文件夹路径
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}
names_vector <- c("tl","da","st","rmse","me","rr","pvalue")
for (i in 1:7) {
  output_path <- file.path(output_folder, paste0("2021_", names_vector[i], ".tif"))
  writeRaster(st_1stmodel[[i]], filename = output_path, overwrite = TRUE)
}


###### 恢复只有一个st的文件  ###
st_1stmodel = st_1stmodel[[3]]   ######是st值 进入第二次拟合
# plot(st_1stmodel)
LST2 <- LST

################### 2.3 第二次拟合 ###################


# 筛选两个特定时间范围内的值为NA，对PHE形成一个function
NA_create<-  function(pixels){
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) | is.na(pixels[6]) | is.nan( pixels[6] ) | pixels[1] > 300 |  pixels[6] > 365 ){
    return( rep(NA,361) ) #不处理边界外数据
  }else{
    seq = rep(1,361)
    seq[ (as.numeric(unlist(pixels[1])) : as.numeric(unlist(pixels[6])) )-2 ] = NA
    return( seq )
  },error =function(e){return( rep(NA,361) )})
}
NA_createRaster <- app(PHE,NA_create)
LST2 = LST2 * NA_createRaster
LST2

#选择LST的第一个图层用于写入
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
    df2$DOY = 2 +( 1:length(LST2[x]) )   ###设置,把最前面改掉
    
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


writeRaster(tl2,"./NA_Results/0.atc_evaluation/2021_tl2.tif",overwrite=T)
writeRaster(da2,"./NA_Results/0.atc_evaluation/2021_da2.tif",overwrite=T)
writeRaster(rmse_2,"./NA_Results/0.atc_evaluation/2021_rmse_2.tif",overwrite=T)
writeRaster(me_2,"./NA_Results/0.atc_evaluation/2021_me_2.tif",overwrite=T)
writeRaster(rr_2,"./NA_Results/0.atc_evaluation/2021_rr_2.tif",overwrite=T)
writeRaster(pvalue_2,"./NA_Results/0.atc_evaluation/2021_pvalue_2.tif",overwrite=T)