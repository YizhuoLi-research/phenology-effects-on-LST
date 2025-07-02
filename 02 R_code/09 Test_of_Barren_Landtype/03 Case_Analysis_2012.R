###### 0. 加载包 ####
library(terra)
library(zoo)
library(minpack.lm)
library(purrr)
library(rasterVis)
library(lattice)
library(raster)
library(stringr)

setwd("D:/VegetationImpact")


###### 0. 拆分波段 ####


# 原始栅格路径
file_path <- "D:/VegetationImpact/0.figure/Reponse_review/download_Sample_MODIS_2012/MODIS_Sample_LST_2012.tif"
raster_stack <- stack(file_path)

# 输出路径
output_dir <- "D:/VegetationImpact/0.figure/Reponse_review/download_Sample_MODIS_2012/2012"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 总波段数
total_bands <- nlayers(raster_stack)

# 遍历并保存
for (i in 1:total_bands) {
  cat(sprintf("正在处理第 %d 个波段，共 %d 个波段...\n", i, total_bands))
  flush.console()
  
  band <- raster_stack[[i]]
  band_name <- names(raster_stack)[i]
  code <- str_extract(band_name, "\\d{3}$")
  
  if (!is.na(code)) {
    output_path <- file.path(output_dir, paste0(code, ".tif"))
    writeRaster(band, output_path, format = "GTiff", overwrite = TRUE)
  } else {
    cat("⚠️ 无法提取波段名后缀:", band_name, "\n")
  }
}



###### 1. 导入数据 ####
LST = rast( list.files("./0.figure/Reponse_review/download_Sample_MODIS_2012/2012/",pattern=".tif$",full.names = T) )  #3-363张图

PHE_q = rast( c("./0.figure/Reponse_review/download_Sample_MODIS_2012/2012-Greenup_1_MODIS.tif",
                "./0.figure/Reponse_review/download_Sample_MODIS_2012/2012-Dormancy_1_MODIS.tif") )
PHE_q[PHE_q > 365 | PHE_q == 0] <- NA
PHE_q = ceiling(PHE_q)  #若有小数位进一天，因为在GEE中存在偶数中值平均有0.5的情况

check_order <- function(pixels) {
  if (any(is.na(pixels))) {            # 输入中是否有缺失值，如果有，则返回NA向量
    return(rep(NA, 2))                 # 如果没有缺失值再进行比较
  } else {
    if (pixels[2] - pixels[1] < 0 ) {
      return(rep(NA, 2)) # 不处理边界外数据
    } else {
      return(pixels)
    }
  }
}
PHE<- app(PHE_q,check_order)
# plot(PHE_q, breaks = seq(20, 360, length.out = 6))
# plot(PHE, breaks = seq(20, 360, length.out = 6))
# summary(PHE_q)
summary(PHE)

##### 2. 数据处理 ####
##### 2.1 取共有像元 ####
sample = LST      
sample[is.finite(sample)] = 1
# plot(sample)

sample2 = PHE
sample2[is.finite(sample2)] = 1

LST = LST * sample[[1]] * sample2[[1]]
PHE = PHE * sample[[1]] * sample2[[1]]

# 计算非 NA 像元数量
non_na_count <- global(!is.na(LST[[1]]), fun = "sum")
print(non_na_count)


##### 2.2 第一次拟合 ####
#构建一个nlstest，对LST形成一个function
step_1 <-  function(pixels){     #表示某一特定像元的时间序列 3-365，一共361个特定像元
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) ){     ##像元的第一张图（DOY=3）
    return( NA )                                             #这个像元如果是NA或缺省的情况不处理
  }else{                                                     #否则该像元处 形成一个dataframe
    df =  data.frame(as.vector(unlist(pixels)))
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
    f3=coef(atc)[3]
    st <- as.numeric(f3)
    return(st)
  },error =function(e){return( NA )})
}

st_1stmodel <- app(LST,step_1)
# plot(st_1stmodel)
summary(st_1stmodel)
levelplot(st_1stmodel, contour = FALSE, margin = FALSE)


LST2 <- LST
##### 2.3 第二次拟合 ####
# 筛选两个特定时间范围内的值为NA，对PHE形成一个function
NA_create<-  function(pixels){
  tryCatch(if( is.na(pixels[1]) | is.nan( pixels[1] ) | is.na(pixels[2]) | is.nan( pixels[2] ) | pixels[1] > 300 |  pixels[2] > 365 ){
    return( rep(NA,361) ) #不处理边界外数据
  }else{
    seq = rep(1,361)
    seq[ (as.numeric(unlist(pixels[1])) : as.numeric(unlist(pixels[2])) )-2 ] = NA   #PHE-2对应LST的第几张影像
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
    df2$DOY = 2 +( 1:length(LST2[x]) )   ###设置,把最前面改掉
    st = as.vector(unlist(st_1stmodel[x]))
    start <- list(tl2=20,da2=20)
    lower=c(0,0)
    upper=c(40,40)
    atc <- nlsLM(LST~tl2+da2*sin(2*pi*DOY/365+st),data=df2,start=start,
                 lower=lower,upper=upper,algorithm = "LM",control = nls.lm.control(maxiter = 50))
    LST2[x] <<- predict(atc,df2)
  }
},.progress = T)

#calculate the difference between the fitted values and true values
# LST_diff <- LST2 - LST
#The case with leaf foliage minus the case without leaf foliage
LST_diff <- LST - LST2   

plot(LST_diff[[1]])   #3-361天的，每天都像元数量是不一样的
# summary(LST_diff[1])


LST_diff[is.nan(LST_diff)]= NA
## 形成文件，以LST第一幅图为cell底板在上面添加差值数据
average_diff_1 <- LST[[1]]
average_diff_6 <- LST[[1]]
sum_diff_16 <- LST[[1]]


for (n in 1:ncell(LST)) {
  cat(paste0(n, "/", ncell(LST), '\n'))
  if( is.na(LST[[1]][n]) ) {
    next
  }else{
    if (is.na(PHE[[1]][n]) | is.na(PHE[[2]][n]) ) {
      next
    } else {
      phe_1 <- as.vector(unlist(PHE[[1]][n]))-2
      phe_6 <- as.vector(unlist(PHE[[2]][n]))-2
      
      if (phe_6 + 5 >= 361){ phe_6 =  361 - 5   }
      
      # 检查索引是否包含NA、NaN
      if (any(is.na(c(phe_1, phe_6)))||any(is.nan(c(phe_1, phe_6)))) {  next     #    略过有两个物候期存在NA的像元的计算
      } else {
        # 进行均值计算
        average_diff_1[n] <- mean(as.vector(unlist(LST_diff[n][(phe_1 + 1):(phe_1 + 5)])), na.rm = TRUE)
        average_diff_6[n] <- mean(as.vector(unlist(LST_diff[n][(phe_6 + 1):(phe_6 + 5)])), na.rm = TRUE)
        sum_diff_16[n] <- sum( as.vector(unlist( LST_diff[n][phe_1:phe_6])),na.rm = T)
        
      }
    }
  }
}


##### 3. 导出文件 ####
# 创建一个非 NA 掩膜：三个栅格都非 NA 的位置(取共有像元进行输出)    ## 1633
common_mask <- !is.na(average_diff_1) & !is.na(average_diff_6) & !is.na(sum_diff_16)
common_pixel_count <- global(common_mask, "sum", na.rm = TRUE)[1, 1]
print(paste("共有非空像元数量为:", common_pixel_count))        

# 应用掩膜
average_diff_1_common <- mask(average_diff_1, common_mask, maskvalues = FALSE)
average_diff_6_common <- mask(average_diff_6, common_mask, maskvalues = FALSE)
sum_diff_16_common    <- mask(sum_diff_16,    common_mask, maskvalues = FALSE)
# 写出结果
out_dir <- "./0.figure/Reponse_review/download_Sample_MODIS_2012/Result_2012"
if (!dir.exists(out_dir)) {  dir.create(out_dir, recursive = TRUE) }
writeRaster(average_diff_1_common, filename = file.path(out_dir, "average_diff_1_common.tif"), overwrite = TRUE)
writeRaster(average_diff_6_common, filename = file.path(out_dir, "average_diff_6_common.tif"), overwrite = TRUE)
writeRaster(sum_diff_16_common, filename = file.path(out_dir, "sum_diff_16_common.tif"), overwrite = TRUE)



# ##### 3.1 简单查看 ####
summary(average_diff_1)
summary(average_diff_6)
summary(sum_diff_16)
plot(sum_diff_16)
