# calculate the mean for envi climate data

rm(list =ls())

library(caTools) # this package is for ENVI process
library(trend)

data_path<-"E:/Data/NDVI/Timesat_result/Float/Timesat_NDVI_AU_5KM_00-12"

data_path<-"E:/result/LAI_GLass_5KM_82-12"
data_path<-"E:/result/LAI_GLOBMAP_7KM_82-11"
data_path<-"E:/result/NDVI_AVHRR_00-12"
data_path<-"E:/result/NDVI_spot_99-12"

mydata = read.ENVI(data_path)

rows<-nrow(mydata)
cols<-ncol(mydata)

tau<-matrix(0.0,nrow=rows,ncol=cols)
pva<-matrix(0.0,nrow=rows,ncol=cols)
sg<-matrix(0.0,nrow=rows,ncol=cols)
changyear<-matrix(0.0,nrow=rows,ncol=cols)
p_change<-matrix(0.0,nrow=rows,ncol=cols)

for (m in 1:rows){
  
  for (n in 1:cols){
    
# ----MK test-------------------------    
    x<-mydata[m,n, ]
    x[x<0]<-NA
    x_na<-na.omit(x)
    
    if (length(x_na)==0){
      
      tau[m,n]<-NA
      pva[m,n]<-NA
      sg[m,n]<-NA
    }
    else{
      
      xtime<-ts(x_na)
      d<-mk.test(xtime)
      tau[m,n]<-d$taug
      pva[m,n]<-d$pvalg
      sg[m,n]<-d$Sg
      
      }
# ---- MK test end-------------------------
      
# --- pettitt.test(x) - change point detection ---- 
    changedata<-pettitt.test(x)
    changyear[m,n]<-1982+as.numeric(changedata$estimate[["probable change point at tau"]])-1
    p_change[m,n]<-changedata$p.value[1]      
# --- end change point detection ----
    
  }
  
}

write.ENVI(tau, "e:/result/LAI_glass_TAU")
write.ENVI(sg, "e:/result/LAI_glass_sg")
write.ENVI(pva, "e:/result/LAI_glass_P")
write.ENVI(changyear, "e:/result/LAI_glass_Y_change")
write.ENVI(p_change, "e:/result/LAI_glass_P_change")




