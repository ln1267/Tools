# calculate the mean for envi climate data

library(caTools) # this package is for ENVI process
library(trend)

data_path<-"H:/AU/AWAP/ENVI/ANN/RUNOFF_AWAP_AU_Ann_5km_13-70"

Pre_ann = read.ENVI(data_path)

tau<-matrix(0.0,nrow=670,ncol=813)
pva<-matrix(0.0,nrow=670,ncol=813)
sg<-matrix(0.0,nrow=670,ncol=813)

for (m in 1:670){
  
  for (n in 1:813){
    
    x<-Pre_ann[m,n,44:1]
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
   
  }
  
}


write.ENVI(tau, "e:/runoff_TAU")
write.ENVI(sg, "e:/runoff_SG")
write.ENVI(pva, "e:/runoff_P")

rm(list =ls())

