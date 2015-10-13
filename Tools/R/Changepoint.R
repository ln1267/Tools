# calculate the mean for envi climate data

library(caTools) # this package is for ENVI process
library(trend)

data_path<-"G:/LAI/global/GLOBMAP/GLOBMAP_LAI_AU_7km_11-82"

mydata1 = read.ENVI(data_path)
str(mydata1)

rows<-nrow(mydata1)
cols<-ncol(mydata1)
years<-13
nums<-12
  
change_P<-matrix(0.0,nrow=rows,ncol=cols)
change_tau<-matrix(0.0,nrow=rows,ncol=cols)

for (m in 1:rows){
  
  for (n in 1:cols){
    
    	x<-mydata1[m,n,44:1]

    	mydata<-pettitt.test(x)
	change_tau[m,n]<-1970+as.numeric(mydata$estimate[["probable change point at tau"]])-1
	change_P[m,n]<-+mydata$p.value[1]-1
       
    
  }
  
}
str(change_tau)

write.ENVI(change_tau, "e:/Pre_change_tau")
write.ENVI(change_P, "e:/Pre_change_P")




net_70<-mean_all-mydata1[,,44]
net_00<-mean_00_all-mydata1[,,14]
net_00_01<-mean_00_all-(mydata1[,,14]+mydata1[,,13])/2


write.ENVI(net_70, "e:/pre_net_70")
write.ENVI(net_00, "e:/pre_net_00")
write.ENVI(net_00_01, "e:/pre_net_00_01")

rm(list =ls())



# pettitt.test(x) - change point detection 
#   mydata<-pettitt.test(x)
#   <-mydata$estimate[["probable change point at tau"]]
#   <-mydata$p.value[1]



m<-407
n<-585
x<-mydata1[m,n,44:1]
x[x<0]<-NA
x_na<-na.omit(x)
x_na
mean(x_na)