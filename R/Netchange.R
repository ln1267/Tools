# calculate the mean for envi climate data
rm(list =ls())




library(caTools) # this package is for ENVI process
library(trend)



data_path<-"G:/LAI/global/GLOBMAP/GLOBMAP_LAI_AU_7km_11-82"

data_path<-"H:/AU/ECV_SM/ECV_SM_AU_25km_13-79"

data_path<-"L:/LAI/Glass_LAI_AU_5km_12-82"

data_path<-"H:/AU/AWAP/ENVI/Month/PRE_AWAP_AU_Mon_5km_13-70"

data_path<-"H:/AU/NDVI_SPOT/NDVI_SPOT_AU_mon_5km_12-99"

data_path<-"E:/Data/NDVI/Timesat_result/Float/Timesat_NDVI_AU_5KM_00-12"

data_path<-"/Volumes/Backup/AU/GLDAS_GW/GLSAS_WaterThickness_AU_01_14"
mydata = read.ENVI(data_path)

rows<-nrow(mydata)
cols<-ncol(mydata)  



mean_all<-matrix(0.0,nrow=rows,ncol=cols)
mean_1<-matrix(0.0,nrow=rows,ncol=cols)
mean_2<-matrix(0.0,nrow=rows,ncol=cols)



for (m in 1:rows){
  
  for (n in 1:cols){
##---- this is for the start year is 2012

     all<-mydata[m,n,25:132]
    data1<-mydata[m,n,1:12]
    data2<-mydata[m,n,133:156]

##---- this is for the start year is 2013    
#     	all<-mydata[m,n,37:144]
#     	data1<-mydata[m,n,145:156]
#     	data2<-mydata[m,n,145:168]

##---- this is for the start year is 2011    
#     	all<-mydata[m,n,13:120]
#     	data1<-mydata[m,n,133:144]
#     	data2<-mydata[m,n,121:144]

##---- this is for the start year is 2000   
# 	    all<-mydata[m,n,25:132]
#     	data1<-mydata[m,n,13:24]
#     	data2<-mydata[m,n,1:24]

	all[all==0]<-NA
	data1[data1==0]<-NA
	data2[data2==0]<-NA

   mean_all[m,n]<-mean(all,na.rm=TRUE) 
   mean_1[m,n]<-mean(data1,na.rm=TRUE)    
    mean_2[m,n]<-mean(data2,na.rm=TRUE)  
   
    
  }
  
}


write.ENVI(mean_all, "E:/result/drought/net/NDVI_NAHH_Mean_02-10")
write.ENVI(mean_1, "E:/result/drought/net/NDVI_NAHH_Mean_00")
write.ENVI(mean_2, "E:/result/drought/net/NDVI_NAHH_Mean_00-01")

#--- This is for net changes

net_00<-(mean_all-mean_2)/mean_2
/mean_all
net_00_01<-(mean_all-mean_2)/mean_all
net_00[net_00 == 0]<-NA
net_00[net_00 > 2]<-NA
write.ENVI(net_00, "E:/result/Drought/net/Net_LAI_GLASS_ratio")
write.ENVI(net_00_01, "E:/result/drought/net/Net_NDVI_spot_09_00-01")

