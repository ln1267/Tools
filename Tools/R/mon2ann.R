# calculate the mean for envi climate data
rm(list =ls())




library(caTools) # this package is for ENVI process
library(trend)



#data_path<-"G:/LAI/global/GLOBMAP/GLOBMAP_LAI_AU_7km_11-82"

#data_path<-"G:/LAI/global/GLOBMAP/GLOBMAP_LAI_AU_7km_11-82"
#data_path<-"L:/LAI/Glass_LAI_AU_5km_12-82"
data_path<-"H:/AU/NDVI_SPOT/NDVI_SPOT_AU_5km"

data_path<-"H:/AU/GLDAS_GW/GLSAS_WaterThickness_AU_01_14"


mydata = read.ENVI(data_path)

rows<-nrow(mydata)
cols<-ncol(mydata)
years<-14
nums<-12

mydata[mydata==32767]<-NA
mean_year<-array(0.0,c(rows,cols,years))

for (m in 1:rows){
  
  for (n in 1:cols){
    
    for (y in 1:years){
      
      start<-(y-1)*nums+1
      end<-nums*y
      year<-mydata[m,n,start:end]
      year[year==0]<-NA
      mean_year[m,n,y]<-mean(year,na.rm=TRUE) 
      
    }
   
    
  }
  
}

mean_year<-mean_year[ , ,years:1]
write.ENVI(mean_year, "E:/GLADS_Ann_01-14")


