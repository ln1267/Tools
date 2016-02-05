name_dir<-dir(path="M:/CLIMATE_RESULTS/OUT_FINAL/ENVI/",include.dirs=TRUE,all.files =TRUE,pattern = "ANN",full.names = TRUE)
length(name_dir)
for (i in 1:length(name_dir)){
  
  mapinfo<-"map info = {Geographic Lat/Lon, 1.0000, 1.0000, 102.36199951, 33.18500137, 1.0000000000e-002, 1.0000000000e-002, WGS-84, units=Degrees}"
  coordinate<-'coordinate system string = {GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]}'
  write(mapinfo,file = name_dir[i],append = TRUE)
  write(coordinate,file = name_dir[i],append = TRUE)
  
}

rm(list =ls()) # this is for clear all the data in memory of R
library(caTools)

name_Tmin<-"H:/AU/Climate/AWAP/ENVI/Month/Tmin_AWAP_AU_Mon_5km_13-70"

data_Tmin<-read.ENVI(name_Tmin)
data_Tmin<-data_Tmin*10
storage.mode(data_Tmin) <- "integer"
             
name_Tmax<-"H:/AU/Climate/AWAP/ENVI/Month/Tmax_AWAP_AU_Mon_5km_13-70"
data_Tmax<-read.ENVI(name_Tmax)
data_Tmax<-data_Tmax*10
storage.mode(data_Tmax) <- "integer"
             
name_Pre<-"H:/AU/Climate/AWAP/ENVI/Month/PRE_AWAP_AU_Mon_5km_13-70"
data_Pre<-read.ENVI(name_Pre)
data_Pre[data_Pre<0]<-0
data_Pre<-data_Pre*10000
storage.mode(data_Pre) <- "integer"
n_day<-rep(c(31,30,31,30,31,31,30,31,30,31,28,31),44)  
for (i in 1:528){
  
  data_Pre[,,i]<-data_Pre[,,i]*n_day[i]
  print(i)
}

write.ENVI(data_Pre,"H:/AU/Climate/AWAP/ENVI/Month/Pre_AWAP_AU_Mon_5km_13-70-10")
rm(data_Tmin)
data_Tmax<-(data_Tmax+data_Tmin)/2
write.ENVI(data_Tmax/10,"H:/AU/Climate/AWAP/ENVI/Month/Tmean_AWAP_AU_Mon_5km_13-70")
a<-(1:5)
rep(a,each=2)
aa<-data.frame(a)
ha<-as.vector(a)
write.table(aa,file = "f:/1",row.names = FALSE,col.names = "ID")
write.table()
x<-rep(a,3)
index<-duplicated(data)
d1<-data[!index]
