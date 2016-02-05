nrows<-nrow(data)
ncols<-ncol(data)
S_y<-2002
E_y<-2010
S_y_LAI<-1982
E_y_LAI<-2013
S_y_LC<-2001
E_y_LC<-2012

S_lat<-9.995
cell_size<-0.05
S_long<-112.5

LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)


INFO<-cbind(data_cell,CLIMATE_ZONE=as.vector(CLIMATE_ZONE),GW=as.vector(GW),VEG=as.vector(VEG),LC=as.vector(LC),SOIL)

rm(ALT,cell_size,ID_LC,i,l,Month_C,Month_LAI,E_y,E_y_LAI,E_y_LC,S_lat,S_long,S_soil,S_y,S_y_LAI,S_y_LC,Year_C,Year_LAI,Year_LC,ID_C,ID_LAI,LC,LAI,Pre,Temp,VEG)

CLIMATE_ZONE,da,data,data_cell,data_climate,data_LAI,data_LC,GW,INFO,LAI_GALSS_ann,LAI_GLASS,LAT,LONG,ncols,NDVI_SPOT_ann,NDVI_SPOT_mon,NET_LAI,NET_NDVI,NET_PRE,nrows,PRE_ann

##---,MOD_GEP=c(as.vector(DATA_MOD_GEP_MON),rep(c(0),nrows*ncols*12))

VI_frame<-data.frame(ID=rep(c(1:(nrows*ncols)),12*11),LAT=rep(LAT,12*11),LONG=rep(LONG,12*11),YEAR=rep(c(2000:2010), each=nrows*ncols*12),Month=rep(rep(c(1:12),each=nrows*ncols),11),NDVI_SPOT=as.vector(NDVI_SPOT_mon),LAI_GLASS=as.vector(LAI_GLASS))

library(plyr)
climate<-data_climate
climate$YEAR[climate$YEAR<2000 |climate$YEAR>2010]<-NA
na.omit(climate)
climate<-arrange(climate,YEAR,Month)
LAI<-data_LAI
LAI$YEAR[climate$YEAR<2000 |LAI$YEAR>2010]<-NA
na.omit(LAI)
LAI<-arrange(LAI,YEAR,Month)
VI_frame<-cbind(VI_frame,LAI_BNU=LAI$LAI)

PRE_VEG_mon<-cbind(VI_frame,PRE=climate$Pre,TEMP=climate$Temp)


