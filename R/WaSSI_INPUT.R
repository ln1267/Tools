
setwd("H:/WaSSI")
rm(list=ls())
library(caTools)
library(plyr)
da<-read.ENVI("1")


nrows<-nrow(da)
ncols<-ncol(da)
S_y<-1970
E_y<-2013
S_y_LAI<-1982
E_y_LAI<-2013
S_y_LC<-2001
E_y_LC<-2012

S_lat<--25.495
cell_size<-0.05
S_long<-116


Pre<-as.vector(da[,,529:2])/10
Temp<-as.vector(da[,,1057:530])/10
LAI<-as.vector(da[,,1453:1070])/10
LC<-as.vector(da[,,1465:1454])
S_soil<-1466
VEG<-as.vector(da[,,1463])
ALT<-as.vector(da[,,1])
rm(da)
gc()
#------------------------------

Year_C<-rep(c(S_y:E_y), each=nrows*ncols*12)
ID_C<-rep(c(1:(nrows*ncols)),12*(E_y-S_y+1))
Month_C<-rep(rep(c(1:12), each=nrows*ncols),E_y-S_y+1)
data_climate<-data.frame(ID=ID_C,YEAR=Year_C,Month=Month_C,Pre=Pre,Temp=Temp)
data_climate<-arrange(data_climate,ID,YEAR,Month)
write.csv(data_climate,"Inputs/CLIMATE.TXT",sep = ',',row.names = FALSE)
rm(Year_C,ID_C,Month_C,Pre,Temp)
gc()
#------------------------------

Year_LAI<-rep(c(S_y_LAI:E_y_LAI), each=nrows*ncols*12)
ID_LAI<-rep(c(1:(nrows*ncols)),12*(E_y_LAI-S_y_LAI+1))
Month_LAI<-rep(rep(c(1:12), each=nrows*ncols),E_y_LAI-S_y_LAI+1)
data_LAI<-data.frame(ID=ID_LAI,YEAR=Year_LAI,Month=Month_LAI,LAI=LAI)
data_LAI<-arrange(data_LAI,ID,YEAR,Month)
write.csv(data_LAI,"Inputs/LANDLAI.TXT",sep = ',',row.names = FALSE)
rm(Year_LAI,ID_LAI,Month_LAI,LAI)
gc()
#------------------------------

SOIL<-data.frame(ID=c(1:(nrows*ncols)),UZTWM=as.vector(da[,,S_soil]),UZFWM=as.vector(da[,,S_soil+1]),UZK=as.vector(da[,,S_soil+2]),ZPERC=as.vector(da[,,S_soil+3]),REXP=as.vector(da[,,S_soil+4]),LZTWM=as.vector(da[,,S_soil+5]),LZFSM=as.vector(da[,,S_soil+6]),LZFPM=as.vector(da[,,S_soil+7]),LZSK=as.vector(da[,,S_soil+8]),LZPK=as.vector(da[,,S_soil+9]),PFREE=as.vector(da[,,S_soil+10]))
write.csv(SOIL,"Inputs/SOILINFO.TXT",sep = ',',row.names = FALSE)
#------------------------------

LAT<-rep(seq(S_lat, by=cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
data_cell<-data.frame(ID=c(1:(nrows*ncols)),LAT=LAT,LONG=LONG,VEG=VEG,ALT=ALT)
write.csv(data_cell,"Inputs/CELLINFO.TXT",sep = ',',row.names = FALSE)
#------------------------------

Year_LC<-rep(c(S_y_LC:E_y_LC), each=nrows*ncols)
ID_LC<-rep(c(1:(nrows*ncols)),(E_y_LC-S_y_LC+1))
data_LC<-data.frame(ID=ID_LC,YEAR=Year_LC,VEG=LC)
data_LC<-arrange(data_LC,ID,YEAR)
write.csv(data_LC,"Inputs/VEGINFO.TXT",sep = ',',row.names = FALSE)
rm(Year_LC,ID_LC,LC)
