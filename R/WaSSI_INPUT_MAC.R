rm(list=ls())
library(caTools)
library(raster)
library(ncdf)
library(plyr)
#setwd("H:/WaSSI")
da<-read.ENVI("/Volumes/RESEARCH/WaSSIC/Data_input/LINSHI/WaSSI_DATA_MJ")
name_tar<-"/Volumes/ELSE/CLIMATE_RESULTS/ENVI/data_tar"
data_tar<-read.ENVI(name_tar)

#---Global variables
nrows<-nrow(da)
ncols<-ncol(da)

S_y<-1982
E_y<-2014
S_y_LAI<-2000
E_y_LAI<-2014
S_y_LC<-2001
E_y_LC<-2012

cell_size<-0.01
S_lat<-33.18482249
E_lat<-S_lat-cell_size*nrows
S_long<-102.36209564
E_long<-S_long+cell_size*ncols

# --- set Cellinfo
DEM<-da[,,1]
VEG<-da[,,6401]
FOREST_LS<-data_tar[,,3]
BASIN<-data_tar[,,1]
BASIN[BASIN<=0]<-NA

range(VEG)
range(FOREST_LS)
range(DEM)
range(BASIN,na.rm = TRUE)

# --- set Soilinfo
SOIL<-da[,,6412:6422]
summary(SOIL)

# --- set LAI data
LAI<-da[,,6647:6468]
summary(LAI)

# --- set LUCC data
LC<-da[,,6400:6411]
# --- set climate data
Pre_1<-da[,,747:424]
Pre_2<-da[,,1575:1516]
Pre_3<-da[,,759:748]
Tem_1<-da[,,375:52]
Tem_2<-da[,,2343:2272]

Pre<-array(0,c(nrows,ncols,33*12))
Pre[,,1:324]<-Pre_1
Pre[,,325:384]<-Pre_2
Pre[,,385:396]<-Pre_3

Temp<-array(0,c(nrows,ncols,33*12))
Temp[,,1:324]<-Tem_1
Temp[,,325:396]<-Tem_2
rm(Tem_1,Tem_2,Pre_1,Pre_2,Pre_3)
#---------------------------------

##---save all original data to Rdata

#Info_year<-data.frame(YEAR=c(1982,2014,2000,2014,2001,2012),INFO=c("Start","End","Start_LAI","End_LAI","Start_LC","End_LC"))
#Info_cell<-data.frame(data=c(33.18482,30.58482,102.3621,104.1821,0.01),INFO=c("LAT_UP","LAT_LOW","LONG_LEFT","LONG_RIGHT","CELL SIZE"))
# list1 = c("BASIN","DEM","FOREST_LS","Info_cell","Info_year","LAI","LC","Pre","SOIL","Temp","VEG")
#DATA_MJ<-list(BASIN=BASIN,DEM=DEM,PRE=Pre,VEG=VEG,TEMP=Temp,SOIL=SOIL,LAI=LAI,LUCC=LC,CELLINFO=Info_cell,YEARINFO=Info_year)

# save(DATA_MJ,file = "In_MJ_array1.RData")

##------set data to each data base

Pre<-as.vector(Pre)/10
Temp<-as.vector(Temp)/10
LAI<-as.vector(LAI)/10
LC<-as.vector(LC)

VEG<-as.vector(VEG)
ALT<-as.vector(DEM)
rm(da)
gc()

#------------------------------

#------------------------------
data_SOIL<-data.frame(ID=c(1:(nrows*ncols)),UZTWM=as.vector(SOIL[,,1]),UZFWM=as.vector(SOIL[,,1+1]),UZK=as.vector(SOIL[,,1+2]),ZPERC=as.vector(SOIL[,,1+3]),REXP=as.vector(SOIL[,,1+4]),LZTWM=as.vector(SOIL[,,1+5]),LZFSM=as.vector(SOIL[,,1+6]),LZFPM=as.vector(SOIL[,,1+7]),LZSK=as.vector(SOIL[,,1+8]),LZPK=as.vector(SOIL[,,1+9]),PFREE=as.vector(SOIL[,,1+10]))
write.csv(data_SOIL,"Inputs/SOILINFO.TXT",row.names = FALSE)
#------------------------------

LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
data_cell<-data.frame(ID=c(1:(nrows*ncols)),LAT=LAT,LONG=LONG,VEG=VEG,ALT=ALT)
write.csv(data_cell,"Inputs/CELLINFO.TXT",row.names = FALSE)
#------------------------------
rm(da)
gc()

Year_C<-rep(c(S_y:E_y), each=nrows*ncols*12)
ID_C<-rep(c(1:(nrows*ncols)),12*(E_y-S_y+1))
Month_C<-rep(rep(c(1:12), each=nrows*ncols),E_y-S_y+1)
data_climate<-data.frame(ID=ID_C,YEAR=Year_C,Month=Month_C,Pre=Pre,Temp=Temp)
data_climate<-arrange(data_climate,ID,YEAR,Month)
write.csv(data_climate,"Inputs/CLIMATE.TXT",row.names = FALSE)
rm(Year_C,ID_C,Month_C,Pre,Temp)
gc()
#------------------------------

Year_LAI<-rep(c(S_y_LAI:E_y_LAI), each=nrows*ncols*12)
ID_LAI<-rep(c(1:(nrows*ncols)),12*(E_y_LAI-S_y_LAI+1))
Month_LAI<-rep(rep(c(1:12), each=nrows*ncols),E_y_LAI-S_y_LAI+1)
data_LAI<-data.frame(ID=ID_LAI,YEAR=Year_LAI,Month=Month_LAI,LAI=LAI)
data_LAI<-arrange(data_LAI,ID,YEAR,Month)
write.csv(data_LAI,"Inputs/LANDLAI.TXT",row.names = FALSE)
rm(Year_LAI,ID_LAI,Month_LAI,LAI)
gc()
#------------------------------

Year_LC<-rep(c(S_y_LC:E_y_LC), each=nrows*ncols)
ID_LC<-rep(c(1:(nrows*ncols)),(E_y_LC-S_y_LC+1))
data_LC<-data.frame(ID=ID_LC,YEAR=Year_LC,VEG=LC)
data_LC<-arrange(data_LC,ID,YEAR)
write.csv(data_LC,"Inputs/VEGINFO.TXT",row.names = FALSE)
rm(Year_LC,ID_LC,LC)

##----out data frame data
# list2 = c("data_cell","data_LC","data_LAI","data_climate","data_SOIL")
# DATA_MJ_INPUTS<-list(CELLINFO=data_cell,LUCC=data_LC,LANDLAI=data_LAI,CLIMATE=data_climate,SOILINFO=data_SOIL)
#  save(DATA_MJ_INPUTS,file = "Inputs_MJ_frame1.RData")

