#---backup all original data to "DATA_ori_list.RData"
DATA_ori_list<-list(WASSI=DATA_MJ,MOD_DSI=DATA_MOD_DSI,MOD_GEP_MON=DATA_MOD_GEP_MON,MOD_ET_MON=DATA_MOD_ET_MON,DNVI=DATA_NDVI,TAR=DATA_TAR,MODIS_ANN=MODIS_ANN)

## creat data frame for WaSSI model input
library(caTools)
library(ncdf)
library(plyr)

#---Global variables
nrows<-nrow(DATA_MJ)
ncols<-ncol(DATA_MJ)

S_y<-2000
E_y<-2014
S_y_LAI<-2000
E_y_LAI<-2014
S_y_LC<-2001
E_y_LC<-2012

cell_size<-0.01
S_lat<-33.185
E_lat<-S_lat-cell_size*nrows
S_long<-102.362
E_long<-S_long+cell_size*ncols

# --- set Cellinfo
DEM<-DATA_TAR[,,2]
VEG<-DATA_TAR[,,5]
FOREST_LS<-DATA_TAR[,,3]
BASIN<-DATA_TAR[,,1]
BASIN[BASIN<=0]<-0

range(VEG)
range(FOREST_LS)
range(DEM)
range(BASIN,na.rm = TRUE)

# --- set Soilinfo
SOIL<-DATA_MJ[,,6412:6422]
summary(SOIL)

# --- set LAI data
LAI<-DATA_MJ[,,6647:6468]
summary(LAI)

# --- set LUCC data
LC<-DATA_MJ[,,6400:6411]
# --- set climate data
Pre<-DATA_MJ[,,927:748]
Temp<-DATA_MJ[,,2451:2272]

Pre_45<-DATA_MJ[,,5104:5535]
Pre_85<-DATA_MJ[,,5968:6399]
Temp_45<-DATA_MJ[,,4672:5103]
Temp_85<-DATA_MJ[,,5536:5967]


# Pre_1<-DATA_MJ[,,747:424]
# Pre_2<-DATA_MJ[,,1575:1516]
# Pre_3<-DATA_MJ[,,759:748]
# Tem_1<-DATA_MJ[,,375:52]
# Tem_2<-DATA_MJ[,,2343:2272]

# Pre<-array(0,c(nrows,ncols,33*12))
# Pre[,,1:324]<-Pre_1
# Pre[,,325:384]<-Pre_2
# Pre[,,385:396]<-Pre_3

# Temp<-array(0,c(nrows,ncols,33*12))
# Temp[,,1:324]<-Tem_1
# Temp[,,325:396]<-Tem_2
# rm(Tem_1,Tem_2,Pre_1,Pre_2,Pre_3)
#---------------------------------

##---save all original data to Rdata

#Info_year<-data.frame(YEAR=c(1982,2014,2000,2014,2001,2012),INFO=c("Start","End","Start_LAI","End_LAI","Start_LC","End_LC"))
#Info_cell<-data.frame(data=c(33.18482,30.58482,102.3621,104.1821,0.01),INFO=c("LAT_UP","LAT_LOW","LONG_LEFT","LONG_RIGHT","CELL SIZE"))
# list1 = c("BASIN","DEM","Info_cell","Info_year","LAI","LC","Pre","SOIL","Temp","VEG")
#DATA_MJ<-list(BASIN=BASIN,DEM=DEM,PRE=Pre,VEG=VEG,TEMP=Temp,SOIL=SOIL,LAI=LAI,LUCC=LC,CELLINFO=Info_cell,YEARINFO=Info_year)

# save(DATA_MJ,file = "In_MJ_array1.RData")

##------set data to each data base
Pre[Pre<0]<-0
Pre<-Pre/10
Temp<-temp/10
LAI<-LAI/10
Pre<-as.vector(Pre)
Temp<-as.vector(Temp)
LAI<-as.vector(LAI)
LC<-as.vector(LC)
VEG<-as.vector(VEG)
ALT<-as.vector(DEM)
BASIN<-as.vector(BASIN)
rm(da)
gc()

#------------------------------

#------------------------------
data_SOIL<-data.frame(ID=c(1:(nrows*ncols)),UZTWM=as.vector(SOIL[,,1]),UZFWM=as.vector(SOIL[,,1+1]),UZK=as.vector(SOIL[,,1+2]),ZPERC=as.vector(SOIL[,,1+3]),REXP=as.vector(SOIL[,,1+4]),LZTWM=as.vector(SOIL[,,1+5]),LZFSM=as.vector(SOIL[,,1+6]),LZFPM=as.vector(SOIL[,,1+7]),LZSK=as.vector(SOIL[,,1+8]),LZPK=as.vector(SOIL[,,1+9]),PFREE=as.vector(SOIL[,,1+10]))
write.csv(data_SOIL,"INPUTS/SOILINFO.TXT",row.names = FALSE)
#------------------------------

LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
data_cell<-data.frame(ID=c(1:(nrows*ncols)),LAT=LAT,LONG=LONG,VEG=VEG,ALT=ALT)
write.csv(data_cell,"INPUTS/CELLINFO.TXT",row.names = FALSE)
#------------------------------
rm(da)
gc()

Year_C<-rep(c(S_y:E_y), each=nrows*ncols*12)
ID_C<-rep(c(1:(nrows*ncols)),12*(E_y-S_y+1))
Month_C<-rep(rep(c(1:12), each=nrows*ncols),E_y-S_y+1)
data_climate<-data.frame(ID=ID_C,YEAR=Year_C,Month=Month_C,Pre=Pre,Temp=Temp)
data_climate<-arrange(data_climate,ID,YEAR,Month)
write.csv(data_climate,"INPUTS/CLIMATE.TXT",row.names = FALSE)
rm(Year_C,ID_C,Month_C,Pre,Temp)
gc()
#------------------------------

Year_LAI<-rep(c(S_y_LAI:E_y_LAI), each=nrows*ncols*12)
ID_LAI<-rep(c(1:(nrows*ncols)),12*(E_y_LAI-S_y_LAI+1))
Month_LAI<-rep(rep(c(1:12), each=nrows*ncols),E_y_LAI-S_y_LAI+1)
data_LAI<-data.frame(ID=ID_LAI,YEAR=Year_LAI,Month=Month_LAI,LAI=LAI)
data_LAI<-arrange(data_LAI,ID,YEAR,Month)
write.csv(data_LAI,"INPUTS/LANDLAI.TXT",row.names = FALSE)
rm(Year_LAI,ID_LAI,Month_LAI,LAI)
gc()
#------------------------------

Year_LC<-rep(c(S_y_LC:E_y_LC), each=nrows*ncols)
ID_LC<-rep(c(1:(nrows*ncols)),(E_y_LC-S_y_LC+1))
data_LC<-data.frame(ID=ID_LC,YEAR=Year_LC,VEG=LC)
data_LC<-arrange(data_LC,ID,YEAR)
write.csv(data_LC,"INPUTS/VEGINFO.TXT",row.names = FALSE)
rm(Year_LC,ID_LC,LC)

#------------------------------
#Future climate data ------2015-2050
future_climate_frame<-data.frame(ID=rep(c(1:47320),36*12),YEAR=rep(c(2015:2050), each=nrows*ncols*12),Month=rep(rep(c(1:12),each=nrows*ncols),36),Pre_45=as.vector(Pre_45),Temp_45=as.vector(Temp_45),Pre_85=as.vector(Pre_85),Temp_85=as.vector(Temp_85))
future_climate_frame<-arrange(future_climate_frame,ID,YEAR,Month)


#------------------------------
#MODIS ET and GEP data monthly ------2000-2011
DATA_MOD_ET_MON<-DATA_MOD_ET_MON[,,180:1] #ET 2000-2014
DATA_MOD_GEP_MON<-DATA_MOD_GEP_MON[,,168:1] #GEP 2000-2013

MODIS_frame<-data.frame(ID=rep(c(1:47320),15*12),YEAR=rep(c(2000:2014), each=nrows*ncols*12),Month=rep(rep(c(1:12),each=nrows*ncols),15),MOD_ET=as.vector(DATA_MOD_ET_MON),MOD_GEP=c(as.vector(DATA_MOD_GEP_MON),rep(c(0),nrows*ncols*12)))

#MODIS ET and GEP , DSI data Annual ------2000-2014
MODIS_ANN_frame<-data.frame(ID=rep(c(1:47320),15),LAT=rep(LAT,15),LONG=rep(LONG,15),YEAR=rep(c(2000:2014), each=nrows*ncols),MOD_ET=as.vector(MODIS_ANN[,,1:15]),MOD_NPP=as.vector(MODIS_ANN[,,16:30]),MOD_GEP=as.vector(MODIS_ANN[,,31:45]),DSI=c(as.vector(DATA_MOD_DSI),rep(c(-99),nrows*ncols*3))) ##-99 is missing value of DSI




#------------------------------
#NDVI ------2001-2014 16days
DATA_NDVI<-DATA_NDVI*0.0001
NDVI_frame<-data.frame(ID=rep(c(1:47320),14*23),LAT=rep(LAT,14*23),LONG=rep(LONG,14*23),YEAR=rep(c(2001:2014), each=nrows*ncols*23),NUM=rep(rep(c(1:23),each=nrows*ncols),14),NDVI=as.vector(DATA_NDVI))

#------------------------------
#MODIS DSI data ----2000-2011
DSI_frame<-data.frame(ID=rep(c(1:47320),12*12),YEAR=rep(c(2000:2011), each=nrows*ncols),DSI=as.vector(DATA_MOD_DSI))
DSI_frame<-arrange(future_climate_frame,ID,YEAR,Month)


##----out data frame data
list2 = c("data_cell","data_LC","data_LAI","data_climate","data_SOIL")
DATA_MJ_INPUTS<-list(CELLINFO=data_cell,LUCC=data_LC,LANDLAI=data_LAI,CLIMATE=data_climate,SOILINFO=data_SOIL)
 save(DATA_MJ_INPUTS,file = "INPUTS_MJ_frame1.RData")

 
 
 ## compare
 INFO[[1]][which( INFO[[1]]!= INFO[[7]])]

 ##-----Result read
 
 name_dir<-dir(path=".",include.dirs=TRUE,all.files =TRUE,pattern = ".TXT",full.names = TRUE)
length(name_dir)
filenames<-NA

for (i in seq(1,length(name_dir),by=2)){
  filenames<-c(filenames,name_dir[i])
}

filenames<-filenames[2:(length(name_dir)/2+1)]
RESULT_MJ <- lapply(filenames, read.ENVI)


filenames<-name_dir[c(1,2,5,6,7,8,9,10)]
RESULT_MJ <-  lapply(filenames, read.delim,header = TRUE, sep = ",")
names(RESULT_MJ) <- substr(filenames, 3, 30)
save(RESULT_MJ,file = "RESULT_MJ.RData")
load("RESULT_MJ_LCMerge.RData")
load("Carbon_ann_MJ.RData")
load("Carbon_ann_MJ_LCmerge.RData")
Carbon_ann_LC_merge<-Carbon_ann
names(RESULT_MJ) <-c("ANNUALCARBON","ANNUALFLOW","DATA_V_F","HUCCARBON","HUCFLOW","MONTHCARBON","MONTHFLOW","SOILSTORAGE")
 
 
IN_frame<-cbind(LAT=rep(LAT,15*12),LONG=rep(LONG,15*12),IN_frame) 
 
d <- data.frame(a = c(1,2,3),b = c(1,2,3))
n <- 3
do.call("rbind", replicate(15*12, d, simplify = FALSE))

 
 data_frame_mon[[1]][which( data_frame_mon[[1]]!= data_frame_mon[[9]])]
 data_frame_mon<-data.frame(ID=rep(as.vector(INFO[[1]]),15*12),LAT=rep(as.vector(INFO[[2]]),15*12),LONG=rep(as.vector(INFO[[3]]),15*12),VEG=rep(as.vector(INFO[[4]]),15*12),ALT=rep(as.vector(INFO[[5]]),15*12),BASIN=rep(as.vector(INFO[[6]]),15*12))
 
 ###------PET
 
days<-rep(rep(c(31,28,31,30,31,30,31,31,30,31,30,31),each=nrows*ncols),15)
MJD<-rep(rep(c(15,46,76,107,137,168,198,229,259,290,321,351),each=nrows*ncols),15)
 

PET <- data_frame_mon$days*0.1651*1.2*216.7*6.108*exp(17.2693882*data_frame_mon$Temp/(data_frame_mon$Temp+237.3))/(data_frame_mon$Temp+237.3)*2*acos(-1*tan(data_frame_mon$LAT*.0174529)*tan(0.4093*sin(2*3.1415*MJD/365.-1.405)))/3.1415

SUN_ET <- -4.79 + 0.75*PET + 3.92*data_frame_mon$LAI+0.04*data_frame_mon$Pre

data_frame_mon<-cbind(data_frame_mon,PET=PET,SUN_ET=SUN_ET)

### SUN GEP

GEP_W<-data_frame_mon$SUN_ET

WUE_merge<-c(2.46,2.59,2.46,3.2,2.74,1.37,2.12,3.13,0)
#WUE_IGBP<-c(2.46,2.59,2.46,3.2,2.74,1.37,1.33,1.26,2.12,3.13,0)
WUE_IGBP<-c(2.46,2.59,2.46,3.2,2.74,1.37,1.33,1.26,1.26,2.12,0,3.13,0,0,0,0)

#LC_1<-which(RESULT_frame$VEG[!is.na(RESULT_frame$VEG)]==1)

for (i in 1:9){
.LC<-which(data_frame_mon$VEG==i)
GEP_W[LC]<-GEP_W[.LC]*WUE_merge[i]
print(i)
}

for (i in 1:16){
.LC<-which(data_frame_mon$VEG==i)
GEP_W[LC]<-GEP_W[.LC]*WUE_IGBP[i]
print(i)
}

save(list=ls(),file="linshi.RData")













 PI = 3.14159265

! --- ESAT = saturated vapor pressure at DTEMP
! --- RHOSAT = saturated vapor density at DTEMP
      
#      ESAT = 6.108*exp(17.2693882*DTEMP/(DTEMP+237.3))
#      RHOSAT = 216.7*ESAT/(DTEMP+273.3)
      
! --- CALCULATE MEAN MONTHLY DAY LENGTH (DAY) BASED ON LATITUDE AND MID-MONTH JULIAN DATE
! --- SHUTTLEWORTH, W.J. 1993. Evaporation, in Handbook of Hydrology, 
! --- MAIDMENT, D.R. ED., MCGRAW HILL, NY, PP. 4, 1-53

! ---    CALCULATE THE ANGLE OF SOLAR DECLINATION IN RADIANS     
 
#         SOLDEC = 0.4093*SIN(2*PI*J/365.-1.405)
      
! ---    CALCULATE THE SUNSET HOUR ANGLE IN RADIANS

#         SSANG = ACOS(-1*TAN(DEGLAT*.0174529)*TAN(SOLDEC))
      
! ---    CALCULATE THE ADJUSTMENT IN LENGTH OF DAY FOR 12 HR PERIOD
      
         DAY = 2*SSANG/PI
         
         
! --- Calculate Daily PE
     
      PE = 0.1651*DAY*RHOSAT*1.2 
 
 