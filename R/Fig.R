rm(list=ls())
library(caTools)
library(raster)
library(ncdf)
library(plyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(RcmdrPlugin.KMggplot2)
#load("In.RData")
load("linshi.RData")
oldwd<-getwd()

name_tar<-"M:/CLIMATE_RESULTS/ENVI/data_tar"
data_tar<-read.ENVI(name_tar)
FOREST_LS<-data_tar[,,3]

setwd("/Volumes/ELSE/CLIMATE_RESULTS/OUT_FINAL/Final/") 
if (1) {setwd("M:/CLIMATE_RESULTS/OUT_FINAL/Final/")} #this directory is for windows

#load("In_MJ_array1.RData") #load DATA_MJ
load("Inputs_MJ_frame1.RData")
load("RESULT_MJ.RData")
load("DATA_MJ.RData")

save(RESULT_frame,file = "RESULT_MJ_frame.RData")
save(DATA_MOD,file = "MODIS_MJ_frame.RData")

##-------delete out of basin area
DATA_MJ$TEMP[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$DEM[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$PRE[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$VEG[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$SOIL[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$LUCC[is.na(DATA_MJ$BASIN)]<-NA
DATA_MJ$LAI[is.na(DATA_MJ$BASIN)]<-NA
FOREST_LS[is.na(DATA_MJ$BASIN)]<-NA
save(DATA_MJ,file = "DATA_MJ.RData") #output only basin result
#--------------------------
names(RESULT_MJ)<-c("AET_ANN","CELL_ANN","ET_Pratio_ANN","PET_ANN","RAIN_ANN","RFACTOR_ANN","RUN_ETRatio_ANN","RUN_Pratio_ANN","RUNOFF_ANN","SNWPCKMON_ANN","Sun_ET_ANN","YEAR_ANN")
RESULT_MJ$AET_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$CELL_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$ET_Pratio_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$PET_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$RAIN_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$RFACTOR_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$RUN_ETRatio_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$RUN_Pratio_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$RUNOFF_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$SNWPCKMON_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$Sun_ET_ANN[is.na(DATA_MJ$BASIN)]<-NA
RESULT_MJ$YEAR_ANN[is.na(DATA_MJ$BASIN)]<-NA
save(RESULT_MJ,file = "RESULT_MJ.RData") #output only basin result
##------------------------------------------------------------------

## Reclassify land cover data
LUCC<-DATA_MJ$VEG

LUCC[LUCC==1]<-1  #Evergreen Needleleaf forest
LUCC[LUCC==2]<-2  #Evergreen Broadleaf forest
LUCC[LUCC==3]<-3  #Deciduous Needleleaf forest
LUCC[LUCC==4]<-4  #Deciduous Broadleaf forest
LUCC[LUCC==5]<-5  #Mixed forest
LUCC[LUCC==6 |LUCC==7]<-6 #shrublands
LUCC[LUCC==8 |LUCC==9 |LUCC==10]<-7 #Grasslands
LUCC[LUCC==12 |LUCC==14]<-8 #Croplands
LUCC[LUCC==11 |LUCC==13 |LUCC==15 |LUCC==16 |LUCC==0 ]<-9 #Nonvegtation
LUCC_F<-LUCC  # forest of MODIS land cover data
LUCC_F[LUCC_F>5]<-NA
range(LUCC,na.rm = TRUE)
Sta_LUCC<-summary(as.factor(LUCC))
Sta_LUCC_F<-summary(as.factor(LUCC_F))
a<-Sta_LUCC/sum(Sta_LUCC[1:9])
sum(a[1:9])

#211-Evergreen Broadleaf forest;212-Deciduous Broadleaf forest;214-Bamboos;221-Evergreen Needleleaf forest;222-Deciduous Needleleaf forest;230-Mixed forest
#-----Set Forest data to the same as MODIS
FOREST_LS[FOREST_LS==211]<-2 #
FOREST_LS[FOREST_LS==212]<-4
FOREST_LS[FOREST_LS==221]<-1
FOREST_LS[FOREST_LS==222]<-3
FOREST_LS[FOREST_LS==230]<-5
FOREST_LS[FOREST_LS==255]<-NA
Sta_FOREST<-summary(as.factor(FOREST_LS))

#----new land cover data merge MODIS and FOREST
LC_Merge<-FOREST_LS
LC_Merge[is.na(LC_Merge)]<-0
linshi<-LUCC
linshi[is.na(linshi)]<-0
LC_Merge[LC_Merge==0 ]<-linshi[LC_Merge==0]
LC_Merge[LC_Merge<=0]<-NA
Sta_LC_Merge<-summary(as.factor(LC_Merge))
Sta_LC_Merge/sum(Sta_LC_Merge[1:9])
#image(LC_Merge)
LC_dif<-LUCC
LC_dif[LC_Merge==LUCC]<-NA
Sta_LC_dif<-summary(as.factor(LC_dif))

range(LC_dif,na.rm = TRUE)
image(LC_dif)


DATA_MJ_INPUTS$CELLINFO<-cbind(DATA_MJ_INPUTS$CELLINFO,VEG_merge=as.vector(LC_Merge))
save(DATA_MJ_INPUTS,file = "DATA_MJ_INPUTS.RData")


LUCC_r<-raster(LUCC,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")
LUCC_F_r<-raster(LUCC_F,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")
FOREST_LS_r<-raster(FOREST_LS,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")
LC_Merge_r<-raster(LC_Merge,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")
LC_dif_r<-raster(LC_dif,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")
DEM_r<-raster(DATA_MJ$DEM,xmn=DATA_MJ$CELLINFO[3,1],xmx=DATA_MJ$CELLINFO[4,1],ymn=DATA_MJ$CELLINFO[2,1],ymx=DATA_MJ$CELLINFO[1,1],crs="+proj=longlat +datum=WGS84")

##----set land cover data to factor
LUCC_r<-as.factor(LUCC_r)
LUCC_F_r<-as.factor(LUCC_F_r)
FOREST_LS_r<-as.factor(FOREST_LS_r)
LC_Merge_r<-as.factor(LC_Merge_r)
LC_dif_r<-as.factor(LC_dif_r)

##----set labels for each land cover data
class_MOD<-c('Evergreen Needleleaf forest', 'Evergreen Broadleaf forest', 'Deciduous Needleleaf forest', 'Deciduous Broadleaf forest','Mixed forest','Shrublands','Grasslands','Croplands','Nonvegtation')
LUCC_r@data@attributes[[1]]<-cbind(LUCC_r@data@attributes[[1]],class=class_MOD)
LUCC_F_r@data@attributes[[1]]<-cbind(LUCC_F_r@data@attributes[[1]],class=class_MOD[1:5])
FOREST_LS_r@data@attributes[[1]]<-cbind(FOREST_LS_r@data@attributes[[1]],class=class_MOD[1:5])
LC_Merge_r@data@attributes[[1]]<-cbind(LC_Merge_r@data@attributes[[1]],class=class_MOD[1:9])
LC_dif_r@data@attributes[[1]]<-cbind(LC_dif_r@data@attributes[[1]],class=class_MOD[1:9])

##----Water balance data

RESULT_MJ$RAIN_ANN[RESULT_MJ$RAIN_ANN<100]<-NA
RESULT_MJ$RUNOFF_ANN[RESULT_MJ$RUNOFF_ANN>1500]<-NA
RESULT_MJ$RUN_Pratio_ANN[RESULT_MJ$RUN_Pratio_ANN>1]<-NA
RESULT_frame<-data.frame(LAT=rep(DATA_MJ_INPUTS$CELLINFO$LAT,12),LONG=rep(DATA_MJ_INPUTS$CELLINFO$LONG,12),BASIN=rep(as.factor(as.vector(DATA_MJ$BASIN)),12),VEG=rep(as.factor(as.vector(LC_Merge)),12),YEAR=rep(c(2002:2013),each=47320),LUCC=as.factor(as.vector(DATA_MJ$LUCC)),PRE=as.vector(RESULT_MJ$RAIN_ANN[,,21:32]),AET=as.vector(RESULT_MJ$AET_ANN[,,21:32]),RUNOFF=as.vector(RESULT_MJ$RUNOFF_ANN[,,21:32]),R_P=as.vector(RESULT_MJ$RUN_Pratio_ANN[,,21:32]))
pdf("water.pdf")
PRE_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=PRE))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="Precipitation\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

AET_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=AET))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="Evapotranspiration\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

RUNOFF_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=RUNOFF))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="RUNOFF\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

R_P_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=R_P))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="R_P",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

LUCC_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=LUCC))+
  facet_wrap( ~ YEAR, ncol=4)+
 # scale_fill_gradient(low = 'red', high = 'green',name="RUNOFF\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")


ggplot(data = RESULT_frame, aes(x = VEG, y = PRE)) + 
    stat_boxplot(geom= "errorbar", stat_params = list(width = 0.5), geom_params = list()) + 
  geom_boxplot(fill="grey") + xlab("Vegetation types") + ylab("Precipitation\n(mm)") + 
  facet_wrap(~ YEAR, ncol=4)+ #YEAR ~ BASIN
  theme_grid()+theme(panel.background = element_rect(fill = "white"),
                     axis.title = element_text(size = 16,face="bold"),
                     axis.text = element_text(colour="black", size=14))


save(list=ls(),file = "linshi.RData")


PRE_ann_g
AET_ann_g
RUNOFF_ann_g
dev.off()
a<-DATA_MJ_INPUTS$CELLINFO$LAT
b<-DATA_MJ_INPUTS$CELLINFO$LONG
##------read carbon result
load("Carbon_ann_MJ.RData")
Carbon_ann<-data.frame(LAT=rep(DATA_MJ_INPUTS$CELLINFO$LAT,each=33),LONG=rep(DATA_MJ_INPUTS$CELLINFO$LONG,each=33),BASIN=rep(as.vector(DATA_MJ$BASIN),each=33),Carbon_ann)
names(Carbon_ann)<-c("LAT","LONG","BASIN","ID","YEAR","GEP","REO","NEE")
Carbon_ann$YEAR[Carbon_ann$YEAR<=2001 |Carbon_ann$YEAR>2013]<-NA
Carbon_ann<-na.omit(Carbon_ann)
Carbon_ann_LUCC<-Carbon_ann[order(Carbon_ann$YEAR,Carbon_ann$ID),]

load("Carbon_ann_MJ_LCmerge.RData")
Carbon_ann<-data.frame(LAT=rep(DATA_MJ_INPUTS$CELLINFO$LAT,each=33),LONG=rep(DATA_MJ_INPUTS$CELLINFO$LONG,each=33),BASIN=rep(as.vector(DATA_MJ$BASIN),each=33),Carbon_ann)
names(Carbon_ann)<-c("LAT","LONG","BASIN","ID","YEAR","GEP","REO","NEE")
Carbon_ann$YEAR[Carbon_ann$YEAR<=2001 |Carbon_ann$YEAR>2013]<-NA
Carbon_ann<-na.omit(Carbon_ann)
Carbon_ann_LC_merge<-Carbon_ann[order(Carbon_ann$YEAR,Carbon_ann$ID),]

RESULT_frame<-cbind(RESULT_frame[1:10],Carbon_ann_LUCC[6:7],Carbon_ann_LC_merge[6:7])
names(RESULT_frame)[11:16]<-c("GEP_LUCC","REO_LUCC","NEE_LUCC","GEP_LC_Merge","REO_LC_Merge","NEE_LC_Merge")

for (i in 11:16){ RESULT_frame[[i]][is.na(RESULT_frame$BASIN)]<-NA}
RESULT_frame$GEP_LUCC[RESULT_frame$GEP_LUCC>2000 | RESULT_frame$GEP_LUCC<0]<-NA
RESULT_frame$GEP_LC_Merge[RESULT_frame$GEP_LC_Merge>2000 | RESULT_frame$GEP_LC_Merge<0]<-NA

RESULT_frame$NEE_LUCC[RESULT_frame$NEE_LUCC>1500 | RESULT_frame$NEE_LUCC< -500]<-NA
RESULT_frame$NEE_LC_Merge[RESULT_frame$NEE_LC_Merge> 1500 | RESULT_frame$NEE_LC_Merge< -500]<-NA

#-----plot carbon

GEP_LUCC_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=GEP_LUCC))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="GEP",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

GEP_LC_Merge_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=GEP_LC_Merge))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="GEP",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

NEE_LUCC_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=NEE_LUCC))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="NEE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

NEE_LC_Merge_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=NEE_LC_Merge))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="NEE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

REO_LUCC_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=REO_LUCC))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="NEE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")




Carbon_ann$NEE[is.na(Carbon_ann$BASIN)]<-NA
Carbon_ann$NEE[Carbon_ann$NEE>1500]<-NA
REO_LC_Merge_ann_g<-ggplot(aes(x = LONG, y = LAT), data = Carbon_ann) +
  geom_raster(aes(LONG, LAT, fill=NEE))+
  facet_wrap( ~ YEAR, ncol=6)+
  scale_fill_gradient(low = 'red', high = 'green',name="NEE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

Carbon_ann$GEP[is.na(Carbon_ann$BASIN)]<-NA
Carbon_ann$NEE[Carbon_ann$NEE>1500]<-NA
REO_LC_Merge_ann_g<-ggplot(aes(x = LONG, y = LAT), data = Carbon_ann) +
  geom_raster(aes(LONG, LAT, fill=NEE))+
  facet_wrap( ~ YEAR, ncol=6)+
  scale_fill_gradient(low = 'red', high = 'green',name="NEE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")





##------process MODIS ET NPP and GEP data
MODIS<-read.ENVI("MODIS")
nrows<-nrow(MODIS)
ncols<-ncol(MODIS)

S_y<-2000
E_y<-2014


cell_size<-0.01
S_lat<-33.18482249
E_lat<-S_lat-cell_size*nrows
S_long<-102.36209564
E_long<-S_long+cell_size*ncols


MOD_LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
MOD_LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
MOD_Year<-rep(c(S_y:E_y), each=nrows*ncols)

MODIS<-MODIS/10

DATA_MOD<-data.frame(LAT=rep(MOD_LAT,15),LONG=rep(MOD_LONG,15),BASIN=rep(as.vector(DATA_MJ$BASIN),15),LC_merge=rep(as.vector(DATA_MJ$VEG),15),LUCC=rep(as.vector(LUCC),15),YEAR=MOD_Year,ET=as.vector(MODIS[,,1:15]),NPP=as.vector(MODIS[,,16:30]),GPP=as.vector(MODIS[,,31:45]))
DATA_MOD<-cbind(DATA_MOD,WUE=DATA_MOD$GPP/DATA_MOD$ET)

for (i in 4:10){ DATA_MOD[[i]][is.na(DATA_MOD$BASIN)]<-NA}
DATA_MOD<-na.omit(DATA_MOD)
                
                      
DATA_MOD$ET[DATA_MOD$ET>1500]<-NA
MOD_ET_ann_g<-ggplot(aes(x = LONG, y = LAT), data = DATA_MOD) +
  geom_raster(aes(LONG, LAT, fill=ET))+
  facet_wrap( ~ YEAR, ncol=5)+
  scale_fill_gradient(low = 'red', high = 'green',name="ET",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

DATA_MOD$GEP[DATA_MOD$GEP>1200]<-NA
MOD_GEP_ann_g<-ggplot(aes(x = LONG, y = LAT), data = DATA_MOD) +
  geom_raster(aes(LONG, LAT, fill=GEP))+
  facet_wrap( ~ YEAR, ncol=5)+
  scale_fill_gradient(low = 'red', high = 'green',name="GEP",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

DATA_MOD$NPP[DATA_MOD$NPP>1500]<-NA
MOD_NPP_ann_g<-ggplot(aes(x = LONG, y = LAT), data = DATA_MOD) +
  geom_raster(aes(LONG, LAT, fill=NPP))+
  facet_wrap( ~ YEAR, ncol=5)+
  scale_fill_gradient(low = 'red', high = 'green',name="NPP",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ #
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

DATA_MOD$WUE[DATA_MOD$WUE>3]<-NA
MOD_WUE_ann_g<-ggplot(aes(x = LONG, y = LAT), data = DATA_MOD) +
  geom_raster(aes(LONG, LAT, fill=WUE))+
  facet_wrap( ~ YEAR, ncol=5)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ 
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

#------------------------------------------------
rm(ET)



##-----WUE for different carbon resource
attach(RESULT_frame)
RESULT_frame<-cbind(RESULT_frame[1:16],MOD_ET=as.vector(MODIS[,,3:14]),MOD_NPP=as.vector(MODIS[,,18:29]),MOD_GEP=as.vector(MODIS[,,33:44]))
for (i in 17:19){ RESULT_frame[[i]][is.na(RESULT_frame$BASIN)]<-NA}
RESULT_frame<-cbind(RESULT_frame,WUE_MOD=RESULT_frame$MOD_GEP/RESULT_frame$MOD_ET,WUE_MOD_W=RESULT_frame$MOD_GEP/RESULT_frame$AET,WUE_W_MOD=RESULT_frame$GEP_LC_Merge/RESULT_frame$MOD_ET)
detach(RESULT_frame)      

RESULT_frame$WUE_MOD[RESULT_frame$WUE_MOD>4]<-NA
WUE_MOD_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=WUE_MOD))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_MOD",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ 
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

RESULT_frame$WUE_MOD_W[RESULT_frame$WUE_MOD_W>4]<-NA
WUE_MOD_W_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=WUE_MOD_W))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_MOD_W",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ 
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

RESULT_frame$WUE_W_MOD[RESULT_frame$WUE_W_MOD>4]<-NA
WUE_W_MOD_ann_g<-ggplot(aes(x = LONG, y = LAT), data = RESULT_frame) +
  geom_raster(aes(LONG, LAT, fill=WUE_W_MOD))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_W_MOD",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ 
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

GEP_vs<-cbind(RESULT_frame[1:6],P_M_W=RESULT_frame$MOD_GEP/RESULT_frame$GEP_LC_Merge)
GEP_vs$P_M_W[GEP_vs$P_M_W>2]<-NA
P_M_W_ann_g<-ggplot(aes(x = LONG, y = LAT), data = GEP_vs) +
  geom_raster(aes(LONG, LAT, fill=P_M_W))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_W_MOD",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ 
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")

pdf("wue.pdf")
WUE_MOD_ann_g
WUE_MOD_W_ann_g
WUE_W_MOD_ann_g
dev.off()


list<-ls()

##------read YU-revised MODIS

YU_ET<-read.ENVI("E:/DATA/Flux_Data/Grow_season_ET-GPP_Yuzhen/gs-et")
YU_GEP<-read.ENVI("E:/DATA/Flux_Data/Grow_season_ET-GPP_Yuzhen/GS-myGPP-gs(4-9)")

nrows<-nrow(YU_ET)
ncols<-ncol(YU_ET)

S_y<-2001
E_y<-2014
N_Y=E_y-S_y+1

cell_size<-0.05
S_lat<-69.3992
E_lat<-S_lat-cell_size*nrows
S_long<-60.041
E_long<-S_long+cell_size*ncols

linshi_LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
linshi_LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
linshi_Year<-rep(c(S_y:E_y), each=nrows*ncols)

YU_MOD<-data.frame(LAT=rep(linshi_LAT,N_Y),LONG=rep(linshi_LONG,N_Y),YEAR=linshi_Year,ET=as.vector(YU_ET),GPP=as.vector(YU_GEP))

YU_MOD<-cbind(YU_MOD,WUE=YU_MOD$GPP/YU_MOD$ET)

save(YU_MOD,file = "YU_MOD_frame.RData")

YU_MOD$WUE[YU_MOD$WUE>3]<-NA

 pdf("YU.pdf") 
  ggplot(aes(x = LONG, y = LAT), data = YU_MOD) +
  geom_raster(aes(LONG, LAT, fill=WUE))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_YU",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ #
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")
dev.off()

rm(BASIN,YU_MOD)
rm(YU_ET,YU_GEP)
MODIS<-MODIS/10

###-----read DSI


##------read YU-revised MODIS

DSI<-read.ENVI("DSI_CN")

nrows<-nrow(DSI)
ncols<-ncol(DSI)

S_y<-2000
E_y<-2001
N_Y=E_y-S_y+1

cell_size<-0.05
S_lat<-65.925
E_lat<-S_lat-cell_size*nrows
S_long<-66.025
E_long<-S_long+cell_size*ncols

linshi_LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
linshi_LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
linshi_Year<-rep(c(S_y:E_y), each=nrows*ncols)

DSI_MOD<-data.frame(LAT=rep(linshi_LAT,N_Y),LONG=rep(linshi_LONG,N_Y),YEAR=linshi_Year,DSI=as.vector(DSI))

save(DSI_MOD,file = "DSI_MOD_frame.RData")

DSI_MOD$DSI[DSI_MOD$DSI>3]<-NA

pdf("DSI_CN.pdf") 
ggplot(aes(x = LONG, y = LAT), data = DSI_MOD) +
  geom_raster(aes(LONG, LAT, fill=DSI))+
  facet_wrap( ~ YEAR, ncol=4)+
  scale_fill_gradient(low = 'red', high = 'green',name="WUE_YU",na.value = "grey70") +
  coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+ #
  labs(x="Latitude (?)",y="Longitude (?)")+ 
  theme_grid()+theme(legend.position="right")
dev.off()












library(rasterVis)
library(plyr)
#tar<-brick(da,xmn=S_long,xmx=E_long,ymn=E_lat,ymx=S_lat,crs="+proj=longlat +datum=WGS84")
# write to netcdf .
if (require(ncdf)) {	
  rnc <- writeRaster(tar,filename = "/Volumes/ELSE/Pre.nc",format="CDF",overwrite=TRUE)   
}

writeRaster(tar,filename = "/Volumes/ELSE/1.nc",format="CDF")
write.ENVI(Pre,filename = "/Volumes/ELSE/2")




