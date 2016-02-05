
#---------------------------
rm(list =ls()) # this is for clear all the data in memory of R
library(caTools)

name_ENVI<-"J:/WaSSIC/Data_input/LINSHI/climate_SUN_1km_00-10"
data_ENVI<-read.ENVI(name_ENVI)
Year_start<-2000
Year_end<-2010
num_year<-Year_end-Year_start+1

Pre_start<-1
Temp_start<-num_year*12+1

nrows<-nrow(data_ENVI)
ncols<-ncol(data_ENVI)
Temp_ann<-array(0,c(nrows,ncols,num_year))
Pre_ann<-array(0,c(nrows,ncols,num_year))
linshi_pre<-0
linshi_temp<-0
for (y in 1: num_year){
  
  S_pre<-((y-1)*12+1)
  E_pre<-y*12
  S_temp<-(y-1+num_year)*12+1
  E_temp<-(y+num_year)*12
  linshi_pre<-data_ENVI[,,S_pre:E_pre]
  linshi_temp<-data_ENVI[,,S_temp:E_temp]
  Pre_ann[,,y]<-apply(linshi_pre,c(1,2),sum)
  Temp_ann[,,y]<-apply(linshi_temp,c(1,2),mean)
  
  }
write.ENVI(Pre_ann[,,num_year:1]/10,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_YU_temp_ann" )
write.ENVI(Temp_ann[,,num_year:1]/10,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_YU_pre_ann")
image(Pre_ann[,,1])
Temp_ann1<-0
Pre_ann1<-0

Temp_ann1<-Pre_ann[,,num_year:1]/10
Pre_ann1<-Temp_ann[,,num_year:1]/10
Temp_ann<-Temp_ann1
Pre_ann<-Pre_ann1

GEP_ann<-array(0,c(nrows,ncols,num_year))
ER_ann<-array(0,c(nrows,ncols,num_year))
NEP_ann<-array(0,c(nrows,ncols,num_year))
GEP_ann<-107.02*Temp_ann+2.18*Pre_ann-0.1*Temp_ann*Pre_ann-544.35
GEP_MEAN_ann1<-apply(GEP_ann,c(1,2),mean)  #[,,20:31]
write.ENVI(GEP_ann,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_SUN_GEP_ann_1" )
write.ENVI(GEP_MEAN_ann1,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_GEP_SUN_1" )

GEP_ann<-38.18*Temp_ann+0.83*Pre_ann+143.95
GEP_MEAN_ann2<-apply(GEP_ann,c(1,2),mean)  #[,,20:31]
write.ENVI(GEP_ann,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_SUN_GEP_ann_2" )
write.ENVI(GEP_MEAN_ann2,filename ="J:/WaSSIC/Data_input/LINSHI/ann/WaSSI_GEP_SUN_2" )

RE_ann<-54.08*Temp_ann+1.19*Pre_ann-0.05*Temp_ann*Pre_ann-103.04
NEP_ann<-48.98*Temp_ann+0.79*Pre_ann-0.05*Temp_ann*Pre_ann-313.85
image(GEP_MEAN_ann2)
write.ENVI(RE_ann,filename ="m:/WaSSI_SUN_RE_ann" )
write.ENVI(NEP_ann,filename ="m:/WaSSI_SUN_NEP_ann" )