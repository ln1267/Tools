rm(list =ls())



data_path<-"H:/AU/ENVI/AWAP_SPOT_ECV_MON_25km"
data_path<-"E:/result/Drought/net/Net_NDVI_LAI_PRE_VEG_C_00-01"

data_path<-"E:/result/Drought/net/Net_VEG_C_SM_00-01"


data.ann = read.ENVI(data_path)
NDVI<-data.ann[,,1]
LAI<-data.ann[,,2]
pre<-data.ann[,,3]
VEG<-data.ann[,,4]
SM<-data.ann[,,3]
CLIMATE<-data.ann[,,5]
rows<-nrow(data.ann)
cols<-ncol(data.ann)
data<-array(0,c(rows,cols,3))
pre[pre==0]<- NA
VEG[VEG < 1]<- NA
DA<-data.ann[,,3]
DA<-NA

m<-7
n<-146

for (m in 1:rows){
  
  for (n in 1:cols){

    if (is.na(pre[m,n])) {
      
      NDVI[m,n]<-NA
      LAI[m,n]<-NA
      VEG[m,n]<-NA
      CLIMATE[m,n]<-NA
    #  SM[m,n]<-NA
    }

  }
}

## statistics VEG and Climate

for (m in 1:rows){
  
  for (n in 1:cols){
    if (is.na(VEG[m,n]) | is.na(pre[m,n])){
      DA[m,n]<-NA
    } else{
       #  if ((VEG[m,n] == 2) & (pre[m,n] < 0)  & ((CLIMATE[m,n] == 8)|(CLIMATE[m,n] == 9))  ) {
         if ((VEG[m,n] == 2) & (pre[m,n] < 0) & ((CLIMATE[m,n] != 8)&(CLIMATE[m,n] != 9))  ) {
            
        DA[m,n]<-1
      } else{
        DA[m,n]<-NA
      }
    }
    
    
  }
}


DA1<-as.numeric(DA)
length(na.omit(DA1))




## statistics VEG/climate and GWD

for (m in 1:rows){
  
  for (n in 1:cols){
    if (is.na(VEG[m,n]) | is.na(GWD[m,n])){
      DA[m,n]<-NA
    } else{
      #   if ((VEG[m,n] == 2)  & ((CLIMATE[m,n] == 8)|(CLIMATE[m,n] == 9))  ) {
     if ((VEG[m,n] == 7) & (CLIMATE[m,n] == 6) ) { # & (pre[m,n] < 0)& (CLIMATE[m,n] != 8) & (CLIMATE[m,n] != 9))
        
        DA[m,n]<-GWD[m,n]
      } else{
        DA[m,n]<-NA
      }
    }
    
    
  }
}


DA1<-as.numeric(DA)
DA1[DA1<0]<-NA
mean(DA1,na.rm = TRUE)
length(na.omit(DA1))

GWD[GWD<0]<-NA




data[,,2]<-NDVI
data[,,3]<-LAI
data[,,1]<-SM
data[,,2]<-VEG
data[,,3]<-CLIMATE
data[,,5]<-CLIMATE

PRE1<-as.numeric(pre)
length(na.omit(PRE1))




NDVI1<-as.numeric(NDVI)
LAI1<-as.numeric(LAI)
LAI1[LAI1>0]<-NA
length(na.omit(LAI1))


SM1<-as.numeric(SM)
SM1[SM1==0]<-NA
length(na.omit(SM1))

VEG1<-as.numeric(VEG)
VEG1[VEG1 < 1]<-NA
length(na.omit(VEG1))

CLIMATE1<-as.numeric(CLIMATE)
CLIMATE1[CLIMATE!=2]<-NA
length(na.omit(CLIMATE1))


NDVI1[NDVI1 < -100]<-NA


write.ENVI(data, "E:/result/Drought/net/Net_VEG_C_SM_NA")   

    