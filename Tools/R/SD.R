rm(list =ls())



data_path<-"H:/AU/ENVI/AWAP_SPOT_ECV_MON_25km"
data_path<-"E:/result/Drought/net/Net_NDVI_LAI_PRE_VEG_C_00-01"

data_path<-"E:/result/Drought/net/Net_VEG_C_SM_00-01"
data_path<-"E:/result/Drought/Data_mon_5km"

data_path<-"E:/ceshi2"


data.ann = read.ENVI(data_path)

rows<-nrow(data.ann)
cols<-ncol(data.ann)

##---- this is for 5km Data
PRE<-data.ann[,,9:1]
NDVI<-data.ann[,,10:18]
LAI<-data.ann[,,19:27]
NET_PRE<-data.ann[,,28]
NET_NDVI<-data.ann[,,29]
NET_LAI<-data.ann[,,30]
VEG<-data.ann[,,34]
CLIMATE<-data.ann[,,32]
GWD<-data.ann[,,33]

data_new<-array(0,c(rows,cols,33))

##---- this is for 25km Data including SM
# SM<-data.ann[,,9:1]
# PRE<-data.ann[,,18:10]
# NDVI<-data.ann[,,19:27]
# LAI<-data.ann[,,28:36]
# NET_PRE<-data.ann[,,37]
# NET_NDVI<-data.ann[,,38]
# NET_LAI<-data.ann[,,39]
# VEG<-data.ann[,,40]
# CLIMATE<-data.ann[,,41]



##---- this is for delete NA data
PRE[PRE<=0]<- NA
VEG[VEG < 1]<- NA

for (m in 1:rows){
  
  for (n in 1:cols){
    for(k in 1:9){
      
      if (is.na(PRE[m,n,k]) | is.na(VEG[m,n])) {
        
        NDVI[m,n,k]<-NA
        LAI[m,n,k]<-NA
        PRE[m,n,k]<-NA
        # SM[m,n,k]<-NA 
        
        NET_PRE[m,n]<-NA
        NET_NDVI[m,n]<-NA
        NET_LAI[m,n]<-NA
        CLIMATE[m,n]<-NA
        VEG[m,n]<-NA
        GWD[m,n]<-NA
      }
      
    }
    
  }
}

SD<-matrix(0,nrow=rows,ncol = cols)

DA<-SD
DA<-NA

m<-500
n<-346

##---- this is caculate SD of each cell

for (m in 1:rows){
  
  for (n in 1:cols){
    
    if (is.na(NET_PRE[m,n]) | is.na(VEG[m,n])) {
      
    SD[m,n]<-NA
    
    }else{
      
      SD[m,n]<-sd((NDVI[m,n,]*0.004-0.1),na.rm = TRUE)
    }
    
  }
}

##---- this is caculate SD of each cell
mydata[mydata<0]<-NA
mydata[mydata>1]<-NA

for (m in 1:rows){
  
  for (n in 1:cols){
    
    if (is.na(data.ann[m,n,265])) {
      
      SD[m,n]<- NA
      
    }else{
      
      SD[m,n]<-sd((data.ann[m,n,1:108]*0.004-0.1),na.rm = TRUE)
    }
    
  }
}



SD[is.na(SD)]<- -999

write.ENVI(SD, "E:/result/Drought/SD_NDVI_SPOT_mon_5km") 


## statistics VEG and Climate


VEG<-data.ann[,,265]
CLIMATE<-data.ann[,,266]


SD<-matrix(0,nrow=rows,ncol = cols)

DA<-array(0,c(rows,cols,9*12))
DA1<-array(0,c(rows,cols,9*12))


for (m in 1:rows){
  
  for (n in 1:cols){
    a<-1
    
    for(y in 1:9){
      
      for(month in 1:12){
        
        if (is.na(VEG[m,n])|is.na(CLIMATE[m,n]) ){
          
        DA[m,n,a]<-NA
        DA1[m,n,a]<-NA
      } else{
        
          if ((VEG[m,n] == 2) & ((CLIMATE[m,n] == 8)|(CLIMATE[m,n] == 9))  ) {
       # if ((VEG[m,n] == 2) & (CLIMATE[m,n] != 8)&(CLIMATE[m,n] != 9)) { #
          
            DA[m,n,a]<-a
            DA1[m,n,a]<-data.ann[m,n,a+131]
          
        } else{
          
          DA[m,n,a]<-NA
          DA1[m,n,a]<-NA
          
        }
        
      }
      a<-a+1
        
      }
        
      
    }
    
  }
}

me<-0

for ( y in 1:108){
  
  me[y]<-mean(DA1[ , ,y],na.rm = TRUE)/10
  
 # print(me[y])
}

sd(me,na.rm = TRUE)
sd(DA1*0.1,na.rm = TRUE)

lins<-data.frame(me)

write.csv(lins, file="E:/result/drought/SD_VEG/LAI_Mon_Mean_V-2-W.csv")
DA1<-as.numeric(DA)
length(na.omit(DA1))


write.ENVI(DA, "E:/result/Drought/dPRE_iNDVI_5km") 
