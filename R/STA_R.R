
rm(list =ls())

data_path<-"E:/result/Drought/R/DATA_GW_LC_R"
data_path<-"E:/result/Drought/R/DATA_SM_R_25km"
data.ann = read.ENVI(data_path)


rows<-nrow(data.ann)
cols<-ncol(data.ann)
years<-13
nums<-12

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


##---- this is for Sta R of each VEG and Climate zone


nDA<-data.ann[,,3]
DA<-NA

N_VEG<-12
N_climate<-8
num3<-1
n1<-0
n2<-0
n3<-0

for (m in 1:rows){
  
  for (n in 1:cols){
    
    if(is.na(VEG[m,n]) | is.na(CLIMATE[m,n])){
      
    } else{

      if (as.numeric(VEG[m,n])==as.numeric(N_VEG) ) {
        n1=n1+1
      }
      if (CLIMATE[m,n]==N_climate){
        n2=n2+1
      }
      
    # if (as.numeric(VEG[m,n])==as.numeric(N_VEG) & (CLIMATE[m,n]==8 | CLIMATE[m,n]==9) ) {
         if (as.numeric(VEG[m,n])==as.numeric(N_VEG) & (CLIMATE[m,n]!=8 & CLIMATE[m,n]!=9) ) {
        n3=n3+1
      }
    }
  }   
}

VEG1<-as.numeric(VEG)
length(na.omit(VEG1))



AWAP.veg<-array(0.0,c(16,9,n1))
AWAP.climate<-array(0.0,c(16,9,n2))
SPOT.veg<-array(0.0,c(16,9,n1))
SPOT.climate<-array(0.0,c(16,9,n2))
LAI.veg<-array(0.0,c(16,9,n1))
LAI.climate<-array(0.0,c(16,9,n2))
# SM.veg<-array(0.0,c(16,9,n1))
# SM.climate<-array(0.0,c(16,9,n2))
AWAP.veg_climate<-array(0.0,c(16,9,n3))
SPOT.veg_climate<-array(0.0,c(16,9,n3))
# SM.veg_climate<-array(0.0,c(16,9,n3))
LAI.veg_climate<-array(0.0,c(16,9,n3))

aa<-1
bb<-1
cc<-1
for (m in 1:rows){
  
  for (n in 1:cols){
    if (is.na(VEG[m,n]) | is.na(NET_PRE[m,n])){
      
    } else{
      if (VEG[m,n]==N_VEG ){
        # print(as.numeric(landtype)==as.numeric(num))
        
        for (y in 1:9){
          if (PRE[m,n,y] < 0.3 & PRE[m,n,y]> 0){
            AWAP.veg[N_VEG,y,aa]<-PRE[m,n,y]
            SPOT.veg[N_VEG,y,aa]<-NDVI[m,n,y]
            LAI.veg[N_VEG,y,aa]<-LAI[m,n,y]  
            # SM.veg[N_VEG,y,aa]<-SM[m,n,y] 
          }else{
            AWAP.veg[N_VEG,y,aa]<-NA
            # MAST.veg[num1,y,aa]<-NA
            SPOT.veg[N_VEG,y,aa]<-NA
            # NAHH.veg[num1,y,aa]<-NA
            LAI.veg[N_VEG,y,aa]<-NA  
            # SM.veg[N_VEG,y,aa]<-NA 
          }
          
        }   
        aa<-aa+1 
      }
      
      if (CLIMATE[m,n]==N_climate){
        for (y in 1:9){  
          if (PRE[m,n,y] > 0 & PRE[m,n,y]>= 0.3){
            AWAP.climate[N_climate,y,bb]<-PRE[m,n,y]
            # MAST.climate[num2,y,bb]<-data.ann[m,n,74+y]
            SPOT.climate[N_climate,y,bb]<-NDVI[m,n,y]
            # NAHH.climate[num2,y,bb]<-data.ann[m,n,101+y]
            LAI.climate[N_climate,y,bb]<-LAI[m,n,y]
            # SM.climate[N_climate,y,bb]<-SM[m,n,y]
          }
        }
        bb<-bb+1
      }     
      
     #  if (as.numeric(VEG[m,n])==as.numeric(N_VEG) & (CLIMATE[m,n]==8 | CLIMATE[m,n]==9) ){
       if (as.numeric(VEG[m,n])==as.numeric(N_VEG) & (CLIMATE[m,n]!=8 & CLIMATE[m,n]!=9) ) {
        for (y in 1:9){  
          if (PRE[m,n,y] > 0 & PRE[m,n,y] < 0.3){
            AWAP.veg_climate[num3,y,cc]<-PRE[m,n,y]
            # MAST.veg_climate[num3,y,cc]<-data.ann[m,n,74+y]
            SPOT.veg_climate[num3,y,cc]<-NDVI[m,n,y]
            # NAHH.veg_climate[num3,y,cc]<-data.ann[m,n,101+y]
            LAI.veg_climate[num3,y,cc]<-LAI[m,n,y]
            # SM.veg_climate[num3,y,cc]<-SM[m,n,y]
          }
        }
        cc<-cc+1
      }
    }
  }
  
}


#  for Vegetation types
num4<-c(1:9)

VX1<-AWAP.veg[N_VEG,num4 , ]
# VX2<-SM.veg[N_VEG,num4 , ]
VY1<-SPOT.veg[N_VEG,num4, ]
VY2<-LAI.veg[N_VEG,num4 , ]

# VY3[VY3>100]<-NA
# VX2[VX2<0]<-NA
#VX1[VX1>0.004]<-NA

#plot(VX1,VY1)
#plot(VX1,VY2)
#plot(VX1,VY3)

VXX1<-as.numeric(VX1)
# VXX2<-as.numeric(VX2)
VYY1<-as.numeric(VY1)
VYY2<-as.numeric(VY2)

MeanVXX1<-0
# MeanVXX2<-0
MeanVYY1<-0
MeanVYY2<-0
for (n in 1:9){
  
  MeanVXX1[n]<-mean(AWAP.veg[N_VEG,n, ],na.rm = TRUE)
  # MeanVXX2[n]<-mean(SM.veg[N_VEG,n, ],na.rm = TRUE)
  MeanVYY1[n]<-mean(SPOT.veg[N_VEG,n, ],na.rm = TRUE)
  MeanVYY2[n]<-mean(LAI.veg[N_VEG,n, ],na.rm = TRUE)
  
}

linsV<-data.frame(MeanVXX1,MeanVYY1,MeanVYY2)
write.csv(linsV, file="E:/result/drought/R/CSV/R_V-12_5km.csv")
linsV<-data.frame(VXX1,VYY1,VYY2)
write.csv(linsV, file="E:/result/drought/R/CSV/R_V-12_all_5km.csv")
lmdata<-data.frame(VYY1,VXX1)

plot(VXX1,VYY1)
plot(MeanVXX1,MeanVYY1)
plot(VXX1,VYY1)
lmMV<-lm(MeanVYY1~MeanVXX1)
summary.lm(lmMV)
cor.test(MeanVYY1,MeanVXX1)


#-----------------------------------------------


#  for VEG and Climate
num4<-c(1:9)

X1<-AWAP.veg_climate[num3,num4 , ]
# X2<-SM.veg_climate[num3,num4 , ]
Y1<-SPOT.veg_climate[num3,num4, ]
Y2<-LAI.veg_climate[num3, num4, ]

XX1<-as.numeric(X1)
# XX2<-as.numeric(X2)
YY1<-as.numeric(Y1)
YY2<-as.numeric(Y2)

MeanXX1<-0
# MeanXX2<-0
MeanYY1<-0
MeanYY2<-0

for (n in 1:9){
  
  MeanXX1[n]<-mean(AWAP.veg_climate[num3,n, ],na.rm = TRUE)
  # MeanXX2[n]<-mean(SM.veg_climate[num3,n, ],na.rm = TRUE)
  MeanYY1[n]<-mean(SPOT.veg_climate[num3,n, ],na.rm = TRUE)
  MeanYY2[n]<-mean(LAI.veg_climate[num3,n, ],na.rm = TRUE)
}

lins<-data.frame(MeanXX1,MeanYY1,MeanYY2)
write.csv(lins, file="E:/result/drought/R/CSV/R_CV-2-E_5km.csv")
lins<-data.frame(XX1,YY1,YY2)
write.csv(lins, file="E:/result/drought/R/CSV/R_CV-2-E-all_5km.csv")
lmdata<-data.frame(YY1,XX1)

plot(XX1,YY1)
plot(MeanXX1,MeanYY1)
plot(XX1,YY1)
lm(YY1~XX1)



#  for Climate
num4<-c(1:13)

CX1<-AWAP.climate[num2,num4 , ]
CX2<-MAST.climate[num2,num4 , ]
CY1<-SPOT.climate[num2,num4, ]
CY2<-NAHH.climate[num2,num4 , ]
CY3<-LAI.climate[num2, num4, ]
CY3[CY3>100]<-NA
CX2[CX2<0]<-NA
#CX1[CX1>0.004]<-NA

#plot(CX1,CY1)
#plot(CX1,CY2)
#plot(CX1,CY3)

CXX1<-as.numeric(CX1)
CXX2<-as.numeric(CX2)
CYY1<-as.numeric(CY1)
CYY2<-as.numeric(CY2)
CYY3<-as.numeric(CY3)
MeanCXX1<-0
MeanCXX2<-0
MeanCYY1<-0
MeanCYY2<-0
MeanCYY3<-0
for (n in 1:13){
  
  MeanCXX1[n]<-mean(AWAP.climate[num2,n, ],na.rm = TRUE)
  MeanCXX2[n]<-mean(MAST.climate[num2,n, ],na.rm = TRUE)
  MeanCYY1[n]<-mean(SPOT.climate[num2,n, ],na.rm = TRUE)
  MeanCYY2[n]<-mean(NAHH.climate[num2,n, ],na.rm = TRUE)
  MeanCYY3[n]<-mean(LAI.climate[num2,n, ],na.rm = TRUE)
}

linsC<-data.frame(MeanCXX1,MeanCXX2,MeanCYY1,MeanCYY2,MeanCYY3)
write.csv(linsC, file="E:/bearf3C-15.csv")

lmdata<-data.frame(CYY1,CXX1)

plot(CXX1,CYY1)
plot(MeanCXX1,MeanCYY1)
lmMC<-lm(MeanCYY1~MeanCXX1)
summary.lm(lnMC)
cor.test(MeanCYY1,MeanCXX1)


#--------------------------------------------------


# draw the images
png("e:/1.png")
plot(MeanCXX1,MeanCYY1)
lmMC<-lm(MeanCYY1~MeanCXX1)
#summary.lm(lnMC)
abline(lmMC)
dev.off()




x_na<-as.numeric(na.omit(AWAP.veg[num,  1,  ]))
y_na<-as.numeric(na.omit(SPOT.veg[num, 1 , ]))

plot(x_na[1:100],y_na[1:100])
plot(x_na,y_na)

cor.test(x_na,y_na)

v<-MAST.veg[num1,  ,  ]
v[v<0]<-NA
plot(v,SPOT.veg[num1, ,  ])
plot(AWAP.veg[num,  1,  ],LAI.veg[num, 1 , ])

plot(AWAP.climate[num,1,],SPOT.climate[num,1,])


write.ENVI(climate1[7,,,], "e:/limate_type")   

pre<-as.numeric(data.ann[ , ,1:13])
spot_NDVI<-as.numeric(data.ann[ , ,14:26])
Nahh_NDVI<-as.numeric(data.ann[ , ,27:39])
plot(pre,spot_NDVI)
plot(pre,Nahh_NDVI)



write.ENVI(GWD, "e:/ceshi1") 
write.ENVI(VEG, "e:/ceshi2") 
