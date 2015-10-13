data_path<-"E:/PRE_VEG_5km1"

data.ann = read.ENVI(data_path)


rows<-nrow(data.ann)
cols<-ncol(data.ann)
years<-13
nums<-12


# data1<-data.ann[m,n,31:43]  #00-12 Pre AWAP
# data2<-data.ann[m,n,75:87]  #00-12 Pre MAST
# data3<-data.ann[m,n,89:101]  #00-12 NDVI SPOT
# data4<-data.ann[m,n,102:114]  #00-12 NDVI NAHH
# data5<-data.ann[m,n,133:145]  #00-12 LAI  
# data6<-data.ann[m,n,13:43]  #82-12 Pre AWAP
# data7<-data.ann[m,n,57:87]  #82-12 Pre MAST
# data8<-data.ann[m,n,115:145]  #82-12 LAI  


  num1<-2
  num2<-15
  num3<-1
  n1<-0
  n2<-0
  n3<-0
  for (m in 1:rows){
    
    for (n in 1:cols){
      landtype<-data.ann[m,n,147]
      landyear<-data.ann[m,n,146]
      climatetype<-data.ann[m,n,148]
      
      if (as.numeric(landtype)==as.numeric(num1)) {
        n1=n1+1
      }
      if (climatetype==num2){
        n2=n2+1
      }
      
      if (as.numeric(landtype)==as.numeric(num1) & climatetype==num2) {
        n3=n3+1
      }
    }   
  }
  
AWAP.veg<-array(0.0,c(16,13,n1))
AWAP.climate<-array(0.0,c(16,13,n2))
MAST.veg<-array(0.0,c(16,13,n1))
MAST.climate<-array(0.0,c(16,13,n2))
SPOT.veg<-array(0.0,c(16,13,n1))
SPOT.climate<-array(0.0,c(16,13,n2))
NAHH.veg<-array(0.0,c(16,13,n1))
NAHH.climate<-array(0.0,c(16,13,n2))
LAI.veg<-array(0.0,c(16,13,n1))
LAI.climate<-array(0.0,c(16,13,n2))
AWAP.veg_climate<-array(0.0,c(16,13,n3))
SPOT.veg_climate<-array(0.0,c(16,13,n3))
LAI.veg_climate<-array(0.0,c(16,13,n3))
NAHH.veg_climate<-array(0.0,c(16,13,n3))
MAST.veg_climate<-array(0.0,c(16,13,n3))


aa<-1
bb<-1
cc<-1
for (m in 1:rows){
    
    for (n in 1:cols){
      
        landtype<-data.ann[m,n,147]
        landyear<-data.ann[m,n,146]
        climatetype<-data.ann[m,n,148]
      
         if (as.numeric(landtype)==as.numeric(num1)){
          # print(as.numeric(landtype)==as.numeric(num))
          
               for (y in 1:13){
                 if (data.ann[m,n,30+y] < 0.3 & data.ann[m,n,30+y]>0){
                 AWAP.veg[num1,y,aa]<-data.ann[m,n,30+y]
                 MAST.veg[num1,y,aa]<-data.ann[m,n,74+y]
                 SPOT.veg[num1,y,aa]<-data.ann[m,n,88+y]
                 NAHH.veg[num1,y,aa]<-data.ann[m,n,101+y]
                 LAI.veg[num1,y,aa]<-data.ann[m,n,132+y]  
                 
                 }else{
                   AWAP.veg[num1,y,aa]<-NA
                   MAST.veg[num1,y,aa]<-NA
                   SPOT.veg[num1,y,aa]<-NA
                   NAHH.veg[num1,y,aa]<-NA
                   LAI.veg[num1,y,aa]<-NA  
                 }
                 
                # print(num)
              #   print(y)
              #   print(aa)
              #   print(SPOT.veg[num,y,aa])
                 }   
                 aa<-aa+1 
          }
             
        if (climatetype==num2){
          for (y in 1:13){  
            if (data.ann[m,n,30+y] > 0.002 & data.ann[m,n,30+y]<0.02){
            AWAP.climate[num2,y,bb]<-data.ann[m,n,30+y]
            MAST.climate[num2,y,bb]<-data.ann[m,n,74+y]
            SPOT.climate[num2,y,bb]<-data.ann[m,n,88+y]
            NAHH.climate[num2,y,bb]<-data.ann[m,n,101+y]
            LAI.climate[num2,y,bb]<-data.ann[m,n,132+y]
            }
          }
          bb<-bb+1
        }     
        
        if (as.numeric(landtype)==as.numeric(num1) & climatetype==num2){
          for (y in 1:13){  
            if (data.ann[m,n,30+y] > 0 & data.ann[m,n,30+y]<0.1){
              AWAP.veg_climate[num3,y,cc]<-data.ann[m,n,30+y]
              MAST.veg_climate[num3,y,cc]<-data.ann[m,n,74+y]
              SPOT.veg_climate[num3,y,cc]<-data.ann[m,n,88+y]
              NAHH.veg_climate[num3,y,cc]<-data.ann[m,n,101+y]
              LAI.veg_climate[num3,y,cc]<-data.ann[m,n,132+y]
            }
          }
          cc<-cc+1
        }
        
        
       }

}

#  for VEG and Climate
num4<-c(1:13)

X1<-AWAP.veg_climate[num3,num4 , ]
X2<-MAST.veg_climate[num3,num4 , ]
Y1<-SPOT.veg_climate[num3,num4, ]
Y2<-NAHH.veg_climate[num3,num4 , ]
Y3<-LAI.veg_climate[num3, num4, ]
Y3[Y3>100]<-NA
X1[X1>0.004]<-NA

plot(X1,Y1)
plot(X1,Y2)
plot(X1,Y3)

XX1<-as.numeric(X1)
XX2<-as.numeric(X2)
YY1<-as.numeric(Y1)
YY2<-as.numeric(Y2)
YY3<-as.numeric(Y3)

for (n in 1:13){
  
  MeanXX1[n]<-mean(AWAP.veg_climate[num3,n, ],na.rm = TRUE)
  MeanXX2[n]<-mean(MAST.veg_climate[num3,n, ],na.rm = TRUE)
  MeanYY1[n]<-mean(SPOT.veg_climate[num3,n, ],na.rm = TRUE)
  MeanYY2[n]<-mean(NAHH.veg_climate[num3,n, ],na.rm = TRUE)
  MeanYY3[n]<-mean(LAI.veg_climate[num3,n, ],na.rm = TRUE)
}

lins<-data.frame(MeanXX1,MeanXX2,MeanYY1,MeanYY2,MeanYY3)
write.csv(lins, file="E:/bearf3.csv")

lmdata<-data.frame(YY1,XX1)

plot(XX1,YY1)
plot(MeanXX1)
lm(YY1~XX1)

#  for Vegetation types
num4<-c(1:13)

VX1<-AWAP.veg[num1,num4 , ]
VX2<-MAST.veg[num1,num4 , ]
VY1<-SPOT.veg[num1,num4, ]
VY2<-NAHH.veg[num1,num4 , ]
VY3<-LAI.veg[num1, num4, ]
VY3[VY3>100]<-NA
VX2[VX2<0]<-NA
#VX1[VX1>0.004]<-NA

#plot(VX1,VY1)
#plot(VX1,VY2)
#plot(VX1,VY3)

VXX1<-as.numeric(VX1)
VXX2<-as.numeric(VX2)
VYY1<-as.numeric(VY1)
VYY2<-as.numeric(VY2)
VYY3<-as.numeric(VY3)
MeanVXX1<-0
MeanVXX2<-0
MeanVYY1<-0
MeanVYY2<-0
MeanVYY3<-0
for (n in 1:13){
  
  MeanVXX1[n]<-mean(AWAP.veg[num1,n, ],na.rm = TRUE)
  MeanVXX2[n]<-mean(MAST.veg[num1,n, ],na.rm = TRUE)
  MeanVYY1[n]<-mean(SPOT.veg[num1,n, ],na.rm = TRUE)
  MeanVYY2[n]<-mean(NAHH.veg[num1,n, ],na.rm = TRUE)
  MeanVYY3[n]<-mean(LAI.veg[num1,n, ],na.rm = TRUE)
}

linsV<-data.frame(MeanVXX1,MeanVXX2,MeanVYY1,MeanVYY2,MeanVYY3)
write.csv(linsV, file="E:/bearf3V-2.csv")

lmdata<-data.frame(VYY1,VXX1)

plot(VXX1,VYY1)
plot(MeanVXX1,MeanVYY1)
lmMV<-lm(MeanVYY1~MeanVXX1)
summary.lm(lnMV)
cor.test(MeanVYY1,MeanVXX1)


#-----------------------------------------------

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
