
rm(list =ls())



data_path<-"E:/SM_SPOT_Ann_25km"
data_path<-"E:/result/Drought/R/DATA_R"
data_path<-"E:/result/Drought/R/DATA_SM_R_25km"
data_path<-"E:/result/season/Season_all_25km"

data_path<-"E:/result/season/Season_all_25km"

data_path<-"E:/result/Drought/R/DATA_GLADS_R"
data_path<-"E:/result/Drought/R/DATA_GW_R"
data.ann = read.ENVI(data_path)


rows<-nrow(data.ann)
cols<-ncol(data.ann)
years<-14
nums<-12


R_1<-matrix(0,nrow=rows,ncol=cols)
R_2<-matrix( 0,nrow=rows,ncol=cols)
R_3<-matrix(0,nrow=rows,ncol=cols)
R_4<-matrix( 0,nrow=rows,ncol=cols)
R_5<-matrix(0,nrow=rows,ncol=cols)
R_6<-matrix( 0,nrow=rows,ncol=cols)
R_7<-matrix(0,nrow=rows,ncol=cols)
R_8<-matrix( 0,nrow=rows,ncol=cols)

P_1<-matrix(0,nrow=rows,ncol=cols)
P_2<-matrix( 0,nrow=rows,ncol=cols)
P_3<-matrix(0,nrow=rows,ncol=cols)
P_4<-matrix( 0,nrow=rows,ncol=cols)
P_5<-matrix(0,nrow=rows,ncol=cols)
P_6<-matrix( 0,nrow=rows,ncol=cols)
P_7<-matrix(0,nrow=rows,ncol=cols)
P_8<-matrix( 0,nrow=rows,ncol=cols)


SM<-data.ann[,,9:1]
NDVI<-data.ann[,,10:18]
LAI<-data.ann[,,19:27]


SM<-data.ann[,,9:1]
PRE<-data.ann[,,18:10]
NDVI<-data.ann[,,19:27]
LAI<-data.ann[,,28:36]
NET_PRE<-data.ann[,,37]
NET_NDVI<-data.ann[,,38]
NET_LAI<-data.ann[,,39]
VEG<-data.ann[,,40]
CLIMATE<-data.ann[,,41]

PRE[PRE<=0]<- NA
VEG[VEG < 1]<- NA
DA<-data.ann[,,3]
DA<-NA

m<-500
n<-446
k=3
for (m in 1:rows){
  
  for (n in 1:cols){
    for(k in 1:9){
       
      if (is.na(PRE[m,n,k]) | is.na(VEG[m,n])) {
      
      NDVI[m,n,k]<-NA
      LAI[m,n,k]<-NA
      PRE[m,n,k]<-NA
       SM[m,n,k]<-NA 
# 
#       NET_PRE[m,n]<-NA
#       NET_NDVI[m,n]<-NA
#       NET_LAI[m,n]<-NA
       CLIMATE[m,n]<-NA
      
    }
  
    }
    
  }
}


for (m in 1:rows){
  
  for (n in 1:cols){
    
#     data1<-data.ann[m,n,31:43]  #00-12 Pre AWAP
#     data2<-data.ann[m,n,75:87]  #00-12 Pre MAST
#     data3<-data.ann[m,n,89:101]  #00-12 NDVI SPOT
#     data4<-data.ann[m,n,102:114]  #00-12 NDVI NAHH
#     data5<-data.ann[m,n,133:145]  #00-12 LAI  
#     data6<-data.ann[m,n,13:43]  #82-12 Pre AWAP
#     data7<-data.ann[m,n,57:87]  #82-12 Pre MAST
#     data8<-data.ann[m,n,115:145]  #82-12 LAI  
         data1<-SM[m,n, ]  #12-99 AWAP
        # data1<-PRE[m,n, ]  #12-99 AWAP
        data2<-NDVI[m,n, ]  #12-99 SPOT
       data3<-LAI[m,n, ]  #12-99 LAI
#         data4<-data.ann[m,n,15:28]  #12-99 dry SPOT
    #     data1[data1<0]<-NA
         if (length(na.omit(data1))<9){
  #    if ((length(na.omit(data1[1:13]))<13 |length(na.omit(data3[1:13]))<13) |(length(na.omit(data2))< 14|length(na.omit(data4))<14) ) {
        R_1[m,n]<- -999
        P_1[m,n]<- -999
        
        R_2[m,n]<- -999
        P_2[m,n]<- -999
#         
#         R_3[m,n]<-NA
#         P_3[m,n]<-NA
#         
#         R_4[m,n]<-NA
#         P_4[m,n]<-NA
        
        
      }else{
        
        
        cor_1<-cor.test(data1,data2) # 00-12 WET AWAP VS  SPOT
        cor_2<-cor.test(data1,data3)# 00-12 WET AWAP VS DRY SPOT
#         cor_3<-cor.test(data2[2:14],data3[1:13])# 00-12 DRY AWAP VS WET SPOT
#         cor_4<-cor.test(data2,data4)# 00-12 DRY AWAP VS  SPOT
         
#     cor_1<-cor.test(data1,data3) # 00-12 Pre AWAP VS NDVI SPOT
#     cor_2<-cor.test(data1,data4)# 00-12 Pre AWAP VS NDVI NAHH
#     cor_3<-cor.test(data1,data5)# 00-12 Pre AWAP VS LAI
#     cor_4<-cor.test(data2,data3)# 00-12 Pre MAST VS NDVI SPOT
#     cor_5<-cor.test(data2,data4)# 00-12 Pre MAST VS NDVI NAHH
#     cor_6<-cor.test(data2,data5)# 00-12 Pre MAST VS LAI
#     cor_7<-cor.test(data6,data8)# 82-12 Pre AWAP VS LAI
#     cor_8<-cor.test(data7,data8)# 82-12 Pre MAST VS LAI
    
    
    
    R_1[m,n]<-cor_1$estimate[["cor"]]
    P_1[m,n]<-cor_1$p.value
    
    R_2[m,n]<-cor_2$estimate[["cor"]]
    P_2[m,n]<-cor_2$p.value
#     
#     R_3[m,n]<-cor_3$estimate[["cor"]]
#     P_3[m,n]<-cor_3$p.value
#     
#     R_4[m,n]<-cor_4$estimate[["cor"]]
#     P_4[m,n]<-cor_4$p.value
    
#     R_5[m,n]<-cor_5$estimate[["cor"]]
#     P_5[m,n]<-cor_5$p.value
    
#     R_6[m,n]<-cor_6$estimate[["cor"]]
#     P_6[m,n]<-cor_6$p.value
#     
#     R_7[m,n]<-cor_7$estimate[["cor"]]
#     P_7[m,n]<-cor_7$p.value
#     
#     R_8[m,n]<-cor_8$estimate[["cor"]]
#     P_8[m,n]<-cor_8$p.value
    }
  }
  
}

for (y in 1:9) {
  
  #print(mean(NDVI[,,y],na.rm=TRUE)*0.004-0.1)
  print(mean(LAI[,,y],na.rm=TRUE))
}






write.ENVI(R_1, "E:/result/drought/r/COR_R_GLADS_SPOT_NA") #WET SM VS  SPOT
write.ENVI(R_2, "E:/result/drought/r/COR_R_GLADS_LAI_NA")#WET SM VS DRY SPOT
write.ENVI(R_3, "E:/result/season/COR_R_dry_SM_wet_SPOT_5km")
write.ENVI(R_4, "E:/result/season/COR_R_dry_SM_SPOT_5km")
# write.ENVI(R_5, "E:/result/R/COR_R_MAST_nahh_00-12")
# write.ENVI(R_6, "E:/result/R/COR_R_MAST_lai_00-12")
# write.ENVI(R_7, "E:/result/R/COR_R_SM_lai_82-12")
# write.ENVI(R_8, "E:/result/R/COR_R_MAST_lai_82-12")

write.ENVI(P_1, "E:/result/drought/r/COR_P_GLADS_SPOT_NA")
write.ENVI(P_2, "E:/result/drought/r/COR_P_GLADS_LAI_NA")
write.ENVI(P_3, "E:/result/season/COR_P_dry_SM_wet_SPOT_5km")
write.ENVI(P_4, "E:/result/season/COR_P_dry_SM_SPOT_5km")
# write.ENVI(P_5, "E:/result/R/COR_P_MAST_nahh_00-12")
# write.ENVI(P_6, "E:/result/R/COR_P_MAST_lai_00-12")
# write.ENVI(P_7, "E:/result/R/COR_P_AWAP_lai_82-12")
# write.ENVI(P_8, "E:/result/R/COR_P_MAST_lai_00-12")


