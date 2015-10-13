
rm(list =ls())



data_path<-"E:/PRE_VEG_5km1"

data.ann = read.ENVI(data_path)


rows<-nrow(data.ann)
cols<-ncol(data.ann)
years<-13
nums<-12


tau_1<-matrix(0,nrow=rows,ncol=cols)
tau_2<-matrix( 0,nrow=rows,ncol=cols)
tau_3<-matrix(0,nrow=rows,ncol=cols)
tau_4<-matrix( 0,nrow=rows,ncol=cols)
tau_5<-matrix(0,nrow=rows,ncol=cols)
tau_6<-matrix( 0,nrow=rows,ncol=cols)
tau_7<-matrix(0,nrow=rows,ncol=cols)
tau_8<-matrix( 0,nrow=rows,ncol=cols)

pva_1<-matrix(0,nrow=rows,ncol=cols)
pva_2<-matrix( 0,nrow=rows,ncol=cols)
pva_3<-matrix(0,nrow=rows,ncol=cols)
pva_4<-matrix( 0,nrow=rows,ncol=cols)
pva_5<-matrix(0,nrow=rows,ncol=cols)
pva_6<-matrix( 0,nrow=rows,ncol=cols)
pva_7<-matrix(0,nrow=rows,ncol=cols)
pva_8<-matrix( 0,nrow=rows,ncol=cols)

sg_1<-matrix(0,nrow=rows,ncol=cols)
sg_2<-matrix( 0,nrow=rows,ncol=cols)
sg_3<-matrix(0,nrow=rows,ncol=cols)
sg_4<-matrix( 0,nrow=rows,ncol=cols)
sg_5<-matrix(0,nrow=rows,ncol=cols)
sg_6<-matrix( 0,nrow=rows,ncol=cols)
sg_7<-matrix(0,nrow=rows,ncol=cols)
sg_8<-matrix( 0,nrow=rows,ncol=cols)

changyear_1<-matrix(0,nrow=rows,ncol=cols)
changyear_2<-matrix( 0,nrow=rows,ncol=cols)
changyear_3<-matrix(0,nrow=rows,ncol=cols)
changyear_4<-matrix( 0,nrow=rows,ncol=cols)
changyear_5<-matrix(0,nrow=rows,ncol=cols)
changyear_6<-matrix( 0,nrow=rows,ncol=cols)
changyear_7<-matrix(0,nrow=rows,ncol=cols)
changyear_8<-matrix( 0,nrow=rows,ncol=cols)

p_change_1<-matrix(0,nrow=rows,ncol=cols)
p_change_2<-matrix( 0,nrow=rows,ncol=cols)
p_change_3<-matrix(0,nrow=rows,ncol=cols)
p_change_4<-matrix( 0,nrow=rows,ncol=cols)
p_change_5<-matrix(0,nrow=rows,ncol=cols)
p_change_6<-matrix( 0,nrow=rows,ncol=cols)
p_change_7<-matrix(0,nrow=rows,ncol=cols)
p_change_8<-matrix( 0,nrow=rows,ncol=cols)



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
    
    data1<-data.ann[m,n,2:15]  #12-99 wet AWAP
    data2<-data.ann[m,n,46:59]  #12-99 dry AWAP
    data3<-data.ann[m,n,89:102]  #12-99 wet SPOT
    data4<-data.ann[m,n,103:116]  #12-99 dry SPOT
    
    data1<-data1[14:1]
    data2<-data2[14:1]
    data3<-data3[13:1]
    data4<-data4[14:1]
 ##1   
    
    # ----MK test-------------------------    
   
      x_1time<-ts(data1)
      d_1<-mk.test(x_1time)
      tau_1[m,n]<-d_1$taug
      pva_1[m,n]<-d_1$pvalg
      sg_1[m,n]<-d_1$Sg
      
# ---- MK test end-------------------------
      
# --- pettitt.test(x) - change point detection ---- 
#     changedata_1<-pettitt.test(data1)
#     changyear_1[m,n]<-1982+as.numeric(changedata_1$estimate[["probable change point at tau"]])-1
#     p_change_1[m,n]<-changedata_1$p.value[1]      
# --- end change point detection ----
    
 ##2   
    
    # ----MK test-------------------------    
    
      x_2time<-ts(data2)
      d_2<-mk.test(x_2time)
      tau_2[m,n]<-d_2$taug
      pva_2[m,n]<-d_2$pvalg
      sg_2[m,n]<-d_2$Sg
      
     
# ---- MK test end-------------------------
      
# --- pettitt.test(x) - change point detection ---- 
#     changedata_2<-pettitt.test(data2)
#     changyear_2[m,n]<-1982+as.numeric(changedata_2$estimate[["probable change point at tau"]])-1
#     p_change_2[m,n]<-changedata_2$p.value[1]      
# --- end change point detection ----

##3

    # ----MK test-------------------------    

      x_3time<-ts(data3)
      d_3<-mk.test(x_3time)
      tau_3[m,n]<-d_3$taug
      pva_3[m,n]<-d_3$pvalg
      sg_3[m,n]<-d_3$Sg
      
      
# ---- MK test end-------------------------
      
# --- pettitt.test(x) - change point detection ---- 
#     changedata_3<-pettitt.test(data3)
#     changyear_3[m,n]<-1982+as.numeric(changedata_3$estimate[["probable change point at tau"]])-1
#     p_change_3[m,n]<-changedata_3$p.value[1]      
# --- end change point detection ----

##4

    # ----MK test-------------------------    
   
   
      x_4time<-ts(data4)
      d_4<-mk.test(x_4time)
      tau_4[m,n]<-d_4$taug
      pva_4[m,n]<-d_4$pvalg
      sg_4[m,n]<-d_4$Sg
      
      
# ---- MK test end-------------------------
      
# --- pettitt.test(x) - change point detection ---- 
#     changedata_4<-pettitt.test(data4)
#     changyear_4[m,n]<-1982+as.numeric(changedata_4$estimate[["probable change point at tau"]])-1
#     p_change_4[m,n]<-changedata_4$p.value[1]      
# --- end change point detection ----

# ##5
# 
#     # ----MK test-------------------------    
#    
#       
#       x_5time<-ts(data5)
#       d_5<-mk.test(x_5time)
#       tau_5[m,n]<-d_5$taug
#       pva_5[m,n]<-d_5$pvalg
#       sg_5[m,n]<-d_5$Sg
#       
#       
# # ---- MK test end-------------------------
#       
# # --- pettitt.test(x) - change point detection ---- 
#     changedata_5<-pettitt.test(data5)
#     changyear_5[m,n]<-1982+as.numeric(changedata_5$estimate[["probable change point at tau"]])-1
#     p_change_5[m,n]<-changedata_5$p.value[1]      
# # --- end change point detection ----
# 
# ##6
# 
# 
#     # ----MK test-------------------------    
#     
#       x_6time<-ts(data6)
#       d_6<-mk.test(x_6time)
#       tau_6[m,n]<-d_6$taug
#       pva_6[m,n]<-d_6$pvalg
#       sg_6[m,n]<-d_6$Sg
#       
#       
# # ---- MK test end-------------------------
#       
# # --- pettitt.test(x) - change point detection ---- 
#     changedata_6<-pettitt.test(data6)
#     changyear_6[m,n]<-1982+as.numeric(changedata_6$estimate[["probable change point at tau"]])-1
#     p_change_6[m,n]<-changedata_6$p.value[1]      
# # --- end change point detection ----
# 
# ##7
# 
#     # ----MK test-------------------------    
#     
#       x_7time<-ts(data7)
#       d_7<-mk.test(x_7time)
#       tau_7[m,n]<-d_7$taug
#       pva_7[m,n]<-d_7$pvalg
#       sg_7[m,n]<-d_7$Sg
#       
#       
# # ---- MK test end-------------------------
#       
# # --- pettitt.test(x) - change point detection ---- 
#     changedata_7<-pettitt.test(data7)
#     changyear_7[m,n]<-1982+as.numeric(changedata_7$estimate[["probable change point at tau"]])-1
#     p_change_7[m,n]<-changedata_7$p.value[1]      
# # --- end change point detection ----


##8
#     # ----MK test-------------------------    
#     
#       x_8time<-ts(data8)
#       d_8<-mk.test(x_8time)
#       tau_8[m,n]<-d_8$taug
#       pva_8[m,n]<-d_8$pvalg
#       sg_8[m,n]<-d_8$Sg
#       
#       
# # ---- MK test end-------------------------
#       
# # --- pettitt.test(x) - change point detection ---- 
#     changedata_8<-pettitt.test(data8)
#     changyear_8[m,n]<-1982+as.numeric(changedata_8$estimate[["probable change point at tau"]])-1
#     p_change_8[m,n]<-changedata_8$p.value[1]      
# # --- end change point detection ----



  }
  
}
   

write.ENVI(p_change_1, "E:/result/changepoint/p_change_AWAP_00-12")
write.ENVI(p_change_2, "E:/result/changepoint/p_change_MAST_00-12")
write.ENVI(p_change_3, "E:/result/changepoint/p_change_SPOT_00-12")
write.ENVI(p_change_4, "E:/result/changepoint/p_change_NAHH_00-12")
write.ENVI(p_change_5, "E:/result/changepoint/p_change_LAI_00-12")
write.ENVI(p_change_6, "E:/result/changepoint/p_change_AWAP_82-12")
write.ENVI(p_change_7, "E:/result/changepoint/p_change_MAST_82-12")
write.ENVI(p_change_8, "E:/result/changepoint/p_change_LAI_82-12")

write.ENVI(changyear_1, "E:/result/changepoint/changyear_AWAP_00-12")
write.ENVI(changyear_2, "E:/result/changepoint/changyear_MAST_00-12")
write.ENVI(changyear_3, "E:/result/changepoint/changyear_SPOT_00-12")
write.ENVI(changyear_4, "E:/result/changepoint/changyear_NAHH_00-12")
write.ENVI(changyear_5, "E:/result/changepoint/changyear_LAI_00-12")
write.ENVI(changyear_6, "E:/result/changepoint/changyear_AWAP_82-12")
write.ENVI(changyear_7, "E:/result/changepoint/changyear_MAST_82-12")
write.ENVI(changyear_8, "E:/result/changepoint/changyear_LAI_82-12")

write.ENVI(tau_1, "E:/result/trend/tau_wet_AWAP_99-12")
write.ENVI(tau_2, "E:/result/trend/tau_dry_AWAP_99-12")
write.ENVI(tau_3, "E:/result/trend/tau_wet_SPOT_99-12")
write.ENVI(tau_4, "E:/result/trend/tau_dry_SPOT_99-12")
write.ENVI(tau_5, "E:/result/trend/tau_LAI_00-12")
write.ENVI(tau_6, "E:/result/trend/tau_AWAP_82-12")
write.ENVI(tau_7, "E:/result/trend/tau_MAST_82-12")
write.ENVI(tau_8, "E:/result/trend/tau_LAI_82-12")

write.ENVI(pva_1, "E:/result/trend/pva_wet_AWAP_99-12")
write.ENVI(pva_2, "E:/result/trend/pva_dry_AWAP_99-12")
write.ENVI(pva_3, "E:/result/trend/pva_wet_SPOT_99-12")
write.ENVI(pva_4, "E:/result/trend/pva_dry_SPOT_99-12")
write.ENVI(pva_5, "E:/result/trend/pva_LAI_00-12")
write.ENVI(pva_6, "E:/result/trend/pva_AWAP_82-12")
write.ENVI(pva_7, "E:/result/trend/pva_MAST_82-12")
write.ENVI(pva_8, "E:/result/trend/pva_LAI_82-12")

write.ENVI(sg_1, "E:/result/trend/sg_wet_AWAP_99-12")
write.ENVI(sg_2, "E:/result/trend/sg_dry_AWAP_99-12")
write.ENVI(sg_3, "E:/result/trend/sg_wet_SPOT_99-12")
write.ENVI(sg_4, "E:/result/trend/sg_dry_SPOT_99-12")
write.ENVI(sg_5, "E:/result/trend/sg_LAI_00-12")
write.ENVI(sg_6, "E:/result/trend/sg_AWAP_82-12")
write.ENVI(sg_7, "E:/result/trend/sg_MAST_82-12")
write.ENVI(sg_8, "E:/result/trend/sg_LAI_82-12")




data1<-data.ann[m,n,2:15]  #12-99 wet AWAP
data2<-data.ann[m,n,46:59]  #12-99 dry AWAP
data3<-data.ann[m,n,89:102]  #12-99 wet SPOT
data4<-data.ann[m,n,103:116]  #12-99 dry SPOT

