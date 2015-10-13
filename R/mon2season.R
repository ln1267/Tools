

rm(list =ls())



data_path<-"H:/AU/NDVI_SPOT/NDVI_SPOT_AU_mon_5km_12-99"

data.ann = read.ENVI(data_path)


rows<-nrow(data.ann)
cols<-ncol(data.ann)
years1<-35
years2<-14
years3<-14



mean_dry1<-array(0.0,c(rows,cols,years1))
mean_wet1<-array(0.0,c(rows,cols,years1))
mean_dry2<-array(0.0,c(rows,cols,years2))
mean_wet2<-array(0.0,c(rows,cols,years2))
mean_dry3<-array(0.0,c(rows,cols,years3))
mean_wet3<-array(0.0,c(rows,cols,years3))



# dry means 10:4
 # wet means 3:11
for (m in 1:rows){
  
  for (n in 1:cols){
    
     data1<-data.ann[m,n,3:417]  #13-79 SM
     data2<-data.ann[m,n,423:945]  #13-70 awap
     data3<-data.ann[m,n,951:1113]  #12-99 NDVI SPOT
    
     
  ## --- Data1-----------
    aa<-1
    for (y in 1:years1){
      
      startdry1<-(y-1)*12+1
      enddry1<-(y-1)*12+7 
      
      
      startwet1<-(y-1)*12+8   
      endwet1<-(y-1)*12+12 
      
      mondry1<-data1[startdry1:enddry1]
      monwet1<-data1[startwet1:endwet1]
      
      if (y==years1) {
        monwet1<-NA
      }
      
      mondry1[mondry1==0]<-NA
      monwet1[monwet1==0]<-NA
      mean_dry1[m,n,y]<-mean(mondry1,na.rm=TRUE)
      mean_wet1[m,n,y]<-mean(monwet1,na.rm=TRUE)
      
    }
   ##- -------------------- 
     ## --- Data2-----------
     aa<-1
     for (y in 1:years2){
       
       startdry2<-(y-1)*12+1
       enddry2<-(y-1)*12+7 
       
       
       startwet2<-(y-1)*12+8   
       endwet2<-(y-1)*12+12 
       
       mondry2<-data2[startdry2:enddry2]
       monwet2<-data2[startwet2:endwet2]
       
       if (y==years2) {
         monwet2<-NA
       }
       
       mean_dry2[m,n,y]<-mean(mondry2,na.rm=TRUE)
       mean_wet2[m,n,y]<-mean(monwet2,na.rm=TRUE)
       
     }
     ##- -------------------- 
     
     ## --- Data3-----------
     aa<-1
     for (y in 1:years3){
       
       startdry3<-(y-1)*12+1
       enddry3<-(y-1)*12+7 
       
       
       startwet3<-(y-1)*12+8   
       endwet3<-(y-1)*12+12 
       
       mondry3<-data3[startdry3:enddry3]
       monwet3<-data3[startwet3:endwet3]
       
       if (y==years3) {
         monwet3<-NA
       }
       
       mondry3[mondry3==0]<-NA
       monwet3[monwet3==0]<-NA
       mean_dry3[m,n,y]<-mean(mondry3,na.rm=TRUE)
       mean_wet3[m,n,y]<-mean(monwet3,na.rm=TRUE)
       
     }
     ##- -------------------- 
     

  }
  
}


write.ENVI(mean_dry1, "E:/result/season/SM_dry_13-79")
write.ENVI(mean_dry2, "E:/result/season/AWAP_dry_13-70")
write.ENVI(mean_dry3, "E:/result/season/SPOT_dry_12-99")
write.ENVI(mean_wet1, "E:/result/season/SM_wet_13-79")
write.ENVI(mean_wet2, "E:/result/season/AWAP_wet_13-70")
write.ENVI(mean_wet3, "E:/result/season/SPOT_wet_12-99")



##---single

for (m in 1:rows){
  
  for (n in 1:cols){
    
 
    data2<-data.ann[m,n,3:165]  #13-70 awap
    ## --- Data2-----------
    aa<-1
    for (y in 1:years2){
      
      startdry2<-(y-1)*12+1
      enddry2<-(y-1)*12+7 
      
      
      startwet2<-(y-1)*12+8   
      endwet2<-(y-1)*12+12 
      
      mondry2<-data2[startdry2:enddry2]
      monwet2<-data2[startwet2:endwet2]
      
      if (y==years2) {
        monwet2<-NA
      }
      
      mean_dry2[m,n,y]<-mean(mondry2,na.rm=TRUE)
      mean_wet2[m,n,y]<-mean(monwet2,na.rm=TRUE)
      
    }
    ##- -------------------- 
    
    
  }
}

write.ENVI(mean_dry2, "E:/result/season/SPOT_dry_5km_12-99")
write.ENVI(mean_wet2, "E:/result/season/SPOT_wet_5km_12-99")