rm(list =ls())


data_path<-"E:/Data/MODIS/MCD12Q1_5km_GLOBAL/MOD_Landcover_5km_12-01"

mydata = read.ENVI(data_path)

rows<-nrow(mydata)
cols<-ncol(mydata)
years<-13
nums<-12


Land_type<-matrix(0,nrow=rows,ncol=cols)
Land_nums<-matrix( 0,nrow=rows,ncol=cols)
#mean_00_all<-matrix(0.0,nrow=rows,ncol=cols)
#pva<-matrix(0.0,nrow=670,ncol=813)
#sg<-matrix(0.0,nrow=670,ncol=813)

for (m in 1:rows){
  
  for (n in 1:cols){
    
   
    x<-mydata[m,n, ]

    zhs<- which.max(table(x)) 

    Land_type[m,n]<-as.numeric(names(zhs)) # extract the row.name of named factor zhs
    Land_nums[m,n]<-max(table(x)) # extract the varible of named factor zhs

  }
  
}

  
write.ENVI(Land_type, "e:/Landcover_maintype")
write.ENVI(Land_nums, "e:/landcover_type_years")