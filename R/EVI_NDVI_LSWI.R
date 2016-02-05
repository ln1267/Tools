### this is for process LWSI data with MODIS 09 8 days B1,2,3,6 data

library(caTools)
library(plyr)
library(reshape2)


B1<-read.ENVI("B1_MJ")
B2<-read.ENVI("B2_MJ")
B3<-read.ENVI("B3_MJ")
B6<-read.ENVI("B6_MJ")

##B1 -red; B2 -NIR, B3-blue; B6 -shortwave infrared (SWIR

nrows<-nrow(B1)
ncols<-ncol(B1)

B1<-B1[,,598:1]
B2<-B2[,,598:1]
B3<-B3[,,598:1]
B6<-B6[,,598:1]

B1<-B1*0.0001
B2<-B2*0.0001
B3<-B3*0.0001
B6<-B6*0.0001


S_y<-2002
E_y<-2014

cell_size<-0.00508657
S_lat<-33.18908277
E_lat<-S_lat-cell_size*nrows
S_long<-102.36097997
E_long<-S_long+cell_size*ncols


LAT<-rep(seq(S_lat, by=-cell_size, length.out = nrows),ncols)
LONG<-rep(seq(S_long, by=cell_size, length.out = ncols),each=nrows)
#This is for define the 46 8days for with month
month<-c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12)

B_frame<-data.frame(ID=rep(c(1:(nrows*ncols)),13*46),LAT=rep(LAT,13*46),LONG=rep(LONG,13*46),YEAR=rep(c(2002:2014), each=nrows*ncols*46),Month=rep(rep(month,each=nrows*ncols),13),num=rep(rep(c(1:46),each=nrows*ncols),13),B1=as.vector(B1),B2=as.vector(B2),B3=as.vector(B3),B6=as.vector(B6))
summary(B_frame)
for (i in 7:10){B_frame[[i]][B_frame[[i]]< -0.01]<-NA}


#NDVI=(B_frame$B2-B_frame$B1)/(B_frame$B2+B_frame$B1)
EVI=2.5*(B_frame$B2-B_frame$B1)/(B_frame$B2+6*B_frame$B1-7.5*B_frame$B3+1)
LSWI=(B_frame$B2-B_frame$B6)/(B_frame$B2+B_frame$B6)
summary(LSWI)
B_frame<-cbind(B_frame,NDVI=(B_frame$B2-B_frame$B1)/(B_frame$B2+B_frame$B1),EVI=,LWSI=)

NDVI[is.infinite(NDVI)]<-NA
EVI[is.infinite(EVI)]<-NA


