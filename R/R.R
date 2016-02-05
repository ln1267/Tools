rm(list=ls())
library(caTools)
library(raster)
library(ncdf)
library(plyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(hydroGOF)
  
setwd("J:/PhD/WICKEPIN/20160106")

data_24_03_11<-read.ENVI("24_03_11")
data_28_09_10<-read.ENVI("28_09_10")
site_buf<-read.ENVI("site_buf")
site_low<-read.ENVI("site_low")
data_f<-data.frame(BLUE_10=as.integer(data_28_09_10[,,1]),BLUE_11=as.integer(data_24_03_11[,,1]),RED_10=as.integer(data_28_09_10[,,3]),RED_11=as.integer(data_24_03_11[,,3]),NIR_10=as.integer(data_28_09_10[,,4]),NIR_11=as.integer(data_24_03_11[,,4]),GREEN_10=as.integer(data_28_09_10[,,2]),GREEN_11=as.integer(data_24_03_11[,,2]))



smoothScatter(data_f$RED_10,data_f$NIR_10)
##3 
#Band 1 – 450nm ± 10nm FWHM (Blue)
#Band 2 – 550nm ± 10nm FWHM (Green)
#Band 3 – 675nm ± 10nm FWHM (Red)
#Band 4 – 780nm ± 10nm FWHM (Near Infra-red)

BLUE_10<-data_28_09_10[,,1]
BLUE_11<-data_24_03_11[,,1]
RED_10<-data_28_09_10[,,3]
RED_11<-data_24_03_11[,,3]
NIR_10<-data_28_09_10[,,4]
NIR_11<-data_24_03_11[,,4]
GREEN_10<-data_28_09_10[,,2]
GREEN_11<-data_24_03_11[,,2]

#---VIs
# NDVI (NIR-RED=NIR+RED)

NDVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10)
NDVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11)

#RVI (NIR/RED)
RVI_10<-(NIR_10/RED_10)
RVI_11<-(NIR_11/RED_11)


#EVI (2.5*(NIR-RED)/(NIR+6RED-7.5blue+1))
EVI_10<-2.5*(NIR_10-RED_10)/(NIR_10+6*RED_10-7.5*BLUE_10+1)
EVI_11<-2.5*(NIR_11-RED_11)/(NIR_11+6*RED_11-7.5*BLUE_11+1)

#SAVI (1+0.5)(NIR-RED)/(NIR+RED+0.5)
SAVI_10<-1.5*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
SAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

#LSAVI
L_10<-1-2*(NDVI_10*(NIR_10-r*RED_10))
LSAVI_10<-L_10*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
LSAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

# MSAVI 

MSAVI_10<-(2*(NIR_10+1)-sqrt((2*NIR_10+1)^2-8*(NIR_10-RED_10)))/2
MSAVI_11<-(2*(NIR_11+1)-sqrt((2*NIR_11+1)^2-8*(NIR_11-RED_11)))/2

#OSAVI

OSAVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10+0.16)
OSAVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11+0.16)


VI<-array(0,c(3174,2161,10))
VI[,,1]<-NDVI_10
VI[,,2]<-NDVI_11
VI[,,3]<-RVI_10
VI[,,4]<-RVI_11
VI[,,5]<-SAVI_10
VI[,,6]<-SAVI_11
VI[,,7]<-MSAVI_10
VI[,,8]<-MSAVI_11
VI[,,9]<-OSAVI_10
VI[,,10]<-OSAVI_11

for( i in 1:10){
  
  VI[,,i][VI[,,8]<0]<-NA
  
}
write.ENVI(VI,"VI_NA_soil")



##---difference
VI_dif<-array(0,c(3174,2161,5))
VI_dif[,,1]<-NDVI_10-NDVI_11
VI_dif[,,2]<-RVI_10-RVI_11
VI_dif[,,3]<-SAVI_10-SAVI_11
VI_dif[,,4]<-MSAVI_10-MSAVI_11
VI_dif[,,5]<-OSAVI_10-OSAVI_11

for( i in 1:10){
  
  VI[,,i][VI_dif[,,4]>-0.2]<-NA
  
}
write.ENVI(VI,"VI_NA")




##----paste

paste_24_03_11<-read.ENVI("24_03_11_paste")
paste_28_09_10<-read.ENVI("28_09_10_paste")

paste_f<-data.frame(BLUE_10=as.integer(paste_28_09_10[,,1]),BLUE_11=as.integer(paste_24_03_11[,,1]),RED_10=as.integer(paste_28_09_10[,,3]),RED_11=as.integer(paste_24_03_11[,,3]),NIR_10=as.integer(paste_28_09_10[,,4]),NIR_11=as.integer(paste_24_03_11[,,4]),GREEN_10=as.integer(paste_28_09_10[,,2]),GREEN_11=as.integer(paste_24_03_11[,,2]))

attach(data_f)
detach(data_f)
attach(paste_f)
detach(paste_f)

#---VIs
# NDVI (NIR-RED=NIR+RED)

NDVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10)
NDVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11)

#RVI (NIR/RED)
RVI_10<-(NIR_10/RED_10)
RVI_11<-(NIR_11/RED_11)


#EVI (2.5*(NIR-RED)/(NIR+6RED-7.5blue+1))
EVI_10<-2.5*(NIR_10-RED_10)/(NIR_10+6*RED_10-7.5*BLUE_10+1)
EVI_11<-2.5*(NIR_11-RED_11)/(NIR_11+6*RED_11-7.5*BLUE_11+1)

#SAVI (1+0.5)(NIR-RED)/(NIR+RED+0.5)
SAVI_10<-1.5*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
SAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

#LSAVI
L_10<-1-2*(NDVI_10*(NIR_10-r*RED_10))
LSAVI_10<-L_10*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
LSAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

# MSAVI 

MSAVI_10<-(2*(NIR_10+1)-sqrt((2*NIR_10+1)^2-8*(NIR_10-RED_10)))/2
MSAVI_11<-(2*(NIR_11+1)-sqrt((2*NIR_11+1)^2-8*(NIR_11-RED_11)))/2

#OSAVI

OSAVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10+0.16)
OSAVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11+0.16)

paste_VI_f<-cbind(NDVI_10=NDVI_10,NDVI_11=NDVI_11,RVI_10=RVI_10,RVI_11=RVI_11,SAVI_10=SAVI_10,SAVI_11=SAVI_11,MSAVI_10=MSAVI_10,MSAVI_11=MSAVI_11,OSAVI_10=OSAVI_10,OSAVI_11=OSAVI_11,EVI_10=EVI_10,EVI_11=EVI_11)

paste_VI_f<-as.data.frame(paste_VI_f)
paste_VI_10<-cbind(group="2010",paste_VI_f[c(1,3,5,7)])

paste_VI_11<-cbind(group="2011",paste_VI_f[c(2,4,6,8)])
names(paste_VI_10)<-c("group","NDVI","RVI","SAVI","MSAVI")
names(paste_VI_11)<-c("group","NDVI","RVI","SAVI","MSAVI")
paste_VI_group<-rbind(paste_VI_10,paste_VI_11)


VI_f<-cbind(NDVI_10=NDVI_10,NDVI_11=NDVI_11,RVI_10=RVI_10,RVI_11=RVI_11,SAVI_10=SAVI_10,SAVI_11=SAVI_11,MSAVI_10=MSAVI_10,MSAVI_11=MSAVI_11,OSAVI_10=OSAVI_10,OSAVI_11=OSAVI_11)
VI_f<-as.data.frame(VI_f)
VI_f<-cbind(Site=as.integer(site_buf),VI_f)
VI_f$Site[VI_f$Site>20 |VI_f$Site<1]<-NA

aa<-melt(VI_f,id=c(1))
Site_mean<-dcast(aa,Site~variable,mean,na.rm=TRUE)
Site_mean<-Site_mean[1:16,]
Site_mean<-cbind(Site_mean,info$V1,info$V2)
names(Site_mean)[1]<-"ID"
names(Site_mean)[13]<-"Site"
Site_all_c<-merge(Site_mean,AGB,by="Site")


for (i in 3:12) {
  
  .df <- data.frame(x = Site_all_c[[i]], y = Site_all_c$X11)
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + geom_point() + 
    stat_smooth(method = "lm", se = FALSE) + scale_y_continuous(expand = c(0.01,
                                                                           0)) + xlab("VI") + ylab("AGB") + theme_bw(base_size = 14, base_family = 
                                                                                                                       "serif")
  print(.plot)
  ggsave(filename = paste("J:/PhD/WICKEPIN/20160106/VI_AGB_all_",names(Site_all_c)[i],".jpg",sep=""), plot = .plot)
  rm(.df, .plot)
  
}





VI_f_NA<-VI_f

for( i in 1:10){
  
  VI[,,i][VI_dif[,,4]>-0.2]<-NA
  
}









smoothScatter(paste_f$RED_10,paste_f$NIR_10)


p1_24_03_11<-read.ENVI("24_03_11_p1")
p1_28_09_10<-read.ENVI("28_09_10_p1")

p1_f<-data.frame(BLUE_10=as.integer(p1_28_09_10[,,1]),BLUE_11=as.integer(p1_24_03_11[,,1]),RED_10=as.integer(p1_28_09_10[,,3]),RED_11=as.integer(p1_24_03_11[,,3]),NIR_10=as.integer(p1_28_09_10[,,4]),NIR_11=as.integer(p1_24_03_11[,,4]),GREEN_10=as.integer(p1_28_09_10[,,2]),GREEN_11=as.integer(p1_24_03_11[,,2]))

p1_f_10<-cbind(group="2010",p1_f[c(1,3,5,7)])

p1_f_11<-cbind(group="2011",p1_f[c(2,4,6,8)])
names(p1_f_10)<-c("group","BLUE","RED","NIR","GREEN")
names(p1_f_11)<-c("group","BLUE","RED","NIR","GREEN")
p1_f_group<-rbind(p1_f_10,p1_f_11)

attach(p1_f)
#---VIs
# NDVI (NIR-RED=NIR+RED)

NDVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10)
NDVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11)

#RVI (NIR/RED)
RVI_10<-(NIR_10/RED_10)
RVI_11<-(NIR_11/RED_11)


#EVI (2.5*(NIR-RED)/(NIR+6RED-7.5blue+1))
EVI_10<-2.5*(NIR_10-RED_10)/(NIR_10+6*RED_10-7.5*BLUE_10+1)
EVI_11<-2.5*(NIR_11-RED_11)/(NIR_11+6*RED_11-7.5*BLUE_11+1)

#SAVI (1+0.5)(NIR-RED)/(NIR+RED+0.5)
SAVI_10<-1.5*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
SAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

#LSAVI
L_10<-1-2*(NDVI_10*(NIR_10-r*RED_10))
LSAVI_10<-L_10*(NIR_10-RED_10)/(NIR_10+RED_10+0.5)
LSAVI_11<-1.5*(NIR_11-RED_11)/(NIR_11+RED_11+0.5)

# MSAVI 

MSAVI_10<-(2*(NIR_10+1)-sqrt((2*NIR_10+1)^2-8*(NIR_10-RED_10)))/2
MSAVI_11<-(2*(NIR_11+1)-sqrt((2*NIR_11+1)^2-8*(NIR_11-RED_11)))/2

#OSAVI

OSAVI_10<-(NIR_10-RED_10)/(NIR_10+RED_10+0.16)
OSAVI_11<-(NIR_11-RED_11)/(NIR_11+RED_11+0.16)

VI_f<-cbind(NDVI_10=NDVI_10,NDVI_11=NDVI_11,RVI_10=RVI_10,RVI_11=RVI_11,SAVI_10=SAVI_10,SAVI_11=SAVI_11,MSAVI_10=MSAVI_10,MSAVI_11=MSAVI_11)

VI_10<-cbind(group="2010",VI_f[c(1,3,5,7)])

VI_11<-cbind(group="2011",VI_f[c(2,4,6,8)])
names(VI_10)<-c("group","NDVI","RVI","SAVI","MSAVI")
names(VI_11)<-c("group","NDVI","RVI","SAVI","MSAVI")
VI_group<-rbind(VI_10,VI_11)

VI_f<-as.data.frame(VI_f)

detach(p1_f)

a<-rep(info$V1,each=10)


Sta_all<-read.table(file="clipboard")
info<-read.table(file="clipboard")
Sta_all<-cbind(a,Sta_all)
names(Sta_all)<-c("Site","Index","min","max","mean","STD")
AGB<-read.table(file="clipboard",T)


a<-merge(Sta_all,AGB,by="Site")

for (i in 1:10) {
sp<-subset(a,Index==i)
.df <- data.frame(x = sp$mean, y = sp$X9)
.plot <- ggplot(data = .df, aes(x = x, y = y)) + geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + scale_y_continuous(expand = c(0.01,
                                                                         0)) + xlab("VI") + ylab("AGB") + theme_bw(base_size = 14, base_family = 
                                                                                                                       "serif")
print(.plot)
ggsave(filename = paste("J:/PhD/WICKEPIN/20160106/VI_AGB_y9_",i,".jpg",sep=""), plot = .plot)
rm(.df, .plot)

}


VI_f<-cbind(NDVI_10=NDVI_10,NDVI_11=NDVI_11,RVI_10=RVI_10,RVI_11=RVI_11,SAVI_10=SAVI_10,SAVI_11=SAVI_11,MSAVI_10=MSAVI_10,MSAVI_11=MSAVI_11,OSAVI_10=OSAVI_10,OSAVI_11=OSAVI_11,EVI_10=EVI_10,EVI_11=EVI_11)
VI_f<-as.data.frame(VI_f)
VI_f<-cbind(Site=as.integer(site_buf),VI_f)
VI_f$Site[VI_f$Site>20 |VI_f$Site<1]<-NA

VI_f$RVI_11[VI_f$RVI_11<0.8]<-NA
VI_f_NA<-na.omit(VI_f)
a1<-melt(VI_f_NA,id=c(1))
Site_mean_NA<-dcast(a1,Site~variable,mean,na.rm=TRUE)
Site_mean_NA<-cbind(Site_mean_NA,info$V1[1:13],info$V2[1:13])
names(Site_mean_NA)[1]<-"ID"
names(Site_mean_NA)[15]<-"Site"
Site_NA_c<-merge(Site_mean_NA,AGB,by="Site")


for (i in 3:14) {
  
  .df <- data.frame(x = Site_NA_c[[i]], y = Site_NA_c$X11)
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + geom_point() + 
    stat_smooth(method = "lm", se = FALSE) + scale_y_continuous(expand = c(0.01,
                                                                           0)) + xlab("VI") + ylab("AGB") + theme_bw(base_size = 14, base_family = 
                                                                                                                       "serif")
  print(.plot)
  ggsave(filename = paste("J:/PhD/WICKEPIN/20160106/VI_AGB_NA_",names(Site_NA_c)[i],".jpg",sep=""), plot = .plot)
  rm(.df, .plot)
  
}







library(gmodels)
library(Hmisc)

MSAVI_11_f<-as.data.frame(MSAVI_11)
MSAVI_11_f<-cbind(Site=as.integer(site_buf),MSAVI_11_f)
MSAVI_11_f$Site[MSAVI_11_f$Site>20 |MSAVI_11_f$Site<1]<-NA
MSAVI_11_f$Site<-as.factor(MSAVI_11_f$Site)
MSAVI_11_f$MSAVI_11[MSAVI_11_f$MSAVI_11< -0.0]<-NA
MSAVI_11_f_NA<-na.omit(MSAVI_11_f)
a<-describe(MSAVI_11_f_NA)
a
inx<-seq(1,23,by=2)
b<-a$Site$values[inx]
ABG<-c(17.38522338,14.90235*384,10.94535961,17.54406754,8.329725967,19.01863156,12.64326683,11.74297438,14.13299562,4.314268248,10.26347865,6.998315696)

cor.test(b,ABG)
plot(b,ABG)
lm(b,ABG)



MSAVI_10_f<-as.data.frame(MSAVI_10)
MSAVI_10_f<-cbind(Site=as.integer(site_buf),MSAVI_10_f)
MSAVI_10_f$Site[MSAVI_10_f$Site>20 |MSAVI_10_f$Site<1]<-NA
MSAVI_10_f$Site<-as.factor(MSAVI_10_f$Site)
MSAVI_10_f$MSAVI_10[MSAVI_10_f$MSAVI_10< 0.05]<-NA
MSAVI_10_f_NA<-na.omit(MSAVI_10_f)
a<-describe(MSAVI_10_f_NA)
a
inx<-seq(1,23,by=2)
b<-a$Site$values[inx]
ABG<-c(17.08414988,14.53237335,9.305106886,13.3471783,6.121482158,15.57647312,9.901127384,11.98044698,10.58439094,5.2528026,8.248789294,6.0644576)
cor.test(b,ABG)




MSAVI_11_f<-as.data.frame(MSAVI_11)
MSAVI_11_f<-cbind(Site=as.integer(site_low),MSAVI_11_f)
MSAVI_11_f$Site[MSAVI_11_f$Site>20 |MSAVI_11_f$Site<1]<-NA
MSAVI_11_f$Site<-as.factor(MSAVI_11_f$Site)
MSAVI_11_f$MSAVI_11[MSAVI_11_f$MSAVI_11< -0.0]<-NA
MSAVI_11_f_NA<-na.omit(MSAVI_11_f)
a<-describe(MSAVI_11_f_NA)
a
inx<-seq(1,11,by=2)
b<-a$Site$values[inx]
ABG<-c(14.90235,19.01863156,8.329725967,10.26347865,6.998315696,12.64326683)

cor.test(b,ABG)
plot(b,ABG)
lm(b,ABG)

a1<-melt(MSAVI_11_f_NA,id=c(1))
Site_mean_NA<-dcast(a1,Site~variable,mean,na.rm=TRUE)
Site_mean_NA<-cbind(Site_mean_NA,info$V1[1:13],info$V2[1:13])
names(Site_mean_NA)[1]<-"ID"
names(Site_mean_NA)[15]<-"Site"
Site_NA_c<-merge(Site_mean_NA,AGB,by="Site")

cor.test(Site_mean_NA$MSAVI_11,ABG)


smoothScatter(p1_f$RED_10,p1_f$NIR_10)



write.ENVI(VI_dif,"VI_dif")
save(list=ls(),file=".RData")
