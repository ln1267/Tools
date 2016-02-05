rm(list=ls())
library(caTools)
library(raster)
library(ncdf)
library(plyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(hydroGOF)
library(randomForest)

setwd("d:/saltbush/")

setwd("J:/PhD/WICKEPIN/20160106")
setwd("J:/PhD/WICKEPIN/20160106/test")

data_24_03_11<-read.ENVI("24_03_11")

new_11<-read.ENVI("data_10_09")


site_buf<-read.ENVI("site_buf")
site_round<-read.ENVI("site-whole_all")
data_11_f<-data.frame(ID=c(1:6859014),BLUE=as.integer(data_24_03_11[,,1]),RED=as.integer(data_24_03_11[,,3]),NIR=as.integer(data_24_03_11[,,4]),GREEN=as.integer(data_24_03_11[,,2]))

#-- read false color image
new_11<-read.ENVI("11_com")
new_11<-read.ENVI("10_com")
new_11_f<-data.frame(ID=c(1:6859014),BLUE=as.integer(new_11[,,3]),RED=as.integer(new_11[,,2]),NIR=as.integer(new_11[,,1]),GREEN=as.integer(new_11[,,4]))

new_11_f<-data.frame(ID=c(1:1028310),BLUE=as.integer(new_11[,,1]),RED=as.integer(new_11[,,3]),NIR=as.integer(new_11[,,4]),GREEN=as.integer(new_11[,,2]))
attach(new_11_f)
attach(data_11_f)
#---VIs
# NDVI (NIR-RED/NIR+RED)
NDVI<-(NIR-RED)/(NIR+RED)

# mNDVI (NIR-RED/NIR+RED)
mNDVI<-(NIR-RED)/((NIR+RED)-2*BLUE)

#RVI (NIR/RED)
RVI<-(NIR/RED)
REEI<-(RED/NIR)

#EVI (2.5*(NIR-RED)/(NIR+6RED-7.5blue+1))
EVI<-2.5*(NIR-RED)/(NIR+6*RED-7.5*BLUE+1)

#SAVI (1+0.5)(NIR-RED)/(NIR+RED+0.5)
SAVI<-1.5*(NIR-RED)/(NIR+RED+0.5)

# MSAVI 
MSAVI<-(2*(NIR+1)-sqrt((2*NIR+1)^2-8*(NIR-RED)))/2

#OSAVI
OSAVI<-(NIR-RED)/(NIR+RED+0.16)

# mARI1
mARI1=(GREEN-1)/(RED-1)

# mARI2
mARI2=NIR*(GREEN-1)/(RED-1)

detach(data_11_f)
detach(new_11_f)
data_11_f<-cbind(data_11_f,NDVI,mNDVI,RVI,REEI,EVI,SAVI,MSAVI,OSAVI,mARI1,mARI2)
new_11_f<-cbind(new_11_f,NDVI,mNDVI,RVI,REEI,EVI,SAVI,MSAVI,OSAVI,mARI1,mARI2)

rm(NDVI,mNDVI,RVI,EVI,SAVI,FCV,MSAVI,OSAVI,mARI1,mARI2,REEI)

data_f<-cbind(Site=as.integer(site_buf),Site_low=as.integer(site_low),data_11_f)
plot_f<-data_f
plot_f<-cbind(plot_f,VEG=resu_f$VEG)
plot_f$Site[plot_f$Site>25 |plot_f$Site<1]<-NA
plot_f$VEG[plot_f$VEG==0]<-NA
plot_f<-na.omit(plot_f)

plot_f<-cbind(plot_f,VEG=NA)
plot_f$VEG[plot_f$RVI > 0.58 & plot_f$RVI < 0.62]<-"Pasture"
plot_f$VEG[plot_f$RVI > 0.78 ]<-"Saltbush"  #& plot_f$RVI < 0.9

plot_f$VEG[plot_f$NDVI > -0.2 & plot_f$NDVI < -0.1]<-"Pasture"
plot_f$VEG[plot_f$NDVI > 0.1 ]<-"Saltbush"

test_f<-na.omit(plot_f)

test<-test_f[4:17]
test$VEG<-as.factor(test$VEG)
test<-test[c(1:5,7,9:14)] # delete EVI and mNDVI
set.seed(101)
ind<-sample(2,nrow(test),replace=TRUE,prob=c(0.7,0.3))
iris.rf<-randomForest(VEG ~.,test[ind==1,],ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
#show the model
print(iris.rf)
importance(iris.rf)
aap<-varImpPlot(iris.rf,n.var = 10)

ggsave(aap,file ="1.pdf",dpi = 300)
plot(iris.rf)
#MDSplot(iris.rf)


iris.pred<-predict( iris.rf,test[ind==2,] )
#show the prediction result compare to original
table(observed=test[ind==2,"VEG"],predicted=iris.pred )

data<-data_f[3:15]
data<-cbind(data,VEG=NA)
data$VEG<-as.factor(data$VEG)
data$VEG<-"Pasture"
iris.pred<-predict( iris.rf,plot_f[c(4:8,10,12:17)])
iris.pred<-predict( iris.rf,plot_f[c(4:17)])
#-----show result for ENVI
result<-cbind(ID=plot_f$ID,VEG=iris.pred)
resu_f<-merge(data_11_f,result,by="ID",all.x = TRUE)
resu_f$VEG[is.na(resu_f$VEG)]<-0
VEG<-array(resu_f$VEG,c(3174,2161,1))

write.ENVI(VEG,"images/VEG")

#---cacluate BIOMass
info<-read.table("clipboard",T)
names(info)<-c("ID","Name","Pixels")
AGB<-read.table("clipboard",T)
AGB<-merge(info,AGB,by="Name",all.x = TRUE)
write.csv(AGB,"AGB.csv")
AGB<-read.csv("AGB.csv",header = TRUE)

BOI_f<-cbind(plot_f[c(1,2,4:16)],VEG=iris.pred)
BOI_f$VEG[BOI_f$VEG=="Pasture"]<-NA
BOI_f<-na.omit(BOI_f)
aa<-melt(BOI_f[1:15],id=c(1))
Site_mean<-dcast(aa,Site~variable,sum,na.rm=TRUE)
names(Site_mean)[1]<-"ID"
names(Site_mean)[2]<-"ID_1"
Site_mean<-merge(Site_mean[1:15],info,by="ID")
Site_all_c<-merge(Site_mean,AGB,by="Name")

Site_mean<-merge(Site_mean,info_round,by="Site")
Site_sum_c<-merge(Site_mean,AGB,by="Name")
for(i in 3:16){Site_sum_c[[i]]<-Site_sum_c[[i]]/1250}

for(i in 3:16){Site_all_c[[i]]<-Site_all_c[[i]]/Site_all_c$Pixels.x}
cor.test(Site_all_c$RVI,Site_all_c$X11)


#--- low

low_f<-cbind(plot_f[c(1,2,4:16)],VEG=iris.pred)
low_f$Site[low_f$Site_low>20 |low_f$Site_low<1]<-NA
low_f<-na.omit(low_f)

BOI_low_f<-low_f
BOI_low_NA_f<-low_f
BOI_low_NA_f$VEG[BOI_low_NA_f$VEG=="Pasture"]<-NA
BOI_low_NA_f<-na.omit(BOI_low_NA_f)

aa<-melt(BOI_low_NA_f[2:15],id=c(1))
Site_low_sum<-dcast(aa,Site_low~variable,sum,na.rm=TRUE)
Site_low_mean<-dcast(aa,Site_low~variable,mean,na.rm=TRUE)
names(Site_low_mean)[1]<-"ID"
names(Site_low_sum)[1]<-"ID"
ABG_low<-c(14.90235,19.01863156,8.329725967,10.26347865,6.998315696,12.64326683)
pixels_low<-c(1691,1728,1667,1462,1884,1604)
for(i in 2:14){Site_low_sum[[i]]<-Site_low_sum[[i]]/pixels_low}
cor.test(Site_low_mean$RVI,ABG_low)
cor.test(Site_low_sum$RVI,ABG_low)
plot(Site_low_sum$RVI,ABG_low)
rmse(Site_low_sum$RVI,ABG_low)
cor.test(Site_low_mean$MSAVI,ABG_low)
cor.test(Site_low_sum$MSAVI,ABG_low)
cor.test(Site_low_sum$NDVI,ABG_low)
write.csv(Site_all_c,"images/re_all_com.csv")
write.csv(Site_low_sum,"images/low_sum_com.csv")



data_f$mNDVI[is.infinite(data_f$mNDVI)]<-NA
data_f$EVI[is.infinite(data_f$EVI)]<-NA

data_f$EVI[data_f$EVI>1 | data_f$EVI < -1 ]<-NA
data<-na.omit(data_f)
summary(data)
data0<-cbind(data[4:16],VEG=NA)
all.pred<-predict( iris.rf,data0)
pre_all<-data.frame(ID=data$ID,VEG=all.pred)

result_f<-merge(data_11_f,pre_all,by="ID",all.x = TRUE)
result_f$VEG<-as.integer(result_f$VEG)
result_f$VEG[is.na(result_f$VEG)]<-0
VEG<-array(result_f$VEG,c(3174,2161,1))

write.ENVI(VEG,"111")




##--------use veg

data_f<-cbind(Site=as.integer(site_buf),Site_low=as.integer(site_low),data_11_f,VEG=result_f$VEG)
plot_f<-data_f
plot_f$Site[plot_f$Site>20 |plot_f$Site<1]<-NA
plot_f<-na.omit(plot_f)

BOI_f<-cbind(plot_f[c(1,2,4:17)])
BOI_f$VEG[BOI_f$VEG==0]<-NA
BOI_f<-na.omit(BOI_f)
aa<-melt(BOI_f[1:15],id=c(1))
Site_mean<-dcast(aa,Site~variable,sum,na.rm=TRUE)
names(Site_mean)[1]<-"ID"
Site_all_c<-merge(Site_mean,AGB,by="ID")

for(i in 3:15){Site_all_c[[i]]<-Site_all_c[[i]]/Site_all_c$Pixels}
cor.test(Site_all_c$mARI2,Site_all_c$X11)


#--- low

low_f<-cbind(plot_f[c(1,2,4:17)])
low_f$Site[low_f$Site_low>20 |low_f$Site_low<1]<-NA
low_f<-na.omit(low_f)

BOI_low_f<-low_f
BOI_low_NA_f<-low_f
BOI_low_NA_f$VEG[BOI_low_NA_f$VEG==1]<-NA
BOI_low_NA_f<-na.omit(BOI_low_NA_f)

aa<-melt(BOI_low_NA_f[2:15],id=c(1))
Site_low_sum<-dcast(aa,Site_low~variable,sum,na.rm=TRUE)
Site_low_mean<-dcast(aa,Site_low~variable,mean,na.rm=TRUE)
names(Site_low_mean)[1]<-"ID"
names(Site_low_sum)[1]<-"ID"
ABG_low<-c(14.90235,19.01863156,8.329725967,10.26347865,6.998315696,12.64326683)
pixels_low<-c(1691,1728,1667,1462,1884,1604)
for(i in 2:14){Site_low_sum[[i]]<-Site_low_sum[[i]]/pixels_low}
cor.test(Site_low_mean$RVI,ABG_low)
cor.test(Site_low_sum$RVI,ABG_low)
plot(Site_low_sum$RVI,ABG_low)
rmse(Site_low_sum$RVI,ABG_low)
cor.test(Site_low_mean$MSAVI,ABG_low)
cor.test(Site_low_sum$MSAVI,ABG_low)
cor.test(Site_low_sum$NDVI,ABG_low)
write.csv(Site_all_c,"images/re_10_com1.csv")
write.csv(Site_low_sum,"images/low_10_com1.csv")

a$Density[a$Density=="HD"]==1

with(a, slope.com(R_veg,C,site, method = 'SMA', alpha = 0.05)) 
 

# T_TEST        
  ## difference of variables for saltbush and pasture
  
  library(Hmisc)
  describe(plot_f$VEG)
  describe(plot_10_f$VEG)
  str(plot_f)
  linshi<-plot_10_f
  for (i in 3:15) {
    
   a<-t.test(linshi[linshi$VEG==1,i],linshi[linshi$VEG==2,i])
   print(c(names(linshi)[i],a$p.value))
    
  }
  ## difference of variables for different season
  linshi1<-plot_10_f
  linshi2<-plot_f
  for (i in 3:15) {
    
    #a<-t.test(linshi1[linshi1$VEG==2,i],linshi2[linshi2$VEG==2,i])
    #print(c(names(linshi1)[i],a$p.value))
    print(names(linshi)[i])
    print(c(mean(linshi1[linshi1$VEG==2,i]),mean(linshi1[linshi1$VEG==1,i]),mean(linshi2[linshi2$VEG==2,i]),mean(linshi2[linshi2$VEG==1,i])),digits=3)
  
    }
  rm(a,linshi,linshi1,linshi2)
#----------------------

# Spearm test for each variables with Carbon
  
  Mean_VI_bio<-read.table("clipboard",T)
  Site_VI_bio<-read.table("clipboard",T)
  RVEG<-read.table("clipboard",T)
  linshi<-RVEG
  linshi<-Mean_VI_bio
  linshi<-Site_VI_bio
  for (i in c(4:16)){
    
#   a<-pspearman::spearman.test(linshi[[i]],linshi[["C"]])
#   print(c(names(linshi)[i],a$estimate,a$p.value))
#   
  # Linear regression model
  a<-summary.lm(lm(linshi[["C"]] ~ linshi[[i]]))
  print(c(names(linshi)[i],a$coefficients[1],a$coefficients[2],a$coefficients[8]))  
  
  }
#---------------------


  
  save(list=ls(),file="result_0205.RData")
