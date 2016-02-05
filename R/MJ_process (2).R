


load("MJ_2002-2012.RData")
load("MJ_2002-2014.RData")
####-----re calculate GEP based on simulated AET

ANN_COM_MJ$YEAR[ANN_COM_MJ$YEAR<2002]<-NA
ANN_COM_MJ<-na.omit(ANN_COM_MJ)

names(ANN_COM_MJ)[11]<-"PET_W"
names(ANN_COM_MJ)[12]<-"ET_W"


GEP_W<-ANN_COM_MJ$AET
GEP_2002<-ANN_COM_MJ$AET
GEP_LC<-ANN_COM_MJ$AET

WUE_merge<-c(2.46,2.59,2.46,3.2,2.74,1.37,2.12,3.13,0)
#WUE_IGBP<-c(2.46,2.59,2.46,3.2,2.74,1.37,1.33,1.26,2.12,3.13,0)
WUE_IGBP<-c(2.46,2.59,2.46,3.2,2.74,1.37,1.33,1.26,1.26,2.12,0,3.13,0,0,0,0)

#LC_1<-which(RESULT_frame$VEG[!is.na(RESULT_frame$VEG)]==1)
.LC<-0
for (i in 1:9){
  .LC<-which(ANN_COM_MJ$VEG==i)
  GEP_W[.LC]<-GEP_W[.LC]*WUE_merge[i]
  print(i)
}


for (i in 1:16){
  .LC_2002<-which(ANN_COM_MJ$LC_2002==i)
  GEP_2002[.LC_2002]<-GEP_2002[.LC_2002]*WUE_IGBP[i]
  .LC<-which(ANN_COM_MJ$LC==i)
  GEP_LC[.LC]<-GEP_LC[.LC]*WUE_IGBP[i]
  print(i)
}

range(GEP_W)
  
summary(GEP_W)
summary(ANN_COM_MJ$GEP.gC.m2.yr.)
rmse(GEP_W1,ANN_COM_MJ$MOD_GEP)
pbias(ANN_COM_MJ$ET_W,ANN_COM_MJ$AET)

pbias(GEP_LC,GEP_2002)
pbias(GEP_W,GEP_2002)
pbias(GEP_W,GEP_LC)

pbias(GEP_2002,ANN_COM_MJ$MOD_GEP)


ANN_COM_MJ<-cnind()

save(list=ls(),file="linshi.RData")



####----------statistic veg and basin flux
library(reshape2)

STA_BASIN<-ANN_COM_MJ_2002[,c(c(1),6:30)]
linshi<-melt(STA_BASIN,id=c(1,2))
STA_BASIN_ANN<-dcast(linshi,BASIN+YEAR~variable,mean)
linshi<-melt(STA_BASIN,id=c(1))
STA_BASIN<-dcast(linshi,BASIN~variable,mean)

STA_VEG<-ANN_COM_MJ_2002[,c(c(2),6:30)]
linshi<-melt(STA_VEG,id=c(1,2))
STA_VEG_ANN<-dcast(linshi,VEG+YEAR~variable,mean)
linshi<-melt(STA_VEG,id=c(1))
STA_VEG<-dcast(linshi,VEG~variable,mean)

STA_BASIN_VEG<-ANN_COM_MJ_2002[,c(c(1,2),6:30)]
linshi<-melt(STA_BASIN_VEG,id=c(1,2,3))
STA_BASIN_VEG_ANN<-dcast(linshi,BASIN+VEG+YEAR~variable,mean)
linshi<-melt(STA_BASIN_VEG,id=c(1,2))
STA_BASIN_VEG<-dcast(linshi,BASIN+VEG~variable,mean)

#---------------------------
ANN_COM_MJ$BASIN<-as.factor(ANN_COM_MJ$BASIN)
ANN_COM_MJ$VEG<-as.factor(ANN_COM_MJ$VEG)
ANN_COM_MJ$LC<-as.factor(ANN_COM_MJ$LC)
ANN_COM_MJ$LC_2002<-as.factor(ANN_COM_MJ$LC_2002)


summary(ANN_COM_MJ)
ANN_COM_MJ$YEAR[ANN_COM_MJ$YEAR<2002]<-NA
ANN_COM_MJ<-na.omit(ANN_COM_MJ)

a<-ANN_COM_MJ[c(c(6),9:32)]
b<-melt(a,id<-c(1))
ann_mean_MJ<-dcast(b,ID~variable,mean)
INFO<-ANN_COM_MJ[1:9]
INFO$YEAR[INFO$YEAR>2002]<-NA
INFO<-na.omit(INFO)
summary(INFO)

ann_mean_MJ<-cbind(INFO[1:8],ann_mean_MJ)
ann_mean_MJ<-cbind(ann_mean_MJ,Q=ann_mean_MJ$Pre-ann_mean_MJ$AET)
summary(ann_mean_MJ)

ann_mean_main_veg<-ann_mean_MJ
ann_mean_main_veg$VEG[ann_mean_main_veg$VEG==6 |ann_mean_main_veg$VEG==2 |ann_mean_main_veg$VEG==3 |ann_mean_main_veg$VEG==9]<-NA
ann_mean_main_veg<-na.omit(ann_mean_main_veg)

summary(ann_mean_main_veg)
ann_mean_main_veg$VEG<-as.integer(ann_mean_main_veg$VEG)
ann_mean_main_veg$VEG<-as.factor(ann_mean_main_veg$VEG)
levels(ann_mean_main_veg$VEG)[levels(ann_mean_main_veg$VEG)=="1"] <- "Evergreen needleleaf forest"
levels(ann_mean_main_veg$VEG)[levels(ann_mean_main_veg$VEG)=="4"] <- "Deciduous broadleaf forest"
levels(ann_mean_main_veg$VEG)[levels(ann_mean_main_veg$VEG)=="5"] <- "Mixed forest"
levels(ann_mean_main_veg$VEG)[levels(ann_mean_main_veg$VEG)=="7"] <- "Grasslands"
levels(ann_mean_main_veg$VEG)[levels(ann_mean_main_veg$VEG)=="8"] <- "Croplands"


##---carbon RE and NEP
NEP_YU<-(0.29*ann_mean_MJ$GEP.gC.m2.yr.-37.22)
RE_YU<-(0.68*ann_mean_MJ$GEP.gC.m2.yr.+81.9)
NEP_YU<-ann_mean_MJ$GEP.gC.m2.yr.-RE_YU
summary(NEP_YU)

ANN_COM_MJ<-cbind(ANN_COM_MJ,RE_YU=(0.68*ANN_COM_MJ$GEP.gC.m2.yr.+81.9),NEP_YU=(ANN_COM_MJ$GEP.gC.m2.yr.-0.68*ANN_COM_MJ$GEP.gC.m2.yr.-81.9))
ANN_COM_MJ<-cbind(ANN_COM_MJ,Q=ANN_COM_MJ$Pre-ANN_COM_MJ$AET)
summary(ANN_COM_MJ)


ann_mean_MJ<-cbind(ann_mean_MJ[1:34],RE_YU=(0.68*ann_mean_MJ$GEP.gC.m2.yr.+81.9),NEP_YU=(ann_mean_MJ$GEP.gC.m2.yr.-0.68*ann_mean_MJ$GEP.gC.m2.yr.-81.9))
summary(ann_mean_MJ)

ann_mean_main_veg<-cbind(ann_mean_main_veg,RE_YU=(0.68*ann_mean_main_veg$GEP.gC.m2.yr.+81.9),NEP_YU=(ann_mean_main_veg$GEP.gC.m2.yr.-0.68*ann_mean_main_veg$GEP.gC.m2.yr.-81.9))
summary(ann_mean_main_veg)


RE_A<-0.87*ann_mean_MJ$GEP.gC.m2.yr.
NEP_A<-ann_mean_MJ$GEP.gC.m2.yr.-RE_A
ann_mean_MJ<-cbind(ann_mean_MJ,NEP_A=NEP_A)

RE_A<-0.87*ann_mean_main_veg$GEP.gC.m2.yr.
NEP_A<-ann_mean_main_veg$GEP.gC.m2.yr.-RE_A
ann_mean_main_veg<-cbind(ann_mean_main_veg,NEP_A=NEP_A)


ann_mean_MJ$NEP_YU[ann_mean_MJ$NEP_YU<0]<-0
ann_mean_main_veg$NEP_YU[ann_mean_main_veg$NEP_YU<0]<-0


##_---- mean annual change of water and carbon flux

mean_ann_MJ<-ANN_COM_MJ[,c(1,2,9,10,17,22,30,34,35)]
mean_ann_MJ$DSI[mean_ann_MJ$DSI< -10]<-NA
linshi<-melt(mean_ann_MJ[3:9],id=c("YEAR"))
mean_ann_MJ_Y<-dcast(linshi,YEAR~variable,mean,na.rm=TRUE)

linshi<-melt(mean_ann_MJ[2:9],id=c("VEG","YEAR"))
mean_ann_MJ_Veg<-dcast(linshi,VEG+YEAR~variable,mean,na.rm=TRUE)


mean_ann_MJ_Veg<-mean_ann_MJ_Veg[,c(1,2,3,5,6,7,8)]
mean_ann_MJ_Veg$VEG[mean_ann_MJ_Veg$VEG==6 |mean_ann_MJ_Veg$VEG==2 |mean_ann_MJ_Veg$VEG==3 |mean_ann_MJ_Veg$VEG==9]<-NA
mean_ann_MJ_Veg<-na.omit(mean_ann_MJ_Veg)

summary(mean_ann_MJ_Veg)
mean_ann_MJ_Veg$VEG<-as.integer(mean_ann_MJ_Veg$VEG)
mean_ann_MJ_Veg$VEG<-as.factor(mean_ann_MJ_Veg$VEG)
levels(mean_ann_MJ_Veg$VEG)[levels(mean_ann_MJ_Veg$VEG)=="1"] <- "Evergreen needleleaf forest"
levels(mean_ann_MJ_Veg$VEG)[levels(mean_ann_MJ_Veg$VEG)=="4"] <- "Deciduous broadleaf forest"
levels(mean_ann_MJ_Veg$VEG)[levels(mean_ann_MJ_Veg$VEG)=="5"] <- "Mixed forest"
levels(mean_ann_MJ_Veg$VEG)[levels(mean_ann_MJ_Veg$VEG)=="7"] <- "Grasslands"
levels(mean_ann_MJ_Veg$VEG)[levels(mean_ann_MJ_Veg$VEG)=="8"] <- "Croplands"

mean_ann<-melt(mean_ann_MJ_Y[,c(1,2,4,5,6,7)],id=("YEAR"))



###-------trend analysis 

rkt(mean_ann_MJ_Y$YEAR,mean_ann_MJ_Y$NEP_YU)
cor.test(mean_ann_MJ_Y$YEAR,mean_ann_MJ_Y$Pre)
a<-ts(mean_ann_MJ_Y$Pre)
mk.test(a)
sens.slope(mean_ann_MJ_Y$Pre, level = 0.95)



##------add new basin


b_new<-read.ENVI("basin_new")
b_frame<-data.frame(b_1=as.vector(b_new[,,1]),b_2=as.vector(b_new[,,5]))

b_frame$b_2[b_frame$b_1<=0]<-NA
b_frame<-na.omit(b_frame)

ANN_COM_new<-cbind(BASIN_new=rep(b_frame$b_2,13),ANN_COM_MJ)
ANN_COM_new$BASIN_new[ANN_COM_new$BASIN_new>10]<-NA
ANN_COM_new<-na.omit(ANN_COM_new)
summary(ANN_COM_new)
names(ANN_COM_new)[1]<-"Basin"
ANN_COM_new$Basin<-as.factor(ANN_COM_new$Basin)
levels(ANN_COM_new$Basin)[levels(ANN_COM_new$Basin)=="0"] <- "Upper Minjiang watershed"
levels(ANN_COM_new$Basin)[levels(ANN_COM_new$Basin)=="1"] <- "Heishui watershed"
levels(ANN_COM_new$Basin)[levels(ANN_COM_new$Basin)=="2"] <- "Lower Minjiang watershed"
levels(ANN_COM_new$Basin)[levels(ANN_COM_new$Basin)=="3"] <- "Zagunao watershed"
levels(ANN_COM_new$Basin)[levels(ANN_COM_new$Basin)=="4"] <- "Yuzixi watershed"

mean_ann_B<-ANN_COM_new[,c(0,2,9,10,22,30,34,35)+1]


linshi<-melt(mean_ann_B[c(c(1),3:8)],id=c("Basin","YEAR"))
mean_ann_B_V_Y<-dcast(linshi,Basin+VEG+YEAR~variable,mean,na.rm=TRUE)


mean_ann_B_V_Y$VEG[mean_ann_B_V_Y$VEG==6 |mean_ann_B_V_Y$VEG==2 |mean_ann_B_V_Y$VEG==3 |mean_ann_B_V_Y$VEG==9]<-NA
mean_ann_B_V_Y<-na.omit(mean_ann_B_V_Y)

summary(mean_ann_B_V_Y)
mean_ann_B_V_Y$VEG<-as.factor(mean_ann_B_V_Y$VEG)
levels(mean_ann_B_V_Y$VEG)[levels(mean_ann_B_V_Y$VEG)=="1"] <- "Evergreen needleleaf forest"
levels(mean_ann_B_V_Y$VEG)[levels(mean_ann_B_V_Y$VEG)=="4"] <- "Deciduous broadleaf forest"
levels(mean_ann_B_V_Y$VEG)[levels(mean_ann_B_V_Y$VEG)=="5"] <- "Mixed forest"
levels(mean_ann_B_V_Y$VEG)[levels(mean_ann_B_V_Y$VEG)=="7"] <- "Grasslands"
levels(mean_ann_B_V_Y$VEG)[levels(mean_ann_B_V_Y$VEG)=="8"] <- "Croplands"



### veg ratio for each basin
aa<-read.ENVI("M:/CLIMATE_RESULTS/OUT_FINAL/Final/ENVI/data_tar")
a_frame<-data.frame(basin=as.vector(b_new[,,5]),b=as.vector(b_new[,,1]),Veg=as.vector(aa[,,5]))

a_frame$basin[a_frame$b<=0]<-NA
a_frame$basin[a_frame$basin>10]<-NA
names(a_frame)<-c("Basin","b","VEG")
a_frame<-na.omit(a_frame)
a_frame$VEG[a_frame$VEG==6 |a_frame$VEG==2 |a_frame$VEG==3 |a_frame$VEG==9]<-NA
a_frame<-na.omit(a_frame)

summary(a_frame)
a_frame$VEG<-as.factor(a_frame$VEG)
levels(a_frame$VEG)[levels(a_frame$VEG)=="1"] <- "Evergreen needleleaf forest"
levels(a_frame$VEG)[levels(a_frame$VEG)=="4"] <- "Deciduous broadleaf forest"
levels(a_frame$VEG)[levels(a_frame$VEG)=="5"] <- "Mixed forest"
levels(a_frame$VEG)[levels(a_frame$VEG)=="7"] <- "Grasslands"
levels(a_frame$VEG)[levels(a_frame$VEG)=="8"] <- "Croplands"



a_frame$Basin<-as.factor(a_frame$Basin)
levels(a_frame$Basin)[levels(a_frame$Basin)=="0"] <- "Upper Minjiang watershed"
levels(a_frame$Basin)[levels(a_frame$Basin)=="1"] <- "Heishui watershed"
levels(a_frame$Basin)[levels(a_frame$Basin)=="2"] <- "Lower Minjiang watershed"
levels(a_frame$Basin)[levels(a_frame$Basin)=="3"] <- "Zagunao watershed"
levels(a_frame$Basin)[levels(a_frame$Basin)=="4"] <- "Yuzixi watershed"
summary(a_frame)



a1<-count(a_frame,c("Basin","VEG"))


linshi<-melt(mean_ann_B_V_Y,id=c(1,2))
mean_ann_B_V_m<-dcast(linshi,Basin+VEG~variable,mean,na.rm=TRUE)
mean_ann_B_V_S<-dcast(linshi,Basin+VEG~variable,sd,na.rm=TRUE)

summary(mean_ann_B_V_Y)

write.csv(mean_ann_B_V_m,"mean_ann_B_V_m.csv")
write.csv(mean_ann_B_V_S,"mean_ann_B_V_S.csv")
write.csv(a1,"ratio_B_V.csv")
pbias()
library(caTools)
ce<-data.frame(v1=c(1:10),v2=c(11:20),YEAR=c(2001:2010))
a<-rkt(date=ce$YEAR,ce$v1)

as<-  as.data.frame(a2)
a<-lapply(ce, rkt,date=ce$YEAR)

library(doParallel)

detectCores()

cl <- makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()

a3<-parLapply(ce, rkt,date=ce$YEAR)
lapply(ce, rkt,date=ce$YEAR)
c
a11<-matrix(as.vector(t(as)),ncol=12,byrow = TRUE)

matrix()

a4<-as.vector(t(as))

a2<-lapply(a,unclass)
lin3<-lapply(lin2, rkt,date=lin2$YEAR)

MK<-data.frame(P=rep(0,length(lin3)-1),Score=rep(0,length(lin3)-1),Slope=rep(0,length(lin3)-1),TAU=rep(0,length(lin3)-1))
for (i in 2:length(lin3)){
  
  MK$P[i]<-lin3[[i+1]][1]
  MK$Score<-lin3[[i+1]][2]
  MK$Slope[i]<-lin3[[i+1]][3]
  MK$TAU[i]<-lin3[[i+1]][12]
  print(i)
}
a1$v1$sl
b<-unlist(aa)

names(aa$v1)


data2 <- as.data.frame(cbind(rnorm(16),rep(c(1,2,3,4),4),rep(c(1,2),each=8)))
colnames(data2) <- c("Result","VAR1","VAR2")
data2.avg1 <- tapply(data2$Result,data2$VAR1,mean)
data2.avg12 <- tapply(data2$Result,list(data2$VAR1,data2$VAR2),mean)


aa<-read.ENVI("F:/MOD09/ceshi")
a<-paste(rev(as.numeric()),collapse = "")
substr(a,1,2)
substr(a,1,2)
