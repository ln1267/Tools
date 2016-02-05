#---------------------------
rm(list =ls()) # this is for clear all the data in memory of R
library(caTools)
library(raster)
library(ggmap)
library(ggplot2)
library(raster)
library(rasterVis)
library(grid)
#pdf("C1.pdf")
name_ET<-"M:/CLIMATE_RESULTS/Climate_new/ENVI_C1/C1/ENVI/AET_ANN_1"
name_RUNOFF<-"M:/CLIMATE_RESULTS/Climate_new/ENVI_C1/C1/ENVI/RUNOFF_ANN_1"
name_tar<-"M:/CLIMATE_RESULTS/ENVI/data_tar"

data_tar<-read.ENVI(name_tar)
data_ET<-read.ENVI(name_ET)
data_RUNOFF<-read.ENVI(name_RUNOFF)
dim_data<-dim(data_ET)
for (i in 1:dim_data[3]){
  
  data_ET[,,i][data_tar[,,1]<=0]<-NA
  data_RUNOFF[,,i][data_tar[,,1]<=0]<-NA
  
}


mean_ET<-apply(data_ET[,,2:13],c(1,2),mean)
mean_RUNOFF<-apply(data_RUNOFF[,,2:13],c(1,2),mean)
ET_r<-raster(mean_ET,xmn=102.367,xmx=104.177,ymn=30.59,ymx=33.18,crs="+proj=longlat +datum=WGS84")

#ET_B<-brick(data_ET,xmn=102.367,xmx=104.177,ymn=30.59,ymx=33.18,crs="+proj=longlat +datum=WGS84")

RUNOFF_r<-raster(mean_RUNOFF,xmn=102.367,xmx=104.177,ymn=30.59,ymx=33.18,crs="+proj=longlat +datum=WGS84")

#plot raster data

ET_g<-gplot(ET_r) + geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'black', high = 'white',name="ET\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.5, 104.3),ylim = c(30.72,33.18))+
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()

#+  ggtitle("ET")

RUNOFF_g<-gplot(RUNOFF_r) + geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'black', high = 'white',name="RUNOFF\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.5, 104.3),ylim = c(30.72,33.18))+
  labs(x="Llatitude (°)",y="Longitude (°)")+ 
  theme_grid()
  

name_PRE<-"M:/CLIMATE_RESULTS/ENVI/Pre_worldclimate_ann"
data_pre<-read.ENVI(name_PRE)
data_pre[data_tar[,,1]<=0]<-NA
PRE_r<-raster(data_pre,xmn=102.367,xmx=104.177,ymn=30.59,ymx=33.18,crs="+proj=longlat +datum=WGS84")
PRE_fram<-as.data.frame(PRE_r,xy=TRUE)
PRE_g<-gplot(PRE_r) + geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = 'black', high = 'white',name="Precipitation\n(mm)",na.value = "grey70") +
  coord_equal(xlim=c(102.5, 104.3),ylim = c(30.72,33.18))+
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()

playwith(qplot(x,y,data=a11))
levelplot(PRE_r))

  ggtitle("PRE")

multiplot(ET_g, RUNOFF_g,PRE_g, PRE_g, cols=2)

##--------------Land cover

LUCC<-data_tar[,,4]
LUCC[data_tar[,,1]<=0]<-NA

LUCC[LUCC==1]<-1
LUCC[LUCC==2]<-2
LUCC[LUCC==3]<-3
LUCC[LUCC==4]<-4
LUCC[LUCC==5]<-5
LUCC[LUCC==6 |LUCC==7]<-6
LUCC[LUCC==8 |LUCC==9 |LUCC==10]<-7
LUCC[LUCC==12 |LUCC==14]<-8
LUCC[LUCC==11 |LUCC==13 |LUCC==15 |LUCC==16 |LUCC==0 ]<-9

LUCC_r<-raster(LUCC,xmn=102.367,xmx=104.177,ymn=30.59,ymx=33.18,crs="+proj=longlat +datum=WGS84")
is.factor(LUCC_r)
LUCC_r<-as.factor(LUCC_r)
LUCC_r1<-ratify(LUCC_r)
rat<-levels(LUCC_r)[[1]]
rat$classes <- c('Evergreen Needleleaf forest', 'Evergreen Broadleaf forest', 'Deciduous Needleleaf forest', 'Deciduous Broadleaf forest','Mixed forest','shrublands','Grasslands','Croplands','Nonvegtation')
levels(LUCC_r) <- rat

pal <- c('#003300', # Forest
         '#00CC33', # Land
         '#336633', # Urban
         '#66FF33',
         '#3399FF',
         '#FFFFCC',
         '#66FF99',
         '#FFFF99',
         '#CCCCCC')      # Snow

catTheme <- modifyList(rasterTheme(),
                       list(panel.background = list(col='white'),
                            regions = list(col= pal)))

levelplot(LUCC_r, maxpixels=3.5e5, par.settings=catTheme,
          panel=panel.levelplot.raster)


##------plot MODIS land cover map
dat <- as.data.frame(LUCC_r, xy=TRUE)
LUCC_g<-ggplot(aes(x = x, y = y), data = dat) +
  geom_raster(aes(x, y, fill=layer_class))+
  coord_equal(xlim=c(102.5, 104),ylim = c(30.72,33.18))+
  labs(x="latitude (°)",y="longitude (°)")+ 
  scale_fill_manual(values=c("#FFFF99", "#66FF33", "#336633","#00CC33", "003300", "#66FF99","#3399FF", "#CCCCCC", "#FFFFCC") , 
                    name="Vegetation types\n",breaks=levels(dat$layer_class),labels=levels(dat$layer_class))+
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()+theme(legend.position="right",
                     panel.background = element_rect(fill = "white"),
                     legend.title = element_text(size=14, face="bold"))

##------plot Merge land cover map
dat <- as.data.frame(LC_Merge_r, xy=TRUE)
LC_Merge_g<-ggplot(aes(x = x, y = y), data = dat) +
  geom_raster(aes(x, y, fill=layer_class))+
  coord_equal(xlim=c(102.5, 104),ylim = c(30.72,33.18))+
  labs(x="latitude (°)",y="longitude (°)")+ 
  scale_fill_manual(values=c("#FFFF99", "#CCFF00", "#336633","#00CC33", "003333", "#00FF00","#3399FF", "#CCCCCC", "#FFFFCC") , 
                    name="Vegetation types\n",breaks=levels(dat$layer_class),labels=levels(dat$layer_class))+
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()+theme(legend.position="right",
                     panel.background = element_rect(fill = "white"),
                     legend.title = element_text(size=14, face="bold"))


##------plot difference of forest and modis land cover map
dat <- as.data.frame(LC_dif_r, xy=TRUE)
LC_dif_g<-ggplot(aes(x = x, y = y), data = dat) +
  geom_raster(aes(x, y, fill=layer_class))+
  coord_equal(xlim=c(102.5, 104),ylim = c(30.72,33.18))+
  labs(x="latitude (°)",y="longitude (°)")+ 
  scale_fill_manual(values=c("#FFFF99", "#66FF33", "#336633","#00CC33", "003300", "#66FF99","#3399FF", "#CCCCCC", "#FFFFCC") , 
                    name="Vegetation types\n",breaks=levels(dat$layer_class),labels=levels(dat$layer_class))+
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()+theme(legend.position="right",
                     panel.background = element_rect(fill = "white"),
                     legend.title = element_text(size=14, face="bold"))


##------plot Merge land cover map VS elevation
data_brick<-brick(LC_Merge_r,DEM_r)
names(data_brick)<-c("LC_Merge","DEM")

dat <- as.data.frame(data_brick, xy=TRUE)
dat$LC_Merge_class[dat$LC_Merge_class=="Nonvegtation"]<-NA
dat$LC_Merge_class[dat$LC_Merge_class==class_MOD[2] | dat$LC_Merge_class==class_MOD[3]| dat$LC_Merge_class==class_MOD[4]| dat$LC_Merge_class==class_MOD[6]| dat$LC_Merge_class==class_MOD[9]]<-NA
dat$LC_Merge_class[dat$LC_Merge_class=="Nonvegtation"]<-NA
dat1<-na.omit(dat)
ggplot(data = dat1, aes(x = LC_Merge_class, y = DEM)) + 
  stat_boxplot(geom= "errorbar", stat_params = list(width = 0.5), geom_params = list()) + 
  geom_boxplot(fill="grey") + xlab("Vegetation types") + ylab("Elevation (m)") + 
  theme_grid()+theme(panel.background = element_rect(fill = "white"),
axis.title = element_text(size = 16,face="bold"),
axis.text = element_text(colour="black", size=14))

###-----------------------------------------------------------------------------------
library(lattice)
library(ggplot2)
library(xlsx)
RUNOFF_V<-read.xlsx("M:/CLIMATE_RESULTS/ENVI/Result_COMP.xlsx", 2, as.data.frame=TRUE,endRow=14, header=TRUE)


#pdf("Valid.pdf")
attach(RUNOFF_V)
sim<-C1[3:9]
obs<-MJ[3:9]
detach(RUNOFF_V)

summary(RUNOFF_V)
data_Q<-data.frame(Year=c(2002:2008),Simulated=sim,Observed=obs)
plot(obs,sim,
     
     xlab = "Observed annual runoff (mm)",
     ylab = "Simulated annual runoff (mm)",
     pch=19,
     xlim = c(450,700),
     ylim=c(350,600))
abline(lm(Simulated ~ Observed, data = data_Q))
text(550, 550, "Y = 0.94 * X - 93.8",
     cex = 1.2)
text(550, 530, expression(R^2*" = 0.75; RMSE = 135 mm"))

a<-read.table("clipboard",T)


cof<-coef(lm(Simulated ~ Observed, data = data_Q))
rmse(data_Q$Observed,data_Q$Simulated)
ggplot(a, aes(x=Q_MJ, y=Q)) +
  geom_point(size=4) +    # Use hollow circles
  geom_abline(intercept = cof[1], slope = cof[2]) +   # Don't add shaded confidence region
  #geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  scale_x_continuous(name="Observed annual runoff (mm)") +
  scale_y_continuous(name="Simulated annual runoff (mm)")+#limits=c(300, 700)
  theme(axis.title.x = element_text(family="Times",face="bold", colour="black", size=12),
        axis.title.y  = element_text(family="Times",face="bold", colour="black", size=12),
        axis.text.x  = element_text(family="Times",face="bold",size=10),
        axis.text.y  = element_text(family="Times",face="bold",size=10))+
  annotate("text",family="Times", x = 500, y = 525, label = "Y = 0.94 * X - 93.8", fontface="italic",size=8)+
annotate("text",family="Times", x = 500, y = 500, label="R^2 = 0.75\n RMSE = 135 mm", size=6)

#ylab(expression("today's temperature is "*-5~degree*C))
qplot(1,1) + ylab(expression(Temp^2))


dev.off()

df <- data.frame(x = RUNOFF_V$MJ[3:9], y = RUNOFF_V$C1[3:9])
.plot <- 
  playwith(
    scatterplot(y~x, reg.line=lm, smooth=FALSE, spread=FALSE, boxplots=FALSE, 
                span=0.5, ellipse=FALSE, levels=c(.5, .9), data=df)
  )
print(.plot)