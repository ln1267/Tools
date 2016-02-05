rm(list=ls())
library(caTools)
library(raster)
library(ncdf)
library(plyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(hydroGOF)

if (1) {setwd("M:/CLIMATE_RESULTS/MJ/")} #this directory is for windows

load("ANN_COM_MJ_2000.RData")

theme_grid <- function(base_size = 12, base_family = "Times"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      axis.title = element_text(size = 14,face="bold"),
      axis.text = element_text(colour="black", size=12),
      #strip.text = element_text(size=12),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "black", size=2),
      panel.background = element_rect(fill = "grey70", colour = "black"), 
      strip.background = element_rect(fill = NA),
      legend.position="right",
      legend.background = element_blank()
      
    )
}

####-------plot data

names(ANN_COM_MJ_2002)
for(i in 7:30){
  gplot<-ggplot(aes(x = LONG, y = LAT), data = ANN_COM_MJ_2002) +
    geom_raster(aes(LONG, LAT, fill=ANN_COM_MJ_2002[[i]]))+
    facet_wrap( ~ YEAR, ncol=2)+
    scale_fill_gradient(low = 'red', high = 'green',name=names(ANN_COM_MJ_2002)[i],na.value = "grey70") +
    coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
    labs(x="Latitude (°)",y="Longitude (°)")+ 
    theme_grid()
  
  ggsave(gplot,file =paste("pdf/",names(ANN_COM_MJ_2002),".pdf",sep="")[i],dpi = 300)
}



####-------plot data

names(ann_mean_MJ)
names(ann_mean_MJ)[i]
i<-34
for(i in 21:33){
  gplot<-ggplot(aes(x = LONG, y = LAT), data = ann_mean_MJ) +
    geom_raster(aes(LONG, LAT, fill=ann_mean_MJ[[i]]))+
  #  facet_wrap( ~ YEAR, ncol=2)+
    scale_fill_gradient(low = 'red', high = 'green',name="",na.value = "grey70") +
    coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
    labs(x="Latitude (°)",y="Longitude (°)")+ 
    theme_grid()
  
  ggsave(gplot,file =paste("pdf/",names(ann_mean_MJ),".pdf",sep="")[i],dpi = 300)
  print(i)
}


##----plot funtion for same scheme plot
plot_f<-function(name1){
  
  gplot<-ggplot(aes(x = LONG, y = LAT), data = ann_mean_MJ) +
    geom_raster(aes(LONG, LAT, fill=ann_mean_MJ[[a]]))+
    #  facet_wrap( ~ YEAR, ncol=2)+
    scale_fill_gradient(low = 'red', high = 'green',name=name1,na.value = "grey70") +
    coord_equal(xlim=c(102.4, 104.1),ylim = c(30.72,33.18))+
    labs(x="Latitude (°)",y="Longitude (°)")+ 
    theme_grid()
  ggsave(gplot,file =paste("pdf/",names(ann_mean_MJ),".pdf",sep="")[a],dpi = 300)
  print(paste("pdf/",names(ann_mean_MJ),".pdf",sep="")[a])
}
####-------plot veg water balance box

veg_plot<-function(name1){
  g_plot<-ggplot(data = ann_mean_main_veg, aes(x = VEG, y = ann_mean_main_veg[[a]])) + 
    stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) +
    geom_boxplot() + xlab("Vegetation types") + 
    ylab(name1) + theme_bw(base_size = 16, base_family = "Times")
  ggsave(g_plot,file =paste("box/",names(ann_mean_MJ),".pdf",sep="")[a],dpi = 300)
  #print(g_plot)
}

###   plot annual mean line 
line_plot<-function(name1){
  r<-coef(lm(mean_ann_MJ_Y[[a]] ~ YEAR, data = mean_ann_MJ_Y))
  print(r[2])
  l_plot<- ggplot(data = mean_ann_MJ_Y, aes(x = YEAR, y = mean_ann_MJ_Y[[a]])) + geom_point(size=4,shape=21, fill="white") + 
    geom_line(size = 1) + scale_x_continuous(breaks=2002:2014)+
    xlab("YEAR") + ylab(name1) + theme_bw(base_size = 14, base_family = "Times") +
    geom_abline(intercept = r[1], slope = r[2])
  ggsave(l_plot,file =paste("line/",names(mean_ann_MJ_Y),".pdf",sep="")[a],dpi = 300)
  print(names(mean_ann_MJ_Y)[a])
}
#---plot pre
a<-11
plot_f("Precipitation \n(mm)")
veg_plot("Precipitation (mm)")
#---plot AET
a<-23
plot_f("Evapotranspiration (ET)\n(mm)")
veg_plot("Evapotranspiration (ET) (mm)")
#---plot Runoff
a<-34
plot_f("Runoff \n(mm)")
veg_plot("Runoff (mm)")


#---plot GEP
a<-31
plot_f("Gross ecosystem productivity\n(GEP)")
veg_plot("Gross ecosystem productivity (GEP)")

#---plot RE
a<-35
plot_f("Ecosystem respiration\n(RE)")
veg_plot("Ecosystem respiration (RE)")

#---plot NEP
a<-36
plot_f("Net ecosystem productivity\n(NEP)")
veg_plot("Net ecosystem productivity (NEP)")

a<-37
plot_f("Net ecosystem productivity\n(NEP)")
veg_plot("Net ecosystem productivity (NEP)")

a<-16
plot_f("Net ecosystem productivity\n(NEP)")
veg_plot("Net ecosystem productivity (NEP)")

#--------------
a=2
line_plot("Precipitation (mm)")
a=4
line_plot("Evapotranspiration (ET) (mm)")
a=5
line_plot("Gross ecosystem productivity")

a=6
line_plot("Net ecosystem productivity")

a=7
line_plot("Runoff (mm)")


####-------plot MJ runoff validation data

cof<-coef(lm(Q[3:9] ~ Observed[3:9], data = ann_mean_MJ))

ggplot(ann_mean_MJ, aes(x=Observed[3:9], y=Q[3:9])) +
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





ggplot(data = .df, aes(x = x, y = y)) + geom_point() + 
  geom_line(size = 1) + scale_y_continuous(expand = c(0.01, 0)) + facet_wrap( 
    ~ t) + xlab("YEAR") + ylab("value") + theme_bw(base_size = 14, base_family 
                                                   = "serif") + theme(panel.margin = unit(0.3, "lines"))



 
 
library(trend)
mk.test(mean_ann_MJ_Y$YEAR)
pettitt.test(mean_ann_MJ_Y$AET)
data(Nile)
data(pie1)
ex<-rkt(mean_ann_MJ_Y$YEAR,mean_ann_MJ_Y$AET)
print(ex)
dev.off()
