

plot_sig<-function(name1,i,v){
   i<<-i
   v<<-v
    Index_sig<-which(STA_ALL[[i]]=="Very significant")
    sp<-STA_ALL[Index_sig,]
    print(summary(sp))
     gplot<-ggplot(aes(x = LONG, y = LAT), data = sp) +
     geom_raster(aes(LONG, LAT, fill=sp[[v]]))+
    #  facet_wrap( ~ YEAR, ncol=2)+
    scale_fill_gradient(low = 'red', high = 'green',name=name1,na.value = "grey70") +
    coord_equal()+ #limits=c(0,1500),xlim=c(114.4, 118.1),ylim = c(-15.5,-11)#,xlim=c(123.4, 128.1),ylim = c(-13,-9),
    labs(x="Latitude (°)",y="Longitude (°)")+ 
    theme_grid()
   print(gplot)
     ggsave(gplot,file =paste("images/",v,".pdf",sep=""),dpi = 300)
  #   print(paste("images/",v,".pdf",sep="")[a])
 
}


i<-"Temp_NDVI_Sig"
v<-"R_Tem_NDVI"
name1<-"R^2 Temp vs NDVI"

Index_sig<-which(STA_ALL[[i]]=="Very significant")
sp<-STA_ALL[Index_sig,]
print(summary(sp))
gplot<-ggplot(aes(x = LONG, y = LAT), data = sp) +
  geom_raster(aes(LONG, LAT, fill=sp[[v]]))+
  #  facet_wrap( ~ YEAR, ncol=2)+
  scale_fill_gradient(low = 'red', high = 'green',name=name1,na.value = "grey70") +
  coord_equal()+ #limits=c(0,1500),xlim=c(114.4, 118.1),ylim = c(-15.5,-11)#,xlim=c(123.4, 128.1),ylim = c(-13,-9),
  labs(x="Latitude (°)",y="Longitude (°)")+ 
  theme_grid()
print(gplot)
ggsave(gplot,file =paste("images/",v,".pdf",sep=""),dpi = 300)



INDEX<-data.frame(Sig=c("Pre_70_Sig","Pre_00_Sig","NDVI_00_Sig","LAI_82_Sig","LAI_00_Sig","SM_79_Sig","SM_00_Sig","Temp_LAI_Sig","Temp_NDVI_Sig","Pre_LAI_Sig","Pre_NDVI_Sig"),Name=c("Precipitation change","Precipitation change","NDVI change","LAI change","LAI change","Soil moisture change","Soil moisture","R_Tem_LAI","R_Tem_NDVI","R_Pre_LAI","R_Pre_NDVI"),Var=c("NET_Pre_70","NET_Pre_00","NET_NDVI_00","NET_LAI_82","NET_LAI_00","NET_SM_79","NET_SM_00","R_Tem_LAI","R_Tem_NDVI","R_Pre_LAI","R_Pre_NDVI"))

for(a in 1:11){
i<-as.character(INDEX$Sig[a])
v<-as.character(INDEX$Var[a])
name1<-as.character(INDEX$Name[a])

Index_sig<-which(STA_ALL_ratio[[i]]=="Very significant" & STA_ALL_ratio$LUCC!="Nonvegtation" & (STA_ALL_ratio[[v]]>=-1 & STA_ALL_ratio[[v]]<=1))
sp<-STA_ALL_ratio[Index_sig,]
print(summary(sp))

gplot<-ggplot(data =sp , aes(x = factor(LUCC), y = sp[[v]])) + 
  stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) + 
  geom_boxplot() + xlab("Vegetation types") + ylab(name1) + 
  theme_bw(base_size = 14, base_family = "Times")
print(gplot)
ggsave(gplot,file =paste("images/",v,"_ratio_box.pdf",sep=""),dpi = 300)

}

for(a in 1:11){
  i<-as.character(INDEX$Sig[a])
  v<-as.character(INDEX$Var[a])
  name1<-as.character(INDEX$Name[a])
  
  Index_sig<-which(STA_ALL_ratio[[i]]=="Very significant" & STA_ALL_ratio$LUCC!="Nonvegtation")
  sp<-STA_ALL_ratio[Index_sig,]
  print(summary(sp))
  
  gplot<-ggplot(data =sp , aes(x = factor(LUCC), y = sp[[v]])) + 
    stat_boxplot(geom = "errorbar", stat_params = list(width = 0.5), geom_params = list()) + 
    geom_boxplot() + xlab("Vegetation types") + ylab(name1) + 
    theme_bw(base_size = 14, base_family = "Times")
  print(gplot)
  ggsave(gplot,file =paste("images/",v,"_ratio_box.pdf",sep=""),dpi = 300)
  
}

