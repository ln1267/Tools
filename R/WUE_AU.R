##----
# this file is used for processing WUE of AU from 2000 to 2014
##---------

##  load library-----
library(caTools)
library(reshape2)
library(plyr)
library(SPEI)
library(pryr)
library(graphics)
library(raster)
library(rasterVis)
setwd("J:/WUE_AU")

##  Funcs---------
source("J:/Dropbox/Dropbox/Tools/R/Funcs_AU_PRE.R")
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
f_MK_CP<-function(ts_in,name="",seasonal=F,plot=T,main="",Y_name="Streamflow (mm)",sig=0.05){
  require(trend)
  require(changepoint)
  ##  MK and CP analysis
  if (! seasonal){
    
    print("Annual trend analysis")
    print(name)
    # changepoint detect
    cp_mean<-cpt.mean(ts_in)
    means<-cp_mean@param.est$mean
    cp_year<-cp_mean@cpts[1]
    ## get changepoint info if it eixts
    if(length(means)>1){
      
      cp_year<-time(ts_in)[cp_year]
      change<-round((means[2]-means[1])/means[1],3)*100
      
    }else{
      
      means<-NA
      cp_year<-NA
      change<-NA
    }
    
    # MK test
    mk.t<-mk.test(ts_in)
    sen.res <- sens.slope(ts_in)
    
    # plot for selected stations
    if(plot & sen.res$b.sen !=0 & mk.t$pvalg< sig){
      #print("plot data")
      pdf(paste("pdf/",name,".pdf",sep=""),family="Times",width=10)
      t <- (1:(length(ts_in)))
      s.pred <- sen.res$intercept + sen.res$b.sen * t
      s.pred.ts <- ts(s.pred)
      tsp(s.pred.ts) <- tsp(ts_in)
      plot(cp_mean,xlab="Year",ylab=Y_name,main=main) 
      lines(s.pred.ts, lty=2)
      dev.off()
    }
    
    # return  
    return(list(CP_M=means,CP_Pec=change,CP_Y=cp_year,MK_P=round(mk.t$pvalg,3),MK_Slope=round(sen.res$b.sen,2)))
    
  }else{
    
    print("Seasonal trend analysis")
    
    # MK test
    mk.t<-smk.test(ts_in)
    #cmk.t<-csmk.test(ts_in)
    sen.res <- sea.sens.slope(ts_in)
    print(names[i])
    print(sen.res)
    print(mk.t)
    
  }
}

##	Load data--------
	AWL_AET_mon<-read.ENVI("mon/ALW_AET_mon_00-15")
	MOD_GPP_mon<-read.ENVI("mon/MOD_GPP_mon_14-00")
	
	
	MOD_ann<-read.ENVI("ann/MODIS_GNPP_AU_ann_5km_14-00")
  MOD_ET_ann<-read.ENVI("ann/MODIS_ET_AU_ann_5km_00-14")
  AWL_AET_ann<-read.ENVI("ann/AWL_AET_ann")
  
  MOD_LC<-read.ENVI("MOD_LC_12_01")
  mask<-MOD_LC[,,1]
  

##	abnormal data process-------
	AWL_AET_mon[AWL_AET_mon==-999.0 | AWL_AET_mon<0]<-NA
	range(AWL_AET_mon,na.rm=T)
	AWL_AET_mon<-AWL_AET_mon[,,1:180]
	MOD_GPP_mon[MOD_GPP_mon==32767]<-NA
	range(MOD_GPP_mon,na.rm=T)
	MOD_GPP_mon<-MOD_GPP_mon*0.1	##transfer to real value
	MOD_GPP_mon<-MOD_GPP_mon[,,180:1]	##change year from 2000 to 2014

	MOD_ET_ann[MOD_ET_ann==65535]<-NA
	range(MOD_ET_ann,na.rm=T)
	MOD_ET_ann<-MOD_ET_ann*0.1

	MOD_ann[MOD_ann==65535]<-NA
	range(MOD_ann,na.rm=T)
	MOD_ann<-MOD_ann*0.1

	MOD_NPP_ann<-MOD_ann[,,15:1]
	MOD_GPP_ann<-MOD_ann[,,30:16]
	
##	transfer monthly AEt to annual------
	
	AWL_AET_ann<-array(0,c(nrow(AWL_AET_mon),ncol(AWL_AET_mon),15))
  a<-1
  b<-1
  	for (y in 2000:2014){
  	  linshi<-matrix(0,ncol=ncol(AWL_AET_mon),nrow=nrow(AWL_AET_mon))
  	  for (m in 1:12){
  	    
  	    linshi<-linshi+AWL_AET_mon[,,a]
  	    print( range(linshi,na.rm=T))
  	    a<-a+1
  	  }
  	 
  	  AWL_AET_ann[,,b]<-linshi
  	  
  	  b<-b+1
  	  print(y)
  	}
    range(AWL_AET_ann,na.rm=T)
    
##  Calculate WUE------
    WUE_NPP_ann<-MOD_NPP_ann/AWL_AET_ann
    WUE_GPP_ann<-MOD_GPP_ann/AWL_AET_ann
    WUE_MOD_GPP_ann<-MOD_GPP_ann/MOD_ET_ann
    WUE_MOD_NPP_ann<-MOD_NPP_ann/MOD_ET_ann
    
    
    f_WUE<-function(carbon=MOD_NPP_ann,et=AWL_AET_ann,mask=NA,plot=F,wue_ab=8,anom_ab=100){
      ##  mask data base on mask       
      if(length(mask)>1){

        for (i in 1:dim(carbon)[3]){
          
          carbon[,,i][mask]<-NA
          et[,,i][mask]<-NA
          
        }
          
      }
      
      ## calculate WUE
      wue<-carbon/et
      
      ##  clear abnormal data
      wue[is.infinite(wue)]<-NA
      wue[wue>wue_ab]<-NA
      print("WUE range")
      print(range(wue,na.rm=T))
      
      ##  mean WUE
      wue_mean<- apply(wue,c(1,2),mean,na.rm=T)
     
      ##  annomly
      anom<-array(0,c(nrow(wue),ncol(wue),dim(wue)[3]))
      for (i in 1:dim(wue)[3]){anom[,,i]<-(wue[,,i]-wue_mean)/wue_mean*100}
      anom[abs(anom)>anom_ab]<-NA
      anom[abs(anom)<5]<-NA
      print("anom range")
      print(range(anom,na.rm=T))
      
      
      if(plot==T){
        require(ggplot2)
        nrows<-nrow(wue_mean)
        ncols<-ncol(wue_mean)
        a<-raster(wue_mean, xmn=112.5, xmx=154, ymn=-44, ymx=-10)
        
        pdf("WUE_mean.pdf")
        plot(a)
#         theme_set(theme_bw())
#         gplot(a) + geom_tile(aes(fill = value)) +
#           facet_wrap(~ variable) +
#           scale_fill_gradient(low = 'brown', high = 'green') +
#           coord_equal()
        dev.off()
        if(1){
          for (i in 1:dim(anom)[3]){
            
            nrows<-nrow(anom)
            ncols<-ncol(anom)
            a<-raster(anom[,,i], xmn=112.5, xmx=154, ymn=-44, ymx=-10)
            
            pdf(paste("anom_",i+1999,".pdf",sep=""))
            plot(a)
            
            dev.off()
            
            
          }
          
        }else{
          LAT<-rep(seq(-10, by=-0.05, length.out = nrows),ncols)
          LONG<-rep(seq(112.5, by=0.05, length.out = ncols),each=nrows)
          anom_frame<-data.frame(ID=rep(c(1:(nrows*ncols)),15),YEAR=rep(c(2000:2014),each=(nrows*ncols)),LONG=rep(LONG,15),LAT=rep(LAT,15),ANOM=as.vector(anom))
          
          gplot<-ggplot(aes(x = LONG, y = LAT), data = anom_frame) +
            geom_raster(aes(LONG, LAT, fill=anom_frame[[5]]))+
            facet_wrap( ~ YEAR, ncol=5)+
            scale_fill_gradient(low = 'red', high = 'green',name=names(anom_frame)[5],na.value = "white") +
            coord_equal()+ #xlim=c(102.4, 104.1),ylim = c(30.72,33.18)
            labs(x="Latitude",y="Longitude")+ 
            theme_grid()
          
          ggsave(gplot,file =paste("Ann_",names(anom_frame),".pdf",sep="")[5],dpi = 300)

        }
      }
      
      return(list(WUE_MEAN=wue_mean,ANOM=anom))
    }
    
    f_anom<-function(wue=data,mask=NA,plot=F,anom_ab=100){
      ##  mask data base on mask       
      if(length(mask)>1){
        
        for (i in 1:dim(wue)[3]){
          
          wue[,,i][mask]<-NA
  
        }
        
      }


      ##  mean WUE
      wue_mean<- apply(wue,c(1,2),mean,na.rm=T)
    
      ##  annomly
      anom<-array(0,c(nrow(wue),ncol(wue),dim(wue)[3]))
      for (i in 1:dim(wue)[3]){anom[,,i]<-(wue[,,i]-wue_mean)/wue_mean*100}
      anom[abs(anom)>anom_ab]<-NA
      anom[abs(anom)<5]<-NA
      print("anom range")
      print(range(anom,na.rm=T))
      
      
      if(plot==T){
        require(ggplot2)
        nrows<-nrow(wue_mean)
        ncols<-ncol(wue_mean)
        a<-raster(wue_mean, xmn=112.5, xmx=154, ymn=-44, ymx=-10)
        
        pdf("WUE_mean.pdf")
        plot(a)
        #         theme_set(theme_bw())
        #         gplot(a) + geom_tile(aes(fill = value)) +
        #           facet_wrap(~ variable) +
        #           scale_fill_gradient(low = 'brown', high = 'green') +
        #           coord_equal()
        dev.off()
        if(1){
          for (i in 1:dim(anom)[3]){
            
            nrows<-nrow(anom)
            ncols<-ncol(anom)
            a<-raster(anom[,,i], xmn=112.5, xmx=154, ymn=-44, ymx=-10)
            
            pdf(paste("anom_",i+1999,".pdf",sep=""))
            plot(a)
            
            dev.off()
            
            
          }
          
        }else{
          LAT<-rep(seq(-10, by=-0.05, length.out = nrows),ncols)
          LONG<-rep(seq(112.5, by=0.05, length.out = ncols),each=nrows)
          anom_frame<-data.frame(ID=rep(c(1:(nrows*ncols)),15),YEAR=rep(c(2000:2014),each=(nrows*ncols)),LONG=rep(LONG,15),LAT=rep(LAT,15),ANOM=as.vector(anom))
          
          gplot<-ggplot(aes(x = LONG, y = LAT), data = anom_frame) +
            geom_raster(aes(LONG, LAT, fill=anom_frame[[5]]))+
            facet_wrap( ~ YEAR, ncol=5)+
            scale_fill_gradient(low = 'red', high = 'green',name=names(anom_frame)[5],na.value = "white") +
            coord_equal()+ #xlim=c(102.4, 104.1),ylim = c(30.72,33.18)
            labs(x="Latitude",y="Longitude")+ 
            theme_grid()
          
          ggsave(gplot,file =paste("Ann_",names(anom_frame),".pdf",sep="")[5],dpi = 300)
          
        }
      }
      
      return(list(WUE_MEAN=wue_mean,ANOM=anom))
    }
    
    
    noforest<-which(!((mask>=2 & mask <=5 )))# | mask==8 
    forest<-which(((mask>=2 & mask <=5 )  )) #| mask==8
    f<-f_WUE(MOD_GPP_ann,MOD_ET_ann,mask=noforest,wue_ab=5,anom_ab=50,plot=T)
    f2<-f_WUE(MOD_GPP_ann,AWL_AET_ann,mask=noforest,wue_ab=5,anom_ab=50,plot=T)
    anom_NPP<-f_anom(MOD_GPP_ann,mask=noforest,anom_ab=50,plot=T)
    
    
    
    WUE_MOD_GPP_anom<-f$ANOM
    WUE_MOD_GPP_F_anom<-WUE_GPP_F_anom
    WUE_MOD_GPP_mean<-f$WUE_MEAN
    
    WUE_MOD_GPP_F_mean<-apply(WUE_MOD_GPP_F_ann,1,mean,na.rm=T)
    
    for(i in 1:15){
      WUE_MOD_GPP_F_anom[,i]<- (WUE_MOD_GPP_F_ann[,i]-WUE_MOD_GPP_F_mean)/WUE_MOD_GPP_F_mean
      
    }
    
    f_max<-function(x,y_s=2000){
      if(length(na.omit(x)>0)){
        
        max.value<-max(x,na.rm=T)
        max.index<-which(x==max.value)
        max.index<-y_s+max.index
        
        min.value<-min(x,na.rm=T)
        min.index<-which(x==min.value)
        min.index<-y_s+min.index
        
        if(length(max.index)>1 | length(min.index)>1){
          in.max<-(max.index[1]-2000)
          for( i in 2:length(max.index)){
            in.max<-in.max+(max.index[i]-2000)*100^(i-1)
          }
          if( length(min.index)>1){
            in.min<-(min.index[1]-2000)
            for( i in 2:length(min.index)){
              in.min<-in.min+(min.index[i]-2000)*100^(i-1)
            }
          }
          res <- setNames(c(max.value, min.value,in.max,in.min), c("max","min","max.index","min.index"))
          
        }else{
          
          res <- setNames(c(max.value, min.value,max.index,min.index), c("max","min","max.index","min.index"))
        }
        
      }else{
        
        res <- setNames(c(NA, NA,NA,NA), c("max","min","max.index","min.index"))
        
      }
      
      
      return(res)
    }		
    
    for( i in 1: length(forest)){
    f_max(WUE_GPP_F_anom[i,])
    }
    anom_max<-apply(WUE_GPP_F_anom,1,f_max)
    
     apply(WUE_GPP_F_anom,2,mean,na.rm=T)
    
    ## mask WUE
    
    WUE_GPP_F_ann<-matrix(NA,nrow=length(forest),ncol = 15)
    WUE_MOD_GPP_F_ann<-matrix(NA,nrow=length(forest),ncol = 15)
    AWL_AET_F_ann<-matrix(NA,nrow=length(forest),ncol = 15)
    MOD_ET_F_ann<-matrix(NA,nrow=length(forest),ncol = 15)
    MOD_GPP_F_ann<-matrix(NA,nrow=length(forest),ncol = 15)
    WUE_GPP_F_anom<-matrix(NA,nrow=length(forest),ncol = 15)
    
    for ( i in 1: 15){
      WUE_GPP_F_ann[,i]<-WUE_GPP_ann[,,i][forest]
      WUE_MOD_GPP_F_ann[,i]<-WUE_MOD_GPP_ann[,,i][forest]
      AWL_AET_F_ann[,i]<-AWL_AET_ann[,,i][forest]
      MOD_ET_F_ann[,i]<-MOD_ET_ann[,,i][forest]
      MOD_GPP_F_ann[,i]<-MOD_GPP_ann[,,i][forest]
      WUE_GPP_F_anom[,i]<-WUE_GPP_anom[,,i][forest]
    }
    
    
    
    
    
    nrows<-680
    ncols<-830
    a<-1
    MK_CP_ann<-list(NA)
#     for (i in 1:nrows){
#       for (j in 1:ncols){
#       indata<-WUE_GPP_ann[i,j,]
    for( i in 1:length(forest)){
      indata<-AWL_AET_F_ann[i,]
      if(length(na.omit(indata))==15){
        ts_in<-ts(data = indata, start = 2000,frequency = 1)
        .a<-f_MK_CP(ts_in,plot=F)
        .a[[ "ID" ]] <- a
      }else{
        .a<-list(CP_M=NA,CP_Pec=NA,CP_Y=NA,MK_P=NA,MK_Slope=NA)
        .a[[ "ID" ]] <- a
        
      }
      
      MK_CP_ann[[a]]<- .a
      a<-a+1
      print(i)
       }
#       
#   } 
    
    cc<-do.call(rbind,MK_CP_ann)
    
    a<-cc[,1]
    Mean1<-c(1:length(cc))
    Mean2<-c(1:length(cc))
    for (i in 1:length(cc)) {
      
      if(length(a[[i]])==2){
        Mean1[i]<-a[[i]][1]
        Mean2[i]<-a[[i]][2]
      }else{
        
        Mean1[i]<-NA
        Mean2[i]<-NA
        
      }
      
    }
    
    WUE_MK_CP_frame_ann<-data.frame(ID=as.numeric(cc[,6]),MK_P=as.numeric(cc[,4]),MK_Slope=as.numeric(cc[,5]))
    WUE_MOD_MK_CP_frame_ann<-data.frame(ID=as.numeric(cc[,6]),MK_P=as.numeric(cc[,4]),MK_Slope=as.numeric(cc[,5]))
    
    MOD_ET_MK_CP_frame_ann<-data.frame(ID=as.numeric(cc[,6]),MK_P=as.numeric(cc[,4]),MK_Slope=as.numeric(cc[,5]))
    MOD_GPP_MK_CP_frame_ann<-data.frame(ID=as.numeric(cc[,6]),MK_P=as.numeric(cc[,4]),MK_Slope=as.numeric(cc[,5]))
    ALW_ET_MK_CP_frame_ann<-data.frame(ID=as.numeric(cc[,6]),MK_P=as.numeric(cc[,4]),MK_Slope=as.numeric(cc[,5]))
    
    slope<-mask
    slope[noforest]<-NA
    slope[forest]<-ALW_ET_MK_CP_frame_ann[[3]]
    mean(slope,na.rm=T)
    plot(raster(slope))
    
    
    noforest<-which(!(mask>=2 & mask <=5 ))
    
    slope<-matrix(as.numeric(cc[,5]),ncol=ncols,nrow=nrows,byrow = T)
    slope[noforest]<-NA
    slope[abs(slope)>0.02]<-NA
    plot(raster(slope))
    
    slope_up<-slope
    slope_up[slope_up<0]<-NA
    plot(raster(slope_up))
    
    slope_dow<-slope
    slope_dow[slope_dow>0 | slope_dow < -0.02 ]<-NA
    plot(raster(slope_dow))
    
    P<-matrix(as.numeric(cc[,4]),ncol=ncols,nrow=nrows,byrow = T)
    P[abs(P)>0.1]<-NA
    plot(raster(P))
    
##  Calculate SPI------    

    forest<-which(mask==2 | mask==8)
    PET_all<-read.ENVI("AWAP_PET_mon") # 13-70
    PRE_all<-read.ENVI("AWAP_PRE_mon")  # 13-70
    
    da<-matrix(0,nrow=528,ncol=34059)
    
    for (i in 1:528){
      # da[i,]<-PET_all[,,i][forest]
      da[i,]<-PRE_all[,,i][forest]
    }
    
    da_pet<-da
    da_pre<-da
    da_pre<-da_pre*0.1
    da_pre<-da_pre[528:1,]
    range(da_pre)
    
    
    
    
        SPI_forest<-spi(da_pre,12)  
        
    
##  output WUE result -----------    
    write.ENVI(WUE_NPP_ann,"WUE_NPP_ann")
    write.ENVI(AWL_AET_ann,"AWL_AET_ann")
    write.ENVI(anom,"anom")
 
    STA1<-do.call(rbind,STA)
    STA1
    da<-STA1[,2]
    da1<-do.call(cbind,da)
    
    
       