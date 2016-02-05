
setwd("/Volumes/ELSE/AU/")

load("AU_pre_veg1.RData")

Cor_1<-within(cor,
              {
                Pre_NDVI_Sig<-NA
                Pre_NDVI_Sig[P_Pre_NDVI<=0.05]<-"Very significant"
                Pre_NDVI_Sig[P_Pre_NDVI<=0.1 & P_Pre_NDVI>0.05]<-"Significant"
                Pre_NDVI_Sig[P_Pre_NDVI>0.1]<-"Not significant"
                
                Pre_LAI_Sig<-NA
                Pre_LAI_Sig[P_Pre_LAI<=0.05]<-"Very significant"
                Pre_LAI_Sig[P_Pre_LAI<=0.1 & P_Pre_LAI>0.05]<-"Significant"
                Pre_LAI_Sig[P_Pre_LAI>0.1]<-"Not significant"
                
                Temp_NDVI_Sig<-NA
                Temp_NDVI_Sig[P_Tem_NDVI<=0.05]<-"Very significant"
                Temp_NDVI_Sig[P_Tem_NDVI<=0.1 & P_Tem_NDVI>0.05]<-"Significant"
                Temp_NDVI_Sig[P_Tem_NDVI>0.1]<-"Not significant" 
                
                Temp_LAI_Sig<-NA
                Temp_LAI_Sig[P_Tem_LAI<=0.05]<-"Very significant"
                Temp_LAI_Sig[P_Tem_LAI<=0.1 & P_Tem_LAI>0.05]<-"Significant"
                Temp_LAI_Sig[P_Tem_LAI>0.1]<-"Not significant" 
                }
)




T_all<-within(T_all,
              {
                Pre_70_Sig<-NA
                Pre_70_Sig[Pre_70_P<=0.05]<-"Very significant"
                Pre_70_Sig[Pre_70_P<=0.1 & Pre_70_P>0.05]<-"Significant"
                Pre_70_Sig[Pre_70_P>0.1]<-"Not significant"
                
                Pre_00_Sig<-NA
                Pre_00_Sig[Pre_00_P<=0.05]<-"Very significant"
                Pre_00_Sig[Pre_00_P<=0.1 & Pre_00_P>0.05]<-"Significant"
                Pre_00_Sig[Pre_00_P>0.1]<-"Not significant"
                
                NDVI_00_Sig<-NA
                NDVI_00_Sig[NDVI_00_P<=0.05]<-"Very significant"
                NDVI_00_Sig[NDVI_00_P<=0.1 & NDVI_00_P>0.05]<-"Significant"
                NDVI_00_Sig[NDVI_00_P>0.1]<-"Not significant" 
                
                LAI_82_Sig<-NA
                LAI_82_Sig[LAI_82_P<=0.05]<-"Very significant"
                LAI_82_Sig[LAI_82_P<=0.1 & LAI_82_P>0.05]<-"Significant"
                LAI_82_Sig[LAI_82_P>0.1]<-"Not significant" 
                
                LAI_00_Sig<-NA
                LAI_00_Sig[LAI_00_P<=0.05]<-"Very significant"
                LAI_00_Sig[LAI_00_P<=0.1 & LAI_00_P>0.05]<-"Significant"
                LAI_00_Sig[LAI_00_P>0.1]<-"Not significant" 
                
                
                SM_79_Sig<-NA
                SM_79_Sig[SM_79_P<=0.05]<-"Very significant"
                SM_79_Sig[SM_79_P<=0.1 & SM_79_P>0.05]<-"Significant"
                SM_79_Sig[SM_79_P>0.1]<-"Not significant" 
                
                SM_00_Sig<-NA
                SM_00_Sig[SM_00_P<=0.05]<-"Very significant"
                SM_00_Sig[SM_00_P<=0.1 & SM_00_P>0.05]<-"Significant"
                SM_00_Sig[SM_00_P>0.1]<-"Not significant" 
              }
)


T_all<-within(T_all,
              {
                Pre_70_Net<-NA
                Pre_70_Net[(Pre_70_M_Y-Pre_70_M_X)>0]<-"Increased"
                Pre_70_Net[(Pre_70_M_Y-Pre_70_M_X)==0]<-"No change"
                Pre_70_Net[(Pre_70_M_Y-Pre_70_M_X)<0]<-"Decreased"
                
                Pre_00_Net<-NA
                Pre_00_Net[(Pre_00_M_Y-Pre_00_M_X)>0]<-"Increased"
                Pre_00_Net[(Pre_00_M_Y-Pre_00_M_X)==0]<-"No change"
                Pre_00_Net[(Pre_00_M_Y-Pre_00_M_X)<0]<-"Decreased"
                
                NDVI_00_Net<-NA
                NDVI_00_Net[(NDVI_00_M_Y-NDVI_00_M_X)>0]<-"Increased"
                NDVI_00_Net[(NDVI_00_M_Y-NDVI_00_M_X)==0]<-"No change"
                NDVI_00_Net[(NDVI_00_M_Y-NDVI_00_M_X)<0]<-"Decreased" 
                
                LAI_82_Net<-NA
                LAI_82_Net[(LAI_82_M_Y-LAI_82_M_X)>0]<-"Increased"
                LAI_82_Net[(LAI_82_M_Y-LAI_82_M_X)==0]<-"No change"
                LAI_82_Net[(LAI_82_M_Y-LAI_82_M_X)<0]<-"Decreased" 
                
                LAI_00_Net<-NA
                LAI_00_Net[(LAI_00_M_Y-LAI_00_M_X)>0]<-"Increased"
                LAI_00_Net[(LAI_00_M_Y-LAI_00_M_X)==0]<-"No change"
                LAI_00_Net[(LAI_00_M_Y-LAI_00_M_X)<0]<-"Decreased" 
                
                
                SM_79_Net<-NA
                SM_79_Net[(SM_79_M_Y-SM_79_M_X)>0]<-"Increased"
                SM_79_Net[(SM_79_M_Y-SM_79_M_X)==0]<-"No change"
                SM_79_Net[(SM_79_M_Y-SM_79_M_X)<0]<-"Decreased" 
                
                SM_00_Net<-NA
                SM_00_Net[(SM_00_M_Y-SM_00_M_X)>0]<-"Increased"
                SM_00_Net[(SM_00_M_Y-SM_00_M_X)==0]<-"No change"
                SM_00_Net[(SM_00_M_Y-SM_00_M_X)<0]<-"Decreased" 
              }
)


for (i in 37:43){  T_all[[i]]<-as.factor(T_all[[i]])  } 
for (i in 18:21){  Cor_1[[i]]<-as.factor(Cor_1[[i]])  } 


Sig_all<-cbind(T_all[c(1:8,30:43)],Cor_1[18:21])

CrossTable(Sig_all$Pre_00_Sig,Sig_all$Pre_NDVI_Sig,missing.include=FALSE,format=c("SPSS"))

CrossTable(Sig_all$LUCC,Sig_all$Pre_NDVI_Sig,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LUCC,Sig_all$Pre_LAI_Sig,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LUCC,Sig_all$Pre_00_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LUCC,Sig_all$NDVI_00_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LUCC,Sig_all$LAI_00_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LUCC,Sig_all$SM_00_Net,missing.include=FALSE,format=c("SPSS"))

CrossTable(Sig_all$Pre_00_Sig,Sig_all$Pre_00_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$NDVI_00_Sig,Sig_all$NDVI_00_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LAI_00_Sig,Sig_all$LAI_00_Net,missing.include=FALSE,format=c("SPSS"))

CrossTable(Sig_all$Pre_70_Sig,Sig_all$Pre_70_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$LAI_82_Sig,Sig_all$LAI_82_Net,missing.include=FALSE,format=c("SPSS"))
CrossTable(Sig_all$SM_00_Sig,Sig_all$SM_00_Net,missing.include=FALSE,format=c("SPSS"))



##----3 way calutation cross

LU_Pre<-table(Sig_all$LUCC,Sig_all$Pre_00_Sig,Sig_all$Pre_00_Net)
print("Sig_all$LUCC,Sig_all$Pre_00_Sig,Sig_all$Pre_00_Net")
names(dimnames(LU_Pre))<-c("LUCC","Sig","Net")
structable(Sig+Net~LUCC,data=LU_Pre)

LU_NDVI<-table(Sig_all$LUCC,Sig_all$NDVI_00_Sig,Sig_all$NDVI_00_Net)
print("Sig_all$LUCC,Sig_all$NDVI_00_Sig,Sig_all$NDVI_00_Sig")
names(dimnames(LU_NDVI))<-c("LUCC","Sig","Net")
structable(Sig+Net~LUCC,data=LU_NDVI)

LU_LAI<-table(Sig_all$LUCC,Sig_all$LAI_00_Sig,Sig_all$LAI_00_Net)
print("Sig_all$LUCC,Sig_all$LAI_00_Sig,Sig_all$LAI_00_Sig")
names(dimnames(LU_LAI))<-c("LUCC","Sig","Net")
structable(Sig+Net~LUCC,data=LU_LAI)

LU_SM<-table(Sig_all$LUCC,Sig_all$SM_00_Sig,Sig_all$SM_00_Net)
print("Sig_all$LUCC,Sig_all$SM_00_Sig,Sig_all$SM_00_Sig")
names(dimnames(LU_SM))<-c("LUCC","Sig","Net")
structable(Sig+Net~LUCC,data=LU_SM)
ftable(LU_SM)

index<-which((Sig_all$LAI_00_Sig=="Very significant" | Sig_all$NDVI_00_Sig=="Very significant") & Sig_all$LUCC!="Nonvegtation")
LU_NET<-table(Sig_all$LUCC[index],Sig_all$NDVI_00_Net[index],Sig_all$LAI_00_Net[index])
print("Sig_all$LUCC,Sig_all$NDVI_00_Net,Sig_all$LAI_00_Net")
names(dimnames(LU_NET))<-c("LUCC","NET_NDVI","NET_LAI")
structable(NET_NDVI+NET_LAI~LUCC,data=LU_NET)
ftable(LU_NET)


index<-which((Sig_all$LAI_00_Sig=="Very significant" | Sig_all$NDVI_00_Sig=="Very significant") & Sig_all$LUCC!="Nonvegtation")
print('Sig_all$LUCC!="Nonvegtation"')
CrossTable(Sig_all$NDVI_00_Net[index],Sig_all$LAI_00_Net[index],missing.include=FALSE,format=c("SPSS"))

for (i in 2:7){
  index<-which((Sig_all$LAI_00_Sig=="Very significant" | Sig_all$NDVI_00_Sig=="Very significant") & Sig_all$LUCC==levels(Sig_all$LUCC)[i])
  print(levels(Sig_all$LUCC)[i])
  CrossTable(Sig_all$NDVI_00_Net[index],Sig_all$LAI_00_Net[index],missing.include=FALSE,format=c("SPSS"))
  
}


##-------out put net and sig for each vegetation types
sta_LU<-function(V1,V2) {
 print(paste(V1,"VS",V2))
  #---all vege
  index<-which(Sig_all$LUCC!="Nonvegtation")
  print('Sig_all$LUCC!="Nonvegtation"')
  CrossTable(Sig_all[[V1]][index],Sig_all[[V2]][index],missing.include=FALSE,format=c("SPSS"))

  # -- each veg
  for (i in 2:7){
    index<-which(Sig_all$LUCC==levels(Sig_all$LUCC)[i])
    print(levels(Sig_all$LUCC)[i]) 
    CrossTable(Sig_all[[V1]][index],Sig_all[[V2]][index],missing.include=FALSE,format=c("SPSS"))
  }

}

sta_LU("NDVI_00_Sig","NDVI_00_Net")
sta_LU("LAI_82_Sig","LAI_82_Net")
sta_LU("LAI_00_Sig","LAI_00_Net")
sta_LU("Pre_70_Sig","Pre_70_Net")
sta_LU("Pre_00_Sig","Pre_00_Net")
sta_LU("SM_79_Sig","SM_79_Net")
sta_LU("SM_00_Sig","SM_00_Net")


LU_NET<-table(Sig_all$LUCC[index],)
print("Sig_all$LUCC,Sig_all$NDVI_00_Net,Sig_all$LAI_00_Net")
names(dimnames(LU_NET))<-c("LUCC","NET_NDVI","NET_LAI")
structable(NET_NDVI+NET_LAI~LUCC,data=LU_NET)
ftable(LU_NET)


LU_all<-table(Sig_all$LUCC,Sig_all$NDVI_00_Sig,Sig_all$NDVI_00_Net,Sig_all$LAI_00_Sig,Sig_all$LAI_00_Net,Sig_all$SM_00_Sig,Sig_all$SM_00_Net)
print("Sig_all$LUCC,Sig_all$NDVI_00_Sig,Sig_all$NDVI_00_Net,Sig_all$LAI_00_Sig,Sig_all$LAI_00_Net,Sig_all$SM_00_Sig,Sig_all$SM_00_Net")
names(dimnames(LU_all))<-c("LUCC","NDVI_Sig","NDVI_Net","LAI_Sig","LAI_Net","SM_Sig","SM_Net")
cc<-structable(NDVI_Sig+NDVI_Net+LAI_Sig+LAI_Net+SM_Sig+SM_Net~LUCC,data=LU_all)
dimnames(LU_all)[[6]]<-c("N","-","Y")
dimnames(LU_all)[[7]]<-c("I","-","D")

Sta_LUCC_all<-as.data.frame(cc)
rm(c1)

ab<-prop.table(aa)*100
LU_P<-round(LU_P,digits = 2)
sum(aa)
as<-xtabs(~Sig_all$LUCC+Sig_all$Pre_00_Sig+Sig_all$Pre_00_Net,data=Sig_all)
structable(as)
sink("a.txt")
sink()
