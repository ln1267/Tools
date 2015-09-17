rows<-nrow(mydata)
cols<-ncol(mydata)


net_year<-matrix(0.0,c(rows,cols,9))
mean1<-matrix(0.0,nrow=rows,ncol=cols)
SD1<-matrix(0.0,nrow=rows,ncol=cols)

NDVI<-mydata[ , ,19:27]


VEG<-mydata[ , ,31]
CLIMATE<-mydata[ ,  ,32]

for (m in 1:rows){
  
  for (n in 1:cols){
  
	if (is.na(VEG[m,n])|is.na(CLIMATE[m,n]) ){

     
 mean1[m,n]<-NA
SD1[m,n]<-NA
	} else {
	all<-mydata[m,n,19:27]
    

   mean1[m,n]<-mean(all,na.rm=TRUE) 
   SD1[m,n]<-sd(all,na.rm=TRUE)  
  
   }
    
  }
  
}


for (y in 1:9) {

NET<- NDVI[,,y]-mean1
aa<-0
for (m in 1:rows){
  
  for (n in 1:cols){
  
	if ( NET[m,n]<((-1)*SD1[m,n])){

     
aa<-aa+1
	} else {
	
   }
    
  }
  
}

print(aa)
}