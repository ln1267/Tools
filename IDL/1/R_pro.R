jpeg(file = "myplot%d.jpeg")
example(rect)
dev.off()

help(a)

getwd()


plot(a)

# Annual precipitation entire Great Lakes
# The time series plot with lowess smooth suggests an upward trend
# The autocorrelation in this data does not appear significant.
# The Mann-Kendall trend test confirms the upward trend.
data(PrecipGL)
plot(PrecipGL)
lines(lowess(time(PrecipGL),PrecipGL),lwd=3, col=2)
acf(PrecipGL)
MannKendall(PrecipGL)
#
#Use block bootstrap
library(boot)
data(PrecipGL)
MKtau<-function(z) MannKendall(z)$tau
tsboot(PrecipGL, MKtau, R=500, l=5, sim="fixed")
#
# Deseasonalize a monthly time series and use the block bootstrap
library(boot)
data(manaus)
z<-matrix(manaus, ncol=12, byrow=12)
zm<-apply(z, MARGIN=2, FUN=mean)
zs<-apply(z, MARGIN=2, FUN=sd)
z2<-sweep(z, MARGIN=2, STATS=zm) #subtract monthly means
z3<-sweep(z2, MARGIN=2, STATS=zs, FUN="/") #divide by monthly sd
zds<-c(t(z3))
attributes(zds)<-attributes(manaus)
plot(zds
     
     
     open.nc("/home/ningliu/1.nc", write=TRUE)

     library(segmented)     
     set.seed(10)
     x<-1:100
     z<-runif(100)
     y<-2+1.5*pmax(x-35,0)-1.5*pmax(x-70,0)+10*pmax(z-.5,0)+rnorm(100,0,2)
     out.lm<-lm(y~x)
     o<-segmented(out.lm,seg.Z=~x+z,psi=list(x=c(30,60),z=.4))
     confint(o)
     
     ############################################
     ############################################
     ##Segemented##    
     
     x <- c(1:10, 13:22)
     y <- numeric(20)
     ## Create first segment
     y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
     ## Create second segment
     y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
     ## Plot it
     par(mar=c(4,4,1,1)+0.2)
     plot(x,y, ylim=c(5, 20), pch=16) 
     
     lin.mod <- lm(y~x)
     segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=14)
     