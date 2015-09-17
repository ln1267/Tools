library(caTools)


temp = read.ENVI("e:/temp")

x<- scan("e:/3.txt")

attributes(temp)


mode(x)
str(temp)
a<-1
str(x)
temp[2,2,2]
x[a]

mydata<-array(1:8,c(2,2,2))

mydata
mydate<-edit(mydata)

rm(mydata)

apply(mydata,3,mean)
help(apply)
b=t(mydata)



mydata <- data.frame(v1=numeric(0),v2=character())
mydata <- edit(mydata)

