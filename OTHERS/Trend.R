c<- scan("e:/3.txt")
ctime<-ts(c)
plot.ts(ctime)
mk.test(ctime)
sens.slope(atime)

require(trend)
data(PagesData)
PagesData
pg<-ts(PagesData)
plot(ctime)

pettitt.test(c)


# change in variance
x=c(rnorm(100,0,1),rnorm(100,0,10))

ansvar=cpt.var(c)
plot(ansvar)
print(ansvar)
# change in mean
y=c(rnorm(100,0,1),rnorm(100,5,1))

# here is usded for changepoints detected

ansmean=cpt.mean(c)
plot(ansmean,cpt.col='blue')
print(ansmean)




# change in mean and variance
z=c(rnorm(100,0,1),rnorm(100,2,10))
ansmeanvar=cpt.meanvar(c)
plot(ansmeanvar,cpt.width=3)
print(ansmeanvar)

# Example of multiple changes in mean at 50,100,150 in simulated data
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))
binseg.mean.cusum(c,Q=5,pen=0.8) # returns optimal number as 3 and the locations as c(50,100,150)
binseg.mean.cusum(x,Q=2,pen=0.8) # returns optimal number as 2 as this is the maximum number of
#changepoints it can find.  If you get the maximum number, you need to increase Q until this is not
#the case.

set.seed(1)
x=c(rexp(50,rate=1),rexp(50,rate=5),rexp(50,rate=1),rexp(50,rate=10))
binseg.meanvar.exp(c,Q=5,pen=2*log(200)) # returns optimal number as 3 and the locations as
#c(50,100,150)
binseg.meanvar.exp(x,Q=10,pen=2*log(200)) # returns optimal number as 2 as this is the maximum number
#of changepoints


binseg.meanvar.exp(c,Q=5,pen=0)
binseg.meanvar.norm(b,Q=5,pen=0)
binseg.mean.cusum(b,Q=5,pen=0)

binseg.meanvar.gamma(b,Q=5,pen=0)
binseg.meanvar.poisson(b, Q=5, pen=0)

binseg.var.css(b, Q=5, pen=0)

binseg.var.norm(b, Q=5, pen=0)





