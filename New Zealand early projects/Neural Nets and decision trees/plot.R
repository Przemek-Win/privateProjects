setwd("C:/Users/Przemek/Documents/INFO324/assignment2")
library(neuralnet)
library(MASS)

Bostoni<-read.csv('Boston.csv', header=TRUE)
# The data.frame used later only for values on x axis in the plot 
Boston3<-data.frame(Bostoni)
Boston3<-Boston3[order(Boston3$rm),]
#Scaling the data and recording scale and center.
Boston2<-scale(Bostoni)

scale.scale <- attr(Boston2,"scaled:scale")
scale.center <- attr(Boston2,"scaled:center")

#unscale function
un.scale.data <- function(data.set,attr.scale, attr.center)
{
  data.set <- t(apply(data.set,1,function(x) x * attr.scale))
  t(apply(data.set,1,function(x) x + attr.center))
}
 
#Ordering variables and transforming into data.frame
Boston2<-data.frame(Boston2)
orBoston <- Boston2[order(Boston2$medv),]

scBostzap<-orBoston

#Transforming other variables into their median apart from rm and medv
for(i in seq(from= 1, to= 3, by=1))
{
  scBostzap[,i]<-rep(median(scBostzap[,i]),length(scBostzap[,i]))
}
for(i in seq(from= 5, to= 6, by=1))
{
  scBostzap[,i]<-rep(median(scBostzap[,i]),length(scBostzap[,i]))
}
#RM variable 
scBostzap[,4]<-seq(from=min(scBostzap[,4]), to=max(scBostzap[,4]), length.out=length(scBostzap[,4]))

#ANN
par(mfrow=c(2,2))

for(i in c(10,11,12,13)){
i=5
net.b3<-neuralnet(medv ~ ptratio+tax+rm+nox+indus+crim,data = scBostzap,hidden=i)

# Computation
net.pred2 <- compute(net.b3, scBostzap[,1:6])

# Unscalling

net.pred.unscaled <- un.scale.data(net.pred2$net.result,scale.scale[7],scale.center[7])
net.pred.unscaled<-net.pred.unscaled[order(net.pred.unscaled)]
#Plot itself
plot(Boston3$rm,net.pred.unscaled, xlab="Number of rooms", ylab="Median price for the room predicted by ANN", main=paste("Response curve for " , i ,"hidden nodes") )



}




