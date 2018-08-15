##############################################################################################
# Example to produce plot of:
  # Relative distance in Feature Space (x) versus Relative Distance in Response space (y)
##############################################################################################
#
# Assume we are given a table with a single response variable
# Want to make a table with the (x) and (y) values as 2 columns
# 
# IN: d - the dataframe or matrix 
#     response.var - the number of the column used as the response variable.  Defaults to last column 
# OP: Calculates the normalised distance between each pair of data items in explanatory space
#     and the distance between their response variables. 
# OUT: Data frame with 2 columns, the distance in feature space (x), and distance in response space (y)
########################################################################################################
dist.table <- function(d, response.var = ncol(d),...)  
{
    d <- scale(d) # scale data
	d.dist <- dist(d[,-response.var])  # distance all X values	
	d.resp <- dist(d[,response.var])
	
	d.dist <- (d.dist-min(d.dist))/(max(d.dist)-min(d.dist))
	d.resp <- (d.resp-min(d.resp))/(max(d.resp)-min(d.resp))
	
	data.frame(cbind(d.dist,d.resp))
}




#
# Example with simple linear response and no noise
#
X1 <- runif(100)
X2 <- runif(100)
Y <- runif(100)
#
ex1 <- data.frame(cbind(X1,X2,Y))
d <- dist.table(ex1, response.var = 3)
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
                              ylab="Normalised Distance in Response Space",cex=0.5)

#Part 3
setwd("C:/Users/Przemek/Documents/INFO324/assignment2")
library(MASS)
data(Boston)
head(Nijak)
summary(Nijak)
sapply(Nijak,class)
d <- dist.table(Boston, response.var = 14)
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
     ylab="Normalised Distance in Response Space",cex=0.5, main="Boston distance plot")
abline(0,1, col=2, lwd=3)  
Nijak<-as.data.frame(read.table("bioavailability.txt"),row.names = NULL)
summary(Nijak)
c<-dist.table(Nijak, response.var = length(names(Nijak)))
plot(x=c$d.dist, y = c$d.resp,xlab="Normalised Distance in Feature Space",
     ylab="Normalised Distance in Response Space",cex=0.5, main="Bioavailability distance plot")
abline(0,1, col=2, lwd=3)

Question 4.1

library(neuralnet)


ydata <- as.data.frame(cbind(y,x1,x2))
colnames(ydata) <- c("y","x1","x2")

ydata.copy <- ydata # Keep a copy before we scale 

Boston.copy <- scale(Boston)
scale.scale <- attr(Boston.copy,"scaled:scale")
scale.center <- attr(Boston.copy,"scaled:center")


ydata <- data.frame(ydata) # So we can use the column names
ydata <- ydata[order(ydata$y),]
head(Boston.copy)
net.y <- neuralnet(medv ~ lstat + black+ptratio+tax+rad+dis+age+rm+nox+chas+indus+zn+crim,data = Boston.copy,hidden=2)  	
net.y

net.pred <- compute(net.y, ydata[,2:3])
net.pred2 <- compute(net.y, Boston.copy)

# BUT THIS IS WITH THE SCALED DATA -- we need to convert back if we want to compare
# with the original values.
# 
# Here we call the un.scale.data function with the result values and the two scaling
# properties we saved earlier.  Note that it has 3 values since there were 3 variables.
# Because of this, the result has 3 columns, but it is just the y column that we want.
#

# Plot the original data in order
#


lines(net.pred.unscaled[,1],col='red')
#
#