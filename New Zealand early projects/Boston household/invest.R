###########################################
# ASSN 3
# invest.R
###########################################
#
# Build a multi-objective constrained model 
# for an investment portfolio
###########################################
install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
# 
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- length(invest[,1])
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type 
# 			    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
	selected <- which(x >= minAMOUNT)
	sumx = sum(x[selected])
	if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
	return(-1) # Fails constraint
}
#
# Each option must either not be selected 
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#
minAMOUNT = 0.05
maxAMOUNT = 0.2
# Set the lower and upper bounds for 
# each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an 
# option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
	selected <- which(x >= minAMOUNT)  # Only count if at least minimum
	over <- which(x[selected] > maxAMOUNT)
	if (length(over) > 0) return(-1)
	return(1)
}
minNumber = 8
maxNumber = 12
portfolioNUM <- function(x)
{
	numselected <- length(which(x >= minAMOUNT))
	if (numselected < minNumber) return(-1)
	if (numselected > maxNumber) return(-1)
	return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investion (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
	selected <- which(x >= minAMOUNT)
	roi <- sum(invest$ROI[selected]*x[selected])
	return(-roi) # Since nsgaII minimises we take the negative
}
#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
	selected <- which(x >= minAMOUNT)
	risk <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
	return(risk) # we want to minimise the risk
}
##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is 
# minimised.
###################################################
funs <- function(x)
{
	return(c(ROI(x),RISK(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
	psum = portfolioSUM(x)
	prange = portfolioRANGE(x)
	pnum = portfolioNUM(x)
	return(c(prange,pnum,psum))
}
# Set the lower and upper bounds for each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,idim=numberOptions,odim=2, #inputs for each option, 
												   #2 outputs (ROI,RISK)
                    popsize=52,generations=1000,
				    lower.bounds=lower,upper.bounds=upper,
					constraints = constraintFNS,cdim=3)#,
					#vectorized=F) # 3 constraints

plot(portfolio,xlab="-ROI (%)",ylab="RISK",main="Objective Space")



# Part2.3


par(mfrow=c(2,2))
sort(portfolio$value)
i= 1
sorted<-portfolio$par[i,]
blend<-invest[which(sorted>0.05),]
share<-portfolio$par[i,which(sorted>0.05)]
blend<-cbind(blend,share)
blend
p1<- ggplot(data = blend[,-3], aes(x=ROI, y=Risk)) +geom_point(size=share*50, col="red") 
p1 + ggtitle("Plot of low risk portfolio") + xlab("Types of investments") 
p2<-ggplot(data = blend[,-3], aes(x=ROI, y=Risk)) +geom_point(size=share*50, col="red") 
p2 + ggtitle("Plot of medium risk portfolio") + xlab("Types of investments") 
p3<-ggplot(data = blend[,-3], aes(x=ROI, y=Risk)) +geom_point(size=share*50, col="red") 
p3 + ggtitle("Plot of high risk portfolio") + xlab("Types of investments") 




#Part 2.4



stocks<-portfolio$par[,seq(1,10,1)]
stocks1<-as.matrix(rep(1,52))
for (i in seq(1,52,1)){
stocks1[i,]<-sum(stocks[i,portfolio$par[i,seq(1,10,1)]>0.05])
}

bonds<-portfolio$par[,seq(11,20,1)]
bonds1<-as.matrix(rep(1,52))
for (i in seq(1,52,1)){
  bonds1[i,]<-sum(bonds[i,portfolio$par[i,seq(11,20,1)]>0.05])
}

cash<-portfolio$par[,seq(21,30,1)]
cash1<-as.matrix(rep(1,52))
for (i in seq(1,52,1)){
  cash1[i,]<-sum(cash[i,portfolio$par[i,seq(21,30,1)]>0.05])
}

portacc<-cbind(stocks1,bonds1,cash1,portfolio$value[,1])
colnames(portacc)<-c("Stocks","Bonds","Cash","ROI")
portacc[,4]<-portacc[,4]*(-1)
portacc<-as.data.frame(portacc)
 portacc<-portacc[order(portacc$ROI),]

portacc

par(mfrow=c(1,1))



z<-ggplot(portacc, aes(x = ROI)) +geom_line(aes(y = Stocks), colour="blue") 
z <- z+ geom_line(aes(y = Bonds), colour = "green")+ ylab(label="investment Proportion") + xlab("ROI %")
z+  geom_line(aes(y = Cash), colour = "red") + guides(col = guide_legend(nrow = 3))



portacc.2 <- rbind(
  data.frame(ROI=portacc$ROI, PortType="Stocks", Value=portacc$Stocks),
  data.frame(ROI=portacc$ROI, PortType="Bonds", Value=portacc$Bonds),
  data.frame(ROI=portacc$ROI, PortType="Cash", Value=portacc$Cash))

z <- ggplot(portacc.2, aes(x=ROI, y=Value, colour=PortType)) + geom_line() +
  ylab(label="investment Proportion") + xlab("ROI %") + theme(legend.position="bottom") +
  scale_colour_discrete(name="Name of Legend")

ggsave("finalplot.pdf", plot=z, width=150, height=150, units="mm")

z

#Part7


                
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- length(invest[,1])
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type 
#   		    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
  selected <- which(x >= minAMOUNT)
  sumx = sum(x[selected])
  if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
  return(-1) # Fails constraint
}
#
# Each option must either not be selected 
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#
minAMOUNT = 0.05
maxAMOUNT = 0.2
# Set the lower and upper bounds for 
# each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an 
# option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
  selected <- which(x >= minAMOUNT)  # Only count if at least minimum
  over <- which(x[selected] > maxAMOUNT)
  if (length(over) > 0) return(-1)
  return(1)
}
minNumber = 8
maxNumber = 12
portfolioNUM <- function(x)
{
  numselected <- length(which(x >= minAMOUNT))
  if (numselected < minNumber) return(-1)
  if (numselected > maxNumber) return(-1)
  return(numselected)
}
Roilimit<-function(x)
{
  selected <- which(x >= minAMOUNT)
  roi <- sum(invest$ROI[selected]*x[selected])
  if (roi<13 & roi>9) return(1)
  return(-1)
  
}


############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investion (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
  selected <- which(x >= minAMOUNT)
  roi <- sum(invest$ROI[selected]*x[selected])
    return(-roi) # Since nsgaII minimises we take the negative
}

#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
  selected <- which(x >= minAMOUNT)
  risk <- sum(invest$Risk[selected]*x[selected]) 
  roi <- sum(invest$ROI[selected]*x[selected])
  return(risk) # we want to minimise the risk
}
##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is 
# minimised.
###################################################
funs <- function(x)
{
  return(c(ROI(x),RISK(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
  psum = portfolioSUM(x)
  prange = portfolioRANGE(x)
  pnum = portfolioNUM(x)
  ROIlim =Roilimit(x)
  return(c(prange,pnum,psum, ROIlim))
}
# Set the lower and upper bounds for each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,idim=numberOptions,odim=2, #inputs for each option, 
                   #2 outputs (ROI,RISK)
                   popsize=52,generations=1000,
                   lower.bounds=lower,upper.bounds=upper,
                   constraints = constraintFNS,cdim=4)#,
#vectorized=F) # 3 constraints


plot(portfolio,xlab="-ROI (%)",ylab="RISK",main="Objective Space")
portf<-as.data.frame(portfolio$value)
portf$V2<-portf$V2*100
s<-ggplot(portf, aes(x = V1)) + ylab(label="Risk %") + xlab("ROI %") 
s + geom_line(aes(y = V2), colour="blue") + ggtitle("Objective space") 
         





