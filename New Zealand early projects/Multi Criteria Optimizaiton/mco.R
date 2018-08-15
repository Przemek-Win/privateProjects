# The following command can be used to clear the working memory of R
#rm(list=ls())
###################

####################################################################
# Example of multicriteria optimisation using the mco package
####################################################################
library(mco)

############################################################
# as.binary
############################################################
# Convert a single integer value to a binary reprsentation
# IN: x = integer value
# 	  minlength = the minimum length of the binary string.
#     Pads out with leading 0's to get to this length
############################################################
as.binary <- function (x,minlength) 
{
	x = trunc(x)
	base = 2 
	r = NULL
	if (x > 0)
	{
		ndigits = 1 + floor(log(x, base)) 
		for (i in ndigits:1) 
		{	 
			r = c(x%%base,r)
			x <- x%/%base 
		}
	}
	if (length(r) < minlength) # pad out with 0's
		r = c(rep(0,minlength-length(r)),r)
	
	class(r) <- "binaryInt" 
	attr(r, "base") <- base 
	return(r) 
}
# ###########################################
# Using nsga2 as a discrete optimiser 
#############################################
items <- data.frame(item = 
    c("pocketknife", "beans", "potatoes", "undies", "sleeping bag", "rope", "compass",
	"multi-tool", "firstaid", "flashlight", "matches", "blanket", "spray","waterbottle"), 
	survivalpoints = c(7,15,15,2,30,10,20,10,15,7,7,12,2,12), 
	weight = c(1,5,10,1,7,5,1,3,6,3,1,9,1,5)
	)
	
weightlimit <- 20
numberbits = length(items[,1])

################################################################
# Fitness evaluation functions for the backpack problem
################################################################
#
# MAXIMISE survival points
# In: x - will be a one-dimensional value with limits between
#     0 and 2^numberbits based on the number of items
#     Convert x to an integer, and then to bits.
# OUT: -Survival points 
# NOTE: pack weight will be a separate function, both for
#       minimising and as a constraint
#
################################################################
survivalPoints <- function(x) 
{
	x = as.binary(x,numberbits)
    pack_survivalpoints <- sum(items$survivalpoints[which(x==1)])
    return(-pack_survivalpoints)
}
#########################################################
# Minimise packWeight
##########################################################
packWeight <- function(x)
{
	x = as.binary(x,numberbits)
    pack_weight <- sum(items$weight[which(x==1)])
	return(pack_weight)
}
###########################################################
# Constraint:  packWeight must be < weightLimit
# All constraints are ok if they are >= 0
# ##########################################################
weightConstraint <- function(x)
{
	return(weightlimit - packWeight(x))
}
minSPW <- function(x)
{
	return(c(survivalPoints(x),packWeight(x)))
}

bag <- nsga2(minSPW,idim=1,odim=2,popsize=100,generations=100,
			lower.bounds = 0, upper.bounds = 2^numberbits,
			constraints=weightConstraint,cdim=1)

plot(bag,xlab="SurvivalPoints",ylab="packWeight",
         main="Objective Space")

##########################################################
# Accessing the elements of the bag
##########################################################
#
# Getting back the solution with the biggest survival value
#
minsurvival <- min(bag$value[,1])
mininds <- which(bag$value[,1]==minsurvival)
baganswer <- as.binary(bag$par[mininds[1]],numberbits)
items[which(baganswer==1),]


minsurvivalpoints = 50 

survivalConstraint <- function(x)
{
	sp <- -survivalPoints(x)  # Turn into a positive value
	return(sp - minsurvivalpoints) # >=0 if sp over minimum
}
allConstraints <- function(x)
{
	wc = weightConstraint(x)
	sc = survivalConstraint(x)
	return(c(wc,sc))
}
bag <- nsga2(minSPW,idim=1,odim=2,popsize=100,generations=100,
			lower.bounds = 0, upper.bounds = 2^numberbits,
			constraints=allConstraints,cdim=2)

plot(bag,xlab="SurvivalPoints",ylab="packWeight",
         main="Objective Space")







