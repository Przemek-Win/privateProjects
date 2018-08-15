############################################################
# Simple GA to solve the Travelling Salesman Problem
############################################################
#
# Travellings Salesman problem(tsp) is conceptually very simple:
# given a list of city locations and a starting city, find the shortest route that travels
# through all cities, and returns to the starting city, without loops or repeated visits.
# 
# 
# init_pop
# IN: cities:  The matrix of city as (x,y) values. Each row represents a city location. 
#              The first entry is the start city.
#     popsize: The size of the population (i.e. the number of individuals in the population)
#     greedy:  TRUE- seed the population with all individuals the same, being the greedy "solution"
#              where we just start at city 1, find the nearest city X and go to that, then the nearest 
#              city from X that hasn't been visited, and so  on until we have been to all...
# OUT: The initial population, where each row represents the proposed route for each individual
#      to each city.  This can be represented as just an index into the cities matrix.  
#      The starting city (the first in the list) is added at the start and end of the defined
#      journey.
# 
# 
init_pop <- function(cities,popsize=10,greedy=FALSE)
{
    if (greedy==TRUE) return(greedy_pop(cities,popsize))
	
	pop <- NULL # the population that we will fill in...
    # 
	# Find out how many cities there are
	numcities = length(cities[,1])
	#
	# we just need to create a random ordering of the city values, but
	# we have to start at city 1, and then randomly visit the other cities
	#
	for (p in 1:popsize)
	{
		pop = rbind(pop,c(1,sample(seq(from=2,to=numcities),numcities-1,replace=FALSE),1))
	}
	return(pop)
}
greedy_pop <- function(cities,popsize)
{
	distcities = as.matrix(dist(cities))	
	numcities = length(cities[,1])
	allcities <- seq(from=1,to=numcities)
	visitedcities <- 1 # list of cities still to add to greedy list
	currcity = 1 # start at the beginning
	while(length(visitedcities) < numcities)
	{
		validcities <- which(!(allcities%in%visitedcities))
		cityorder <- order(distcities[currcity,])[-1] # remove first since it is itself
		nextcity <- cityorder[which(cityorder%in%validcities)[1]]
		visitedcities <- c(visitedcities,nextcity)
		currcity <- nextcity
	}
	visitedcities <- c(visitedcities,1) # and add back to the start
	
	# Now make the entire population look like this!
	pop <- rep(visitedcities,popsize)
	pop <- matrix(pop,popsize,byrow=TRUE)
	
}
#
# Path is a matrix where each row represents an (x,y) of a city
# The path distance is then the euclidean sum of the paths from each city.
# 
path_distance <- function(path)
{
	numcities = length(path[,1])
	plen = 0
	for (c in 2:numcities)
	{
		plen = plen + sqrt((path[c-1,1]-path[c,1])^2 + (path[c-1,2]-path[c,2])^2)
	}
	return(plen)
}
fitness <- function(pop,cities)
{
	return(apply(pop,1,function(x) path_distance(cities[x,])))
}
#################################################################################
# Tournament Selection
#################################################################################
#
# Assumes smaller f is better
#
##################################
# tsel
#################################
# IN:
#    v is the number of ids (individuals) to select from
#    tsize is size of tournament
# ASSUMES: that the population is sorted from best to worst, so that lower index values are better
# OUT: The selected individual id
########################################
tsel <- function(v,tsize)
{
	min(sample(seq(from=1,to=v),tsize))
}
#####################################
# tselectN
########################################
# Do a batch of selections, returning a list of all the parents to be selected
# IN: P - size of population
#     N - number of parents to be selected in total
#     tsize - size of each tournament
# OUT: List of all the parent ids selected
#
####
tselectN <- function(P, N, tsize=3)
{
	v <- as.matrix(rep(P,N))  # make a vector with P as the value, size N
	return(apply(v,1,tsel,tsize))
}

##################################################################################
# Search operators
###################################################################################

############################################################
# reverse
############################################################
# Reverse the representation of a subpath of an individual
# IN: ind - the individual
#     prob - probability of applying this mutation operator
# OUT: The mutated individual
#
reverse <- function(ind,prob)
{
    # Now just decide if we are going to reverse based on prob
	if (runif(1) < prob)
	{
		# find 2 points as sub-path to reverse
		# remember that the 1st and last city can't be changed
		cityids = seq(from=2,to=(length(ind)-1),by=1)
		cuts = sample(cityids,2,replace=FALSE) 
		cuts = cuts[order(cuts)]
		
		# and now reverse this subpath
		
		ind[cuts] = rev(ind[cuts])
	}
	return(ind)
}
##################################################
# swap
###################################################
# IN: ind - individual
#     prob - probability (per locus) of swapping two cities
# OP: Randomly picks two cities in the individual and swaps their positions
#     This can happen more than once.
# OUT: The mutated individual
##################################################
swap <- function(ind,prob)
{
	doit = runif(length(ind)) # assume the probability of swapping depends on the length
	numswaps = length(which(doit < prob)) # and find out how many swaps to make

	if (numswaps==0) return(ind)
	# We aren't allowed to change to first or last spot, since they are the 
	# fixed locations of the start/end city
	
	cityids = seq(from=2,to=(length(ind)-1),by=1)
	
	for (s in 1:numswaps) # for the number of swaps that we are going to do...
	{
		# find the two cities that we will swap position, make sure they are different
		swapcities = sample(cityids,2,replace=FALSE) 
		#
		# and swap their values
		temp = ind[swapcities[1]]
		ind[swapcities[1]] = ind[swapcities[2]]
		ind[swapcities[2]]=temp
	}
	return(ind)
}
#######################################################################
# nextgen
########################################################################
# Create the next generation of individuals
# IN:
# pop - the population ordered by shortest to longest path
# tsize, probswap, probrev - tournament size and mutation probabilities
# OP: Create the next population, and mutate the individuals
# OUT: The new population
#
nextgen <- function(pop,tsize=3,probswap=0.05,probrev=0.5)
{	
	tselect <- tselectN(length(pop[,1]),length(pop[,1]),tsize)
	pop <- pop[tselect,]
	
	# and mutate each population member by doing some random swapping of path elements
	
	pop <- t(apply(pop,1,swap,probswap))
	
	# and now do the reverse
	
	pop <- apply(pop,1,reverse,probrev)
	
	return(t(pop))
}

#######################################################################
# evolve
########################################################################
# Main call to evolve a population
#
# IN:
#  cities:  Geometric coordinates for a set of cities
# other parameters controls the evolving population
# OUT: Final evolved solutions
##########################################################################
evolve <- function(cities,popsize=10,gens = 500,tsize = 3,probswap=0.01,probrev=0.9,
													keepbest=TRUE, greedy=FALSE, show.plots=TRUE)
{
# Set up 3 windows for plotting
#
	if (show.plots==TRUE) old.par <- par(mfrow=c(3,1)) else old.par <- par(mfrow=c(1,1))
#
	bestfitness = NULL  # will build these up
	avfitness = NULL    # for plotting
#	
    pop = init_pop(cities,popsize=popsize,greedy=greedy)  # Create initial population
	
	f <- fitness(pop,cities)  # fitness of initial population	
	forder <- order(f)# order by fitness
	pop=pop[forder,] # and order population by fitness
	best = pop[1,]  # and get the current best -- so that we never loss it
	
	bestfitness = c(bestfitness,f[forder[1]])
	avfitness = c(avfitness,mean(f))
	
	for (g in 1:gens)
	{
		pop <- nextgen(pop,tsize=tsize,probswap,probrev)
		if (keepbest==TRUE)	pop[length(pop[,1]),]=best # make sure the best is in the next generation
		f <- fitness(pop,cities)  # fitness of initial population	
		forder <- order(f)# order by fitness
		pop=pop[forder,] # and order population by fitness
		best = pop[1,]   # and find out the (possibly new) best

######################################################
# PLOT OUT INFO...
######################################################
		bestfitness = c(bestfitness,f[forder[1]])
		avfitness = c(avfitness,mean(f))
		currgens = seq(from=1,to=g,by=1)
######
		if (show.plots==TRUE)
		{
			plot(bestfitness,type='l',xlab="Gens",ylab="Path Length",col='red',ylim=c(0,max(bestfitness,avfitness)), main="Blue - Average Path Length, Red - Best")
			lines(avfitness,col='blue')		
			hist(f,main="Fitness Distribution") # plot a histogram of the fitness	
		}
		plot_path(cities,best)
		tp = paste("Best solution, Number cities:", length(cities[,2]))
		title(tp)
		info <- paste(" Shortest path:",round(f[forder[1]],2))
		cat('Generation:'); cat(g); cat(info); cat('\n'); flush.console(); 
	}
	par(old.par)
	return(pop)
}

##################################################################################
# Functions to make cities
# cities_circle makes cities in a circle, since it will be obvious what the best
#               solution is (i.e. go around the circle
# cities_random makes a set of randomly placed cities.  Not obvious what the answer
#               should be....
##################################################################################
#
# cities_circle
# Generate number_cities around a circle, centred on the origin and with radius 1.
# and evenly spaced around the circle.
cities_circle <- function(number_cities = 10)
{
	cities <- NULL
	angles <- seq(from=0,to=2*pi,length.out=(number_cities+1))
	
	# Note we remove the first angle since it is repeated at 2pi, and we just want
	# each city once.  This could be done in one line, but I thought this was less
	# confusing...
	
	for (a in angles[-1])
	{
		cities <- rbind(cities,cbind(cos(a),sin(a)))
	}
	return(cities)
}
#
# Read list of cities from a file, where each row is format id x y, ... 
# These cities have been obtained from the website:
# http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/
# and then edited to remove the header and the EOF at the end of the file
# 
read_cities <- function(fname = 'berlin52.tsp')
{
		locs = read.csv(fname,header=FALSE,sep=' ')
		cities = locs[,2:3]
		return(as.matrix(cities))
}

plot_path <- function(cities, path)
{
# Given a path as a set of from-to city locations (i.e. an individual)
# plot this path...
	plot(cities,xlab='x',ylab='y') # plot the cities as points
	pathpoints = cities[path,]
	lines(pathpoints,col='red')
}

cities <- cities_circle(20)
res <- evolve(cities)   #show.plots=FALSE   


