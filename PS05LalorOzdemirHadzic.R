##Maggie Lalor
##PS4625 Applied Statistical Programming Spring 2014
##Problem Set 05 Due 03/04/2014

#Housekeeping
rm(list=ls()) #after cleaning space
set.seed(25)
library(MASS) #contains mvnorm
library(plyr)
library()
#install.packages("pdist") #if you didn't install before
library(pdist)

### Part I: Simulation Setup ###

### NOTES: REWRITE FUNCTION TO BE MORE EFFICIENT/SHORTER, if time/desired ####
## 1) and 2) 
#' This function creates 2-dimensional policy preferences for n voters drawn from specified distributions
#'@param draw is the type of distribution to draw policy preferences, with possible values "standard" for standard
#'normal, "unique vari" for normal distributions with specified variances, "uniform" for the uniform distribution, "multivariate"
#'from a multivariate distribution, or "mixture" from three multivariate distributions
#'@param n is the number of voters
#'@param cov is the optional variance covariance matrix for drawing from multivariate normal distributions
#'@param var is the optional length 2 vector of variances when drawing from two separate normal distributions
#'@param min is the optional minimum preference value when drawing from an uniform distribution
#'@param max is the optional maximum preference value when drawing from an uniform distribution
#'@param mu is the optional length 3 vector of means when drawing from 3 multivariate distributions
#'@return returns a 2 by n matrix where each row is a simulated voter, and the columns are the two policy dimensions
#'
#'@author Margaret Lalor

##Values for testing function as it's being written
n <- 25
var <- c(4,9)
min=0
max=7
#Elif: Changed all min/max values to (0,7) for ideological scales of parties, so we will have consistency. We may need to make changes in the voterPref function to have consistency with voter preferences as well. For example standard draw always have a sample between (0,1) as it is now.

#Dino: Revising code for "multivariate" and "mixture" draws so that n x 2 matrix is created by function.

voterPref<- function(draw, cov=0, var=0, n, min=0, max=7, mu=0){ #variance is 2 vector, cov is variance-cov matrix

  #matrix of NAs to store preferences
  vals <- matrix(nrow=n, ncol=2)

  #x1, x2 are the two dimensions for a given voter's policy preferences
  if(draw=="standard"){
  #draw independently from standard normal distributions
    vals<- matrix(rnorm(2*n, mean=4, sd=2), nrow=n, ncol=2)
  }
      
  #draw independently from normal distributions where each variance is set separately
  if(draw=="uniquevari" & var !=0) {
    vals<- cbind(rnorm(n,sd = sqrt(var[1])), rnorm(n,sd = sqrt(var[2])))
  }

  #draw from uniform
  if(draw=="uniform") {
    vals <- matrix(runif(2*n, min=min, max=max), nrow=n, ncol=2)
  }
  
  #draw from multivariate with specified variate-covariate
  if(draw=="multivariate" & cov != 0) {
    vals <- matrix(mvrnorm(n=2*n, Sigma=cov, mu=mu)) #mu argument added.
    vals <- matrix(sample(vals), 2*n, nrow=n, ncol=2) #Added to create n x 2 matrix.
  }
  
  #draw from mixture of up to 3 multivariate normal distributions (where centered as well as var-cov matrix), just doens't want unimodal symmetric
  #can have the same variance-covariance matrix
  if(draw=="mixture" & cov != 0 & mu !=0) {
    vals <- matrix(mvrnorm(n=2*n, Sigma=cov, mu=mu))
    vals <- matrix(sample(vals), 2*n, nrow=n, ncol=2) #Added to create n x 2 matrix.
  }
  return(vals)
}

#random values for 2 parties
party1<- runif(2, min=-5, max=5)
party2<- runif(2, min=-5, max=5)


### NOTES: REWRITE FUNCTION TO ATTACH AS ANOTHER COLUMN IN VALS ###
## 3) 

#'Function that returns "P1" if voter is closer to party1, "P2" if party2
#'@param party1 length 2 vector of policy preferences for Party 1
#'@param party2 length 2 vector of policy preferences for Party 2
#'@param vals 2 by n matrix of voter policy preferences
#'@return returns an n length vector where the ith value is "P1" if the euclidian distance
#'from the ith voter to Party 1 is smaller than the distance to Party 2, and "P2" otherwise
#'
#'@author Margaret Lalor
partyAffil<- function(party1, party2, vals){
  
  # calculate distances, find smaller, associate with that party
  P1dist <- pdist(X=vals, Y=party1)
  P2dist <- pdist(X=vals, Y=party2)
  PreferP1 <- P1dist@dist  < P2dist@dist #Returns True when distance to P1 is smaller
  Party <- ifelse(PreferP1, "P1", "P2")
  return(Party)
} # End party function
  
Party <- partyAffil(party1,party2, vals)

## 4) VISUALIZE

#' Function to visualize
#'@param party1 length 2 vector of policy preferences for Party 1
#'@param party2 length 2 vector of policy preferences for Party 2
#'@param vals 2 by n matrix of voter policy preferences
#'@param Party n length vector of voters' preferred party
#'@return returns a pdf image of voters' preferences, red for Party 1, blue Party 2,
#'with labels dots for the positions of the parties themselves
#'
#'@author Margaret Lalor

#Dino: changing xlim and ylim for visualprefs to c(-6,6). Othewise, when we plot positions, the parties will
#occassionally not appear on the graph.

visualprefs <- function(party1, party2, vals, Party) {
P1 <- vals[Party=="P1",] #Subset the dataset by closest party
P2 <- as.matrix(vals[Party=="P2",], ncol=2)

#positions of the voters and their affiliation (graph) - b) and c)
plot(P1[,1], P1[,2], xlab="Preference X1", ylab="Preference X2", ylim=c(-6, 6), xlim=c(-6, 6), type="p", pch=24, col="blue", main="Policy Preferences") 
points(P2[,1], P2[,2], col="red", pch=24) #Voters
  
#positions of the parties (add points and label) - a)
points(t(party1), col="blue") #if not transposed get two points
text(t(party1), "Party 1", pos=4, cex=0.6) #4 is to right
points(t(party2), col="red")
text(t(party2), "Party 2", pos=4, cex=0.6) #4 is to right

} #End image function


### Part II: Getting Things Moving ###


## 1) For each iteration t of the model, the parties locate at the “mean” position of all
#voters who affiliated with them in period t − 1.

##Maggie comment: I assume this means we figure out how to find new party positions:

P1 <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position

P2 <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position


## 2) Write a function that chooses the “starting” position of the parties at random
#'@param min is the minimum value of a policy preference, default 0
#'@param max is the maximum value of a policy preference, default 7
#'@return returns a two item list with the positions for two parties
#'
#'@author Margaret Lalor
PartyStart <- function(min=0, max=7){ #min and max for party positions
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  return(list("P1start"=P1start, "P2start"=P2start))
} #returns starting positions as list


## 3) Write a function that “re-locates” the parties according to this heuristic.

#' Function to visualize
#'@param vals is the matrix of policy positions of n voters in time t-1
#'@param party is the vector of n voter's closest party, "P1" for those affiliated with party 1,
#'"P2" for this with party 2
#'@return returns a two item list with the positions for two parties
#'
#'@author Margaret Lalor

RelocateParty <- function(vals, Party) {
  #Calulate Party Positions
  P1 <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position  
  P2 <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position
  
  return(list("P1start"=P1start, "P2start"=P2start))
}


## 4) Write a “master” function that runs the simulation.
#'Function to iterate simulation several times
#'@param vals is the matrix of policy positions of n voters in time t-1
#'@param min is the minimum value of a policy preference, default 0
#'@param max is the maximum value of a policy preference, default 7
#'@param sim is the number of iterations

masterSim<- function(min=0, max=7, vals){
  n<- 100 #number of voters
  vals<- matrix(rnorm(2*n, mean=4, sd=2), nrow=n, ncol=2) #standard draw
  #Party 1 and 2 starts by choosing a random position between 0 and 7 on 2 dimensional policy scale
  sim<- 5 #number of iterations
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  #By the loop below, parties adjust their position according to their mean voter in previous elections and voters vote accordingly
  P1<- P1start #matrix to store the evolution of P1's position
  P2<- P2start #matrix to store the evolution of P2's position
  PartyAff<- NULL #matrix to store the evolution of voter preferences
  for (i in 1:sim){
  #Voters cast their ballot to closest party by calculating distance
  P1dist <- pdist(X=vals, Y=P1start)
  P2dist <- pdist(X=vals, Y=P2start)
  PreferP1 <- P1dist@dist  < P2dist@dist #Returns True when distance to P1 is smaller
  Party <- ifelse(PreferP1, "P1", "P2")
  #Parties relocate themselves
  P1start <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position  
  P2start <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position
  P1<- rbind(P1, P1start) #P1's position shift in time
  P2<- rbind(P2, P2start) #P2's position shift in time
  PartyAff<- cbind(PartyAff, Party) #Change in voter prefereces in time
  } #End loop
  return(list("P1"=P1, "P2"=P2, "PartyAff"=PartyAff))
} #End master simulation function

## 5) Visualization of this process (which we will find helpful and FUN)
#Elif: I did this as a seperate question not to slow down the simulation function in 4, but we can move this into the function above if you like.

#To follow how parties changed their positions in time during the iterations, while voters have stable preferences

for (i in 1:nrow(P1)){
  P1<- masterSim()$P1
  P2<- masterSim()$P2
plot(vals, xlab="Preference X1", ylab="Preference X2", ylim=c(min,max), xlim=c(min,max), type="p", main="Policy Preferences", pch=1)
points(P1[i,1], P1[i,2], col="blue", pch=17)   
points(P2[i,1], P2[i,2], col="red", pch=17)
}

### Part III: Explore Your Model ###

## 1 & 2) Alter your function so it takes 
#'@param draw for choosing the distribution of voters
#'@param rseed for setting a random seed
#'@param sim as number of total iterations
#'as inputs
#'Output should be the vector of positions the parties take throughout the sim

masterSim2<- function(min=0, max=7, sim=5, draw="standard", n=100, rseed=25){
  set.seed(rseed)
  vals<- voterPref(draw=draw, n=n)
  #Party 1 and 2 starts by choosing a random position between 0 and 7 on 2 dimensional policy scale
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  #By the loop below, parties adjust their position according to their mean voter in previous elections and voters vote accordingly
  P1<- P1start #matrix to store the evolution of P1's position
  P2<- P2start #matrix to store the evolution of P2's position
  for (i in 1:sim){
    #Voters cast their ballot to closest party by calculating distance
    P1dist <- pdist(X=vals, Y=P1start)
    P2dist <- pdist(X=vals, Y=P2start)
    PreferP1 <- P1dist@dist  < P2dist@dist #Returns True when distance to P1 is smaller
    Party <- ifelse(PreferP1, "P1", "P2")
    #Parties relocate themselves
    P1start <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position  
    P2start <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position
    P1<- rbind(P1, P1start) #P1's position shift in time
    P2<- rbind(P2, P2start) #P2's position shift in time
  } #End loop
  return(list("P1"=P1, "P2"=P2, "vals"=vals))
} #End master simulation function 2
## masterSim2() returns some Nan in matrices/check why


## 3) Use expand.grid() function to set up a data frame 
require(utils)
library('doMC')
library('multicore')
library('foreach')
dfParam<- expand.grid(draw=c("standard", "uniquevari", "uniform", "multivariate", "mixture"), sim=c(10:100), rseed=c(1:100), stringsAsFactors=FALSE)
str(dfParam)
registerDoMC(cores=8) 
out2 <- aaply(.data=dfParam, .margins=1, .fun=masterSim2, .parallel=TRUE)


## 4) Use plots to compare some comparative static of interest
#'@param sim as the number of iterations
#'@param draw for different methods to generate voters
#'@param rseed for setting random seed
# Elif: Here I used two different draw types to compare. But we can use other parameters you would like as well.

Plot1<- masterSim2(draw="standard")
Plot2<- masterSim2(draw="uniform")
par(mfrow=c(1,2))
for (i in 1:nrow(P1)){
  plot(vals, xlab="Preference X1", ylab="Preference X2", ylim=c(min,max), xlim=c(min,max), type="p", main="Standard Draw", pch=1)
  points(Plot1$P1[i,1], Plot1$P1[i,2], col="blue", pch=17)   
  points(Plot1$P2[i,1], Plot1$P2[i,2], col="red", pch=17)
  plot(vals, xlab="Preference X1", ylab="Preference X2", ylim=c(min,max), xlim=c(min,max), type="p", main="Univariate Draw", pch=1)
  points(Plot2$P1[i,1], Plot2$P1[i,2], col="blue", pch=17)   
  points(Plot2$P2[i,1], Plot2$P2[i,2], col="red", pch=17)
}


