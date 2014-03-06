##Dino Hadzic, Maggie Lalor, and Elif Ozdemir
##PS4625 Applied Statistical Programming Spring 2014
##Problem Set 05 Due 03/04/2014

#Housekeeping
rm(list=ls()) #after cleaning space
set.seed(25)
library(MASS) #contains mvnorm
library(plyr)
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
#'@author Dino Hadzic, Maggie Lalor, and Elif Ozdemir

##Values for testing function as it's being written
n <- 25
var <- c(4,9)
min=0
max=7

voterPref<- function(draw, n, cov=0, var=0, min=0, max=7, mu=0){ #variance is 2 vector, cov is variance-cov matrix

  #matrix of NAs to store preferences
  vals <- matrix(nrow=n, ncol=2)

  #x1, x2 are the two dimensions for a given voter's policy preferences
  if(draw=="standard"){
  #draw independently from standard normal distributions
    vals<- matrix(rnorm(2*n, mean=0, sd=1), nrow=n, ncol=2) #defaults, but reminder that standard normal
  }
      
  #draw independently from normal distributions where each variance is set separately
  if(draw=="uniquevari" & var !=0) {
    vals<- cbind(rnorm(n, mean=0, sd = sqrt(var[1])), rnorm(n,sd = sqrt(var[2])))
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

# example of running function
vals <- voterPref(draw="uniform", n=5, cov=0, var=0, min=0, max=7)
vals <- voterPref(draw="standard", n=25)


#random values for 2 parties
party1<- runif(2, min=-5, max=5)
party2<- runif(2, min=-5, max=5)

## 3) 

#'Function that returns "P1" if voter is closer to party1, "P2" if party2
#'@param party1 length 2 vector of policy preferences for Party 1
#'@param party2 length 2 vector of policy preferences for Party 2
#'@param vals 2 by n matrix of voter policy preferences
#'@return returns an n length vector where the ith value is "P1" if the euclidian distance
#'from the ith voter to Party 1 is smaller than the distance to Party 2, and "P2" otherwise
#'
#'@author Dino Hadzic, Maggie Lalor, and Elif Ozdemir

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
#'@author Dino Hadzic, Maggie Lalor, and Elif Ozdemir

#Dino: changing xlim and ylim for visualprefs to c(-6,8). Othewise, when we plot positions, the parties will
#occassionally not appear on the graph. 8 is the uppper bound since when drawing from uniform distribution for voters, 
#7 is the maximum value.
#Had to tried to make it the min and max relative to range of voter prefs

visualprefs <- function(party1, party2, vals, Party) {
  P1 <- vals[Party=="P1",] #Subset the dataset by closest party
  P2 <- as.matrix(vals[Party=="P2",], ncol=2)

  #positions of the voters and their affiliation (graph) - b) and c)
  plot(P1[,1], P1[,2], xlab="Preference X1", ylab="Preference X2", ylim=c(-6, 8), xlim=c(-6, 8), type="p", pch=24, col="blue", main="Policy Preferences") 
  points(P2[,1], P2[,2], col="red", pch=24) #Voters
  
  #positions of the parties (add points and label) - a)
  points(t(party1), col="blue") #if not transposed get two points
  text(t(party1), "Party 1", pos=4, cex=0.6) #4 is to right
  points(t(party2), col="red")
  text(t(party2), "Party 2", pos=4, cex=0.6) #4 is to right

} #End image function

visualprefs(party1, party2, vals, Party)


### Part II: Getting Things Moving ###


## 1) For each iteration t of the model, the parties locate at the “mean” position of all
#voters who affiliated with them in period t − 1.

##Maggie comment: I assume this means we figure out how to find new party positions:
#This can also be done all at once
P1 <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position
P2 <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position


## 2) Write a function that chooses the “starting” position of the parties at random
#'@param min is the minimum value of a policy preference, default 0
#'@param max is the maximum value of a policy preference, default 7
#'@return returns a two item list with the positions for two parties
#'
#'@author Dino Hadzic, Maggie Lalor, and Elif Ozdemir

PartyStart <- function(min=0, max=7){ #min and max for party positions
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  return(list("P1start"=P1start, "P2start"=P2start))
} #returns starting positions as list

start <- PartyStart()


## 3) Write a function that “re-locates” the parties according to this heuristic.

#' Function to visualize
#'@param vals is the matrix of policy positions of n voters in time t-1
#'@param party is the vector of n voter's closest party, "P1" for those affiliated with party 1,
#'"P2" for this with party 2
#'@return returns a two item list with the positions for two parties
#'
#'@author Dino Hadzic, Maggie Lalor, and Elif Ozdemir

RelocateParty <- function(vals, Party) {
  #Calulate Party Positions
  P1start <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) #Party 1 position  
  P2start <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) #Party 2 position
  
  return(list("P1start"=P1start, "P2start"=P2start))
}

Relocate <- RelocateParty(vals=vals, Party=Party)


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
#Elif: I did this as a seperate question not to slow down the simulation function in 4

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
  } #End for loop
  
  return(list("P1"=P1, "P2"=P2, "vals"=vals))
} #End master simulation function 2
 masterSim2() #returns some Nan in matrices/check why


## 3) Use expand.grid() function to set up a data frame 
require(utils)
library('doMC')
library('multicore')
library('foreach')
dfParam<- expand.grid(draw=c("standard", "uniquevari", "uniform", "multivariate", "mixture"), sim=c(10:100), rseed=c(1:100), stringsAsFactors=FALSE)
# str(dfParam)
registerDoMC(cores=4) 
out2 <- aaply(.data=dfParam, .margins=1, .fun=masterSim2, .parallel=TRUE)


## 4) Use plots to compare some comparative static of interest
#'@param sim as the number of iterations
#'@param draw for different methods to generate voters
#'@param rseed for setting random seed
# Elif: Here I used two different draw types to compare.

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


#Expand your model
#1. Alter your model so that the number of parties is an optional input. How do your results
#change as a result?

#'@param min indicates the minimum preference value.
#'@param max indicates the maximum preference value.
#'@param rseed is the random seed for the function, which can be adjusted.
#'@param draw indicates the type of distribution from  which voters' preferences are selected.
#'@param n represents the number of voters being drawn.
#'@param nparty is the number of parties, which can be adjusted.

Mult_Parties <- function(min=0, max=7, rseed=7, draw="uniform", n=20, nparty=10){
  set.seed(rseed)                                         #Sets the seed.
  vals <- voterPref(draw=draw, n=n)                       #Draws voter preferences and stores them as vals.
  
  PartyPrefs <- matrix(nrow=nparty, ncol=2)                   
  for(i in 1:nparty){
    PartyPrefs[i,] <- runif(2, min=0, max=7)
  } #This for loop stores preferences for a specified number of voters in PartyPrefs.
  
  Pdist <- list()
  for(i in 1:nparty){
    Pdist[i] <- pdist(X=vals, Y=PartyPrefs[i,])
  } #Measures the distances between each voters and each party and stores it as Pdist.
  
  Prefmatrix <- matrix(nrow=nparty, ncol=nrow(vals))
  for(i in 1:nparty){
    Prefmatrix[i,] <- Pdist[[i]]@dist[1:nrow(vals)]
  } #Creates a matrix called Prefmatrix which rows denoting the parties and columns voters. The cells represent the
  #distances between a particular party and voter.
  
  Prefer <- character()
  for(i in 1:nrow(vals)){
    Prefer[i] <- which(Prefmatrix[,i] == min(Prefmatrix[,i]))
    PartyPrefer <- as.numeric(Prefer)
  } #Creates a vector of the parties each voter most prefers and stores it as PartyPrefer.
  
  Prefmatrix <- rbind(Prefmatrix, PartyPrefer) #combines the Prefmatrix with PartyPrefer, stores it as Prefmatrix. The 
  #bottom row of Prefmatrix denotes the party voter i prefers.
  
  return(Prefmatrix)
}

#Increasing the number of parties has an effect that is quite intuitive. The support for parties among voters
#becomes more splintered, with the share of support each party receives from voters decreasing as we increase the 
#number of parties.


#2. Alter your model so that voters vote "probabilistically" as some function of the distance betwen the two parties. 
#(That is, alow them to make the "wrong" decision if they are nearly indifference between the parties.) Do the
#results change?

#We can alter the model, masterSim2, from the previous section and create a new model named masterSim3. This new 
#model permits "probabilistic" voting. All of the parameters remain unchanged, but we do add random noise to the 
#distance between party 1 and the voters, conceptually similar to permitting voters to cast ballot "probabilistically."

masterSim3<- function(min=0, max=7, sim=5, draw="standard", n=100, rseed=25){
  set.seed(rseed)
  vals<- voterPref(draw=draw, n=n)
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  P1<- P1start 
  P2<- P2start 
  for (i in 1:sim){
    P1dist <- pdist(X=vals, Y=P1start)
    P2dist <- pdist(X=vals, Y=P2start)
    
    P1dist@dist <- P1dist@dist + rnorm(n) #random noise added to simulate voters casting ballots "probabilistically." 
    #The random noise generally shifts distances by a magnitude between 0.3 and 1.5. 
    
    PreferP1 <- P1dist@dist  < P2dist@dist 
    Party <- ifelse(PreferP1, "P1", "P2")
    P1start <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) 
    P2start <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) 
    P1<- rbind(P1, P1start) 
    P2<- rbind(P2, P2start) 
  } 
  return(list("P1"=P1, "P2"=P2, "vals"=vals))
} 

#The results do not really change after permitting voters to cast ballots "probabilistically." This is likely due to the
#fact that a party both gains and loses votes when "probabilistic" voting is introduced.  Ultimately, any policy 
#preference changes parties make while permitting for such voting is minor.

#3. Add at least one heuristic to the model. How does that change thhe behavior of the model?

#We will be adding a heuristic that permits a party that receives fewer votes in an election against another party to adopt policy
#preferences closer to the party that prevailed in the election.  To do this, we will alter the function from the previous question, 
#masterSim3, and create a new function called masterSim4. The parameters are the same. The only changes are the if statements located
#in the middle of the function, which move the losing party closer to the policy preferences of the winning party.

masterSim4<- function(min=0, max=7, sim=5, draw="standard",n=100, rseed=25){
  set.seed(rseed)
  vals<- voterPref(draw=draw, n=n)
  P1start <- runif(2, min, max)
  P2start <- runif(2, min, max)
  for (i in 1:sim){
    P1dist <- pdist(X=vals, Y=P1start)
    P2dist <- pdist(X=vals, Y=P2start)
    PreferP1 <- P1dist@dist  < P2dist@dist
    Party <- ifelse(PreferP1, "P1", "P2")
    
    if(length(which(Party == "P1")) > length(which(Party == "P2"))){ #In case party 1 receives more votes, party 2 moves closer to the policy
      #preferences of the winning party 1.
      P2dist@dist <- (P1dist@dist + P2dist@dist)/2 
    }
    
    if(length(which(Party == "P2")) > length(which(Party == "P1"))){ #In case party 2 receives more votes, party 1 moves closer to the policy
      #preferences of the winning party 2.
      P1dist@dist <- (P1dist@dist + P2dist@dist)/2
    }
  }
  for(i in 1:sim){
    PreferP1 <- P1dist@dist  < P2dist@dist
    Party <- ifelse(PreferP1, "P1", "P2")
    P1start <- aaply(.data=vals[Party=="P1",], .margins=2, .fun= mean) 
    P2start <- aaply(.data=vals[Party=="P2",], .margins=2, .fun= mean) 
    P1 <- rbind(P1, P1start) 
    P2 <- rbind(P2, P2start) 
  } 
  return(list("P1"=P1, "P2"=P2, "vals"=vals))
} 

#By adding the heuristic described above, the parties settle on policy preferences faster than if we exclude the heuristic. Also,
#the distance in policy preferences between the two parties decreases somewhat.
