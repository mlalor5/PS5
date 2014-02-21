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
n <- 5
var <- c(4,9)
min=-5
max=5


voterPref<- function(draw, cov=0, var=0, n, min=0, max=1, mu=0){ #variance is 2 vector, cov is variance-cov matrix

  #matrix of NAs to store preferences
  vals <- matrix(nrow=n, ncol=2)

  #x1, x2 are the two dimensions for a given voter's policy preferences
  if(draw=="standard"){
  #draw independently from standard normal distributions
    vals<- matrix(rnorm(2*n), nrow=n, ncol=2)
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
    vals <- matrix(mvrnorm(n=2*n, Sigma=cov) 
  }
  
  #draw from mixture of up to 3 multivariate normal distributions (where centered as well as var-cov matrix), just doens't want unimodal symmetric
  #can have the same variance-covariance matrix
  if(draw=="mixture" & cov != 0 & mu !=0) {
    vals <- matrix(mvrnorm(n=2*n, Sigma=cov, mu=mu) 
  }
  return(vals)
}

#random values for 2 parties
party1<- c(0,1)
party2<- c(2,3)


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
}
  
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

visualprefs <- function(party1, party2, vals, Party) {
P1 <- vals[Party=="P1",]
P2 <- as.matrix(vals[Party=="P2",], ncol=2)
  
#positions of the voters and their affiliation (graph)
plot(P1[,1], P1[,2], col="blue")
points(P2[,1], P2[,2], col="red")
  
# positions of the parties (add points and label)
points(party1, col="blue")
points(party2, col="red")

}

