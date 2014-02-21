
#Housekeeping
set.seed(25)
library(MASS) #contains mvnorm
library(plyr)
#install.packages("pdist") #if you didn't install before
library(pdist)

#'@param draw is the type of distribution to draw policy preferences

voterPref<- function(draw, cov=0, var=0, n, min=0, max=1, mu=0){ #variance is 2 vector, cov is variance-cov matrix
  n <- 5
  var <- c(4,9)
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
  min=-5
  max=5
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

##Function that returns "P1" if voter is closer to party1, "P2" if party2
partyAffil<- function(party1, party2, vals){
  
  # calculate distances, find smaller, associate with that party
  P1dist <- pdist(X=vals, Y=party1)
  P2dist <- pdist(X=vals, Y=party2)
  PreferP1 <- P1dist@dist  < P2dist@dist #Returns True when distance to P1 is smaller
  Party <- ifelse(PreferP1, "P1", "P2")
  return(Party)
}
  
## 4) Function to visualize

visualprefs <- function(vals, Party) {
P1 <- vals[Party=="P1",]
P2 <- as.matrix(vals[Party=="P2",], ncol=2)
  
#positions of the voters and their affiliation (graph)
plot(P1[,1], P1[,2], col="blue")
points(P2[,1], P2[,2], col="red")
  
  
# positions of the parties (add points)

  
}