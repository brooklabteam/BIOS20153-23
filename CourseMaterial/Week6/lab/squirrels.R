stages<-c("0", "1","2","3","4", "5") # sets up a vector of stage names
lx <-c(1, 0.3, 0.15, 0.09, 0.04, 0.01)

### put the mx vector below *********************************
 mx<-    
 
 ### put your calculation of Ro below *************************and remove the #s
#Ro <- 
#Ro

 #### run your code down to here to verify you got the correct value of Ro
#transform data for the Leslie matrix

k <- length(lx)-1 # i.e., almost as big as the vector of lx
#calculate the px values from lx
px <- 1:k
 for (i in 1:k){
px[i] <- lx[i+1]/lx[i] }

mx <-mx[-1] # remove the first  element from mx

#adjust the fertilities for the survival to that age
Fx <- mx*px
Fx <- c(Fx,0) #add a zero at end

# create the matrix
A <- matrix(0, nrow=k+1, ncol=k+1, dimnames=list(stages,stages)) # make a  matrix of zeros
A[row(A) == col(A)+1] <- px # put px on the subdiagonal
A[1,] <- Fx        # put adjusted mx on first row
A                  # check that it worked


# Population Projections

# need to load the package popbio
# first go Tools, install packages type popbio in the box if you haven't done this and are not on a lab computer

require(popbio)

###put your vector of initial population sizes here ***********************
n <- 

# Project the population for 10 years
p<-pop.projection(A,n, 10)


#plot the total population size
plot(p$pop.sizes, type="l", xlab="Time", ylab="Total population")

#  to list all the results of the projection remove hash mark below
#p  
Vect.t <- c(0:10) ## creates a vector for the time
Popest.t <- sum(n) * p$lambda^Vect.t
lines(Popest.t, col="green")

# plot the proportion in each age class over time
stage.vector.plot(p$stage.vectors, proportions=TRUE, 
                  ylim=NULL, xlab="Years", ylab=NULL, col=rainbow(8) )
title(main = "something appropriate")

