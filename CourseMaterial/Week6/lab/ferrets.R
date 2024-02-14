stages<-c( "1","2","3","4", "5") # sets up a vector of stage names

k <- 5 ## number of stages

# ****** Fill in the vectors  below based on the life cycle graph ************
Fx <- c(   ) #add a zero at end

px <- c(   ) # add a zero at the end

# code below create the matrix
A <- matrix(0, nrow=k, ncol=k, dimnames=list(stages,stages)) # make a  matrix of zeros
A[row(A) == col(A)+1] <- px # put px on the subdiagonal
A[1,] <- Fx        # put adjusted mx on first row
A                  # check that it worked


# Population Projections

# need to load the package popbio
# first go Tools, install packages type popbio in the box if you haven't done this and are not on a lab computer

require(popbio)

###vector of initial population sizes, approx 50 individuals in 2003 ***********************
n <- c(10,10,10,10,10)

# Project the population for 10 years
p<-pop.projection(A,n, 10)



#  to list all the results of the projection
p  


Vect.t <- c(0:10) ## creates a vector for the time
Popest.t <- sum(n) * p$lambda^Vect.t
lines(Popest.t, col="green")

# plot the proportion in each age class over time
stage.vector.plot(p$stage.vectors, proportions=TRUE, 
                  ylim=NULL, xlab="Years", ylab=NULL, col=rainbow(8) )
title(main = "Stage class vectors")

