
# Discrete Logistic is N(t+1) = rN(t)(1-N(t)/K) + N(t)

rm(list=ls()) #clear workspace

#Define next population size via Discrete Logistic Growth given
#intrinsic growth rate R
#Current population N
#Carrying capacity K
# Logistic Growth returns Nt+1 for a particular N
LogisticGrowth <- function(R, N, K) {
  Nnext <- R*N*(1-N/K) + N
  return(Nnext)
}

# deltaN returns the change in population size for a particular N
deltaN <- function(R, N, K) {
  delN <- R*N*(1-N/K) 
  return(delN)
}

#Define parameters
R <- 0.25#intrinsic growth rate
N <- 0:1000 #population range
K <- 1000 #carrying capacity

#calculate functions

Logisticcurve <- LogisticGrowth(R, N, K) #calculate N+1 across range of N for Logistic
delCurve <- deltaN(R,N,K) # calculate delta N across range


#Set up plot and Logistic curve
plot(N,delCurve, type="l", col="blue", xlab="Population Size N(t)", ylab="deltaN/dt",  main="Logistic in Discrete Time") 
mtext(paste("Parameters: r = ", R, ", K = ", K), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid

plot(N,Logisticcurve, type="l", col="blue", xlab="Population Size N(t)", ylab="N(t+1)",  main="Logistic in Discrete Time") 
mtext(paste("Parameters: r = ", R, ", K = ", K), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid

## ****  with constant harvest H  ***********************
# deltaNwH returns the change in population size for a particular N
deltaNwH <- function(R, N, K, H) {
  delNwH <- R*N*(1-N/K) - H 
  return(delNwH)
}

#Define parameters
R <- 0.25#intrinsic growth rate
N <- 0:1000 #population range
K <- 1000 #carrying capacity


par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
delCurve <- deltaNwH(R,N,K, H=0) # calculate delta N across range
plot(N,delCurve, type="l", col="blue", xlab="Population Size N(t)", ylab="deltaN/dt",  main="Logistic in Discrete Time w harvest") 
mtext(paste("Parameters: r = ", R, ", K = ", K, ", H= 0"), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid
abline(h=40)

delCurve <- deltaNwH(R,N,K, H=40) # calculate delta N across range
plot(N,delCurve, type="l", col="blue", xlab="Population Size N(t)", ylab="deltaN/dt",  main="Logistic in Discrete Time w harvest") 
mtext(paste("Parameters: r = ", R, ", K = ", K, ", H=", H), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid
abline(h=0)

## ****  with  harvest H incteasing with N  ***********************
# deltaNwH returns the change in population size for a particular N
deltaNwNH <- function(R, N, K, H) {
  delNwNH <- R*N*(1-N/K) - H*N 
  return(delNwNH)
}

#Define parameters
R <- 0.25#intrinsic growth rate
N <- 0:1000 #population range
K <- 1000 #carrying capacity


par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
delCurve <- deltaNwNH(R,N,K, H=0) # calculate delta N across range
plot(N,delCurve, type="l", col="blue", lwd=2,  xlab="Population Size N(t)", ylab="deltaN/dt",  main="Logistic in Discrete Time w harvest") 
mtext(paste("Parameters: r = ", R, ", K = ", K, ", H= 0"), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid
abline(a = 0, b = 0.08)        # Add line with intercept & slope
text(500,20, "Harvest rate")


delCurve <- deltaNwNH(R,N,K, H=0.03) # calculate delta N across range
plot(N,delCurve, type="l", col="blue", lwd=2, xlab="Population Size N(t)", ylab="deltaN/dt",  main="Logistic in Discrete Time w harvest") 
mtext(paste("Parameters: r = ", R, ", K = ", K, ", H=0.03N"), side=3) #annotate plot with parameters concatenated into a strong
grid(col = "gray") # add a grid
abline(h=0)

