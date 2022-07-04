# Island Biogeography simulation

# Define parameters

# Parameters to vary
IslandSize =20; #width of island, vary between 5 and 25
IslandDistance = 25; #distance of island from mainland; vary between 5 and MaxDist - IslandSize

# Parameters less likely to want to vary
Pool = 100; # number of species on mainland
MaxYears = 50; # time to be modelled

MaxDist = 100; # width of ocean
Colonization = 0; # initialize colonization number
Extinction = 0; # initialize extinction number

# Make a vector of species on the island, initially empty (all zeros)
IslandSpp <- vector("numeric", Pool)
# Make empty vector to hold extinctions
ExtinctionSpI <- vector("numeric", Pool)
# Make an empty vector of dispersal distances
DispDist <-vector("numeric", Pool)
# Make an empty matrix of colonizations and extinctions for each year
 # columns are  immigrants, extinctions, species on island
IslandBiog <-  matrix(numeric((MaxYears+1)*3), nrow = MaxYears+1, ncol = 3)

### The loop begins here
#do over  the years 
for (j in 1:MaxYears) { 

# for each species on the mainland
for (i in 1:Pool) { 
   # check that the species has not currently on the island first
  if (IslandSpp[i] == 0) {
    #make dispersal distances by drawing from positive values of normal distribution
  DispDist[i] <- MaxDist* abs(rnorm(n=1, mean=0, sd=0.5)) 
   #hist(DispDist) #this will create a histogram of the dispersal distances
  #check if the disperser has hit the island, 
  if ( (DispDist[i]>IslandDistance)&(DispDist[i]<(IslandDistance + IslandSize)))  {
    IslandSpp[i]<- 1  #and if so, change to 1 (present) in Island Spp
    Colonization <- Colonization +1 } # add one to colonizations counter
   }}

# see whether the species goes extinct (draw a random number, check if < 1/ island size)
for (i in 1:Pool) {
  if ((IslandSpp[i]>0)&(runif(1)<1/IslandSize)) {
    Extinction= Extinction +1   # if so, add to extinction counter
    IslandSpp[i] <- 0 }   # and remove the  species from the island
  
  } #end of loop over mainland species
   
# store values in the matrix for the year
IslandBiog[j,1] <- Colonization 
IslandBiog[j+1,2] <- Extinction # store in next year
IslandBiog[j+1,3] <- sum(IslandSpp) # store in next year, # of spp on island

# reset the counters prior to next loop
Colonization <-0
Extinction <- 0

} # end of data creating loop

IslandBiog <- IslandBiog[-(MaxYears+1),] #remove the extra row

par(mfrow=c(1,2)); # this tells it to plot the two graphs side-by-side
plot(IslandBiog[,3], IslandBiog[,1], xlab = "species on island", 
     ylab = "colonizations or extinctions", 
     main=paste("Island Size = ", IslandSize, " Distance = ",IslandDistance ),ylim=c(0,max(IslandBiog[,1])))
# Use symbol '*' for extinction points.
points(IslandBiog[,3], IslandBiog[,2], col="red", pch="*")
# create regression lines
Reg1 <-lm(IslandBiog[,1] ~ IslandBiog[,3]) # for colonization
abline(Reg1)
Reg2 <-lm(IslandBiog[,2] ~ IslandBiog[,3]) # for extinction
abline(Reg2, col="red", lty=2)

#plot the total number of species on the island over time
plot(IslandBiog[,3], xlab = "time",  ylab = "species on island")


