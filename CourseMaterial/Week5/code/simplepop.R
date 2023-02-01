rm(list=ls())
library(ggplot2)
library(scales)

#### 
homewd = paste0(getwd(), "/CourseMaterial/Week5/code/")

## plots for lecture

## graph trajectory of mussels and starfish in short time frame only
dat <- cbind.data.frame(mussels=rev(seq(1,1000,100)))
dat$starfish = seq(1,100, length=nrow(dat))

#add some noise for biological realism
add.noise<- function(par, sd){
  par.new <-  rnorm(n=1, mean=par, sd=sd)    
  #not allowed to be negative, so if, so, give a small number:
  if(par.new<0){
    par.new = .0000001
  }
  return(par.new)
}

dat$mussels <- round(sapply(dat$mussels, add.noise, sd=10),1)
dat$starfish <- round(sapply(dat$starfish, add.noise, sd=10),1)

p1 <- ggplot(data=dat) + geom_point(aes(x=starfish, y=mussel))

#add linear regression to these data
m1 <- lm(mussels~starfish, data=dat)
summary(m1) #R2 =.97

dat$predicted <- predict(m1)

p2 <- ggplot(data=dat) + geom_point(aes(x=starfish, y=mussels), size=3) +
      geom_line(aes(x=starfish, y=predicted), size=1, color="red") + theme_bw() +
      theme(panel.grid = element_blank(), axis.title = element_text(size=18),
            axis.text = element_text(size=16))


ggsave(file = paste0(homewd, "figs/starfish_mussel.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2, 
       dpi=300)


### what about global population?

world_pop <- read.csv(paste0(homewd,"world_pop.csv"), stringsAsFactors = FALSE)
head(world_pop)
tail(world_pop)

# by default only plot the population
p3 <- ggplot(data=world_pop, 
             aes(x = year, y = pop, color=country)) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_x_continuous("Year") + 
  scale_y_log10("Population", labels = scales::comma) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "top",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))


ggsave(file = paste0(homewd, "figs/country_pop.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)


### birth dates

births <- read.csv(paste0(homewd,"births_69_88.csv"), stringsAsFactors = FALSE)
head(births)
birthdays <- as.Date(paste(births$month, births$day, "1969"), "%m %d %Y")

pl_birthdays_nohigh <- ggplot(data = data.frame(date_b = as.Date(birthdays), 
                                                number_of_births = births$births), 
                              aes(x = date_b, y = number_of_births)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14, face="bold")) +
  scale_x_date("", labels = date_format("%b")) + 
  scale_y_continuous("Number of births (1969-1988)") + 
  annotate("text", x = birthdays[185], y = 175000, label = "Fourth of July", colour = "blue", alpha = 0) + 
  annotate("text", x = birthdays[5], y = 162000, label = "New Year", colour = "blue", alpha = 0) + 
  annotate("text", x = birthdays[359], y = 155000, label = "Xmas", colour = "blue", alpha = 0) + 
  annotate("text", x = birthdays[259], y = 215000, label = "38 weeks after Xmas", colour = "blue", alpha = 0) 
pl_birthdays_nohigh 

ggsave(file = paste0(homewd, "figs/births_US.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)


pl_birthdays <- ggplot(data = data.frame(date_b = as.Date(birthdays), 
                                         number_of_births = births$births), 
                       aes(x = date_b, y = number_of_births)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14, face="bold")) +
  scale_x_date("", labels = date_format("%b")) + 
  scale_y_continuous("Number of births (1969-1988)") + 
  annotate("text", x = birthdays[185], y = 175000, label = "Fourth of July", colour = "blue") + 
  annotate("text", x = birthdays[5], y = 162000, label = "New Year", colour = "blue") + 
  annotate("text", x = birthdays[359], y = 155000, label = "Xmas", colour = "blue") + 
  annotate("text", x = birthdays[259], y = 215000, label = "38 weeks after Xmas", colour = "blue") 


ggsave(file = paste0(homewd, "figs/births_US_text.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)


###### pop models

#look at the first few rows of the data
head(world_pop)



##Make a sub-vector called 'mada.pop' that is the row of your dataset which corresponds to Madagascar.
mada.pop <- subset(world_pop, country!="USA")$pop


## 5. Make a second sub-vector called 'usa.pop' that corresponds to USA.
usa.pop <-  subset(world_pop, country=="USA")$pop


## Now, we can calculate the growth rate, N_t+1/N_t, by taking a vector that goes from the
## 2nd until the last value in our population vectors (this is years 1961 until 2016, and 
## thus we call it N_t+1). We will divide this by the the vector that goes from the first 
## until the before-last column, which will correspond to 1960 to 2015. Relative to the 
## first vector, this is N_t - i.e., the first number in the N_t+1 vector is 1961; and the 
## first number in the N_t vector is 1960, and so on.  

## 7. First make a vector of Nt Mada
Nt.mada <- mada.pop[1:(length(mada.pop)-1)]
## 8. Next, make a vector of Nt+1 for Mada
Nt1.mada <- mada.pop[2:length(mada.pop)]

## 9. Make a vector of Nt for USA
Nt.usa <- usa.pop[1:(length(usa.pop)-1)]
## 10. Make a vector of Nt+1 for USA
Nt1.usa <- usa.pop[2:length(usa.pop)]

## 11. Now estimate population growth rate for Madagascar (what is the equation for lambda?)
pop.growth.mada <- Nt1.mada/Nt.mada
## 12. do the same for France
pop.growth.usa <- Nt1.usa/Nt.usa

## ??? Is the population growing or declining?
pop.growth.mada
pop.growth.usa
## growing! above 1.

## 13. Now plot (a) the population sizes for Madagascar and USA from 1960-2021,
## side-by-side with the (b) population growth rates
## 13.1 Set up plot window to hold two graphs
par(mfrow=c(1,2))
## 13.2 plot Mada pop data as a solid line
plot(x=seq(1960,2021,1), y=mada.pop,type="l", xlab="",  #plot mada pop data
     ylab="Population size (World Bank estimate)", 
     ylim=range(c(mada.pop,usa.pop),na.rm=TRUE))
## 13.3 add France data as dotted line
points(x=seq(1960,2021,1), y=usa.pop, type = "l", lty=2) #add france data
## 13.4 add legend
legend("topleft",legend=c("Madagascar","USA"), lty=1:2, bty="n") #add legend

## 13.5 plot Mada pop growth rate as solid line
plot(x=seq(1960,2020,1), y=pop.growth.mada,type="l", xlab="",  #plot mada pop growth rate
     ylab="N(t+1)/N(t)", 
     ylim=range(c(pop.growth.mada,pop.growth.usa),na.rm=TRUE))
## 13.6 add USA pop growth rate as a dotted line
points(x=seq(1960,2020,1), y=pop.growth.usa, type = "l", lty=2) #add USA


#plot in ggplot with above
world_pop$pop_rate_increase = c(NA, pop.growth.usa, pop.growth.mada, NA)

p4 <- ggplot(data=world_pop, 
             aes(x = year, y = pop_rate_increase, color=country)) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_x_continuous("Year") + 
  scale_y_continuous("population rate of increase") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "top",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))


ggsave(file = paste0(homewd, "figs/country_growth_rate.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)

cowplot::plot_grid(p3,p4, rel_widths = c(1.1,1))
ggsave(file = paste0(homewd, "figs/country_pop_growth_rate.png"),
       units="mm",  
       width=110, 
       height=45, 
       scale=2.5, 
       dpi=300)




## ?? The growth rates for Madagascar and USA never drop below 1 (although USA comes close). 
## What does this tell us? Which country's population is growing more quickly relative to its population size?

# The population is always increasing. sometimes it is increasing more slowly, sometimes more quickly. 
# Madagascar is increasing more quickly.


## TO DO: 
#### 1) Change the colour of the plots and the legend; 
#### 2) Experiment with plotting out different countries (Germany? Tanzania?)
#### 3) [maybe later] See if you can find which country in the entire data-base 
####    has the fastest growth, and when that occurred. You might want to use 
####    the 'which' command, with the argument 'arr.ind=TRUE' since this will 
####    index the matrix, i.e., tell you the row (which corresponds to the country) 
####    and the column (which corresponds to the year). Note that you will need 
####    to apply this to a matrix of growth rates!


### plot madagascar's pop not on log scale


# by default only plot the population
p5 <- ggplot(data=subset(world_pop, country=="Madagascar"), 
             aes(x = year, y = pop, color=country)) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_x_continuous("Year") + 
  scale_y_continuous("Population", labels = scales::comma) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "top",
        axis.title = element_text(size=18),
        axis.text = element_text(size=16))


ggsave(file = paste0(homewd, "figs/mada_pop.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)


mean(pop.growth.mada)#1.029


##############################################################################
## Investigating continuous vs discrete time 
##############################################################################

## Is the model above structured like a continuous or a discrete time model?

## Let's make a discrete time population model. We will use a for-loop to project a 
## population 100 time-steps, starting with population size of 10, and with a population 
## growth rate equal to 1.1

## Set up your (a) vector "N" of future population sizes, (b) fill in your initial population size,
## and (c) define "lambda", the population growth rate.
N <- rep(NA,100)
N[1] <- 10
lambda=1.02


## Now make a discrete time model using a for-loop to iterate this population 100 years
## into the future.

for(t in 2:100){
  N[t] <- lambda*N[t-1]
}


## 1. Does anyone remember the equation to shorten this discrete time model? 
## If we do this, do we get the same result?
Ndirect = N[1]*lambda^(0:99) 

## Plot the results together.
par(mfrow=c(1,1))
plot(x=seq(1,100,1), y=N, pch=15, col="blue", ylab="N", xlab="time")
points(x=seq(1,100,1), y=Ndirect, pch=19, col="red", cex=0.5)

## What if we want to make a continuous time model? How do we write that?
## First, write the differential equation dN(t)/dt = rN in a function
## called 'ngrow'

ngrow <- function(t,y,parms){
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(c(as.list(y),parms),{
    dNdt <- r*N
    list(c(dNdt))
  })
}


## To solve this, we're going to need to use the R library called "deSolve".
## We also need to define the starting variables and parameters: 

library(deSolve)              # Load libary to be used for numerical integration
Pop <- c(N=10)                # Define starting variables (P(0)=10, as above)
values <- c(r=log(lambda))    # Define parameters - use an approx of the lambda used in the discrete time ex above
time.out <- seq(0,100,by=0.1) # Set up time-steps for variables - here go for 100 years
ngrow(t=0,y=Pop,parms=values)  #try out the function for t=0

## The function outputs the time derivative of N at the time t. 
# This means that in order to get the (approximate) values of N at some time 
# delta.t in the future, we need to calculate the equivalent of N(t+delta.t)=N(t)+rN(t)*delta.t. 
# We can do this by hand: 

delta.t <- 0.1                    # Set a small value for delta.t (0.1 day)
pop.next <- Pop + unlist(ngrow(t=time,y=Pop,parms=values)) * delta.t
pop.next

## We could iterate this process, updating our values of Pop 
## and time with each iteration to get a discrete-time approximation 
## of the values of $P$ through time. However, differential equations describe 
## the rates of change. It turns out that using the discrete time approximation 
## of this process leads to rapid accumulation of error in our estimate of the state 
## variables through time, even if we set our value of delta.t to be very small. 
## Fortunately,  a number of algorithms have been developed to come up with better 
## (both more accurate and more computationally efficient) approximations to the values of the 
## state variables through time. One such is "lsoda" which we loaded above, and which we can run, 
## here storing the result in a variable called 'Ncontinuous': 


Ncontinuous <- lsoda(
  y = Pop,               # Initial conditions for population
  times = time.out,      # Timepoints for evaluation
  func = ngrow,          # Function to evaluate
  parms = values         # Vector of parameters
)

## We can check the first few lines of the output:
head(Ncontinuous)

## The data frame has 2 columns showing the values of time (labeled "time") 
## and the state variable ("N", which is population size) through time. Let's see what 
## has happened after 2 years, by printing it out:
subset(Ncontinuous,time==2)

## Now we can also plot the full time-series:  
Ncontinuous$type="continuous"

png(paste0(homewd, "figs/discrete_continuous.png"), width = 60, height = 45)

plot(Ncontinuous[,"time"],               # Time on the x axis
     Ncontinuous[,"N"],                  # Number people on the y axis
     xlab = "Time in years",     # Label the x axis
     ylab = "Pop size",         # Label the y axis
     main = " ",                # Plot title
     xlim = c(0,100),           # 
     type = "l",                # Use a line plot
     bty = "n")                 # Remove the box around the plot
points(1:100,N, pch=19,col="red",cex=0.2)  #compare with our discrete time
legend("topleft",legend=c("continuous","discrete"), lty=c(1,3), col=c("black", "red"), bty="n") #add legend

dev.off()
# Note that we overlaid our discrete time model predictions (the variable N) for comparison.
## This is close, but not an exact match to our discrete time version, which makes sense, 
## since these are different processes.  


## TO DO: 
#### 1) See what happens to a population where r<0
#### 2) Change the time resolution (set by "time.out"") 
####   and see if/where the results change


