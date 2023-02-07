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
## growth rate equal to 1.02

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
Ncontinuous <- data.frame(Ncontinuous)
Ncontinuous$type="continuous"

Ndiscrete <- cbind.data.frame(time=1:100, N=N, type="discrete")

Ncontinuous <- rbind(Ncontinuous, Ndiscrete)

colz=c('continuous'= "black", 'discrete'="red")
p6 <- ggplot(data=Ncontinuous)+
      geom_point(data=subset(Ncontinuous, type=="discrete"), aes(x=time, y=N, color=type), size=2) +
      geom_line(data=subset(Ncontinuous, type!="discrete"), aes(x=time, y=N, color=type), size=1) +
      theme_bw() + 
      scale_color_manual(values=colz) +
      theme(panel.grid = element_blank(), 
        legend.position = c(.2,.8),
        legend.title = element_blank(),
        axis.title = element_text(size=18),
        legend.text = element_text(size=12),
        axis.text = element_text(size=16))

ggsave(file = paste0(homewd, "figs/discrete_continuous.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)




################## logistic growth

library(deSolve)
library(reshape2)


logistic_growth <- function(time, state, params){
    with(as.list(c(state, params)), {
      dXdt <- r * X  * (1 - X / K)
      return(list(c(dXdt)))
    })
}

  pars <- c(r = 0.25, K = 5)
  xini <- c(X = 0.5)
  times <- seq(0, 100, by = 0.05)
  out <- as.data.frame(ode(xini, times, logistic_growth, pars))
  out$K = 5
  out$label="K=5"
  
  pars["K"] <- 10
  out2 <- as.data.frame(ode(xini, times, logistic_growth, pars))
  out2$K = 10
  out2$label="K=10"
  
  out<- rbind(out, out2)
  head(out)
  m <- melt(out, id.vars = c("time", "K", "label"))
  m$K<- as.character(m$K)
  head(m)
  
  pl_logistic <- ggplot(m) + 
    geom_line(aes(x = time, y = value, colour = K), size = 2, alpha = 0.7) + 
    scale_x_continuous("Time t") + 
    scale_y_continuous("X(t)", limits = c(0, NA)) +
    geom_hline(yintercept = 5, linetype = 2, colour = "black") + 
    geom_hline(yintercept = 10, linetype = 2, colour = "black") + 
    theme_bw() + 
    theme(axis.text=element_text(size=14, face="bold"), 
          axis.title=element_text(size=14, face="bold"),
          legend.position = c(.8,.2)) 
    
  show(pl_logistic)
  



  ################ 
  
  #estimating parameters from data  

  head(world_pop)
  

comp.log.growth<- function(par, xini, times, data, K){
  
  #simulate logistic growth with the guess parameters
  pars = c(par, K)
  
  out <- as.data.frame(ode(xini, times, logistic_growth, pars))
  
  #and compare with data
  out$data <- data$pop
  out$sq_diff <- ((out$data-out$X)^2)
  
  sm.sq = sum(out$sq_diff)
  return(sm.sq)
  
}  
fit.diff.subs <- function(data, pars, times, min.fit, max.fit){
  
  
  dat = subset(data, year >=min.fit & year<=max.fit)
  time.comp = dat$year
  xini1=dat$pop[dat$year==min(dat$year)]
  names(xini1) <- "X"
  
  fit <- optim(par=pars["r"], fn=comp.log.growth, xini=xini1, times=time.comp, data=dat, K=pars["K"],
        method = "Nelder-Mead")
  
  #and return dataset of pars and fit range
  fit.dat <- cbind.data.frame(r=fit$par["r"], K=pars["K"], min_fit=min.fit, max_fit =max.fit)

  return(fit.dat)  
}
  
  
  
#fit to 3 different subsets of years 
#(1960:1970)
#(1960:1990)
#(1960:2018)


join.all.fit <- function(test.dat, pars, year1, year2, year3){
  
  fit1 <- fit.diff.subs(data=test.dat, pars = pars, times=test.dat$year, min.fit =  min(test.dat$year), max.fit = year1)
  fit2 <- fit.diff.subs(data=test.dat, pars = pars, times=test.dat$year, min.fit =  min(test.dat$year), max.fit = year2)
  fit3 <- fit.diff.subs(data=test.dat, pars = pars, times=test.dat$year, min.fit =  min(test.dat$year), max.fit = year3)
  
  all.fit <- rbind(fit1, fit2, fit3)
  
  return(all.fit)
}

fit.pars = join.all.fit(test.dat=subset(world_pop, country=="USA"),
                        pars = list(r=0.03, K=320000000*2),
                        year1=1970,
                        year2=1990,
                        year3=2018)


#and sim and plot
sim.plot <- function(fit.pars, test.dat, plotK){
  
  if(nrow(fit.pars)==3){
    
    
    
    xini <- c(X = min(test.dat$pop))
    times <- test.dat$year
    out1 <- as.data.frame(ode(xini, times, logistic_growth, fit.pars[1,1:2]))
    out2 <- as.data.frame(ode(xini, times, logistic_growth, fit.pars[2,1:2]))
    out3 <- as.data.frame(ode(xini, times, logistic_growth, fit.pars[3,1:2]))
    
    out1$fit <- paste0("1960:", fit.pars[1,4], "; r=", round(fit.pars[1,1],3))
    out2$fit <- paste0("1960:", fit.pars[2,4], "; r=", round(fit.pars[2,1],3))
    out3$fit <- paste0("1960:", fit.pars[3,4], "; r=", round(fit.pars[2,1],3))
    
    
    fit.dat <- rbind(out1,out2,out3)
    
  }else{
    xini <- c(X = min(test.dat$pop))
    times <- test.dat$year
    fit.dat <- as.data.frame(ode(xini, times, logistic_growth, fit.pars[1,1:2]))
    fit.dat$fit <- paste0("1960:", fit.pars[1,4], "; r=", round(fit.pars[1,1],3))
    
  }
  
  p <- ggplot(data = test.dat) + 
    geom_point(aes(x = year, y = pop), size=3) + 
    #geom_line(aes(x = year, y = pop), size=1)  + 
    geom_line(data=fit.dat, aes(x = time, y = X,color=fit), size=1)  + 
    theme_bw() + 
    scale_y_continuous(labels = scales::comma, limits = range(test.dat$pop))+
    scale_color_discrete("date range for fitting") +
    theme(axis.text=element_text(size=14, face="bold"), 
          axis.title=element_text(size=14, face="bold"),
          legend.position = c(.25,.8),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    ylab(paste0(unique(test.dat$country), " population"))
  
  if(plotK==T){
    p <- ggplot(data = test.dat) + 
      geom_point(aes(x = year, y = pop), size=3) + 
      #geom_line(aes(x = year, y = pop), size=1)  + 
      geom_line(data=fit.dat, aes(x = time, y = X,color=fit), size=1)  + 
      geom_hline(yintercept = unique(fit.pars$K), linetype=2) +
      theme_bw() + 
      scale_y_continuous(labels = scales::comma, limits = range(test.dat$pop))+
      scale_color_discrete("date range for fitting") +
      theme(axis.text=element_text(size=14, face="bold"), 
            axis.title=element_text(size=14, face="bold"),
            legend.position = c(.25,.8),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12)) +
      ylab(paste0(unique(test.dat$country), " population"))
    
  }
  
  return(p)
  
}

sim.plot(fit.pars = fit.pars, plotK = F,
         test.dat=subset(world_pop, country=="USA"))

ggsave(file = paste0(homewd, "figs/exponential_fit_USA.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)


fit.pars2 = join.all.fit(test.dat=subset(world_pop, country=="Madagascar"),
                        pars = list(r=0.03, K=320000000*2),
                        year1=1970,
                        year2=1990,
                        year3=2018)

sim.plot(fit.pars = fit.pars2, plotK = F,
         test.dat=subset(world_pop, country=="Madagascar"))

ggsave(file = paste0(homewd, "figs/exponential_fit_Madagascar.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)

#and with K

#fit to 3 different subsets of years 
#(1960:1970)
#(1960:1990)
#(1960:2018)



fit.pars = join.all.fit(test.dat=subset(world_pop, country=="USA"),
                        pars = list(r=0.03, K=320000000),
                        year1=1970,
                        year2=1990,
                        year3=2018)


#and sim and plot


sim.plot(fit.pars = subset(fit.pars, max_fit==1990),plotK = T,
         test.dat=subset(world_pop, country=="USA"))

ggsave(file = paste0(homewd, "figs/logistic_fit_USA.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)
  



fit.pars2 = join.all.fit(test.dat=subset(world_pop, country=="Madagascar"),
                         pars = list(r=0.03, K=28000000),
                         year1=1970,
                         year2=1990,
                         year3=2018)



sim.plot(fit.pars = subset(fit.pars2, max_fit==1990),plotK = T,
         test.dat=subset(world_pop, country=="Madagascar"))

ggsave(file = paste0(homewd, "figs/logistic_fit_Madagascar.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)    



###############
#############
# carrying capacity
Xs <- seq(0, 1.15 * 5, length.out = 100)
growth_rate_logistic <- data.frame(X = Xs, dXdt = 0.25 * Xs * (1 - Xs / 5))

eq_logistic <- ggplot(growth_rate_logistic, 
                      aes(x = X, y = dXdt)) + 
  geom_line(size = 2, alpha = 0.5, colour = "black") +
  scale_x_continuous("N(t)") + 
  scale_y_continuous("dN(t)/dt") +
  geom_point(x = 0, y = 0, shape = 1, size = 4) +
  geom_point(x = 5, y = 0, shape = 1, size = 4) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14, face="bold")) +
  theme(legend.position = "none")
show(eq_logistic)


ggsave(file = paste0(homewd, "figs/dNdt_logistic.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)    



#and for harvesting
library(latex2exp)

harvest_plot  <- ggplot(growth_rate_logistic, 
                      aes(x = X, y = dXdt)) + 
  geom_line(size = 2, alpha = 0.5, colour = "black") +
  scale_x_continuous("N(t), Population size at time t") + 
  scale_y_continuous("H(t), Harvesting rate at time t") +
  geom_point(x = 0, y = 0, shape = 16, size = 4) +
  geom_point(x = 1, y = .2, shape = 1, size = 4) +
  geom_point(x = 5, y = 0, shape = 16, size = 4) +
  geom_point(x = 4, y = .2, shape = 16, size = 4) +
  #geom_point(x = 5/2, y = max(growth_rate_logistic$dXdt), shape = 1, size = 4) +
  geom_label(x = 5, y = -.04, label="K=5", size=4, label.size=0) +
  #geom_label(x = 5/2, y = (max(growth_rate_logistic$dXdt)-.05), label=TeX(r"( $N_{MSY} = \frac{K}{2}$ )", output = "character"), parse = T, size=4, label.size=0) +
  #geom_hline(yintercept = max(growth_rate_logistic$dXdt), linetype=1, color="blue") +
  geom_hline(yintercept = 0.2, linetype=1, color="blue") +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14, face="bold")) +
  theme(legend.position = "none")
show(harvest_plot)


ggsave(file = paste0(homewd, "figs/harvest.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)    



harvest_plot_MSY  <- ggplot(growth_rate_logistic, 
                        aes(x = X, y = dXdt)) + 
  geom_line(size = 2, alpha = 0.5, colour = "black") +
  scale_x_continuous("N(t), Population size at time t") + 
  scale_y_continuous("H(t), Harvesting rate at time t") +
  geom_point(x = 0, y = 0, shape = 16, size = 4) +
  #geom_point(x = 1, y = .2, shape = 1, size = 4) +
  geom_point(x = 5, y = 0, shape = 16, size = 4) +
  #geom_point(x = 4, y = .2, shape = 16, size = 4) +
  geom_point(x = 5/2, y = max(growth_rate_logistic$dXdt), shape = 1, size = 4) +
  geom_label(x = 5, y = -.04, label="K=5", size=4, label.size=0) +
  geom_label(x = 5/2, y = (max(growth_rate_logistic$dXdt)-.05), label=TeX(r"( $N_{MSY} = \frac{K}{2}$ )", output = "character"), parse = T, size=4, label.size=0) +
  geom_hline(yintercept = max(growth_rate_logistic$dXdt), linetype=1, color="blue") +
  #geom_hline(yintercept = 0.2, linetype=1, color="blue") +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  theme(axis.text=element_text(size=14, face="bold"), axis.title=element_text(size=14, face="bold")) +
  theme(legend.position = "none")
show(harvest_plot_MSY)

ggsave(file = paste0(homewd, "figs/harvest_MSY.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)    



###########
###########
#halley

halley <- read.csv(paste0(homewd,"halley.csv"), header = T, stringsAsFactors = F)
pl_halley <- ggplot(data = halley, aes(x = Age, y = Number)) + geom_line() + 
  theme_bw() + 
  theme(axis.text = element_text(size = 14, face = "bold"), 
        axis.title = element_text(size=14, face="bold")) +
  theme(legend.position = "none")
show(pl_halley)


#pyramid

pl_halley_bar <- ggplot(data = halley, aes(x = Age, y = Number)) + geom_bar(stat = "identity") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 14, face = "bold"), 
        axis.title = element_text(size=14, face="bold")) +
  theme(legend.position = "none") + 
  coord_flip()
show(pl_halley_bar)

ggsave(file = paste0(homewd, "figs/halley_pyramid.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2.5, 
       dpi=300)   
