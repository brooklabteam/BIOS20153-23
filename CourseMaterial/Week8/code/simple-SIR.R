rm(list=ls())

library(deSolve)
library(ggplot2)
library(reshape2)


#load bat model functions
homewd="/Users/carabrook/Developer/BIOS20153-23"

source(paste0(homewd, "/CourseMaterial/Week8/code/BatModelFunctions.R"))

load(paste0(homewd,"/CourseMaterial/Week8/code/bat.mod.par.Rdata"))

par.dat$sigma = 0
par.dat$wane = 0.0769 #6 month maternal immunity

out <- sim.model.TS(par=par.dat, model = "MSIRN")
head(out)
out$duration <- "6 months"




colz = c('M'="purple", 'S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=out) + geom_line(aes(x=time, y=proportion, color=class),show.legend = F, size=1) + 
  scale_color_manual(values=colz) + ylab('proportion by epidemic state') + theme_bw() + 
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) 
print(p1)


par.dat$wane = .15 #3 month maternal immunity
out2 <- sim.model.TS(par=par.dat, model = "MSIRN")
head(out2)
out2$duration <- "3 months"

out <- rbind(out, out2)

colz = c('6 months' = "gray28", '3 months' = "tomato")
p2 <- ggplot(data=subset(out, class=="I")) + 
  scale_color_manual(values=colz, name = "duration maternal\nimmunity") +
  geom_line(aes(x=time, y=proportion, color=duration), size=.8) + 
  ylab('proportion infected') + theme_bw() + xlab("years") + coord_cartesian(ylim=c(0,.2)) +
  theme(panel.grid = element_blank(), 
        legend.position = c(.8,.8),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=14)) 


ggsave(file = paste0(homewd, "/CourseMaterial/Week8/code/figs/maternal_immunity.pdf"),
       units="mm",  
       width=50, 
       height=40, 
       scale=3, 
       dpi=300)



print(p2)


#and try wit shorter duration maternal immunity

simple.MSIR <- function(t,x,parms){
  
  M = x[1]
  S = x[2]
  I = x[3]
  R = x[4]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(as.list(parms),{
    
    dMdt = parms$b*(I+R) - parms$omega*M - parms$mu*M
    dSdt <- parms$b*S -parms$beta*S*I-parms$mu*S
    dIdt <- parms$beta*S*I - parms$mu*I - parms$sigma*I
    dRdt <- parms$sigma*I - parms$mu*R
    
    list(c(dMdt, dSdt,dIdt,dRdt))
  })
}


params.b = list(omega= 1/10,
                b = .02,#births, per capita per day
                beta = .002,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c(M=80, S = 19, I = 1, R = 0)

times = seq(1, 365*3, 1) #10 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.MSIR, parms = params.b))

head(out)
out.long <- melt(out, id="time")
names(out.long) <- c("time", "state", "count")


out.long$state = factor(out.long$state, levels = c("M", "S", "I", "R"))
out.long$duration="30 days"


colz = c('M'="purple", 'S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=out.long) + geom_line(aes(x=time, y=count, color=state),show.legend = F, size=1) + 
  scale_color_manual(values=colz) + ylab('counts by epidemic state') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) 
print(p1)


cowplot::plot_grid(p1, p2, nrow=2)

#and longer


params.b = list(omega= 1/90,
                b= .02,#births, per capita per day
                beta = .002,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



out = as.data.frame(lsoda(y = xstart, times = times, func = simple.MSIR, parms = params.b))

head(out)
out.long2 <- melt(out, id="time")
names(out.long2) <- c("time", "state", "count")


out.long2$state = factor(out.long2$state, levels = c("M", "S", "I", "R"))
out.long2$duration="90 days"

out.long <- rbind(out.long, out.long2)
head(out.long)
unique(out.long$duration)

max(out.long$time)

p2 <- ggplot(data=subset(out.long, state=="I")) + 
  geom_line(aes(x=time, y=count, color=duration),show.legend = F, size=.8) + 
  ylab('counts by epidemic state') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) 
  
  
  
print(p2)





#######
#from before

simple.SIR <- function(t,x,parms){
  
  S = x[1]
  I = x[2]
  R = x[3]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(as.list(parms),{
    
    dSdt <- parms$b*(S+I+R)-parms$beta*S*I-parms$mu*S
    dIdt <- parms$beta*S*I - parms$mu*I - parms$sigma*I
    dRdt <- parms$sigma*I - parms$mu*R
    
    list(c(dSdt,dIdt,dRdt))
  })
}
noise <- function(param){ # no noise to zero params but shakes up 
  if(param == 0){             #the others (both fecundity and survivorship) quite a bit
    return(param)
  }else{
    return(rnorm(1, mean=param, sd = 2))
  }
} # no

params.b = list(b= .01,#births, per capita per day
                beta = .002,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR, parms = params.b))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))

#and include data
dat =c(5,18,25,39,57,75,95,120, 135, 156, 170, 195, 215, 235, 249, 265,  296, 309, 321, 333, 342, 357)
dat = cbind.data.frame(dat, rep(NA, length(dat)))
names(dat) <- c("time", "I")
for(i in 1:length(dat$time)){
  dat$I[i] = noise(out.long$density[out.long$state=="I" & out.long$time==dat$time[i]])
}

dat.R =c(5,45,95,135,185, 235, 275, 325)
dat.R = cbind.data.frame(dat.R, rep(NA, length(dat.R)))
names(dat.R) <- c("time", "R")
noise <- function(param){ # no noise to zero params but shakes up 
  if(param == 0){             #the others (both fecundity and survivorship) quite a bit
    return(param)
  }else{
    return(rnorm(1, mean=param, sd = 8))
  }
} # no

for(i in 1:length(dat.R$time)){
  dat.R$R[i] = noise(out.long$density[out.long$state=="R" & out.long$time==dat.R$time[i]])
}



colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=out.long) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('counts by epidemic state') + theme_bw() + xlab("days") +# ylab('proportion')
      theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100)) #
print(p1)

ggsave(file = "Fig1_simple_birth_death_SIR_count.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)

#add data - for infecteds
p2 <- ggplot(data=subset(out.long, state=="I")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1)) +
  geom_point(data=dat, aes(x=time, y=I), size=3) + coord_cartesian(ylim = c(0,100))
print(p2)

#or 
p2 <- ggplot(data=subset(out.long, state=="I"))  + scale_color_manual(values=colz) + ylab('infectious case counts') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  geom_point(data=dat, aes(x=time, y=I), size=5, color="black") + geom_line(aes(x=time, y=density), color="firebrick", show.legend = F, size=2) + coord_cartesian(ylim = c(0,100))
print(p2)

ggsave(file = "Fig2_SIR_Infectious_cases_counts.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)

#and just the model
p2 <- ggplot(data=subset(out.long, state=="I")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('infected cases') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) +# scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1)) +
   coord_cartesian(ylim = c(0,100))
print(p2)

ggsave(file = "FigInfecteds_ModelOnly.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)


#just data for infecteds
p2b <- ggplot(data=subset(out.long, state=="I")) +  ylab('infectious case counts') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,20,40,60,80,100)) +
  geom_point(data=dat, aes(x=time, y=I), size=5, color="black") + geom_point(data=dat, aes(x=time, y=I), size=3, color="red") + coord_cartesian(ylim = c(0,100))
print(p2b)

ggsave(file = "Fig2b_SIR_Infectious_case_counts.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)

#and the seroprevalence
p3 <- ggplot(data=subset(out.long, state=="R")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1)) +
  geom_point(data=dat.R, aes(x=time, y=R), size=3) + coord_cartesian(ylim = c(0,100))
print(p3)

#or
#and the seroprevalence
p3 <- ggplot(data=subset(out.long, state=="R")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('number seropositive') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  geom_point(data=dat.R, aes(x=time, y=R), size=3) + coord_cartesian(ylim = c(0,100))
print(p3)


ggsave(file = "Fig3_SIR_Seroprev_cases_counts.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)


#and age-seroprevalence
rm(list=ls())
end.SIR <- function(t,x,parms){
  
  S = x[1]
  I = x[2]
  R = x[3]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(as.list(parms),{
    
    dSdt <- -parms$beta*S*I
    dIdt <- parms$beta*S*I- parms$sigma*I
    dRdt <- parms$sigma*I 
    
    list(c(dSdt,dIdt,dRdt))
  })
}
noise <- function(param){ # no noise to zero params but shakes up 
  if(param == 0){             #the others (both fecundity and survivorship) quite a bit
    return(param)
  }else{
    return(rnorm(1, mean=param, sd = 10))
  }
} # no
params.end = list(beta = .0025,#per capita infectious contacts per day for an infected individual
                sigma = 1/14 #recovery rate, 1/duration of infection (in days). here 1/14, like measles
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 200, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = end.SIR, parms = params.end))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")
out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))
dat =c(5,18,25,34,39,45,57,67,75,86,95,120, 135,198)
dat = cbind.data.frame(dat, rep(NA, length(dat)))
names(dat) <- c("time", "R")
for(i in 1:length(dat$time)){
  dat$R[i] = noise(out.long$density[out.long$state=="R" & out.long$time==dat$time[i]])
}

dat$R[dat$R>100] = 100
colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p4 <- ggplot(data=subset(out.long, state == "R")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("age") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1)) + 
  scale_x_continuous(breaks=c(50, 100, 150, 200), labels=c(5,10,15, 20)) + geom_point(data=dat, aes(x=time, y=R), size=3) 
print(p4)

ggsave(file = "Fig4_SIR_age_seroprev.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)

#SIR with birth pulse
simple.SIR.pulse <- function(t,x,parms){
  

  S = x[1]
  I = x[2]
  R = x[3]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(as.list(parms),{
    
    if ((t >= 1 & t <= 30) |(t >= 1*2 & t <= 30*2)| (t >= 1*3 & t <= 30*3)| (t >= 1*4 & t <= 30*4)| (t >= 1*5 & t <= 30*5)) {
      B = parms$b
    }
    else {
      B = 0
    }
    
    

    dSdt <-  B*(S+I+R)-parms$beta*S*I-parms$mu*S
    dIdt <- parms$beta*S*I - parms$mu*I - parms$sigma*I
    dRdt <- parms$sigma*I - parms$mu*R
    
    list(c(dSdt,dIdt,dRdt))
  })
}

params.pulse = list(b= .1,#births, per capita per day
                beta = .02,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c( S = 99, I = 1, R = 0)

times = seq(1, 365*5, 1) #5 years in days

out.MSIR = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR.pulse, parms = params.pulse))

out.S =  cbind.data.frame(out.MSIR[,1], out.MSIR[,2])
out.S$state = "S"
out.I = cbind.data.frame(out.MSIR[,1], out.MSIR[,3])
out.I$state = "I"
out.R = cbind.data.frame(out.MSIR[,1], out.MSIR[,4])
out.R$state = "R"
 names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind( out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))

colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p5 <- ggplot(data=out.long) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1))
print(p5)

#and SIRS
#MSIR with birth pulse
simple.MSIR <- function(t,x,parms){
  
  M = x[1]
  S = x[2]
  I = x[3]
  R = x[4]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
   with(as.list(parms),{
  #   
  #   if ((t >= 1 & t <= 30) |(t >= 1*2 & t <= 30*2)| (t >= 1*3 & t <= 30*3)| (t >= 1*4 & t <= 30*4)| (t >= 1*5 & t <= 30*5)) {
  #     B = parms$b
  #   }
  #   else {
  #     B = 0
  #   }
    
    
    dMdt <- sin(parms$b)*(R+I) - parms$wane*M - parms$mu*M
    dSdt <- parms$wane*M + sin(parms$b)*(S)-parms$beta*S*I-parms$mu*S
    dIdt <- parms$beta*S*I - parms$mu*I - parms$sigma*I
    dRdt <- parms$sigma*I - parms$mu*R
    
    list(c(dMdt, dSdt,dIdt,dRdt))
  })
}

params.M = list(wane = 1/(31), #rate of waning maternal immunity (1/5 months)
                b= .01,#births, per capita per day
                beta = .01,#per capita infectious contacts per day for an infected individual
                sigma = 1/28, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c(M= 0, S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #5 years in days

out.MSIR = as.data.frame(lsoda(y = xstart, times = times, func = simple.MSIR, parms = params.M))

out.M = out.MSIR[,1:2]
out.M$state = "M"
out.S =  cbind.data.frame(out.MSIR[,1], out.MSIR[,3])
out.S$state = "S"
out.I = cbind.data.frame(out.MSIR[,1], out.MSIR[,4])
out.I$state = "I"
out.R = cbind.data.frame(out.MSIR[,1], out.MSIR[,5])
out.R$state = "R"
names(out.M) <- names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.M, out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("M", "S", "I", "R"))

colz = c('M' = "cornflowerblue", 'S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p5 <- ggplot(data=out.long) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("days") +
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,20,40,60,80,100), labels =c(0,.2,.4,.6,.8,1))
print(p5)


#and with no births

params.b = list(b= 0,#births, per capita per day
                beta = .002,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR, parms = params.b))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))



colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p6 <- ggplot(data=subset(out.long, state=="I")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('infected cases') + theme_bw() + xlab("days") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + scale_y_continuous(breaks = c(0,3,6,9), labels=c(0,50,100,150)) #
print(p6)


ggsave(file = "Fig_Infected_Cases.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)



#lower R0

params.b = list(b= 0,#births, per capita per day
                beta = .001,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01 #natural death rate, deaths per capita, per day)
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR, parms = params.b))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))

head(out.long)
# out.long$time = out.long$time+1
# 
# # out.long = subset(out.long, state=="I")
#  out.long = rbind(c(0,.5,"I"), out.long)
#  out.long$density = as.numeric(out.long$density)
#  out.long$time = as.numeric(out.long$time)
 colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p6 <- ggplot(data=subset(out.long, state=="I")) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('infected cases') + theme_bw() + xlab("days") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) + coord_cartesian(ylim=c(0,50)) + scale_y_continuous(breaks = c(0,15,30,45), labels=c(0,50,100,150)) #+ ylim(0,2)#
print(p6)


ggsave(file = "Fig_Infected_Cases_Few.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)



#vaccination

rm(list=ls())

simple.SIR.vacc <- function(t,x,parms){
  
  S = x[1]
  I = x[2]
  R = x[3]
  
  
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(as.list(parms),{
    
    dSdt <- parms$b*(S+I+R)-parms$p*S - parms$beta*S*I-parms$mu*S
    dIdt <- parms$beta*S*I - parms$mu*I - parms$sigma*I
    dRdt <- parms$sigma*I - parms$mu*R + parms$p*S 
    
    list(c(dSdt,dIdt,dRdt))
  })
}
params.b = list(b= .01,#births, per capita per day
                beta = .004,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = .01, #natural death rate, deaths per capita, per day)
                p = 1/5 #1/ duration of time before vaccinated
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR.vacc, parms = params.b))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))

colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p6 <- ggplot(data=out.long) + geom_line(aes(x=time, y=density, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('total population') + theme_bw() + xlab("days") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), axis.text = element_text(size=14)) #+ coord_cartesian(ylim=c(0,50)) + scale_y_continuous(breaks = c(0,15,30,45), labels=c(0,50,100,150)) #+ ylim(0,2)#
print(p6)


ggsave(file = "Fig_Recovery.pdf",
       units="mm",  
       width=40, 
       height=30, 
       scale=3, 
       dpi=300)





#and with no births


params.b = list(b= 0,#births, per capita per day
                beta = .002,#per capita infectious contacts per day for an infected individual
                sigma = 1/14, #recovery rate, 1/duration of infection (in days). here 1/14, like measles
                mu = 0 #natural death rate, deaths per capita, per day)
)



xstart = c(S = 99, I = 1, R = 0)

times = seq(1, 365*1, 1) #1 years in days

out = as.data.frame(lsoda(y = xstart, times = times, func = simple.SIR, parms = params.b))

out.S = out[,1:2]
out.S$state = "S"
out.I = cbind.data.frame(out[,1], out[,3])
out.I$state = "I"
out.R = cbind.data.frame(out[,1], out[,4])
out.R$state = "R"
names(out.I) <- names(out.S) <-  names(out.R) <- c("time", "density", "state")


out.long = rbind(out.S, out.I, out.R)
out.long = out.long[complete.cases(out.long),]

out.long$state = factor(out.long$state, levels = c("S", "I", "R"))

head(out.long)

out.long$proportion <- out.long$density/100

colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=out.long) + geom_line(aes(x=time, y=proportion, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("time") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), 
        axis.text = element_text(size=14), strip.background = element_rect(fill="white"),
        strip.text = element_text(size=18)) + facet_grid(~state) 
  
print(p1)


#and save


ggsave(file = "simpleSIRnobirthsnodeaths.pdf",
       units="mm",  
       width=90, 
       height=50, 
       #limitsize = F,
       scale=3)#, 



colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=subset(out.long, state=="S")) + geom_line(aes(x=time, y=proportion, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("time") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), 
        axis.text = element_text(size=14), strip.background = element_blank(),
        strip.text = element_blank()) + facet_grid(~state) + coord_cartesian(ylim=c(0,1))

print(p1)


#and save


ggsave(file = "simpleSIR_S.pdf",
       units="mm",  
       width=40, 
       height=50, 
       #limitsize = F,
       scale=2)#, 


colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=subset(out.long, state=="I")) + geom_line(aes(x=time, y=proportion, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("time") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), 
        axis.text = element_text(size=14), strip.background = element_blank(),
        strip.text = element_blank()) + facet_grid(~state)  + coord_cartesian(ylim=c(0,1))

print(p1)


#and save


ggsave(file = "simpleSIR_I.pdf",
       units="mm",  
       width=40, 
       height=50, 
       #limitsize = F,
       scale=2)#, 

colz = c('S' = "green", 'I' = "tomato", 'R'="cornflowerblue")
p1 <- ggplot(data=subset(out.long, state=="R")) + geom_line(aes(x=time, y=proportion, color=state),show.legend = F, size=1) + scale_color_manual(values=colz) + ylab('proportion') + theme_bw() + xlab("time") +# ylab('proportion')
  theme(panel.grid = element_blank(), axis.title = element_text(size=18), 
        axis.text = element_text(size=14), strip.background = element_blank(),
        strip.text = element_blank()) + facet_grid(~state) + coord_cartesian(ylim=c(0,1))

print(p1)


#and save


ggsave(file = "simpleSIR_R.pdf",
       units="mm",  
       width=40, 
       height=50, 
       #limitsize = F,
       scale=2)#, 





