
require(deSolve); # uses a differential equation solver package. You will need to install this
# under Tools find Install Packages and type deSolve in the box

# here we define the set of equations we want to solve
# In this case it is SIR  
SIR<-function(t,y,p){
	S = y[1];
	I = y[2];
	with(as.list(p), {
		dS.dt = -beta*S*I;
		dI.dt = beta*S*I - gamma*I;
		return(list(c(dS.dt,dI.dt)));
	})
}


################
#Parameter Values
################
beta = 2; 
gamma = 1/5;
p2 = c(beta=beta,gamma=gamma); # we put these in a vector

#########
#Initial Conditions for time series
########
S0Start = 0.3; # start point of Susceptibles as fraction of population
I0Start = 0.001;  # start point of Infecteds as a fraction of the population
y0 = c(S=S0Start,I=I0Start); # put these together as a x,y coordinate 

EndTime = 100;  # length of time to run
t2 = seq(from=0,to=EndTime,by=0.1); #time here is in days.

########
#Solving and plotting time series
########
par(mfrow=c(1,2)); # this tells it to plot the two graphs side-by-side
out = ode(y=y0,times=t2,func=SIR,parms=p2); # call the ODE solver
# it puts out S and I at each time step and  out[,1] is time 
plot(out[,1],out[,2],type="l",lwd=2,xlab="Time",ylab="Susceptible or Infected Hosts",ylim=c(0,S0Start));
lines(out[,1],out[,3]); # overlay the infected time series
legend(x="topright", legend=c("S","I"),bty="n",lwd=c(2,1));
title(main="your names") ###################### change to YOUR NAMES
#########
#The first phase plot lines -
#########
plot(out[,2],out[,3],type="l",xlab="Susceptible Hosts",ylab="Infected Hosts");
abline(v=(gamma/beta),lty=2)

### Stop the first run here and answer questions 2 & 3





##### Just above the threshold
SStart = (gamma/beta)*1.5;    # x value at start in relation to threshold
I0Start = 0.001           # y value at start
y0 = c(S=SStart,I=I0Start);  # make this  starting point
t2 = seq(from=0,to=EndTime,by=0.1); # run for chosen time interval
out = ode(y=y0,times=t2,func=SIR,parms=p2);  # solve the equations from this start point
 lines(out[,2],out[,3]);  # add the line to the figure


