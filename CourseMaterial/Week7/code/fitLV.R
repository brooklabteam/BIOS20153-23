require("deSolve")
require("sfsmisc")

##################################################################################
##################################################################################
##################################################################################
PredPrey=function(t
                  ,x
                  ,vparameters
){
  Prey     = x[1]
  Predator = x[2]
  Prey[Prey<0] = 0
  Predator[Predator<0] = 0
  
  with(as.list(vparameters),{
    dPrey     = a*Prey - b*Prey*Predator
    dPredator = e*b*Prey*Predator - c*Predator
    out = c(dPrey,dPredator)
    list(out)
  })
}

##################################################################################
##################################################################################
# Canada Lynx Hare data from
# http://www-rohan.sdsu.edu/~jmahaffy/courses/f00/math122/labs/labj/q3v1.htm
##################################################################################
homewd = paste0(getwd(), "/CourseMaterial/Week7/code/")
setwd(homewd)

hdat <- read.csv("hudson-bay-lynx-hare.csv", header = T, stringsAsFactors = F)
head(hdat)
names(hdat) <- c("year", "hare", "lynx")

vt=hdat$year
Prey = mean(hdat$hare)
Predator = mean(hdat$lynx)

##################################################################################
# now set up a loop over a range of model parameters, and calculate
# the Pearson chi-squared statistic comparing the data to the model prediction
# for the parameter hypotheses
##################################################################################
aminchi = 1e10
wchi = numeric(0)
wa = numeric(0)
wb = numeric(0)
wc = numeric(0)
we = numeric(0)
wprey = numeric(0)
wpredator = numeric(0)

nmc = 10000
for (iter in 1:nmc){
  prey_0 = runif(1,1000,2000000)
  predator_0 = runif(1,1000,100000)
  a = runif(1,0.20,1.40)
  b = runif(1,0.25e-5,1e-4)
  c = runif(1,0.01,1.5)
  e = runif(1,0.01,1.00)
  
  Prey = prey_0
  Predator = predator_0
  vparameters=c(a=a,b=b,c=c,e=e)
  inits=c(Prey=Prey,Predator=Predator)
  lotka = as.data.frame(lsoda(inits, vt, PredPrey, vparameters))
  
  lotka$Prey[lotka$Prey<=0] = 1e-9
  lotka$Predator[lotka$Predator<=0] = 1e-9
  ascale = sum(hdat$hare)/sum(lotka$Prey)
  bscale = sum(hdat$lynx)/sum(lotka$Predator)
  achi = sum((hdat$hare-lotka$Prey*ascale)^2/(lotka$Prey*ascale))
  bchi = sum((hdat$lynx-lotka$Predator*bscale)^2/(lotka$Predator*bscale))
  
  if ((achi+bchi)<aminchi){
    lotka_best = lotka
    ascale_best = ascale
    bscale_best = bscale
    aminchi = achi+bchi
  }
  if (iter%%100==0){
    amax = min(wchi)+2000
    par(mfrow=c(3,3))
    mult.fig(9)
    plot(hdat$year,hdat$hare,col=1,main="Hare",type="l",xlab="Year",ylab="\043 pelts")
    lines(hdat$year,lotka_best$Prey*ascale_best,col=2)
    
    plot(hdat$year,hdat$lynx,type="l",main="Lynx",xlab="Year",ylab="\043 pelts")
    lines(hdat$year,lotka_best$Predator*bscale_best,col=2)
    
    if (length(wprey[wchi<amax])>1){
      plot(wprey[wchi<amax],wchi[wchi<amax],xlab="prey_0",ylab="Pearson chi-squared")
      plot(wpredator[wchi<amax],wchi[wchi<amax],xlab="predator_0",ylab="Pearson chi-squared")
      plot(wa[wchi<amax],wchi[wchi<amax],xlab="a",ylab="Pearson chi-squared")
      plot(wb[wchi<amax],wchi[wchi<amax],xlab="b",ylab="Pearson chi-squared")
      plot(wc[wchi<amax],wchi[wchi<amax],xlab="c",ylab="Pearson chi-squared")
      plot(we[wchi<amax],wchi[wchi<amax],xlab="e",ylab="Pearson chi-squared")
    }
  }
  wa = append(wa,a)
  wb = append(wb,b)
  wc = append(wc,c)
  we = append(we,e)
  wprey = append(wprey,Prey)
  wpredator = append(wpredator,Predator)
  wchi = append(wchi,(achi+bchi))
  cat("doing iteration",iter,"out of",nmc,"  ",a,b,c,e,achi+bchi,"\n")
}

bdat=data.frame(seq(1,length(wprey)),wprey,wpredator,wa,wb,wc,we,wchi)
write.table(bdat,"hare_lynx_step1.out",row.names=F,col.names=F)

##################################################################################
# scale the Pearson chi-squared statistic to account for over-dispersion
##################################################################################
nparam = 6
wchib = wchi*(2*nrow(hdat)-nparam)/min(wchi)
wprob = dnorm(sqrt(wchib-min(wchib)))

##################################################################################
# now make a final plot
##################################################################################

Prey = wprey[which.min(wchi)]
Predator = wpredator[which.min(wchi)]
a = wa[which.min(wchi)]
b = wb[which.min(wchi)]
c = wc[which.min(wchi)]
e = we[which.min(wchi)]
vparameters=c(a=a,b=b,c=c,e=e)
inits=c(Prey=Prey,Predator=Predator)
lotka = as.data.frame(lsoda(inits, vt, PredPrey, vparameters))
lotka$Prey[lotka$Prey<=0] = 1e-9
lotka$Predator[lotka$Predator<=0] = 1e-9
ascale = sum(hdat$hare)/sum(lotka$Prey)
bscale = sum(hdat$lynx)/sum(lotka$Predator)
mult.fig(9)
plot(hdat$year,hdat$hare,col=1,main="Hare",type="l",xlab="Year",ylab="\043 pelts")
lines(hdat$year,lotka$Prey*ascale,col=2)

plot(hdat$year,hdat$lynx,type="l",main="Lynx",xlab="Year",ylab="\043 pelts")
lines(hdat$year,lotka$Predator*bscale,col=2)

amax = min(wchib)+200 
plot(wprey[wchib<amax],wchib[wchib<amax],xlab="prey_0",ylab="scaled Pearson chi-squared",xlim=c(min(wprey),max(wprey)))
plot(wpredator[wchib<amax],wchib[wchib<amax],xlab="predator_0",ylab="scaled Pearson chi-squared",xlim=c(min(wpredator),max(wpredator)))
plot(wa[wchib<amax],wchib[wchib<amax],xlab="a",ylab="scaled Pearson chi-squared",xlim=c(min(wa),max(wa)))
plot(wb[wchib<amax],wchib[wchib<amax],xlab="b",ylab="scaled Pearson chi-squared",xlim=c(min(wb),max(wb)))
plot(wc[wchib<amax],wchib[wchib<amax],xlab="c",ylab="scaled Pearson chi-squared",xlim=c(min(wc),max(wc)))
plot(we[wchib<amax],wchib[wchib<amax],xlab="e",ylab="scaled Pearson chi-squared",xlim=c(min(we),max(we)))

##################################################################################
##################################################################################
mean_prey     = weighted.mean(wprey,wprob)
mean_predator = weighted.mean(wpredator,wprob)
mean_a = weighted.mean(wa,wprob)
mean_b = weighted.mean(wb,wprob)
mean_c = weighted.mean(wc,wprob)
mean_e = weighted.mean(we,wprob)

va = cov.wt(as.matrix(cbind(wprey,wpredator,wa,wb,wc,we)),wprob,cor=T)
mycov = va$cov
mycor = va$cor

cat("The mean and std deviation of hare_0 is ",mean_prey,sqrt(mycov[1,1]),"\n")
cat("The mean and std deviation of lynx_0 is ",mean_predator,sqrt(mycov[2,2]),"\n")
cat("The mean and std deviation of a is ",mean_a,sqrt(mycov[3,3]),"\n")
cat("The mean and std deviation of b is ",mean_b,sqrt(mycov[4,4]),"\n")
cat("The mean and std deviation of c is ",mean_c,sqrt(mycov[5,5]),"\n")
cat("The mean and std deviation of e is ",mean_e,sqrt(mycov[6,6]),"\n")


##################################################################################
##################################################################################


#and scale and save for plotting
lotka_melt= melt(lotka_best, id.vars = c("time"))
head(lotka_melt)
names(lotka_melt) <- c("year", "species", "count")
lotka_melt$pelts <- lotka_melt$count*ascale
lotka_melt$species <- as.character(lotka_melt$species)
lotka_melt$pelts[lotka_melt$species=="Predator"] <- lotka_melt$count[lotka_melt$species=="Predator"]*bscale

lotka_melt$species[lotka_melt$species=="Predator"] <-"Lynx"
lotka_melt$species[lotka_melt$species=="Prey"] <-  "Hare"

#and save
write.csv(lotka_melt, file = "LVfit_lynxhare.csv", row.names=F)

