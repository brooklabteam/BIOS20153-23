rm(list=ls())

homewd=  "/Users/carabrook/Developer/BIOS20153-23"

png(filename=paste0(homewd, "/CourseMaterial/Week9/Lec8CommunityAssemblyConservation/code/figures/islandbio1.png"), 
    width=120, height=100, res=300, units="mm")
I0 <- log(1)
b <- .11
curve(exp(I0 - b*x),0, 40, col="red", lwd=2,
      xlab='No. of Species', ylab="Rate (I or E)", tcl=-1, lab=c(10,5, 7)) # long ticks
#for the minor ticks
xtmp = seq(from=0,to=40,by=1)
axis(1,xtmp,lwd=1.5, labels=FALSE, tcl=-.5)
 
d <- .02
curve(exp(d*x)-1, 0, 40, add=TRUE, lwd=2, col="blue")
deltaR <- function(R) { (exp(I0-b*R) - (exp(d*R)-1))^2 }
S1 <- optimize(f=deltaR, interval=c(1,40))$minimum
I1 <- exp(I0-b*S1)
dev.off()
#save 


#and with with four
png(filename=paste0(homewd, "/CourseMaterial/Week9/Lec8CommunityAssemblyConservation/code/figures/islandbio2.png"), 
    width=120, height=100, res=300, units="mm")
I0 <- log(1)
b <- .11
curve(exp(I0 - b*x),0, 40, col="red", lwd=2,
      xlab='No. of Species', ylab="Rate (I or E)", tcl=-1, lab=c(10,5, 7)) # long ticks
#for the minor ticks
xtmp = seq(from=0,to=40,by=1)
axis(1,xtmp,lwd=1.5, labels=FALSE, tcl=-.5)

d <- .02
curve(exp(d*x)-1, 0, 40, add=TRUE, lwd=2, col="blue")
deltaR <- function(R) { (exp(I0-b*R) - (exp(d*R)-1))^2 }
S1 <- optimize(f=deltaR, interval=c(1,40))$minimum
I1 <- exp(I0-b*S1)

 
I0 <- log(1/2)
curve(exp(I0-b*x), 0,40, add=TRUE, lty=2 , lwd=2, col="red")
d <- .014
curve(exp(d*x)-1, 0, 40, add=TRUE, lty=2,lwd=2, col="blue")
S2 <- optimize(f=deltaR, interval=c(1,50))$minimum
I2 <- exp(I0-b*S2)
#segments(S2, -.1, S2, I2, lty=2)
# add the labels for the lines
text(4,0.85, "close", cex =.9)
text(0,0.4, "far", cex =.9)
text(35, 0.9, "small", cex =.9)
text(39, 0.65, "large", cex =.9)
dev.off() 
