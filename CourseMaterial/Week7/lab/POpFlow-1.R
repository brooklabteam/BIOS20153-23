
# Define the Rates function
pop.flow = function(b,d,a,c) {
  
  # Population size
  N = seq(0,100,5)
  
  # Define per-capita birth and death rates
  pcbr = b - a*N
  pcdr = d + c*N
  
  # Plot percapita birth and death rates as a function of N
  plot(N,pcbr,type='l',col='blue',lwd=2,ylim=c(min(c(pcbr,pcdr)),max(c(pcbr,pcdr))),xlab='Population Size (N)',ylab='per capita Rate')
  lines(N,pcdr,col='red',lwd=2) 
  legend(0.5,max(c(pcbr,pcdr)),c(' birth rate','death rate'),pch=15,col=c('blue','red'))
  rug(x=seq(5, 95, by = 5), side=1, ticksize=-0.02)
  # Add flow information
  text(1,(1-0.25)*min(c(pcbr,pcdr)),'Flow',cex=1.5)
  for (i in 1:length(N)) {
    loc = i
    if (pcbr[loc] > pcdr[loc]) {
      text(N[i],min(c(pcbr,pcdr)),'>',cex=1.5)
    } 
    if (pcbr[loc] < pcdr[loc]) {
      text(N[i],min(c(pcbr,pcdr)),'<',cex=1.5)
    }
    if (pcbr[loc] == pcdr[loc]) {
      text(N[i],min(c(pcbr,pcdr)),'--',cex=1.5)
    }
  }
}
# Plug in parameter values and run
pop.flow(b = 0.8, d =0.2, a = 0.01,c = 0.01)