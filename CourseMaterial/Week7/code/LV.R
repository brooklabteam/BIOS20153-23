rm(list=ls())

require(deSolve)
require(ggplot2)
require(scales)
require(reshape2)

homewd = paste0("/Users/carabrook/Developer/BIOS20153-23/CourseMaterial/Week7/code/")
setwd(homewd)

LotkaVolterra <- function(time, state, params){
  # state:
  # x1, x2
  # params
  # r[1,2], interactions A
  with(as.list(c(state, params)), {
    x1 <- max(x1, 0)
    x2 <- max(x2, 0)
    dx1dt <- x1 * (r[1] - A[1,1] * x1 - A[1,2] * x2)
    dx2dt <- x2 * (r[2] - A[2,2] * x2 - A[2,1] * x1)
    return(list(c(dx1dt, dx2dt)))
    })
  
}

plotresults <- function(out, pars, plotEQ = TRUE, plotN1 = TRUE, plotN2 = TRUE, plotDYN = TRUE){
  r <- pars$r
  A <- pars$A
  eq1 <- c(0,0)
  eq2 <- c(0, r[2]/A[2,2])
  eq3 <- c(r[1]/A[1,1], 0)
  eq4 <- c((A[2,2] * r[1] - A[1,2] * r[2]) / det(A),
           (A[1,1] * r[2] - A[2,1] * r[1]) / det(A)
           )
  dfeq <- as.data.frame(rbind(eq1, eq2, eq3, eq4))
  if (eq4[1] < 0 | eq4[2] < 0) dfeq <- as.data.frame(rbind(eq1, eq2, eq3))
  colnames(dfeq) <- c("x1", "x2")
  
  line1 <- data.frame(
    x1 = seq(0, eq3[1], length.out = 3),
    x2 = (r[1] - A[1,1] * seq(0, eq3[1], length.out = 3)) / A[1,2]
    )
  line2 <- data.frame(
    x2 = seq(0, eq2[2], length.out = 3),
    x1 = (r[2] - A[2,2] * seq(0, eq2[2], length.out = 3)) / A[2,1]
  )
  
  pl <- ggplot(data = dfeq, aes(x1, x2)) 
  if (plotEQ == TRUE){
    pl <- pl + geom_point(size = 4)
  } else {
    pl <- pl + geom_point(size = 4, colour = NA)
  }
  if (plotN1 == TRUE){
    pl <- pl + geom_line(data = line1, aes(x1, x2), color="cornflowerblue") 
  }
  if (plotN2 == TRUE){
    pl <- pl + geom_line(data = line2, aes(x1, x2), color="tomato")
  }
  if (plotDYN == TRUE){
    pl <- pl + geom_point(data = out, aes(x1, x2, colour =time)) + 
      scale_color_gradient(low="yellow", high = "red", trans="log10")
     
      #scale_colour_gradient2(low = muted("blue"), mid = muted("red"),
       #                      high = muted("green"))
  }
  pl <- pl + theme_bw() + theme(axis.text = element_text(size = 14, face = "bold"), axis.title.x = element_text(size=14, face="bold", color="cornflowerblue"), axis.title.y = element_text(size=14, face="bold", color="tomato")) +
    theme(legend.position = "bottom") + theme(plot.margin = unit(c(.3,.3,.3,.3), "lines"))
  return(pl)
}
# 
# # test competition

#first, coexistence guaranteed!

r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)

ggsave(file = paste0(homewd, "/figs/stable-coexistence.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)

#always move on x1 nullcline if you start in bottom half of the plot

r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)



r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.22)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)




r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.45)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)





r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.39)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)




r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.05, x2 = 0.2)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p1a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
colz =c('x1'="cornflowerblue", 'x2'="tomato")

p1b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
#print(p1b)

p1 <- cowplot::plot_grid(p1a, p1b)
print(p1)












# next, spp 1 always outcompetes spp 2
A <- matrix(c(2.5,1.4, 2.6, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p2a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")

p2b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))
print(p2b)


p2b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p2 <- cowplot::plot_grid(p2a, p2b)
print(p2)

ggsave(file = paste0(homewd, "/figs/sppx1-wins.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)


#spp 2 always outcompetes spp 1
A <- matrix(c(2.5,1.6, 2.1, 1.4), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0


p3a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")

p3b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p3 <- cowplot::plot_grid(p3a, p3b)
print(p3)

ggsave(file = paste0(homewd, "/figs/sppx2-wins.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)


#finally
#outcome depends on initial conditions: here, x2 wins

r <- c(0.5,0.5)
A <- matrix(c(1.4, 2.7, 1.6, 2.1), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.1, x2 = 0.3)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p4a<- plotresults(out, pars)



ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")

p4b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p4 <- cowplot::plot_grid(p4a, p4b)
print(p4)

ggsave(file = paste0(homewd, "/figs/precedence-x2-wins.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)


#and here x1 wins
x0 <- c(x1 = 0.35, x2 = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p5a<- plotresults(out, pars)
head(out)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")
p5b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p5 <- cowplot::plot_grid(p5a, p5b)
print(p5)

ggsave(file = paste0(homewd, "/figs/precedence-x1-wins.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)




#and here, coexistence
x0 <- c(x1 = 0.3, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p6a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")


p6b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p6 <- cowplot::plot_grid(p6a, p6b)
print(p6)

ggsave(file = paste0(homewd, "/figs/precedence-coexistence.png"),
       units="mm",  
       width=90, 
       height=55, 
       scale=2, 
       dpi=300)



#and example for quiz Q



r <- c(0.5,0.5)
A <- matrix(c(2.5,1.4, 2.1, 1.6), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.1)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

head(out)

p7 <- plotresults(out, pars, plotDYN = FALSE, plotEQ = FALSE)

ggsave(file = paste0(homewd, "/figs/blanknullclines.png"),
       units="mm",  
       width=50, 
       height=45, 
       scale=2, 
       dpi=300)


#and try some other precedence. starting low for both in upper left
r <- c(0.5,0.5)
A <- matrix(c(1.4, 2.7, 1.6, 2.1), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.03, x2 = 0.15)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p6a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")


p6b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p6 <- cowplot::plot_grid(p6a, p6b)
print(p6)



#and try some other precedence. starting low for both in upper left
x0 <- c(x1 = 0.02, x2 = 0.2)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p6a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")


p6b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p6 <- cowplot::plot_grid(p6a, p6b)
print(p6)

#and in the middle

r <- c(0.5,0.45)
A <- matrix(c(1.4, 2.7, 1.6, 2.1), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.05, x2 = 0.06)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p6a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")


p6b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p6 <- cowplot::plot_grid(p6a, p6b)
print(p6)


r <- c(0.5,0.5)
A <- matrix(c(1.4, 2.7, 1.6, 2.1), 2, 2, byrow = TRUE)
pars <- list(r = r, A = A)
x0 <- c(x1 = 0.2, x2 = 0.0665)
times <- seq(0, 2000, by = 0.1)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
out[out < 0] <- 0

p6a <- plotresults(out, pars)
ts.long <- melt(out, id.vars  = "time", variable.name = "species", value.name = "proportion")


p6b <- ggplot(ts.long) +
  geom_line(aes(x=time, y=proportion, color=species), size=1) +theme_bw() +
  scale_color_manual(values=colz) + ylim(c(0,.4)) +
  theme(plot.margin = unit(c(.3,.3,4,.3), "lines"), legend.position = c(.8,.8),
        axis.title = element_text(size=14), axis.text = element_text(size=12))


p6 <- cowplot::plot_grid(p6a, p6b)
print(p6)
