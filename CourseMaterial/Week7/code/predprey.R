rm(list=ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(deSolve)


## load lynx-hare data and fit model
#### 
homewd = paste0(getwd(), "/CourseMaterial/Week7/code/")
setwd(homewd)

dat <- read.csv("hudson-bay-lynx-hare.csv", header = T, stringsAsFactors = F)
head(dat)

#and plot
lynx_hare_melted_df <- melt(as.matrix(dat[, 2:3]))
colnames(lynx_hare_melted_df) <- c("year", "species", "pelts")
lynx_hare_melted_df$year <-
  lynx_hare_melted_df$year +
  rep(1899, length(lynx_hare_melted_df$year))

lynx_hare_melted_df$species <- as.character(lynx_hare_melted_df$species)


colz = c('Lynx' = "tomato", 'Hare'="cornflowerblue")

p1 <- ggplot(data=lynx_hare_melted_df) + theme_bw() + ylab("pelts (thousands)") +
      geom_point(aes(x=year, y=pelts, color=species), size=3)+
      scale_color_manual(values=colz) +
      geom_line(aes(x=year, y=pelts, color=species), linetype=2)+
      theme(panel.grid = element_blank(), 
            axis.title = element_text(size=18),
            legend.title = element_blank(),
            legend.position = c(.85,.85),
            axis.text = element_text(size=16))
p1

ggsave(file = paste0(homewd, "/figs/lynxhare_data.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2, 
       dpi=300)

#and load fitted model
lotka_fit <- read.csv("LVfit_lynxhare.csv", header = T, stringsAsFactors = F)


p2 <- ggplot(data=lotka_fit) + 
  geom_line(aes(x=year, y=pelts, color=species), linewidth=1) + theme_bw()+ ylab("pelts (thousands)") +
  geom_point(data=lynx_hare_melted_df, aes(x=year, y=pelts, color=species), size=3)+
  geom_line(data=lynx_hare_melted_df, aes(x=year, y=pelts, color=species), linetype=2)+
  scale_color_manual(values=colz) +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.85,.85),
        axis.text = element_text(size=16))
p2
  


ggsave(file = paste0(homewd, "/figs/lynxhare_fit.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2, 
       dpi=300)


### and plot orbits
lotka_wide <- dcast(lotka_fit, year~species)
lotka_wide<-  arrange(lotka_wide, Hare)

p3 <- ggplot(data=lotka_wide) + ylab("Hare pelts (thousands)") + xlab("Lynx pelts (thousands)") +
  geom_point(aes(x=Lynx, y=Hare), size=3) + theme_bw()+ #ylab("pelts (thousands)") +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.85,.85),
        axis.text = element_text(size=16))
p3


ggsave(file = paste0(homewd, "/figs/lynx_byhare.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2, 
       dpi=300)

### ode model 
source("LV.R")

A <- matrix(c(0, 0.5, -0.5, 0), 2, 2, byrow = TRUE)
r <- c(0.1, -0.5)
pars <- list(r = r, A = A)
x0 <- c(x1 = 1, x2 = 0.08127632)
times <- seq(0, 200, by = 0.075)
out <- as.data.frame(ode(x0, times, LotkaVolterra, pars))
plvdyn <- ggplot(data = out, aes(x = time, y = x1)) + geom_line(colour = "blue") +
  geom_line(data = out, aes(x = time, y = x2), colour = "red") + theme_bw() + theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size=14, face="bold")) +
  theme(legend.position = "bottom") + xlab("time") + ylab("density") 
show(plvdyn)  


#and run with different pars - relax mortality on predator

A <- matrix(c(0, 0.5, -0.5, 0), 2, 2, byrow = TRUE)
r <- c(0.2, -0.4)
pars <- list(r = r, A = A)
out2 <- as.data.frame(ode(x0, times, LotkaVolterra, pars))

A <- matrix(c(0, 0.5, -0.5, 0), 2, 2, byrow = TRUE)
r <- c(0.3, -0.3)
pars <- list(r = r, A = A)
out3 <- as.data.frame(ode(x0, times, LotkaVolterra, pars))

out$A_strength <- "high"
out2$A_strength <- "low"
out3$A_strength <- "lower"


out<- rbind(out, out2, out3)
colz = c('high' = "orchid", 'low'="slateblue3", 'lower' ="navy")
p4 <- ggplot(data=out) + ylab("y") + xlab("x") +
  scale_color_manual(name =  "background predator\nmortality rate", values=colz) +
  geom_point(aes(x=x1, y=x2, color=A_strength), size=3) + theme_bw()+ ylab("number predators") +
  xlab("number prey") +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(size=18, color="firebrick"),
        axis.text.x = element_text(size=18, color="firebrick"),
        axis.title.y = element_text(size=18, color="cornflowerblue"),
        axis.text.y = element_text(size=18, color="cornflowerblue"),
        legend.position = c(.7,.7),
        axis.text = element_text(size=16))
p4

ggsave(file = paste0(homewd, "/figs/pred_by_prey.png"),
       units="mm",  
       width=60, 
       height=45, 
       scale=2, 
       dpi=300)

