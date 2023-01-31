## an answer to the question on Robins
require(ggplot2)
library(readr)
#read in the data
homewd=getwd()
X4birdsNashvilleMI <- read_csv(paste0(homewd,"/CourseMaterial/Week7/Lab6/4birdsNashvilleMI.csv"))

avgeRob <- mean(X4birdsNashvilleMI$Robin)
avgeRob

## plot these data (not required for answer)
ggplot(data=X4birdsNashvilleMI, aes(x=Year, y=Robin)) + geom_line() + geom_point(pch=1) + ylim(0,105)

attach(X4birdsNashvilleMI) # keep the data
# now we create a vector of the year to year change in the  population: R = N(t+1) / Nt
# the use of [-1] in the index tells R to exclude the first element.
# length() is the length of a vector, so [-length(X)] means exclude the last
ShortRobin <- Robin[-length(Robin)] # this is a shorter vector of population sizes
Robin.R <- Robin[-1]/Robin[-length(Robin)] # these are the R values of same length



#create new dataframe with Robin population and the R = N(t+1)/Nt for each year (except last)
NewRobin.df <- data.frame(Robin.R, ShortRobin)

qplot(x=Year[-length(Robin)], y=Robin.R, geom="point") + geom_hline(yintercept=1, lty=3) + 
  labs(y=bquote(N[t+1]/N[t]), x="Year (t)")


# run a regression to get the line's slope and intercept
reg<-lm(formula = Robin.R ~ ShortRobin,
        data=NewRobin.df)                      

#get intercept and slope value
coeff<-coefficients(reg)          
intercept<-coeff[1]
slope<- coeff[2]

# Create basic ggplot
ggp <- ggplot(NewRobin.df, aes(ShortRobin, Robin.R)) +   
  geom_point() +
  xlab("Robin population size N") + ylab("Nt+1/Nt")

# add the regression line
ggp+geom_abline(intercept = intercept, slope = slope, color="red", 
                linetype="dashed", size=1.5)+
  ggtitle("Robins")  + 
     geom_segment(x = 76.5, y = 0.6, xend = 76.5, yend = 1,
             arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
      geom_hline(yintercept=1, linetype="dashed", color = "blue")


          