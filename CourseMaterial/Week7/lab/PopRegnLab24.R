# Robins
install.packages(readr)
library(readr)
#read in the data  -- from the File menu or from the Environment window using readr 
# whatever way you do it, it must be called X4birdsNashvilleMI for the rest of the code to work

## insert a line of code here to calculate the average population size of robins



## plot these data (not required for answer; this is how we got the graph in the lab instructions (except I did it with ggplot)
#plot(Year, Robin, type = "b", pch = 19,  col = "maroon")

attach(X4birdsNashvilleMI) # keep the data
# now we create a vector of the year to year change in the  population: R = N(t+1) / Nt
# the use of [-1] in the index tells R to exclude the first element.
# length() is the length of a vector, so [-length(X)] means exclude the last
ShortRobin <- Robin[-length(Robin)] # this is a shorter vector of population sizes
Robin.R <- Robin[-1]/Robin[-length(Robin)] # these are the R values of same length


#create new dataframe with Robin population and the R = N(t+1)/Nt for each year (except last)
NewRobin.df <- data.frame(Robin.R, ShortRobin)

# this creates a quick plot of Nt+1/Nt versus time
plot(x=Year[-length(Robin)], y=Robin.R,xlab="Year (t)", ylab= "(N[t+1]/N[t])") 
abline(h=1, lty=3) # adds a horizontal line



# run a regression to get the line's slope and intercept
reg<-lm(formula = Robin.R ~ ShortRobin,
        data=NewRobin.df)                      


# Create basic plot
plot(ShortRobin, Robin.R, xlab= "Robin population size N", ylab = "Nt+1/Nt", main = "Robins and YOUR NAMES")
abline(h=1, lty=3) # adds a horizontal line
abline(reg, lty=2, col="red") # adds the regression line

 