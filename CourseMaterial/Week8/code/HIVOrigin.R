rm(list=ls())
library(ape)
library(phangorn)

## set the working directory to the appropriate location 

homewd="/Users/carabrook/Developer/BIOS20153-23"



##read in the file and save it as dat
file=paste0(homewd,"/CourseMaterial/Week8/code/hivsivseqs.fasta")
dat = read.aa(file, format = "fasta")


## compute the pairwise distances using maximum likelihood and the JTT model
## of amino acid substitution rates
HSdm = dist.ml(dat, model="JTT")
##make a tree of these distances using the neighbour-joining method
tree = NJ(HSdm)

##plot the tree
plot(tree, edge.width=2)
