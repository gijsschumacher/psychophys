# In this script we run different tests and simulations to address the question how to model fEMG data
rm(list=ls())

#outdir <- paste0("C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science")
#change the outdir to a local folder to store the output folders there
outdir <- "outdir"


source("Simulations/simulation mean vs ml.R") # here we run a number of simulations to contrast results obtained by taking the mean vs following a multilevel specification

source("Simulations/tumor_baby.R") # here we run a number of alternative model specifications using multilevel models and different preprocessing steps

source("Simulations/simulation_moderator.R") # same as before, but now we evaluate what happens to moderating variables; this script isn't used in the paper.

