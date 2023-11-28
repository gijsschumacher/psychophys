# In this script we run different tests and simulations to address the question how to model fEMG data
rm(list=ls())

#outdir <- paste0("C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science")
#change the outdir to a local folder to store the output folders there
outdir <- "outdir"
load("All Syncs/emg.RData")

source("Analysis/descriptives.R") # print and display the data's descriptive statistics

source("Analysis/modelspecification.R") # run another set of simulations using different variables in the specification.

source("Analysis/iaps_means_comparision.R") # this visualizes corrugator and zygomaticus responses to IAPS images.

source("Analysis/analysis.R") # this runs the main set of preregistered analyses

source("Analysis/visualization.R") # this creates the visualizations of the analyses, specifically subset of the main analysis & the moderators of emotional stimuli

source("Analysis/print analyses.R") # this prints the tables from the regressions run in the analysis.R scripts