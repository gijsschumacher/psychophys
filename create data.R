#outdir <- paste0("C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science")

# Make sure the working directory is the directory this file is in

source("All Syncs/make datasets.R") # this script combines different studies together. This is not fully replicable because datasets need to be used that aren't online.

source("All Syncs/calculate baselines.R") # this script calculates baseline measures

source("All Syncs/calculate activity measures.R") # this script calculates activity measures

source("Annotations/check_lastsync.R") # this script investigates issues with the final reported sync. The conclusion is that these syncs are typically wrong and need to be removed.

source("Annotations/check_coding.R") # this script identifies the coders, therefore the original coding is not shared

source("All Syncs/update dataset.R") # this script adds a few more variables, and removes a few observations 

