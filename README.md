# Replication files of the "Establishing the validity and robustness of facial electromyography measures for political science" paper

preregistered_analysis.R contains preregistered analyses

To replicate the paper execute the following R-scripts in this order:

create data.R # this creates the dataset emg.RData, because some proprietary data and external data is used here this script is not fully replicable.
simulation.R # this runs a number of evaluations regarding how to model emg data.
analysis.R # this calls a series of scripts that do the main analysis and visualize the results as presented in the paper

To execute these scripts it is always assumed the root folder location is the working directory.

The folders:
All Syncs: contains the main dataset plus the scripts called from "create data.R"
Annotations: contains human coding of errors in signals, plus some scripts.
Markers: contains information regarding the meaning of each marker in the study
Outdir: contains the figures and tables produced by the scripts. 
Simulations: contains the scripts called from "simulations.R"


