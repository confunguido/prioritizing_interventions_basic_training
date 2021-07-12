# Prioritizing interventions for preventing COVID-19 outbreaks in military basic training
This repository includes the code necessary to reproduce the figures of the manuscript entitled "Prioritizing interventions for preventing COVID-19 outbreaks in military basic training"


## Organization
The contents of this repository are organized based on scripts, data, figures, and output. 

## Scripts
This folder contains the code necessary to run the model `simOutbreak.R` and load the default parameters `paramDefaults.R`. The calibration and sensitivity analysis scripts are setup to run in a cluster. To run the experiments, the scripts are named as `run_model*.R`. After this step, figures that appear in the manuscript can be generated under a similar name pattern `manuscript_figure_*.R`. 

## Data
The data folder contains estimates of the exposure by day in different states in the US. These files are used in the calibration routines to setup the baseline levels of previous exposure in the model. 
