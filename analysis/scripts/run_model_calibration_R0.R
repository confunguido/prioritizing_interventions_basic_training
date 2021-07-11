##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(scam)
library(mgcv)
library(MASS)

testpos.time = read.csv('../output/calibration_sim.csv')
