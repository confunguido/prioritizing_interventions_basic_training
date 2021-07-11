#### Set default seed
##set.seed(123456)

load_network_params = function(site_name, params_file){
    params_df_calib = read.csv(params_file)
    if(site_name == 'full'){
        network_file = '../output/network.RData'
        R0.mag = mean(params_df_calib$R0_mag)
        initial.infected = mean(params_df_calib$initial_inf)
        genint_scale = mean(params_df_calib$genint_scale)
        genint_shape = mean(params_df_calib$genint_shape)
    }else{        
        network_file = sprintf('../output/network_%s.RData', training_site)
        R0.mag = params_df_calib$R0_mag[params_df_calib$base_name == site_name]
        initial.infected = params_df_calib$initial_inf[params_df_calib$base_name == site_name]
        genint_scale = params_df_calib$genint_scale[params_df_calib$base_name == site_name]
        genint_shape = params_df_calib$genint_shape[params_df_calib$base_name == site_name]
    }
    
    return(list(R0.mag = R0.mag, initial.infected = initial.infected, network_file = network_file,
                genint_scale = genint_scale, genint_shape = genint_shape))
}
#### load libraries
library('scam')

## Check if training_site is specified, else, specified it as 'full'
if(!exists('training_site')){
    training_site = 'full'
}
if(!(training_site %in% c('FB', 'FLW'))){
    training_site = 'full'
}


#### Load data
calib_params_list = load_network_params(training_site, '../output/calibrated_parameters_forecast.csv')

load(calib_params_list$network_file)
## parameter values for infection processes
R0.mag = calib_params_list$R0.mag
initial.infected = calib_params_list$initial.infected
R0.tim = 5.2 ## 7.8
symp.mag = 0.57
symp.tim = 5.8
symp.mean = 10
asymp.adjust = 0.8
initial.immune = 0.026
importation = c(rep(0, numrecruits), rep(0.01/84, numdrillserg + numStaff)) # 1% chance of infection over 84 day period

R0.prob = R0.mag / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)


## parameter values for control processes
## testing
BOOL_testDaily = FALSE
BOOL_testOnArrival = 1
testDelayQuarantine = 3
testReturn = 1
testdates = c(14)
testdates_type = c('pcr')

## Staff routine testing
testStaffFreq = 0
testStaffType = 'antigen'

## testing upon exit
## Not necessary, just set BOOL_clinicalrelease = 0

## isolation/quarantine and contact tracing
BOOL_clinicalrelease = 1
isolate.max = 10
isolate.min = 10
isolate.length = 10
isolate.nosymp = 0
isolate.effectiveness = 0.99
quarantine.max = 10
quarantine.effectiveness = 0.99
quarantine.contacts = 0

## masks
mask.protection = 0.3
mask.compliance = 0.2
compliance.avg = mask.protection * mask.compliance
compliance.spread = 1e1
compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)

mask.final.compliance = 0.1
compliance.final.avg = mask.protection * mask.final.compliance
BOOL_compliance_time = 0

## All the individuals have the same compliance, 
## compliance = rbeta(
##   numrecruits+numdrillserg+numStaff,
##   compliance.avg * compliance.spread,
##   (1 - compliance.avg) * compliance.spread
## )

## specify arrival dates on campus
arrivebase = c(sample(1:1, numrecruits, replace = T), rep(0, numdrillserg + numStaff))

pcr.spec.screen = 0.998
pcr.spec.commercial = 0.998

pcr.sens.screen.target = 0.859
pcr.sens.commercial.target = 0.859

## timespan of simulation
time = 1:56

##======================================#
## Incubation period and sens of tests
##======================================#
## Time from symptom onset to transmission
incub.shape =  5.807
incub.scale = 0.948
tost.mu = 0.46
tost.ke = 2.8
k_inc = incub.shape
gamma = 1 / (k_inc * incub.scale)
mu = tost.mu
k_E = tost.ke
k_I = 1
alpha = 1
k_P = k_inc - k_E
C = k_inc * gamma * mu / (alpha * k_P * mu + k_inc * gamma)

t_tost = seq(-23,23,0.05)
fm = alpha * C * (1 - pgamma(-t_tost, shape=k_P, scale=1/(k_inc*gamma)))
fp = C * (1 - pgamma(t_tost, shape=k_I, scale=1/(k_I*mu)))
f = ifelse(t_tost>0,fp,fm)
f = approxfun(t_tost, f)

t_tost = -22:22
f_tost = sapply(t_tost,function(t)integrate(f,t,t+1)[[1]])
f_tost = f_tost / sum(f_tost)
tost = cbind(t_tost,f_tost)

load('sens_postOnset.RData')
sens_draw = sens_postOnset.median
## sensitivity =
##   rbind(
##     cbind(
##       tost[2:23,1],
##       tost[1:22,2]/tost[22,2]*max(sens_draw)),
##     cbind(
##       1:33,
##       sens_draw[-1]))

if(is.null(calib_params_list$genint_scale)){
    calib_params_list$genint_scale = 5.67
    calib_params_list$genint_shape = 2.89
    
}
genint_shape = calib_params_list$genint_shape
genint_scale = calib_params_list$genint_scale
genint = pweibull(1:21,scale=genint_scale,shape=genint_shape) - pweibull(0:20,scale=genint_scale,shape=genint_shape)
genint = genint / sum(genint)

sensitivity = rbind(
    cbind(
        1:6,
        genint[1:6]/genint[6] * max(sens_draw)),
    cbind(
        (1:(length(sens_draw) - 1))  + 6,
        sens_draw[-1]))



sensitivity_array = sensitivity[,2]
sensitivity_time = sensitivity[,1]

sens.daily = sens_postOnset.median * (pweibull(1:34,shape=0.9,scale=5.657) -
                                      pweibull(0:33,shape=0.9,scale=5.657))
sens.overall.default = sum(sens.daily)

sens.fun = function(sens.overall){
  pmin(1, (sens.overall / sens.overall.default) * sensitivity[,2])
}

pcr.sens.commercial = function(dd){
    ## Day here means day relative to infection
    dd[dd > length(sensitivity_array)] = length(sensitivity_array)
    dd[dd < 1] = 1
    return(sens.fun(pcr.sens.commercial.target)[dd])
}


pcr.sens.screen = function(dd){
    ## Day here means day relative to infection
    dd[dd > length(sensitivity_array)] = length(sensitivity_array)
    dd[dd < 1] = 1
    return(sens.fun(pcr.sens.screen.target)[dd])
}
