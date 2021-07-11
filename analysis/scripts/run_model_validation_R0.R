##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(tidyverse)
source('simOutbreak.R')
nreps = 1e2

##==================================================#
## run sims across different immunity levels----------
##==================================================#
training_site_list = c('FB', 'FLW')
time_max = 30

init.testDay = inf.testDay = matrix(NA,nreps,length(training_site_list) + 1)
init.testDay[,length(training_site_list) + 1] = 1:nreps
inf.testDay[,length(training_site_list) + 1] = 1:nreps

inf.time = matrix(NA,nreps*time_max,length(training_site_list) + 1)
inf.time[,length(training_site_list) + 1] = rep(1:nreps, each = time_max)

testpos.time = matrix(NA,nreps*time_max,length(training_site_list) + 1)
testpos.time[,length(training_site_list) + 1] = rep(1:nreps, each = time_max)

import.time = matrix(NA,nreps*time_max,length(training_site_list) + 1)
import.time[,length(training_site_list) + 1] = rep(1:nreps, each = time_max)

R0_list = list()
for(ii_param in 1:length(training_site_list)){
    print(ii_param)
    training_site = training_site_list[ii_param]
    source('paramDefaults.R')

    calib_params_list = load_network_params(training_site, '../output/calibrated_parameters.csv')
    R0.mag = calib_params_list$R0.mag
    initial.infected = calib_params_list$initial.infected
    R0.prob = R0.mag / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)
    
    time = 1:time_max

    ## Importations
    importation = c(rep(0, numrecruits), rep(50 * 0.01/84, numdrillserg + numStaff))

    BOOL_testDaily = TRUE
    BOOL_testOnArrival = 1
    testdates = c()
    quarantine.contacts = 0

    ## Assume mask compliance = 0
    mask.compliance = 0.2
    compliance.avg = mask.protection * mask.compliance
    compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)
        
    s = replicate(nreps,simOutbreak())
    total_R0_df = data.frame()
    for(nn_i in 1:ncol(s)){
        a = s[4,nn_i][[1]]
        new_infections_s = as.numeric(table(a[a[,'ii'] != 0,'ii']))
        new_infections_df = as.data.frame((table(new_infections_s)))
        new_infections_df$rep = nn_i
        total_R0_df = rbind(total_R0_df, new_infections_df)
    }

    total_R0_df = as.data.frame(do.call(rbind,by(total_R0_df,list(total_R0_df$new_infections_s),FUN = function(x){mean(x$Freq)}, simplify = F)))
    total_R0_df$R0 = as.numeric(rownames(total_R0_df))
    total_R0_df$Freq = total_R0_df$V1
    total_R0_df = total_R0_df[order(total_R0_df$R0),]

    R0_list[[ii_param]] = total_R0_df
}

tmp_r0_df = R0_list[[2]]
barplot(tmp_r0_df$Freq, names.arg = tmp_r0_df$R0,xlab = 'Secondary infections', ylab = 'Frequency')
sum(tmp_r0_df$Freq * tmp_r0_df$R0 / sum(tmp_r0_df$Freq))

##==================================================#
## Reshape and save----------
##==================================================#
## colnames(inf.testDay) = c(sim_scenarios, c('rep'))
## inf.testDay$importation = 1

## colnames(inf.time) = c(sim_scenarios, c('rep'))
## inf.time$time = rep(1:time_max, nreps)
## inf.time$importation = 1

## inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep', 'importation'),
##                              value.name = 'infections', variable.name = 'sim_scenario')

## inf.time = reshape2::melt(inf.time, id.vars = c('time','rep','importation'),
##                            value.name = 'infections', variable.name = 'sim_scenario')

##write.csv(inf.testDay, '../output/output_validation_R0_scenarios.csv', row.names = F)
##write.csv(inf.time, '../output/output_baseline_time_scenarios.csv', row.names = F)


