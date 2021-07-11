##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
source('simOutbreak.R')
nreps = 1e3
set.seed(123456)

##==================================================#
## run sims across different sites----------
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

imports_df = read.csv('../output/calibrated_imports_site.csv')

##R0_list = list()
for(ii_param in 1:length(training_site_list)){
    print(ii_param)
    training_site = training_site_list[ii_param]
    site_imports = imports_df$imports[imports_df$site == training_site]
    source('paramDefaults.R')

    calib_params_list = load_network_params(training_site, '../output/calibrated_parameters.csv')
    R0.mag = calib_params_list$R0.mag
    initial.infected = calib_params_list$initial.infected
    R0.prob = R0.mag / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)
    
    time = 1:time_max

    ## Importations
    importation = c(rep(0, numrecruits), rep(site_imports, numdrillserg + numStaff))

    BOOL_testDaily = TRUE
    BOOL_testOnArrival = 1
    testdates = c()
    quarantine.contacts = 0

    ## Assume mask compliance = 0
    mask.compliance = 0.2
    compliance.avg = mask.protection * mask.compliance
    compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)
        
    s = replicate(nreps,simOutbreak())

    ss_summary = matrix(unlist(s[1,]), ncol = nreps)
    ss_time_inf = do.call(rbind, s[2,])
   
    inf.time[,ii_param] = ss_time_inf$numInfected
    import.time[,ii_param] = ss_time_inf$numImported
    testpos.time[,ii_param] = ss_time_inf$numTestPositive

    init.testDay[,ii_param] = ss_summary[1,]
    inf.testDay[,ii_param] = ss_summary[4,]
    ## total_R0_df = data.frame()
    ## for(nn_i in 1:ncol(s)){
    ##     a = s[4,nn_i][[1]]
    ##     new_infections_s = as.numeric(table(a[a[,'ii'] != 0,'ii']))
    ##     new_infections_df = as.data.frame((table(new_infections_s)))
    ##     new_infections_df$rep = nn_i
    ##     total_R0_df = rbind(total_R0_df, new_infections_df)
    ## }

    ## total_R0_df = as.data.frame(do.call(rbind,by(total_R0_df,list(total_R0_df$new_infections_s),FUN = function(x){mean(x$Freq)}, simplify = F)))
    ## total_R0_df$R0 = as.numeric(rownames(total_R0_df))
    ## total_R0_df$Freq = total_R0_df$V1
    ## total_R0_df = total_R0_df[order(total_R0_df$R0),]

    ## R0_list[[ii_param]] = total_R0_df
}
inf.testDay = as.data.frame(inf.testDay)
inf.time = as.data.frame(inf.time)
import.time = as.data.frame(import.time)
testpos.time = as.data.frame(testpos.time)

##barplot(total_R0_df$Freq, names.arg = total_R0_df$R0,xlab = 'Secondary infections', ylab = 'Frequency')
##sum(total_R0_df$Freq * total_R0_df$R0 / sum(total_R0_df$Freq))

##==================================================#
## Reshape and save----------
##==================================================#
colnames(inf.testDay) = c(training_site_list, c('rep'))
colnames(inf.time) = c(training_site_list, c('rep'))
colnames(import.time) = c(training_site_list, c('rep'))
colnames(testpos.time) = c(training_site_list, c('rep'))

inf.time$time = rep(1:time_max, nreps)
import.time$time = rep(1:time_max, nreps)
testpos.time$time = rep(1:time_max, nreps)

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep'),
                              value.name = 'infections', variable.name = 'training_site')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep'),
                           value.name = 'infections', variable.name = 'training_site')

import.time = reshape2::melt(import.time, id.vars = c('time','rep'),
                             value.name = 'importations', variable.name = 'training_site')

testpos.time = reshape2::melt(testpos.time, id.vars = c('time','rep'),
                             value.name = 'testPositive', variable.name = 'training_site')

inf.time = merge(inf.time, import.time, by = c('time', 'rep', 'training_site'), all.x = T)
inf.time = merge(inf.time, testpos.time, by = c('time', 'rep', 'training_site'), all.x = T)

write.csv(inf.testDay, '../output/output_calibration_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_calibration_time_scenarios.csv', row.names = F)


