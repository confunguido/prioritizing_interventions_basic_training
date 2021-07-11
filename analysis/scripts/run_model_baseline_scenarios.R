##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
source('simOutbreak.R')
nreps = 1e3


##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##==================================================#
## run sims across different immunity levels----------
##==================================================#
testdates_list = list(numeric(),c(14))
testarrival_list = c(0,1)
test_type = list(c(),c('pcr'))
sim_scenarios = c('NoIntervention','Baseline')
fm_list = c(0, 0.2)

time_max = 73

init.testDay = inf.testDay = matrix(NA,nreps,length(sim_scenarios) + 1)
init.testDay[,length(sim_scenarios) + 1] = 1:nreps

inf.testDay[,length(sim_scenarios) + 1] = 1:nreps

inf.time = matrix(NA,nreps*time_max,length(sim_scenarios) + 1)
inf.time[,length(sim_scenarios) + 1] = rep(1:nreps, each = time_max)


for(ii_param in 1:length(sim_scenarios)){
    print(ii_param)
    source('paramDefaults.R')
    time = 1:time_max
    ## Importations
    importation = c(rep(0, numrecruits), rep(1*0.01/time_max, numdrillserg + numStaff))
    
    ## Default quarantine contacts = 0
    quarantine.contacts = 0
    ##testdates_type = test_type
    ## Change values
    BOOL_testOnArrival = testarrival_list[ii_param]
    testdates = testdates_list[[ii_param]]
    testdates_type = test_type[[ii_param]]

    ## Change values masks
    mask.compliance = fm_list[ii_param]
    compliance.avg = mask.protection * mask.compliance
    compliance = rbeta(
        numrecruits+numdrillserg+numStaff,
        compliance.avg * compliance.spread,
        (1 - compliance.avg) * compliance.spread
    )
    
    ## simulate
    s = replicate(nreps,simOutbreak())
    ss_summary = do.call(rbind,s[1,])
    ##ss_summary = matrix(unlist(s[1,]), ncol = nreps)
    ss_time_inf = do.call(rbind, s[2,])

    inf.time[,ii_param] = ss_time_inf[,1]

    init.testDay[,ii_param] = ss_summary[,1]
    inf.testDay[,ii_param] = ss_summary[,4]
}
inf.testDay = as.data.frame(inf.testDay)
inf.time = as.data.frame(inf.time)

##==================================================#
## Reshape and save----------
##==================================================#
colnames(inf.testDay) = c(sim_scenarios, c('rep'))
inf.testDay$importation = 1

colnames(inf.time) = c(sim_scenarios, c('rep'))
inf.time$time = rep(1:time_max, nreps)
inf.time$importation = 1

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep', 'importation'),
                             value.name = 'infections', variable.name = 'sim_scenario')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep','importation'),
                           value.name = 'infections', variable.name = 'sim_scenario')

write.csv(inf.testDay, '../output/output_baseline_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_baseline_time_scenarios.csv', row.names = F)


