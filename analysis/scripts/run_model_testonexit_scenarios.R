##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
source('simOutbreak.R')
nreps = 1e3
set.seed(123456)

##==================================================#
## run sims for testing on exit ----------
##==================================================#
importations_list = c(0,1,10)
test_list = c(0,1,1)
scenarios_names = c('None','Imperfect','Perfect')
time_max = 73

init.testDay = inf.testDay = matrix(NA,nreps * length(importations_list),length(test_list) + 2)
init.testDay[,length(test_list) + 2] = rep(1:length(importations_list), each = nreps)
init.testDay[,length(test_list) + 1] = rep(1:nreps, length(importations_list))

inf.testDay[,length(test_list) + 2] = rep(1:length(importations_list), each = nreps)
inf.testDay[,length(test_list) + 1] = rep(1:nreps, length(importations_list))

inf.time = matrix(NA,nreps*time_max*length(importations_list),length(test_list) + 2)
inf.time[,length(test_list) + 1] = rep(rep(1:nreps, each = time_max), length(importations_list))
inf.time[,length(test_list) + 2] = rep(1:length(importations_list), each = time_max * nreps)

for(pp_import in 1:length(importations_list)){    
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    for(ii_param in 1:length(test_list)){
        print(ii_param)
        source('paramDefaults.R')
        time = 1:time_max

        testdates_type = c('pcr','pcr')
        testdates = c(7,14)
        
        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/time_max, numdrillserg + numStaff))
        
        quarantine.contacts = 0

        ## Change values for testing on exit: 0 test on exit, 1 no test
        BOOL_clinicalrelease = 1 - test_list[ii_param]
        if(scenarios_names[ii_param] == 'Perfect'){
            pcr.sens.commercial.target = 1.0
        }
        
        pcr.sens.commercial = function(dd){
            ## Day here means day relative to symptom onset
            dd = dd - min(sensitivity_time)
            dd[dd > length(sensitivity_array)] = length(sensitivity_array)
            dd[dd < 1] = 1
            return(sens.fun(pcr.sens.commercial.target)[dd])
        }


        pcr.sens.screen = function(dd){
            ## Day here means day relative to symptom onset
            dd = dd - min(sensitivity_time)
            dd[dd > length(sensitivity_array)] = length(sensitivity_array)
            dd[dd < 1] = 1
            return(sens.fun(pcr.sens.screen.target)[dd])
        }

        
        s = replicate(nreps,simOutbreak())
        ##s = replicate(nreps,simOutbreak()[[1]])
        ss_summary = do.call(rbind,s[1,])
        ##ss_summary = matrix(unlist(s[1,]), ncol = nreps)
        ss_time_inf = do.call(rbind, s[2,])

        ind_timeMat = which(inf.time[,ncol(inf.time)] == pp_import)
        inf.time[ind_timeMat,ii_param] = ss_time_inf[,1]

        ind_testMat = which(init.testDay[,ncol(init.testDay)] == pp_import)
        init.testDay[ind_testMat,ii_param] = ss_summary[,1]
        inf.testDay[ind_testMat,ii_param] = ss_summary[,4]
    }
}
inf.testDay = as.data.frame(inf.testDay)
inf.time = as.data.frame(inf.time)

##==================================================#
## Reshape and save----------
##==================================================#
colnames(inf.testDay) = c(scenarios_names, c('rep','importation'))

colnames(inf.time) = c(scenarios_names, c('rep','importation'))
inf.time$time = rep(1:time_max, nreps * length(importations_list))

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep','importation'),
                             value.name = 'infections', variable.name = 'testonexit')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep','importation'),
                           value.name = 'infections', variable.name = 'testonexit')

write.csv(inf.testDay, '../output/output_testonexit_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_testonexit_time_scenarios.csv', row.names = F)

