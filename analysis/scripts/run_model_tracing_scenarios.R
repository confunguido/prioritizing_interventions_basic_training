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
importations_list = c(0,1,10)
quarantine_contact_list = c(0,5,10)

time_max = 73

init.testDay = inf.testDay = matrix(NA,nreps * length(importations_list),length(quarantine_contact_list) + 2)
init.testDay[,length(quarantine_contact_list) + 2] = rep(1:length(importations_list), each = nreps)
init.testDay[,length(quarantine_contact_list) + 1] = rep(1:nreps, length(importations_list))

inf.testDay[,length(quarantine_contact_list) + 2] = rep(1:length(importations_list), each = nreps)
inf.testDay[,length(quarantine_contact_list) + 1] = rep(1:nreps, length(importations_list))

inf.time = matrix(NA,nreps*time_max*length(importations_list),length(quarantine_contact_list) + 2)
inf.time[,length(quarantine_contact_list) + 1] = rep(rep(1:nreps, each = time_max), length(importations_list))
inf.time[,length(quarantine_contact_list) + 2] = rep(1:length(importations_list), each = time_max * nreps)

for(pp_import in 1:length(importations_list)){
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    
    for(ii_param in 1:length(quarantine_contact_list)){
        print(ii_param)
        source('paramDefaults.R')
        time = 1:time_max
        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/time_max, numdrillserg + numStaff))

        ## Change values
        quarantine.contacts = quarantine_contact_list[ii_param]

        ## simulate
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
colnames(inf.testDay) = c(quarantine_contact_list, c('rep','importation'))

colnames(inf.time) = c(quarantine_contact_list, c('rep','importation'))
inf.time$time = rep(1:time_max, nreps * length(importations_list))

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep','importation'),
                              value.name = 'infections', variable.name = 'tracing')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep','importation'),
                           value.name = 'infections', variable.name = 'tracing')

write.csv(inf.testDay, '../output/output_tracing_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_tracing_time_scenarios.csv', row.names = F)

