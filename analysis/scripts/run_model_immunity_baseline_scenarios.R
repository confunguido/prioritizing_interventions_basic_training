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
immunity_list = seq(from = 0.1, to = 0.9, by = 0.1)

time_max = 73

init.testDay = inf.testDay = matrix(NA,nreps * length(importations_list),length(immunity_list) + 2)
init.testDay[,length(immunity_list) + 2] = rep(1:length(importations_list), each = nreps)
init.testDay[,length(immunity_list) + 1] = rep(1:nreps, length(importations_list))

inf.testDay[,length(immunity_list) + 2] = rep(1:length(importations_list), each = nreps)
inf.testDay[,length(immunity_list) + 1] = rep(1:nreps, length(importations_list))

inf.time = matrix(NA,nreps*time_max*length(importations_list),length(immunity_list) + 2)
inf.time[,length(immunity_list) + 1] = rep(rep(1:nreps, each = time_max), length(importations_list))
inf.time[,length(immunity_list) + 2] = rep(1:length(importations_list), each = time_max * nreps)

for(pp_import in 1:length(importations_list)){
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    for(ii_param in 1:length(immunity_list)){
        print(ii_param)
        source('paramDefaults.R')
        time = 1:time_max
        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/time_max, numdrillserg + numStaff))
        
        quarantine.contacts = 0
        
        ## Change values
        initial.immune = immunity_list[ii_param]

        ## simulate
        s = replicate(nreps,simOutbreak())
        ss_summary = do.call(rbind,s[1,])
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
colnames(inf.testDay) = c(immunity_list, c('rep','importation'))

colnames(inf.time) = c(immunity_list, c('rep','importation'))
inf.time$time = rep(1:time_max, nreps * length(importations_list))

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep','importation'),
                              value.name = 'infections', variable.name = 'immunity')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep','importation'),
                           value.name = 'infections', variable.name = 'immunity')

write.csv(inf.testDay, '../output/output_immunity_baseline_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_immunity_baseline_time_scenarios.csv', row.names = F)

