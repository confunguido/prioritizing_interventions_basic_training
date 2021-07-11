##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
source('simOutbreak.R')
nreps = 1e3
set.seed(123456)

##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##==================================================#
## run sims across different immunity levels----------
##==================================================#
importation_list = c(0,1,10)

time_max = 73

init.testDay = inf.testDay = matrix(NA,nreps,length(importation_list) + 1)
init.testDay[,length(importation_list) + 1] = 1:nreps

inf.testDay[,length(importation_list) + 1] = 1:nreps

inf.time = matrix(NA,nreps*time_max,length(importation_list) + 1)
inf.time[,length(importation_list) + 1] = rep(1:nreps, each = time_max)



for(ii_param in 1:length(importation_list)){
    print(sprintf("Importaion multiplier = %.0f", importation_list[ii_param]))
    source('paramDefaults.R')
    time = 1:time_max
    ## Importations
    importation = c(rep(0, numrecruits), rep(importation_list[ii_param] * 0.01/time_max, numdrillserg + numStaff))
    
    ## Default quarantine contacts = 0
    quarantine.contacts = 0
    ##testdates_type = test_type
    
    ## simulate
    s = replicate(nreps, simOutbreak())
    
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
colnames(inf.testDay) = c(importation_list, c('rep'))

colnames(inf.time) = c(importation_list, c('rep'))
inf.time$time = rep(1:time_max, nreps)

inf.testDay = reshape2::melt(inf.testDay, id.vars = c('rep'),
                             value.name = 'infections', variable.name = 'importation')

inf.time = reshape2::melt(inf.time, id.vars = c('time','rep'),
                           value.name = 'infections', variable.name = 'importation')

write.csv(inf.testDay, '../output/output_importation_scenarios.csv', row.names = F)
write.csv(inf.time, '../output/output_importation_time_scenarios.csv', row.names = F)


