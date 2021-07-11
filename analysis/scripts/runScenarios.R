## load function to simulate outbreaks
source('simOutbreak.R')
nreps = 1e4



# run simulations across different testing days
init.testDay = inf.testDay = matrix(NA,nreps,4)

source('paramDefaults.R')
importation = rep(0,length(importation))
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,1] = s[1,]
inf.testDay[,1] = s[4,]

source('paramDefaults.R')
importation = rep(0,length(importation))
testdates = 3
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,2] = s[1,]
inf.testDay[,2] = s[4,]

source('paramDefaults.R')
importation = rep(0,length(importation))
testdates = c(3,5)
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,3] = s[1,]
inf.testDay[,3] = s[4,]

source('paramDefaults.R')
importation = rep(0,length(importation))
testdates = c(3,5,7)
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,4] = s[1,]
inf.testDay[,4] = s[4,]


## quarantine based on time and no test, just set testDelayquarantine > quarantine.max
source('paramDefaults.R')
importation = rep(0,length(importation))
testDelayQuarantine = quarantine.max + 1
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,4] = s[1,]
inf.testDay[,4] = s[4,]

## quarantine based on time and no test, just set testDelayquarantine > quarantine.max
source('paramDefaults.R')
importation = rep(0,length(importation))
quarantine.contacts = 0
s = replicate(nreps,simOutbreak()[[1]])
init.testDay[,4] = s[1,]
inf.testDay[,4] = s[4,]
