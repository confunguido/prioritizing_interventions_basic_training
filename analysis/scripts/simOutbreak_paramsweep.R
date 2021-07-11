
# load libraries
library('scam')
library('lhs')
library('foreach')
library('doParallel')

# load functions
source('outbreakfunctions.R')

# number of recruits per cohort, full capacity = 1200
numrecruits = 640

# sample parameter sets using lhs
nSets = 1000
A = randomLHS(nSets,16)
P = matrix(nrow = nSets, ncol = 16)
P[,1] = qunif(A[,1], min = 1.5, max = 6) # R0.mag
P[,2] = qunif(A[,2], min = 3.0, max = 6) # R0.tim
P[,3] = qunif(A[,3], min = 0.2, max = 0.6) # symp.mag
P[,4] = qunif(A[,4], min = 4.0, max = 8) # symp.tim
P[,5] = qunif(A[,5], min = 0.0, max = 0.5) # asymp.adjust
P[,6] = qunif(A[,6], min = 0.01, max = 0.25) # initial.infected
P[,7] = qunif(A[,7], min = 0.75, max = 0.95) # isolate.effectiveness
P[,8] = qunif(A[,8], min = 0.75, max = 0.95) # quarantine.effectiveness
P[,9] = qunif(A[,9], min = 1, max = 20) # quarantine.contacts
P[,10] = qunif(A[,10], min = 0.1, max = 0.6) # mask.protection
P[,11] = qunif(A[,11], min = 0.3, max = 0.8) # mask.compliance
P[,12] = qunif(A[,12], min = 30, max = 70) # cocoon.size
P[,13] = qunif(A[,13], min = 0.05, max = 0.4) # contacts.cocoon
P[,14] = qunif(A[,14], min = 0.005, max = 0.2) # contacts.company
P[,15] = qunif(A[,15], min = 0.001, max = 0.1) # contacts.rand1
# P[,1] = qunif(A[,1], min = 3.5, max = 6) # R0.mag
# P[,2] = qunif(A[,2], min = 3.0, max = 5) # R0.tim
# P[,3] = qunif(A[,3], min = 0.4, max = 0.4) # symp.mag
# P[,4] = qunif(A[,4], min = 7.2, max = 7.2) # symp.tim
# P[,5] = qunif(A[,5], min = 0.5, max = 0.5) # asymp.adjust
# P[,6] = qunif(A[,6], min = 0.1, max = 0.1) # initial.infected
# P[,7] = qunif(A[,7], min = 0.9, max = 0.9) # isolate.effectiveness
# P[,8] = qunif(A[,8], min = 0.9, max = 0.9) # quarantine.effectiveness
# P[,9] = qunif(A[,9], min = 7, max = 7) # quarantine.contacts
# P[,10] = qunif(A[,10], min = 0.3, max = 0.3) # mask.protection
# P[,11] = qunif(A[,11], min = 0.5, max = 0.5) # mask.compliance


# parameter values for control processes
# testing
BOOL_testAfterArrival = 1
testDelayArrival = 1
testDelayQuarantine = 3
testReturn = 1
testdates = c(14)

# specify arrival dates on campus
arrivebase = sample(1:3, numrecruits, replace = T)

# specify delay between infection and PCR positivity
# https://www.medrxiv.org/content/10.1101/2020.04.07.20051474v1
pcr.sens = pmax(
  1e-10,
  1 - c(
    1,
    1,
    0.9,
    0.6,
    0.4,
    0.3,
    0.25,
    0.25,
    0.25,
    0.28,
    0.3,
    0.35,
    0.39,
    0.42,
    0.45,
    0.48,
    0.51,
    0.53,
    0.55,
    0.58,
    0.60
  )
)
pcr.sens = data.frame(sens = pcr.sens, day = 1:length(pcr.sens))
pcr.sens$sens = log(pcr.sens$sens / (1 - pcr.sens$sens))
pcr.sens.scam = scam(sens ~ s(day, bs = 'cv'), data = pcr.sens)
pcr.sens = function(dd) {
  1 / (1 + exp(-predict(pcr.sens.scam, newdata = data.frame(day = dd))))
}
pcr.spec = 0.994



# parallel loop
#ptm = proc.time()

result = foreach (i = 1:nSets, .packages = 'scam', .combine = rbind) %dopar% {
  out = simOutbreakFun(i)
  return(out)
}

#print(proc.time()-ptm)
stopImplicitCluster()


