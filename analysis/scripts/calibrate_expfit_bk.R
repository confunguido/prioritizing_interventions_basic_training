##=========================================#
## Fit the R0 and initial infections to
## data from two outbreaks in Fort Benning
## and Fort Leonard Wood
## Author: Alex Perkins
##=========================================#
## Initial parameters ----------
##=========================================#
library(scam)
## parameter assumptions that calibration is conditional on
R0.tim = 5.2

params_calibrated = data.frame(
    base_name = c('FB', 'FLW'),
    R0 = c(0,0),
    R0_mag = c(0,0),
    initial_inf = c(0,0), stringsAsFactors = F)

##=========================================#
## Specify delay from inf. to pcr pos ------
##=========================================#
## https://www.medrxiv.org/content/10.1101/2020.04.07.20051474v1
pcr.sens = pmax(1e-10,1-c(1,1,0.9,0.6,0.4,0.3,0.25,0.25,0.25,0.28,0.3,0.35,
                          0.39,0.42,0.45,0.48,0.51,0.53,0.55,0.58,0.60))
pcr.sens = data.frame(sens = pcr.sens, day = (1:length(pcr.sens))+(R0.tim-8))
pcr.sens$sens = log(pcr.sens$sens / (1 - pcr.sens$sens))
pcr.sens.scam = scam(sens ~ s(day, bs = 'cv'), data = pcr.sens)
pcr.sens.screen.target = 0.777
pcr.sens.commercial.target = 0.869
pcr.sens = function(dd){
  1 / (1 + exp(-predict(pcr.sens.scam, newdata = data.frame(day = dd))))
}
pcr.sens.avg = mean(pcr.sens(order(dpois(1:14,R0.tim),decreasing=T)[1:7]))
pcr.sens.screen = function(dd){
  pmin(1, pcr.sens.screen.target / pcr.sens.avg * pcr.sens(dd))
}
pcr.sens.commercial = function(dd){
  pmin(1, pcr.sens.commercial.target / pcr.sens.avg * pcr.sens(dd))
}
pcr.spec.screen = 0.975
pcr.spec.commercial = 0.997
max.days = max(
  which(cumsum(pcr.sens.commercial(1:100)) <
    0.99 * sum(pcr.sens.commercial(1:100))))
Se = mean(pcr.sens.commercial(1:max.days))
Sp = pcr.spec.commercial

##=========================================#
## Calibrate fort benning ------
##=========================================#
## Fort Benning
p = seq(0, 1, length.out = 1e6)
Pr = dbinom(4, 640, p * Se + (1 - p) * (1 - Sp))
Pr = Pr / sum(Pr)
prev.init = sum(p * Pr)

p[max(which(cumsum(Pr)<0.025))+1]
p[max(which(cumsum(Pr)<0.975))+1]

print(sprintf("Fort Benning. Init %.2f", prev.init * 100))
params_calibrated[params_calibrated$base_name == 'FB','initial_inf'] = prev.init

##=========================================#
## Calibrate fort leonard wood ------
##=========================================#
## Fort Leonard Wood
p = seq(0, 1, length.out=1e6)
Pr = dbinom(0, 500, p * Se + (1 - p) * (1 - Sp))
Pr = Pr / sum(Pr)
prev.init = sum(p*Pr)
prev.init
p[max(which(cumsum(Pr)<0.025))+1]
p[max(which(cumsum(Pr)<0.975))+1]

print(sprintf("Fort Leonard Wood. initial inf. %f%%", prev.init*100))

params_calibrated[params_calibrated$base_name == 'FLW','initial_inf'] = prev.init


##=========================================#
## Write results ------
##=========================================#
write.csv(params_calibrated, '../output/calibrated_parameters.csv', row.names = F)
