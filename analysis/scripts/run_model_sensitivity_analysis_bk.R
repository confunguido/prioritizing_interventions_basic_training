##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(tidyverse)
source('simOutbreak.R')
nreps = 1e2


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
calibrated_R0 = read.csv('../output/calibrated_parameters_forecast.csv')
R0_interval = mean(calibrated_R0$R0_mag) + c(-1,1)
scenarios_list = data.frame(
    R0.mag = c(min(R0_interval), max(R0_interval)),
    R0.tim = c(4.2, 6.2),
    symp.mag = c(0.54, 0.6),
    symp.tim = c(4.8, 6.8),
    symp.mean = c(8,11),
    asymp.adjust = c(0.5, 1.0),
    pcr.sens.commercial.target = c(0.768, 0.924),
    pcr.spec.commercial = c(0.974, 0.998),
    pcr.sens.screen.target = c(0.407, 0.987),
    pcr.spec.screen = c(0.884, 0.999),
    isolate.length = c(7,14),
    mask.protection = c(0.2, 0.5),
    initial.infected = c(0.001,0.05),
    default.params = c(1.0,1.0),
    stringsAsFactors = F
)
scenarios_list = reshape2::melt(scenarios_list, value.name = 'param_value', variable.name = 'param')
scenarios_list$param = as.character(scenarios_list$param)

time_max = 73
sim_output_scenarios_list = list()
for(pp_import in 1:length(importations_list)){
    tmp_scenarios_df = scenarios_list
    tmp_scenarios_df$importation = importations_list[pp_import]
    
    inf.testDay = matrix(NA,nrow = nrow(scenarios_list), ncol = nreps)
    colnames(inf.testDay) = 1:nreps
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    for(ii_param in 1:nrow(tmp_scenarios_df)){
        print(ii_param)
        source('paramDefaults.R')
        
        time = 1:time_max
        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/time_max, numdrillserg + numStaff))
        
        ## Default quarantine contacts = 0
        quarantine.contacts = 0

        ## Change values
        param_change = tmp_scenarios_df$param[ii_param]
        assign(param_change, tmp_scenarios_df$param_value[ii_param])
        if(param_change %in% c('pcr.sens.screen.target', 'pcr.spec.screen')){            
            testdates = c(3,5)
            testdates_type = c('antigen','antigen')
        }
        if(param_change %in% c('R0.tim', 'pcr.sens.screen.target', 'pcr.sens.commercial.target')){
            pcr.sens.commercial = function(dd){
                ## Day here means day relative to symptom onset
                dd = dd - min(sensitivity_time)
                dd[dd > length(sensitivity_array)] = length(sensitivity_array)
                pmin(1, (pcr.sens.commercial.target / pcr.sens.avg) * sensitivity_array[dd])
            }
            
            pcr.sens.screen = function(dd){
                ## Day here means day relative to symptom onsetw
                dd = dd - min(sensitivity_time)
                dd[dd > length(sensitivity_array)] = length(sensitivity_array)
                pmin(1, (pcr.sens.screen.target / pcr.sens.avg) * sensitivity_array[dd])
            }            
        }
        if(param_change %in% c('symp.mag', 'asymp.adjust', 'R0.mag')){
            R0.prob = R0.mag / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)
        }
        if(param_change == 'mask.protection'){
            compliance.avg = mask.protection * mask.compliance
            compliance.spread = 1e1
            compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)
        }        
        ## simulate
        s = replicate(nreps,simOutbreak())
        ss_summary = do.call(rbind,s[1,])

        inf.testDay[ii_param,] = ss_summary[,4]
        sim_output_scenarios_list[[pp_import]] = gather(cbind(tmp_scenarios_df, inf.testDay), key = rep, value = infections, -c('param', 'param_value', 'importation'))
    }
}
sim_output_df = do.call(rbind, sim_output_scenarios_list)
write.csv(sim_output_df, '../output/output_sensitivity_analysis_scenarios.csv', row.names = F)
