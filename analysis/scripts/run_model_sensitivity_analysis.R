##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(BayesianTools)
source('simOutbreak.R')
nreps = 100

##==================================================#
## Functions----------
##==================================================#

##==================================================#
##Get the distributions----------
##==================================================#
source('paramDefaults.R')
calibrated_R0 = read.csv('../output/calibrated_parameters_forecast.csv')
R0_interval = mean(calibrated_R0$R0_mag) + c(-1,1)

scenarios_range = data.frame(
    R0.mag = c(min(R0_interval), max(R0_interval)),
    initial.infected = c(0.00086,0.022),
    mask.compliance = c(0.1,0.5),
    initial.immune = c(0.018, 0.033),
    incub.scale = c(0.368, 1.696),
    incub.shape = c(3.58,13.865),
    symp.mean = c(8,11),
    symp.mag = c(0.54, 0.6),
    genint_shape = c(1.7,4.7),
    genint_scale = c(4.6,6.9),
    pcr.sens.commercial.target = c(0.547, 0.994),
    pcr.spec.commercial = c(0.992, 0.999),
    mask.protection = c(0.2, 0.5),    
    isolate.length = c(7,14),
    asymp.adjust = c(0.5, 1.0),
    default.params = c(1.0,1.0),
    stringsAsFactors = F
)

scenarios_list = scenarios_range[rep(1:nrow(scenarios_range), times = nreps),]
scenarios_list$rep = rep(1:nreps, each = nrow(scenarios_range))

scenarios_list = reshape2::melt(scenarios_list, id.vars = 'rep',value.name = 'param_value', variable.name = 'param')
scenarios_list$sim_rep = 1:nrow(scenarios_list)

output_dir = '../output'

##==================================================#
## Read inputs -------------------
##==================================================#
args = commandArgs(TRUE)
if(length(args) >= 1){
    output_dir = '../output_experiments'
    sim_ID = as.numeric(args[1])
    scenarios_list = read_csv(file.path(output_dir,sprintf('sensitivity_params_file.csv'))) %>%
        dplyr::filter(simID == sim_ID) %>%
        dplyr::select(-simID)    
}

##==================================================#
## Run the simulation -------------------
##==================================================#
importations_list = c(0,1,10)
time_max = 73
sim_output_scenarios_list = list()
for(pp_import in 1:length(importations_list)){
    tmp_scenarios_df = scenarios_list
    tmp_scenarios_df$importation = importations_list[pp_import]
    
    inf.testDay = data.frame(sim_rep = scenarios_list$sim_rep, importation = NA, total_inf = NA)
    
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    inf.testDay$importation = importations_list[pp_import]
    for(i_rep in 1:length(scenarios_list$sim_rep)){
        ii_rep = scenarios_list$sim_rep[i_rep]
        params_tmp = subset(scenarios_list, sim_rep == ii_rep)
        source('paramDefaults.R')
        
        time = 1:time_max
        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/84, numdrillserg + numStaff))
        
        ## Default quarantine contacts = 0
        quarantine.contacts = 0

        ## Change values
        for(xx in 1:nrow(params_tmp)){
            assign(params_tmp$param[xx], params_tmp$param_value[xx])
        }

        ## Change the parameters that depend on others====================#
        k_inc = incub.shape
        gamma = 1 / (k_inc * incub.scale)
        mu = tost.mu
        k_E = tost.ke
        k_I = 1
        alpha = 1
        k_P = k_inc - k_E
        C = k_inc * gamma * mu / (alpha * k_P * mu + k_inc * gamma)

        t_tost = seq(-23,23,0.05)
        fm = alpha * C * (1 - pgamma(-t_tost, shape=k_P, scale=1/(k_inc*gamma)))
        fp = C * (1 - pgamma(t_tost, shape=k_I, scale=1/(k_I*mu)))
        f = ifelse(t_tost>0,fp,fm)
        f = approxfun(t_tost, f)

        t_tost = -22:22
        f_tost = sapply(t_tost,function(t)integrate(f,t,t+1)[[1]])
        f_tost = f_tost / sum(f_tost)
        tost = cbind(t_tost,f_tost)

        compliance.avg = mask.protection * mask.compliance
        compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)

        R0.prob = R0.mag / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)


        genint = pweibull(1:21,scale=genint_scale,shape=genint_shape) - pweibull(0:20,scale=genint_scale,shape=genint_shape)
        genint = genint / sum(genint)

        sensitivity = rbind(
            cbind(
                1:6,
                genint[1:6]/genint[6] * max(sens_draw)),
            cbind(
            (1:(length(sens_draw) - 1))  + 6,
            sens_draw[-1]))
        
        sensitivity_array = sensitivity[,2]
        sensitivity_time = sensitivity[,1]
        
        pcr.sens.commercial = function(dd){
            ## Day here means day relative to symptom onset
            dd[dd > length(sensitivity_array)] = length(sensitivity_array)
            dd[dd < 1] = 1
            return(sens.fun(pcr.sens.commercial.target)[dd])
        }


        pcr.sens.screen = function(dd){
            ## Day here means day relative to symptom onset
            dd[dd > length(sensitivity_array)] = length(sensitivity_array)
            dd[dd < 1] = 1
            return(sens.fun(pcr.sens.screen.target)[dd])
        }
        ## End of parameter changes ====================================#
                
        ## simulate
        s = simOutbreak()
        ss_summary = s[[1]]
        inf.testDay$total_inf[i_rep] = ss_summary[4]
        sim_output_scenarios_list[[pp_import]] = left_join(inf.testDay, scenarios_list, by = 'sim_rep')
    }
}


##=====================================#
## Write output ------------
##=====================================#
##sim_output_scenarios_list[[pp_import]] = gather(cbind(tmp_scenarios_df, inf.testDay), key = rep, value = infections, -c('param', 'param_value', 'importation'))
sim_output_df = do.call(rbind, sim_output_scenarios_list)

if(length(args) >= 1){
    write.csv(sim_output_df, file.path(output_dir, sprintf('output_sensitivity_analysis_sims_%d.csv', sim_ID)), row.names = F)
}else{
    write.csv(sim_output_df, '../output/output_sensitivity_analysis_scenarios.csv', row.names = F)
}
