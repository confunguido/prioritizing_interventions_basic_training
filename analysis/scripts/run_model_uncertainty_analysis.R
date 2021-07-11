##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(BayesianTools)
source('simOutbreak.R')
nreps = 50000

##==================================================#
## Functions----------
##==================================================#
sample_normal <- function(n_r, med_in, low_in, high_in){
    y = c(med_in, med_in - low_in, high_in - med_in)
    
    opt_res = optimize(function(sigma_in){
        xr = rnorm(10000,med_in, sigma_in)
        xr[xr<=0] = sample(xr[xr>0],size = length(xr[xr<=0]), replace = T)
        x = c(median(xr), sort(xr)[c(250,9750)])
        return(sum((x -y )^2))
    },
    interval = c(min(y),max(y)))
    xrfit = rnorm(n_r,mean = med_in, sd = opt_res$minimum)
    xrfit[xrfit<=0] = sample(xrfit[xrfit>0],size = length(xrfit[xrfit<=0]), replace = T)
    return(xrfit)
}

sample_beta <- function(n_r, med_in, low_in, high_in){
    y = c(low_in, high_in)
    opt_res = optimize(function(var_in){
        alpha_t = med_in * (((med_in * (1 - med_in))/var_in) - 1)
        beta_t = alpha_t * (1 - med_in )/med_in
        ##print(sprintf("var_in: %.2f, alpha %.2f, beta: %.2f", var_in, alpha_t, beta_t))
        if(alpha_t > 0 & beta_t > 0){
            fit.l = sum(sort(rbeta(10000,alpha_t,beta_t))[c(250,9750)] - y)^2
            return(fit.l)
        }else{
            return(10000000)
        }
    }, interval = c(0,max(c(high_in - med_in,med_in - low_in))))
    alpha_t = med_in * (((med_in * (1 - med_in))/opt_res$minimum) - 1)
    beta_t = alpha_t * (1 - med_in )/med_in
    print(alpha_t)
    print(beta_t)
    return(rbeta(n_r, alpha_t, beta_t))
}

sample_gamma <- function(n_r, med_in, low_in, high_in){
    y = c(low_in, high_in)
    opt_res = optimize(function(var_in){
        alpha_t = (med_in^2) / var_in
        beta_t = med_in/var_in
        ##print(sprintf("var_in: %.2f, alpha %.2f, beta: %.2f", var_in, alpha_t, beta_t))
        if(alpha_t > 0 & beta_t > 0){
            fit.l = sum(sort(rgamma(10000,alpha_t,beta_t))[c(250,9750)] - y)^2
            return(fit.l)
        }else{
            return(10000000)
        }
    }, interval = c(0,max(c(high_in - med_in,med_in - low_in))))
    alpha_t = (med_in^2) / opt_res$minimum
    beta_t = med_in/opt_res$minimum
    print(alpha_t)
    print(beta_t)
    return(rbeta(n_r, alpha_t, beta_t))
}

##==================================================#
##Get the distributions----------
##==================================================#
## Use beta distribution for proportions
## Use normal for not proportions
## TODO:
## - [X] ADD GENERATION INTERVAL
## - [ ] ADD IMPORTATIONS
## - [X] ADD PREVALENCE OF INFECTIONS
## - [X] CHANGE SENSITIVITY DIST.
load('./mcmc_sensitivity_pcr.RData')
source('paramDefaults.R')
sensitivity_dist = post[,1]
specificity_dist = post[,2]
R0_dist = read.csv('../output/calibrated_R0_distribution.csv')
R0_dist$R0_fit = R0_dist$R0_fit - (median(R0_dist$R0_fit) - R0.mag)
R0_dist$R0_fit[R0_dist$R0_fit <= 0] = 0.01
scenarios_list = data.frame(rep = 1:nreps,stringsAsFactors = F)
scenarios_list$symp.mean = round(sample_normal(nreps,10,8,11))
scenarios_list$isolate.length = round(sample_normal(nreps,10,7,14))

## Beta distributions
scenarios_list$symp.mag = sample_beta(nreps,0.57,0.54,0.6) # kasper et al 14
scenarios_list$pcr.spec.commercial = sample(specificity_dist,nreps, replace = T)
scenarios_list$pcr.sens.commercial.target = sample(sensitivity_dist, nreps, replace = T)
scenarios_list$mask.protection = sample_beta(nreps,0.3,0.2,0.5) # Payne et al 15
scenarios_list$asymp.adjust = sample_beta(nreps,0.8,0.5,1.0) # Assumed

scenarios_list$R0.mag = sample(R0_dist$R0_fit, nreps, replace = T)
scenarios_list$tost.ke = sample_normal(nreps,2.8,2.1,3.49)
scenarios_list$tost.mu = sample_beta(nreps,0.46,0.36,0.58)
## Sample difference between ke and shape

scenarios_list$genint_shape = sample_normal(nreps,2.89,1.7,4.7)
scenarios_list$genint_scale = sample_normal(nreps,5.67,4.6,6.9)

scenarios_list$incub.shape_dif = sample_normal(nreps,3,1.485,10.375)
scenarios_list$incub.shape = scenarios_list$tost.ke + scenarios_list$incub.shape_dif
scenarios_list$incub.scale = sample_normal(nreps,0.948,0.368,1.696)

## Prevalence of infections
params_calibrated_df = read_csv('../output/calibrated_parameters.csv')

scenarios_list$initial.infected = runif(nreps,min = mean(params_calibrated_df$initial_inf_low), mean(params_calibrated_df$initial_inf_high))
    
scenarios_list = reshape2::melt(scenarios_list,id.vars = 'rep', value.name = 'param_value', variable.name = 'param')
scenarios_list$param = as.character(scenarios_list$param)

output_dir = '../output'

##==================================================#
## Read inputs -------------------
##==================================================#
args = commandArgs(TRUE)
if(length(args) >= 1){
    output_dir = '../output_experiments'
    sim_ID = as.numeric(args[1])
    scenarios_list = read_csv(file.path(output_dir,sprintf('uncertainty_params_file.csv'))) %>%
        dplyr::filter(simID == sim_ID) %>%
        dplyr::select(-simID)
    scenarios_list = reshape2::melt(scenarios_list,id.vars = c('rep'),
                                    value.name = 'param_value', variable.name = 'param')
    scenarios_list$param = as.character(scenarios_list$param)
}

##==================================================#
## Run the simulation -------------------
##==================================================#
scenarios_params = tidyr::spread(scenarios_list, key = param, value = param_value)
importations_list = c(0,1,10)
time_max = 73
sim_output_scenarios_list = list()
for(pp_import in 1:length(importations_list)){
    tmp_scenarios_df = scenarios_list
    tmp_scenarios_df$importation = importations_list[pp_import]
    
    inf.testDay = data.frame(rep = scenarios_params$rep, importation = NA, total_inf = NA)
    
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    inf.testDay$importation = importations_list[pp_import]
    for(i_rep in 1:length(scenarios_params$rep)){
        ii_rep = scenarios_params$rep[i_rep]
        params_tmp = subset(scenarios_list, rep == ii_rep)
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
        sim_output_scenarios_list[[pp_import]] = left_join(inf.testDay, scenarios_params, by = 'rep')
    }
}


##=====================================#
## Write output ------------
##=====================================#
##sim_output_scenarios_list[[pp_import]] = gather(cbind(tmp_scenarios_df, inf.testDay), key = rep, value = infections, -c('param', 'param_value', 'importation'))
sim_output_df = do.call(rbind, sim_output_scenarios_list)

if(length(args) >= 1){
    write.csv(sim_output_df, file.path(output_dir, sprintf('output_uncertainty_analysis_sims_%d.csv', sim_ID)), row.names = F)
}else{
    write.csv(sim_output_df, '../output/output_uncertainty_analysis_scenarios.csv', row.names = F)
}
