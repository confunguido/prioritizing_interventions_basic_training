##==================================================#
## Load libraries----------------------
##==================================================#
library(reshape2)
library(tidyverse)
set.seed(123456)

##==================================================#
## Functions----------
##==================================================#
sample_normal <- function(n_r, med_in, low_in, high_in){
    y = c(med_in - low_in, high_in - med_in)
    
    opt_res = optimize(function(sigma_in){
              x = 1.96*c(sigma_in, sigma_in)
              return(sum((x -y )^2))
    },
    interval = c(min(y),max(y)))
    return(rnorm(n_r,mean = med_in, sd = opt_res$minimum))
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

##=============================================#
## Split simulations----------------------
##=============================================#
nreps = 50000
reps = 75

output_dir = '../output_experiments'
## Remove previous simulation outputs
if(!dir.exists(output_dir)){
    dir.create(output_dir)
}
system(sprintf('rm %s/uncertainty_sim_*.csv', output_dir,sep = ''))


##==================================================#
##Get the distributions----------
##==================================================#
## Use beta distribution for proportions
## Use normal for not proportions
source('paramDefaults.R')

R0_dist = read.csv('../output/calibrated_R0_distribution.csv')
R0_dist = subset(R0_dist, site == 'FB')
R0_dist$R0_fit = R0_dist$R0_fit - (median(R0_dist$R0_fit) - R0.mag)
R0_dist$R0_fit[R0_dist$R0_fit <= 0] = 0.01
scenarios_list = data.frame(rep = 1:nreps,stringsAsFactors = F)
scenarios_list$symp.mean = round(sample_normal(nreps,10,8,11))
scenarios_list$isolate.length = round(sample_normal(nreps,10,7,14))

## Beta distributions
scenarios_list$symp.mag = sample_beta(nreps,0.57,0.54,0.6) # kasper et al 14
scenarios_list$pcr.spec.commercial = sample_beta(nreps,0.989,0.974,0.998) # Butler-Laporte 28
scenarios_list$pcr.sens.commercial.target = sample_beta(nreps,0.848,0.768,0.924) # Butler-Laporte 28
scenarios_list$mask.protection = sample_beta(nreps,0.3,0.2,0.5) # Payne et al 15
scenarios_list$asymp.adjust = sample_beta(nreps,0.8,0.5,1.0) # Assumed

scenarios_list$R0.mag = sample(R0_dist$R0_fit, nreps, replace = T)
scenarios_list$tost.ke = sample_normal(nreps,2.8,2.1,3.49)
scenarios_list$tost.mu = sample_beta(nreps,0.46,0.36,0.58)
## Sample difference between ke and shape

scenarios_list$incub.shape_dif = sample_gamma(nreps,3,1.485,10.375)
scenarios_list$incub.shape = scenarios_list$tost.ke + scenarios_list$incub.shape_dif
scenarios_list$incub.scale = sample_gamma(nreps,0.948,0.368,1.696)

scenarios_list$simID = sort(rep(1:reps,length.out = nreps))

write_csv(scenarios_list, '../output_experiments/uncertainty_params_file.csv')

##=============================================================================##
## Submit to CRC array---------------------------
##=============================================================================##
cores_total = 4
jobname = "COVID_DOD_UNCERTAINTY"
submission_template = "#!/bin/csh
#$ -q long
#$ -t 1:JOBSQUEUE:1
#$ -N JOBNAME
#$ -pe smp JOBCORES

Rscript run_model_uncertainty_analysis.R ${SGE_TASK_ID}
"    
submission_str = submission_template %>%
    str_replace_all(pattern="JOBNAME", replacement = jobname) %>%
    str_replace_all(pattern="JOBSQUEUE", replacement = as.character(reps)) %>%
    str_replace_all(pattern="JOBCORES", replacement = as.character(cores_total))

submission_file = sprintf("%s.sh",jobname)
file.connection = file(submission_file)
write(submission_str,file.connection)
close(file.connection)

system(sprintf("qsub %s", submission_file))
