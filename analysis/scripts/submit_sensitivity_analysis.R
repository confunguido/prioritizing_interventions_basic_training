##==================================================#
## Load libraries----------------------
##==================================================#
library(reshape2)
library(tidyverse)
set.seed(123456)

##=============================================#
## Split simulations----------------------
##=============================================#
nreps = 200
reps = 75

output_dir = '../output_experiments'
## Remove previous simulation outputs
if(!dir.exists(output_dir)){
    dir.create(output_dir)
}
system(sprintf('rm %s/sensitivity_sim_*.csv', output_dir,sep = ''))


##==================================================#
##Get the distributions----------
##==================================================#
## Use beta distribution for proportions
## Use normal for not proportions
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


scenarios_list$simID = sort(rep(1:reps,length.out = nrow(scenarios_list)))

write_csv(scenarios_list, '../output_experiments/sensitivity_params_file.csv')

##=============================================================================##
## Submit to CRC array---------------------------
##=============================================================================##
cores_total = 4
jobname = "COVID_DOD_SENSITIVITY"
submission_template = "#!/bin/csh
#$ -q long
#$ -t 1:JOBSQUEUE:1
#$ -N JOBNAME
#$ -pe smp JOBCORES

Rscript run_model_sensitivity_analysis.R ${SGE_TASK_ID}
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
