##=============================================================================##
## Load libraries----------------------
##=============================================================================##
library(tidyverse)
set.seed(123456)

##=============================================================================##
## Split simulations----------------------
##=============================================================================##
reps = 75
simulation_IDs = 1:reps
output_dir = '../output_experiments'
## Remove previous simulation outputs
if(!dir.exists(output_dir)){
    dir.create(output_dir)
}
calib_params_df = read_csv('../output/calibrated_parameters_forecast.csv')

system(sprintf('rm %s/calibration_sim_*.csv', output_dir,sep = ''))
total_reps = 2e5
R0_sweep = seq(from = 2, to = 20, length.out = total_reps)

initial_inf_sweep_fb = runif(total_reps, calib_params_df$initial_inf_low[calib_params_df$base_name == 'FB'],
                             calib_params_df$initial_inf_high[calib_params_df$base_name == 'FB'])

initial_inf_sweep_flw = runif(total_reps, calib_params_df$initial_inf_low[calib_params_df$base_name == 'FLW'],
                             calib_params_df$initial_inf_high[calib_params_df$base_name == 'FLW'])

sim_ids = sort(rep(1:reps,length.out = length(R0_sweep)))

params_df = data.frame(simID = sim_ids, R0 = R0_sweep, FB_initial_inf = initial_inf_sweep_fb,
                       FLW_initial_inf = initial_inf_sweep_flw, stringsAsFactors = F)
write_csv(params_df, '../output_experiments/params_file.csv')

##=============================================================================##
## Submit to CRC array---------------------------
##=============================================================================##
cores_total = 4
jobname = "COVID_DOD_R0_FIT"
submission_template = "#!/bin/csh
#$ -q long
#$ -t 1:JOBSQUEUE:1
#$ -N JOBNAME
#$ -pe smp JOBCORES

Rscript calibrate_model_param_sweep_abm.R ${SGE_TASK_ID}
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
