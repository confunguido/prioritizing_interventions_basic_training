##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(scam)
library(mgcv)
library(MASS)

source('simOutbreak.R')
nreps = 1
sim_ID = -1
output_dir = '../output'
args = commandArgs(TRUE)

if(length(args) < 1){
    set.seed(123456)
}
if(length(args) >= 1){
    output_dir = '../output_experiments'
    sim_ID = as.numeric(args[1])
    set.seed(runif(1,min = 1, max = as.integer(Sys.time())))    
}


##==================================================#
## run sims across different sites----------
##==================================================#
if(sim_ID <= 0){
    R0_sweep = seq(from = 2, to = 20, length.out = 2e5)
    calib_params_df = read_csv('../output/calibrated_parameters_forecast.csv')
    FB_init_sweep = seq(from = calib_params_df$initial_inf_low[calib_params_df$base_name == 'FB'],
                           to = calib_params_df$initial_inf_low[calib_params_df$base_name == 'FB'], length.out = 2e5)
    FLW_init_sweep = seq(from = calib_params_df$initial_inf_low[calib_params_df$base_name == 'FLW'],
                                to = calib_params_df$initial_inf_low[calib_params_df$base_name == 'FLW'], length.out = 2e5)

}else{
    R0_df = read.csv('../output_experiments/params_file.csv')
    R0_sweep = R0_df$R0[R0_df$simID == sim_ID]
    FLW_init_sweep = R0_df$FLW_initial_inf[R0_df$simID == sim_ID]
    FB_init_sweep = R0_df$FB_initial_inf[R0_df$simID == sim_ID] 
}

training_site_list = c('FB', 'FLW')
time_max = 22
imports_df = read.csv('../output/calibrated_imports_site.csv')

testpos.time = expand.grid(Ind = 1:length(R0_sweep), time = 1:time_max, FB = 0, FLW = 0, stringsAsFactors = F)
testpos.time$R0_mag = R0_sweep[testpos.time$Ind]
testpos.time$FLW_init = FLW_init_sweep[testpos.time$Ind]
testpos.time$FB_init = FB_init_sweep[testpos.time$Ind]
for(nn_r in 1:length(R0_sweep)){
    if(nn_r %% 50 == 0){print(nn_r)}
    for(ii_param in 1:length(training_site_list)){        
        training_site = training_site_list[ii_param]
        site_imports = imports_df$imports[imports_df$site == training_site]
        source('paramDefaults.R')
        time = 1:time_max
        R0.prob = R0_sweep[nn_r] / mean(total.contacts) / (symp.mag + (1 - symp.mag) * asymp.adjust)
        if(training_site == 'FB'){
            initial.infected = FB_init_sweep[nn_r]
        }else{
            initial.infected = FLW_init_sweep[nn_r]
        }
        
        ## Importations
        importation = c(rep(0, numrecruits), rep(site_imports, numdrillserg + numStaff))

        BOOL_testDaily = TRUE
        BOOL_testOnArrival = 1
        testdates = c()
        quarantine.contacts = 0

        ## Assume mask compliance = 0
        mask.compliance = 0.2
        compliance.avg = mask.protection * mask.compliance
        compliance = rep(compliance.avg, numrecruits + numdrillserg + numStaff)
        
        s = replicate(nreps,simOutbreak())
        
        ss_summary = matrix(unlist(s[1,]), ncol = nreps)
        ss_time_inf = do.call(rbind, s[2,])
        ss_time_inf$time = rep(time, nreps)
        tmp_testpos = aggregate(numTestPositive ~ time, data = ss_time_inf, FUN = median)
        testpos.time[testpos.time$R0_mag == R0_sweep[nn_r],training_site_list[ii_param]] = tmp_testpos$numTestPositive         
    }
}
file_outsims = ifelse(sim_ID != -1, sprintf('calibration_sim_%.0f.csv',sim_ID),'calibration_sim.csv')
write.csv(testpos.time, file.path(output_dir,file_outsims), row.names = F)
