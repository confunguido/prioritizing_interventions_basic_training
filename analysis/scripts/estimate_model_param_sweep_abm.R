##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(scam)
library(mgcv)
library(MASS)

output_dir = '../output'

## Gather testpos.time from outputs
testpos.time = read.csv(file.path(output_dir,'calibration_sim.csv'))

##==================================================#
## Get R0 intervals----------
##==================================================#
source('paramDefaults.R')
params_df = read.csv('../output/calibrated_parameters.csv',stringsAsFactors = F)
testpos.time$R0_prob = testpos.time$R0_mag * symp.mag + testpos.time$R0_mag * (1 - symp.mag)*asymp.adjust
tmp_df_FB = testpos.time[testpos.time$time == 22,]
tmp_df_FB$outbreak_size = tmp_df_FB$FB
tmp_df_FLW = testpos.time[testpos.time$time == 19,]
tmp_df_FLW$outbreak_size = tmp_df_FLW$FLW


R0_pred_FB = quantile(subset(tmp_df_FB,outbreak_size==142)$R0_mag,c(0.001,0.5,0.975))
R0_pred_FLW = quantile(subset(tmp_df_FLW,outbreak_size==70)$R0_mag,c(0.001,0.5,0.975))

R0_dist_FB = subset(tmp_df_FB,outbreak_size==142)$R0_mag
R0_dist_FLW =subset(tmp_df_FLW,outbreak_size==70)$R0_mag

init_dist_FB = subset(tmp_df_FB,outbreak_size==142)$FB_init
init_dist_FLW =subset(tmp_df_FLW,outbreak_size==70)$FLW_init

init_pred_FB = quantile(subset(tmp_df_FB,outbreak_size==142)$FB_init, c(0.025,0.5,0.975))
init_pred_FLW =quantile(subset(tmp_df_FLW,outbreak_size==70)$FLW_init, c(0.025,0.5,0.975))


R0_prob_dist_FB = subset(tmp_df_FB,outbreak_size==142)$R0_prob
R0_prob_dist_FLW =subset(tmp_df_FLW,outbreak_size==70)$R0_prob

R0_prob_pred_FB = quantile(subset(tmp_df_FB,outbreak_size==142)$R0_prob,c(0.001,0.005,0.01,0.025,0.5,0.975))
R0_prob_pred_FLW = quantile(subset(tmp_df_FLW,outbreak_size==70)$R0_prob,c(0.001,0.005,0.01,0.025,0.5,0.975))

R0_dist_df = rbind(data.frame(R0_fit = R0_dist_FB, site = 'FB', stringsAsFactors = F), data.frame(R0_fit = R0_dist_FLW, site = 'FLW', stringsAsFactors = F))

params_df$initial_inf[params_df$base_name == 'FB']  = init_pred_FB[2]
params_df$initial_inf_high[params_df$base_name == 'FB']  = init_pred_FB[3]
params_df$initial_inf_low[params_df$base_name == 'FB']  = init_pred_FB[1]

params_df$initial_inf[params_df$base_name == 'FLW']  = init_pred_FLW[2]
params_df$initial_inf_high[params_df$base_name == 'FLW']  = init_pred_FLW[3]
params_df$initial_inf_low[params_df$base_name == 'FLW']  = init_pred_FLW[1]

params_df$R0_mag[params_df$base_name == 'FB']  = R0_pred_FB[2]
params_df$R0_max[params_df$base_name == 'FB']  = R0_pred_FB[3]
params_df$R0_min[params_df$base_name == 'FB']  = R0_pred_FB[1]

params_df$R0_mag[params_df$base_name == 'FLW']  = R0_pred_FLW[2]
params_df$R0_max[params_df$base_name == 'FLW']  = R0_pred_FLW[3]
params_df$R0_min[params_df$base_name == 'FLW']  = R0_pred_FLW[1]

params_df$R0_prob_med = params_df$R0_mag * symp.mag + params_df$R0_mag * (1 - symp.mag)*asymp.adjust
params_df$R0_prob_min = params_df$R0_min * symp.mag + params_df$R0_min * (1 - symp.mag)*asymp.adjust
params_df$R0_prob_max = params_df$R0_max * symp.mag + params_df$R0_max * (1 - symp.mag)*asymp.adjust

params_df_forecast = params_df
params_df_forecast$R0_mag = params_df_forecast$R0_min

params_df_forecast$R0 = params_df_forecast$R0_mag * symp.mag + params_df_forecast$R0_mag * (1 - symp.mag)*asymp.adjust
write.csv(params_df, '../output/calibrated_parameters.csv', row.names = F)
write.csv(params_df_forecast, '../output/calibrated_parameters_forecast.csv', row.names = F)
write.csv(R0_dist_df, '../output/calibrated_R0_distribution.csv', row.names = F)
##plot(tmp_df_FLW$R0_prob, tmp_df_FLW$outbreak_size, col = '#50505020')


