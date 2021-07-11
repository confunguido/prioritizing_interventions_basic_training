##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
library(reshape2)
library(scam)
library(mgcv)
library(MASS)
nreps = 1e3

min_ess <- function(par, scam_fit, data_in){
    tmp_pred = predict(scam_fit, newdata = data.frame(R0_mag = par[1]))
    return((tmp_pred - data_in) ^ 2)
}

##==================================================#
## Calculate likelihood and save----------
##==================================================#
## select parameter values
source('./paramDefaults.R')
testpos.time = read.csv('../output/calibration_sim.csv', stringsAsFactors = F)
testpos.time$R0_prob = testpos.time$R0_mag * symp.mag + testpos.time$R0_mag * (1 - symp.mag)*asymp.adjust

tmp_df_FB = testpos.time[testpos.time$time == 22,]
tmp_df_FB$outbreak_size = tmp_df_FB$FB
tmp_df_FLW = testpos.time[testpos.time$time == 19,]
tmp_df_FLW$outbreak_size = tmp_df_FLW$FLW

## =================================================#
## Create the figure-------
## =================================================#
jpeg('../figures/manuscript_figure_calibration_R0.jpeg',  width=7,height=4, units="in", res = 300)
layout(matrix(1:2, nrow = 1, byrow = T))
plot(tmp_df_FB$R0_prob, tmp_df_FB$FB,ylab = "Outbreak size", xlab = expression("R"[0]), col = '#C0C0C005', pch = 20, cex = 0.2, yaxt = 'n', xaxt = 'n')
abline(h = 142)
axis(1, cex.axis = 0.8)
axis(2,las = 2)
mtext("Fort Benning", side =3)

plot(tmp_df_FLW$R0_prob, tmp_df_FLW$FLW,ylab = "Outbreak size", xlab = expression("R"[0]), col = '#C0C0C005', pch = 20, cex = 0.2, yaxt = 'n', xaxt = 'n')
axis(1, cex.axis = 0.8)
axis(2,las = 2)
abline(h = 70)

mtext("Fort Leonard Wood", side = 3)
dev.off()

