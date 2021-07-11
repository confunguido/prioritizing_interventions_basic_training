##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
inf.testDay = read.csv('../output/output_calibration_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_calibration_time_scenarios.csv', stringsAsFactors = F)
training_site_list = sort(unique(inf.testDay$training_site))
nreps = max(inf.testDay$rep)
training_site_names = str_replace(training_site_list, 'FB', 'Fort Benning')
training_site_names = str_replace(training_site_names, 'FLW', 'Fort Leonard Wood')

##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length.in=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length.in, ...)
}


##==================================================#
## Positive tests over time figures----------
##==================================================#
cases_data = list(
    FB = data.frame(infections = c(4,142),
                    time = c(1,22)),
    FLW = data.frame(infections = c(0,70),
                     time = c(1,18)))
    
outbreak_min = 1

jpeg('../figures/manuscript_figure_calibration_scenarios.jpeg',  width=6.5,height=4, units="in", res = 300)
layout(matrix(1:length(training_site_list), nrow = 1, byrow = T))
par(mar = c(0,0,0,0), oma = c(5,5,3,3))
tmp_out = inf.time
for(cc in 1:length(training_site_list)){
    tmp_out_scenario = tmp_out[tmp_out$training_site == training_site_list[cc],]
    tmp_data = cases_data[[training_site_list[cc]]]
    tmp_model_data = tmp_out_scenario[tmp_out_scenario$time == max(tmp_data$time),]
    tmp_model_mat = spread(tmp_out_scenario[,c('time','rep','testPositive')],key = rep,value = 'testPositive')
    tmp_model_median = apply(tmp_model_mat,1,median)
    for(nn in 1:nreps){
        tmp_inf = tmp_out_scenario[tmp_out_scenario$rep == nn,]
        tmp_inf = tmp_inf[sort(tmp_inf$time, index.return = T)$ix,]
        if(nn == 1){
            plot(tmp_inf$time, tmp_inf$testPositive, type = "l", col = "#bcbddc15",
                 ylim = c(0,300), xlim = c(1,max(tmp_data$time)), yaxt = 'n', xaxt = 'n')
            mtext(sprintf("%s", training_site_names[cc]), side = 3, line = 1)
            if(cc == 1){
                mtext("Positive tests (if all recruits tested)",side = 2, line = 3)
                axis(2, las = 2)
            }
            axis(1)
        }else{
            lines(tmp_inf$time, tmp_inf$testPositive,   col = "#bcbddc15")
        }            
    }
    lines(tmp_model_mat$time, tmp_model_median, col = 'black', lwd = 2)
    
    cum_inf = aggregate( infections ~ rep, data = tmp_out_scenario, FUN = sum)
    cum_inf = cum_inf[cum_inf$infections > outbreak_min,]
    error.bar(max(tmp_data$time), median(tmp_model_data$testPositive),  quantile(tmp_model_data$testPositive, probs = c(0.75)) ,
              quantile(tmp_model_data$testPositive, probs = c(0.25)), cex = 0.5, col = "black")
    
    points(max(tmp_data$time), median(tmp_model_data$testPositive), pch = 18, cex = 1.5, col = "black")
    points(tmp_data$time, tmp_data$infections, pch = 1, cex = 1.5, col = 'red')
}

mtext("Days since arrival of trainees", side = 1, outer = T, line = 3)
dev.off()

##==================================================#
## curveplots------------------------
##==================================================#
jpeg('../figures/manuscript_figure_calibration_scenarios_curveplots.jpeg',  width=6.5,height=3.5, units="in", res = 300)
layout(matrix(1:length(training_site_list), nrow = 1, byrow = T))
par(mar = c(0,0.5,0,0), oma = c(5,5,3,3))
tmp_out = inf.time
for(cc in 1:length(training_site_list)){
    tmp_out_scenario = tmp_out[tmp_out$training_site == training_site_list[cc],]
    tmp_data = cases_data[[training_site_list[cc]]]
    tmp_model_data = tmp_out_scenario[tmp_out_scenario$time == max(tmp_data$time),]
    tmp_model_mat = spread(tmp_out_scenario[,c('time','rep','testPositive')],key = rep,value = 'testPositive')
    tmp_model_median = apply(tmp_model_mat,1,median)


    fbplot(as.matrix(tmp_model_mat[,-1]),x = tmp_model_mat$time, method = 'Both',
           xaxt = 'n', yaxt = 'n', ylab = '', xlab = '',
           color = "#a6cee3", barcol ="#1f78b4", outliercol ="#b2df8a",
           xlim = c(1,max(tmp_data$time)),
           ylim = c(0, max(tmp_out_scenario$testPositive)*1.15))

    mtext(sprintf("%s", training_site_names[cc]), side = 3, line = 1)
            if(cc == 1){
                mtext("Positive tests (if all recruits tested)",side = 2, line = 3)
                axis(2, las = 2)
            }
            axis(1)
    cum_inf = aggregate( infections ~ rep, data = tmp_out_scenario, FUN = sum)
    cum_inf = cum_inf[cum_inf$infections > outbreak_min,]
    ## error.bar(max(tmp_data$time), median(tmp_model_data$testPositive),  quantile(tmp_model_data$testPositive, probs = c(0.75)) ,
    ##           quantile(tmp_model_data$testPositive, probs = c(0.25)), cex = 0.5, col = "black")
    
    ##points(max(tmp_data$time), median(tmp_model_data$testPositive), pch = 18, cex = 1.5, col = "black")
    points(tmp_data$time, tmp_data$infections, pch = 1, cex = 1.5, col = 'red')
}

mtext("Days since arrival of trainees", side = 1, outer = T, line = 3)
dev.off()
