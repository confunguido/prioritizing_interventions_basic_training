##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
inf.testDay = read.csv('../output/output_importation_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_importation_time_scenarios.csv', stringsAsFactors = F)
importations_list = sort(unique(inf.testDay$importation))
importations_names = c('None', '1%', '10%')
nreps = max(inf.testDay$rep)

##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##==================================================#
## Create figures----------
##==================================================#
jpeg('../figures/manuscript_figure_importation_scenarios.jpeg',  width=6.5,height=2.5, units="in", res = 300)
min_outbreak = 100
layout(matrix(1:3,nrow = 1, byrow = T))
par(mar = c(3,2.5,3,0), oma = c(1,3,2,1))
for(ss in 1:length(importations_list)){
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[ss],]
    rep_outbreak = tmp_out$rep[tmp_out$infections > min_outbreak]
    
    tmp_out_time = inf.time[inf.time$importation == importations_list[ss],]
    for(nn in 1:length(rep_outbreak)){
        inf_timeseries = tmp_out_time[tmp_out_time$rep == rep_outbreak[nn],]
        if(nn == 1){
            plot(inf_timeseries$time, inf_timeseries$infections, type = "l",
                 xlim = c(1,max(inf_timeseries$time)),
                 ylim = c(0, max(inf.time$infections)), yaxt = "n", ylab = "", xlab = "", main = "",
                 col = "#bcbddc80")
        }else{
            lines(inf_timeseries$time, inf_timeseries$infections, col = "#bcbddc05")
        }
    }

    if(length(rep_outbreak) > 0){
        if(length(rep_outbreak) > 5){
            color_reps = sample(1:length(rep_outbreak), size = 5, replace = F)
        }else{
            color_reps = 1:length(rep_outbreak)                
        }
        ## plot random trajectories
        for(nn in 1:length(color_reps)){
            tmp_inf = tmp_out_time[tmp_out_time$rep == rep_outbreak[nn],]
            lines(tmp_inf$time, tmp_inf$infections,   col = nn)
        }
    }
    axis(2, las = 2)
    ##text(x=0,y = max(inf.time$infections)*0.9, sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f", length(rep_outbreak) / nrow(tmp_out), median(tmp_out$infections[tmp_out$infections > min_outbreak])), pos = 4)

    text(x=0,y = max(inf.time$infections)*0.9,
         sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f (%.0f-%.0f)",
                 length(rep_outbreak) / nrow(tmp_out),
                 median(tmp_out$infections[tmp_out$infections > min_outbreak]),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25)),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.75))), pos = 4)
    
    
    mtext(importations_names[ss], side = 3, line = 1)    
    mtext("Days", side = 1, line = 2.5)
    mtext("Trainers and support staff exposed in community", side = 3, line = 0, outer = T)
    
    print(importations_list[ss])
    print(quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25, 0.5, 0.75)))
}

mtext("New daily infections", side = 2, line = 1, outer = T)
dev.off()

## Print to screen the value needed in the manuscript

##==================================================#
## fbplots figures----------
##==================================================#
jpeg('../figures/manuscript_figure_importation_scenarios_curvedplots.jpeg',  width=6.5,height=2.5, units="in", res = 300)
min_outbreak = 100
layout(matrix(1:3,nrow = 1, byrow = T))
par(mar = c(3,2.5,3,0), oma = c(1,3,2,1))
for(ss in 1:length(importations_list)){
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[ss],]
    rep_outbreak = tmp_out$rep[tmp_out$infections > min_outbreak]
    
    tmp_out_time = inf.time[inf.time$importation == importations_list[ss],]
    
    tmp_df = tidyr::spread(tmp_out_time[,c('time','rep','infections')], key = 'rep', value = 'infections')
    fbplot(as.matrix(tmp_df[,-1]),x = tmp_df$time, method = 'BD2',
           xaxt = 'n', yaxt = 'n', ylab = '', xlab = '',
           color = "#a6cee3", barcol ="#1f78b4", outliercol ="#b2df8a",
           xlim = c(1,max(inf_timeseries$time)),
           ylim = c(0, max(inf.time$infections)*1.2),fullout = F)
    axis(1)
    axis(2, las = 2)
    ##text(x=0,y = max(inf.time$infections)*0.9, sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f", length(rep_outbreak) / nrow(tmp_out), median(tmp_out$infections[tmp_out$infections > min_outbreak])), pos = 4)

    text(x=0,y = max(inf.time$infections)*1.1,
         sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f (%.0f-%.0f)",
                 length(rep_outbreak) / nrow(tmp_out),
                 median(tmp_out$infections[tmp_out$infections > min_outbreak]),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25)),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.75))), pos = 4)
    
    
    mtext(importations_names[ss], side = 3, line = 1)    
    mtext("Days", side = 1, line = 2.5)
    mtext("Trainers and support staff exposed in community", side = 3, line = 0, outer = T)
    
    print(importations_list[ss])
    print(quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25, 0.5, 0.75)))
}

mtext("New daily infections", side = 2, line = 1, outer = T)
dev.off()
