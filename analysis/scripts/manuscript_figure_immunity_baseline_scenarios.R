##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
library(PropCIs)

inf.testDay = read.csv('../output/output_immunity_baseline_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_immunity_baseline_time_scenarios.csv', stringsAsFactors = F)
importation_name = c('None','1%','10%')
importations_list = sort(unique(inf.testDay$importation))
immunity_list = as.character(sort(unique(inf.testDay$immunity)))
nreps = max(inf.testDay$rep)

##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
    pos.indx = which(upper - lower > 0)
    x = x[pos.indx]
    upper = upper[pos.indx]
    lower = lower[pos.indx]

    arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##==================================================#
## Create figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_immunity_baseline_scenarios.jpeg',  width=6.5,height=4, units="in", res = 300)
layout(matrix(1:(2*length(importations_list)),ncol = length(importations_list), byrow = F))

par(mar = c(2,2,2,2), oma = c(2,3,4,0))
pplt = par("plt")
adjx = (0.1 - pplt[1]) / (pplt[2] - pplt[1])
for(pp_import in 1:length(importations_list)){
    n_fig = pp_import * 2 - 2
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~ immunity, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])
    
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})
    outbreak_prob_ci = lapply(1:length(outbreak_prob_array),
                function(x){
                    exactci(outbreak_prob_array[x]*nrow(tmp_out),nrow(tmp_out),conf.level = 0.95)
                })
    outbreak_prob_ci = matrix(unlist(outbreak_prob_ci),ncol = 2, byrow = T)
    rownames(outbreak_prob_ci) = names(outbreak_prob_array)
    
    br_prob = barplot(outbreak_prob_array,ylim = c(0,max(outbreak_prob_ci[,2])*1.2), yaxt = 'n', xaxt = 'n')
    error.bar(br_prob,outbreak_prob_array[immunity_list],
              outbreak_prob_ci[immunity_list,2], outbreak_prob_ci[immunity_list,1])
    
    axis(side = 1, at = br_prob, labels = immunity_list,cex.axis = 0.6)
    ##axis(1, cex.axis = 0.7)
    axis(side = 2, las = 2)
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak probability", side = 2, line = 3.5)
    }
    mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)

    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min])})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75)})

    ## br_size = barplot(outbreak_size_array, yaxt = 'n', xaxt = 'n',  ylim = c(0,max(outbreak_size_array_high, na.rm = T)))
    ## error.bar(br_size, outbreak_size_array, outbreak_size_array_high, outbreak_size_array_low)
    outbreak_out_box = gather(as.data.frame(tmp_out),key = 'scenario',value = 'infections')
    outbreak_out_box$scenario = factor(outbreak_out_box$scenario, levels = immunity_list)

    br_size = boxplot(
        infections ~ scenario,data = outbreak_out_box,
        ylim = c(0,min(1100,max(outbreak_out_box$infections)*1.2)),
        yaxt = 'n', xaxt = 'n',
        lwd = 0.6, cex = 0.3,
        frame.plot = F)
    
    axis(side = 1, at = 1:length(immunity_list), labels = immunity_list,cex.axis = 0.6)
    axis(side = 2, las = 2)
    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak size", side = 2, line = 3.5)
    }

}
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 2.5)
mtext("Proportion immune upon arrival", side = 1, line = 1, outer = T)

dev.off()


##==================================================#
## Infections over time figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_immunity_baseline_scenarios_time.jpeg',  width=6.5,height=4, units="in", res = 300)
layout(matrix(1:(length(importations_list) * length(immunity_list)),nrow = length(importations_list), byrow = T))
par(mar = c(0.5,0,0.5,0), oma = c(5,5,3,3))
for(pp_import in 1:length(importations_list)){
    tmp_out = inf.time[inf.time$importation == importations_list[pp_import],]    
    for(cc in 1:length(immunity_list)){
        tmp_out_scenario = tmp_out[tmp_out$immunity == immunity_list[cc], ]
        for(nn in 1:nreps){
            tmp_inf = tmp_out_scenario[tmp_out_scenario$rep == nn,]
            if(nn == 1){
                plot(tmp_inf$time, tmp_inf$infections, type = "l", col = "#bcbddc80",
                     ylim = c(0,max(tmp_out$infections, na.rm = T)), yaxt = 'n', xaxt = 'n')
                if(pp_import == 1){
                    mtext(immunity_list[cc], side = 3, line = 0.2, cex = 1.0)
                }
                if(cc == 1){
                    axis(2, las = 2, cex.axis = 0.7)
                }
                if(pp_import == length(importations_list)){
                    axis(1, cex.axis = 0.6, las = 2)
                }
            }else{
                lines(tmp_inf$time, tmp_inf$infections,   col = "#bcbddc80")
            }            
        }
        ## plot random trajectories
        cum_inf = aggregate( infections ~ rep, data = tmp_out_scenario, FUN = sum)
        cum_inf = cum_inf[cum_inf$infections > outbreak_min,]

        if(nrow(cum_inf) > 0){
            if(nrow(cum_inf) > 5){
                color_reps = sample(1:nrow(cum_inf), size = 5, replace = F)
            }else{
                color_reps = 1:nrow(cum_inf)                
            }
            ## plot random trajectories
            for(nn in 1:length(color_reps)){
                tmp_inf = tmp_out_scenario[tmp_out_scenario$rep == cum_inf$rep[nn],]
                lines(tmp_inf$time, tmp_inf$infections,   col = nn)
            }
        }
    }
    mtext(importation_name[importations_list[pp_import]], side = 4, line = 0.5, cex = 0.6)    
}
mtext("Trainers and support staff exposed in community", side = 4, outer = T, line = 1.7)
mtext("New daily infections",side = 2, line = 3, outer = T)
mtext("Days", side = 1, outer = T, line = 3)
mtext("Proportion immune upon arrival", side = 3, line = 1.2, outer = T)
dev.off()



##==================================================#
## Infections over time curvedplots----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_immunity_baseline_scenarios_time_curvedplots.jpeg',  width=6.5,height=4, units="in", res = 300)
layout(matrix(1:(length(importations_list) * length(immunity_list)),nrow = length(importations_list), byrow = T))
par(mar = c(0.5,0,0.5,0), oma = c(5,5,3,3))
for(pp_import in 1:length(importations_list)){
    tmp_out = inf.time[inf.time$importation == importations_list[pp_import],]    
    for(cc in 1:length(immunity_list)){
        tmp_out_scenario = tmp_out[tmp_out$immunity == immunity_list[cc], ]
        tmp_df = tidyr::spread(tmp_out_scenario[,c('time','rep','infections')], key = 'rep', value = 'infections')
        fbplot(as.matrix(tmp_df[,-1]),x = tmp_df$time, method = 'MBD',
               xaxt = 'n', yaxt = 'n', ylab = '', xlab = '',
               color = "#a6cee3", barcol ="#1f78b4", outliercol ="#b2df8a",
               xlim = c(1,max(tmp_out$time)),
               ylim = c(0, max(tmp_out$infections)))
        if(pp_import == 1){
            mtext(sprintf("%s", immunity_list[cc]), side = 3, line = 0.2, cex = 1.0)
        }
        if(cc == 1){
            axis(2, las = 2, cex.axis = 0.7)
        }
        if(pp_import == length(importations_list)){
            axis(1, cex.axis = 0.6, las = 2)
        }        
    }
    mtext(importation_name[importations_list[pp_import]], side = 4, line = 0.5, cex = 0.9)    
}
mtext("Trainers and support staff exposed in community", side = 4, outer = T, line = 1.7)
mtext("New daily infections",side = 2, line = 3, outer = T)
mtext("Days", side = 1, outer = T, line = 3)
mtext("Proportion immune upon arrival", side = 3, line = 1.2, outer = T)
dev.off()


