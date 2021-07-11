##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
inf.testDay = read.csv('../output/output_baseline_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_baseline_time_scenarios.csv', stringsAsFactors = F)
scenario_list = sort(unique(inf.testDay$sim_scenario))
scenario_names = str_replace(scenario_list, "No", "No")
scenario_names = str_replace(scenario_list, "Intervention", " interventions")

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
jpeg('../figures/manuscript_figure_baseline_scenarios.jpeg',  width=6.5,height=6, units="in", res = 300)
min_outbreak = 100
layout(matrix(1:4,nrow = 2, byrow = T))
pp_import = 1
par(mar = c(4,4,3,2))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

for(ss in 1:length(scenario_list)){
    n_fig = ss*(ss - 1)
    tmp_out = inf.testDay[inf.testDay$sim_scenario == scenario_list[ss],]
    hist(tmp_out$infections, ylab = "", xlab = "", main = "", breaks = seq(from = 0, to = 1200, by = 50), col = "gray", yaxt = "n")
    axis(2, las = 2)
    abline(v = 100, lty = 3)
    mtext("Total infections", side = 1, line = 2.5)
    mtext("Simulations", side = 2, line = 3)
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

    rep_outbreak = tmp_out$rep[tmp_out$infections > min_outbreak]
    
    tmp_out_time = inf.time[inf.time$sim_scenario == scenario_list[ss],]
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

    sum_tmp_out = tmp_out_time %>% filter(rep %in% rep_outbreak) %>%
        group_by(time) %>%
        summarize(low_infections = quantile(infections, probs = c(0.25)),
                  high_infections = quantile(infections, probs = c(0.75))) %>%
        ungroup() %>%
        arrange(time)

    ##lines(sum_tmp_out$time, sum_tmp_out$low_infections, lty = 3)
    ##lines(sum_tmp_out$time, sum_tmp_out$high_infections, lty = 3)
    axis(2, las = 2)
    text(x=0,y = max(inf.time$infections)*0.9,
         sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f (%.0f-%.0f)",
                 length(rep_outbreak) / nrow(tmp_out),
                 median(tmp_out$infections[tmp_out$infections > min_outbreak]),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25)),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.75))), pos = 4)
    

    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    
    mtext(scenario_names[ss], side = 4, line = 1)
    mtext("New daily infections", side = 2, line = 3)
    mtext("Days", side = 1, line = 2.5)
    ##print(sprintf("%s - Probability of outbreak: %.2f, outbreak size: %.0f", scenario_list[ss], sum(tmp_out$infections > min_outbreak) / nrow(tmp_out), max(inf_timeseries$median_infections)))
}

dev.off()

## Print to screen the value needed in the manuscript

##==================================================#
## Create boxplot figures----------
##==================================================#
jpeg('../figures/manuscript_figure_baseline_scenarios_curvedplots.jpeg',  width=6.5,height=6, units="in", res = 300)
min_outbreak = 100
layout(matrix(1:4,nrow = 2, byrow = T))
pp_import = 1
par(mar = c(4,4,3,2))
pplt = par("plt")
adjx = (0 - pplt[1]) / (pplt[2] - pplt[1])

for(ss in 1:length(scenario_list)){
    n_fig = ss*(ss - 1)
    tmp_out = inf.testDay[inf.testDay$sim_scenario == scenario_list[ss],]
    hist(tmp_out$infections, ylab = "", xlab = "", main = "", breaks = seq(from = 0, to = 1200, by = 50), col = "gray", yaxt = "n")
    axis(2, las = 2)
    abline(v = 100, lty = 3)
    mtext("Total infections", side = 1, line = 2.5)
    mtext("Simulations", side = 2, line = 3)
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)

    rep_outbreak = tmp_out$rep[tmp_out$infections > min_outbreak]
    
    tmp_out_time = inf.time[inf.time$sim_scenario == scenario_list[ss],]
    tmp_df = tidyr::spread(tmp_out_time[,c('time','rep','infections')], key = 'rep', value = 'infections')
    fbplot(as.matrix(tmp_df[,-1]),x = tmp_df$time, method = 'Both',
           xaxt = 'n', yaxt = 'n', ylab = '', xlab = '',
           color = "#a6cee3", barcol ="#1f78b4", outliercol ="#b2df8a",
           xlim = c(1,max(inf_timeseries$time)),
           ylim = c(0, max(inf.time$infections)*1.15))

    sum_tmp_out = tmp_out_time %>% filter(rep %in% rep_outbreak) %>%
        group_by(time) %>%
        summarize(low_infections = quantile(infections, probs = c(0.25)),
                  high_infections = quantile(infections, probs = c(0.75))) %>%
        ungroup() %>%
        arrange(time)

    ##lines(sum_tmp_out$time, sum_tmp_out$low_infections, lty = 3)
    ##lines(sum_tmp_out$time, sum_tmp_out$high_infections, lty = 3)
    axis(1)
    axis(2, las = 2)
    text(x=0,y = max(inf.time$infections)*1.05,
         sprintf("Prob. outbreak: %.2f\nOutbreak size: %.0f (%.0f-%.0f)",
                 length(rep_outbreak) / nrow(tmp_out),
                 median(tmp_out$infections[tmp_out$infections > min_outbreak]),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.25)),
                 quantile(tmp_out$infections[tmp_out$infections > min_outbreak], probs = c(0.75))), pos = 4, cex = 0.8)
    

    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    
    mtext(scenario_names[ss], side = 4, line = 1)
    mtext("New daily infections", side = 2, line = 3)
    mtext("Days", side = 1, line = 2.5)
    ##print(sprintf("%s - Probability of outbreak: %.2f, outbreak size: %.0f", scenario_list[ss], sum(tmp_out$infections > min_outbreak) / nrow(tmp_out), max(inf_timeseries$median_infections)))
}

dev.off()

## Print to screen the value needed in the manuscript
