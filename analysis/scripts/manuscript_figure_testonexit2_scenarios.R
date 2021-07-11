##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
library(PropCIs)

inf.testDay = read.csv('../output/output_testonexit2_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_testonexit2_time_scenarios.csv', stringsAsFactors = F)
importation_name = c('None','1%','10%')
importations_list = sort(unique(inf.testDay$importation))
##test_scenarios = sort(unique(inf.testDay$testing))
test_scenarios = c("No", "Arrival", "Arrival_14", "Arrival_7_14", "Arrival_3_5")
test_names = c("None", "1", "1,14", "1,7,14", "1,3,5*")
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
outbreak_min = 100
jpeg('../figures/manuscript_figure_testonexit2_scenarios.jpeg',  width=6.5,height=4, units="in", res = 300)

layout(matrix(1:(2*length(importations_list)),ncol = length(importations_list), byrow = F))
par(mar = c(2,2,2,2), oma = c(2,3,4,0))
pplt = par("plt")
adjx = (0.1 - pplt[1]) / (pplt[2] - pplt[1])
for(pp_import in 1:length(importations_list)){
    n_fig = pp_import * 2 - 2
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~ testing, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])

    outbreak_array = tmp_out
    outbreak_array[outbreak_array < outbreak_min] = 0
    outbreak_array[outbreak_array > outbreak_min] = 1
    
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})

    outbreak_prob_ci = lapply(1:length(outbreak_prob_array),
                function(x){
                    exactci(outbreak_prob_array[x]*nrow(tmp_out),nrow(tmp_out),conf.level = 0.95)
                })
    outbreak_prob_ci = matrix(unlist(outbreak_prob_ci),ncol = 2, byrow = T)
    rownames(outbreak_prob_ci) = names(outbreak_prob_array)
    
    br_prob = barplot(outbreak_prob_array[test_scenarios], yaxt = 'n', xaxt = 'n')
    error.bar(br_prob,outbreak_prob_array[test_scenarios],
              outbreak_prob_ci[test_scenarios,2], outbreak_prob_ci[test_scenarios,1])
    
    axis(side = 1, at = br_prob, labels = test_names,cex.axis = 0.7, las = 1)
    axis(side = 2, las = 2)
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak probability", side = 2, line = 3.5)
    }
    mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))
    print(outbreak_prob_array[test_scenarios])
    
    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min], na.rm = T)})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25, na.rm = T)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75, na.rm = T)})

    ## br_size = barplot(outbreak_size_array[test_scenarios], yaxt = 'n', xaxt = 'n',
    ##                   ylim = c(0,max(outbreak_size_array_high, na.rm = T)))
    ## error.bar(br_size, outbreak_size_array[test_scenarios], outbreak_size_array_high[test_scenarios], outbreak_size_array_low[test_scenarios])

    
    outbreak_out_box = gather(as.data.frame(tmp_out),key = 'scenario',value = 'infections')
    outbreak_out_box$scenario = factor(outbreak_out_box$scenario, levels = test_scenarios)

    br_size = boxplot(infections ~ scenario,data = outbreak_out_box, yaxt = 'n', xaxt = 'n',
            ylim = c(0,max(outbreak_size_array_high, na.rm = T)),lwd = 0.6, frame.plot = F)
    
    ##axis(side = 1, at = br_size, labels = test_names, cex.axis = 0.7, las = 1)
    axis(side = 1, at = seq(from=1,by=1,to=length(br_size$names)), labels = test_names, cex.axis = 0.7, las = 1)
    axis(side = 2, las = 2)
    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak size", side = 2, line = 3.5)
    }

}
##mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 2.5)
mtext("Testing scenario", side = 1, line = 1, outer = T)
dev.off()

