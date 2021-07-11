##==================================================#
## load files--------------
##==================================================#
library(reshape2)
library(tidyverse)
library(fda)
library(PropCIs)

inf.testDay = read.csv('../output/output_testonexit_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_testonexit_time_scenarios.csv', stringsAsFactors = F)

inf.testDay.alt = read.csv('../output/output_alternative_testonexit_scenarios.csv', stringsAsFactors = F)
inf.time.alt = read.csv('../output/output_alternative_testonexit_time_scenarios.csv', stringsAsFactors = F)

importation_name = c('None','1%','10%')
importations_list = sort(unique(inf.testDay$importation))

test_list = as.character(sort(unique(inf.testDay$testonexit)))
names(test_list) = test_list
test_list = test_list[c('None','Imperfect','Perfect')]
test_labels_list = test_list
##test_labels_list = ifelse(test_list == '0', "No test", "Test")
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
jpeg('../figures/manuscript_figure_testonexit_scenarios.jpeg',  width=6.5,height=4, units="in", res = 300)
layout(matrix(1:(2*length(importations_list)),ncol = length(importations_list), byrow = F))
par(mar = c(2,2,2,2), oma = c(2,3,4,0))

pplt = par("plt")
adjx = (0.1 - pplt[1]) / (pplt[2] - pplt[1])
for(pp_import in 1:length(importations_list)){
    n_fig = pp_import * 2 - 2
    
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~testonexit, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])
    
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})
    outbreak_prob_ci = lapply(1:length(outbreak_prob_array),
                function(x){
                    exactci(outbreak_prob_array[x]*nrow(tmp_out),nrow(tmp_out),conf.level = 0.95)
                })
    outbreak_prob_ci = matrix(unlist(outbreak_prob_ci),ncol = 2, byrow = T)
    rownames(outbreak_prob_ci) = names(outbreak_prob_array)
    
    br_prob = barplot(outbreak_prob_array, yaxt = 'n', xaxt = 'n', ylim = c(0,max(outbreak_prob_ci[,2])*1.2))
    error.bar(br_prob,outbreak_prob_array[test_list],
              outbreak_prob_ci[test_list,2], outbreak_prob_ci[test_list,1])

    axis(side = 1, at =br_prob, labels = test_labels_list,cex.axis = 0.7, las = 1)
    ##axis(side = 1, at = br_prob, labels = colnames(tmp_out),cex.axis = 0.7)
    axis(side = 2, las = 2)

    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak probability", side = 2, line = 3.5)
    }
    mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))
    print(outbreak_prob_array)
    
    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min], na.rm = T)})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25, na.rm = T)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75, na.rm = T)})
    outbreak_out_box = gather(as.data.frame(tmp_out),key = 'scenario',value = 'infections')
    outbreak_out_box$scenario = factor(outbreak_out_box$scenario, levels = test_list)

    br_size = boxplot(infections ~ scenario,data = outbreak_out_box, yaxt = 'n', xaxt = 'n',                      
                      lwd = 0.6, cex = 0.3, frame.plot = F, ylim = c(0, max(outbreak_out_box$infections)*1.2))
    
    ## axis(side = 1, at = br_size, labels = fm_list, cex.axis = 0.7)
    axis(side = 1, at = 1:length(test_list), labels = test_labels_list,cex.axis = 0.7, las = 1)
    
    axis(side = 2, las = 2)
    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak size", side = 2, line = 3.5)
    }
}
## mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Testing before exit sick bay", side = 1, line = 0.9, outer = T)
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 2.5)
dev.off()



##==================================================#
##Outbreak prob----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_testonexit_scenarios_outbreak_prob.jpeg',  width=6.5,height=4, units="in", res = 300)

layout(matrix(1:(2*length(importations_list)),ncol = length(importations_list), byrow = F))
par(mar = c(2,2,2,2), oma = c(2,3,4,0))
pplt = par("plt")
adjx = (0.1 - pplt[1]) / (pplt[2] - pplt[1])
for(pp_import in 1:length(importations_list)){
    n_fig = pp_import * 2 - 2
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~testonexit, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])
    
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})
    outbreak_prob_ci = lapply(1:length(outbreak_prob_array),
                function(x){
                    exactci(outbreak_prob_array[x]*nrow(tmp_out),nrow(tmp_out),conf.level = 0.95)
                })
    outbreak_prob_ci = matrix(unlist(outbreak_prob_ci),ncol = 2, byrow = T)
    rownames(outbreak_prob_ci) = names(outbreak_prob_array)
    
    br_prob = barplot(outbreak_prob_array[test_list], yaxt = 'n', xaxt = 'n', ylim = c(0,max(outbreak_prob_ci[,2])*1.2))
    error.bar(br_prob,outbreak_prob_array[test_list],
              outbreak_prob_ci[test_list,2], outbreak_prob_ci[test_list,1])
            
    axis(side = 1, at = br_prob, labels = test_labels_list,cex.axis = 0.7, las = 1)
    axis(side = 2, las = 2)
    
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak probability", side = 2, line = 0.5, outer = T)
    }
    if(pp_import == 3){
        mtext("Specificity: 0.998", side = 4, line = 0.5, cex = 0.8)
    }
    mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))

    tmp_out_alt = inf.testDay.alt[inf.testDay.alt$importation == importations_list[pp_import],]
    tmp_out_alt = as.matrix(dcast(tmp_out_alt,rep ~testonexit, value.var = 'infections')[,-which(colnames(tmp_out_alt) == 'rep')])

    
    outbreak_prob_array = apply(tmp_out_alt, 2, function(x){sum(x>outbreak_min)/length(x)})
    outbreak_prob_ci = lapply(1:length(outbreak_prob_array),
                function(x){
                    exactci(outbreak_prob_array[x]*nrow(tmp_out_alt),nrow(tmp_out_alt),conf.level = 0.95)
                })
    outbreak_prob_ci = matrix(unlist(outbreak_prob_ci),ncol = 2, byrow = T)
    rownames(outbreak_prob_ci) = names(outbreak_prob_array)
    
    br_prob = barplot(outbreak_prob_array[test_list], yaxt = 'n', xaxt = 'n', ylim = c(0,max(outbreak_prob_ci[,2])*1.2))
    error.bar(br_prob,outbreak_prob_array[test_list],
              outbreak_prob_ci[test_list,2], outbreak_prob_ci[test_list,1])
            
    axis(side = 1, at = br_prob, labels = test_labels_list,cex.axis = 0.7, las = 1)
    axis(side = 2, las = 2)
        
    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 3){
        mtext("Specificity: 0.992", side = 4, line = 0.5, cex = 0.8)
    }
    ##mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))
}

##mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 2.5)
mtext("Testing scenario", side = 1, line = 1, outer = T)
dev.off()

##==================================================#
## Outbreak size testing alternative----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_testonexit_scenarios_outbreak_size.jpeg',  width=6.5,height=4, units="in", res = 300)

layout(matrix(1:(2*length(importations_list)),ncol = length(importations_list), byrow = F))
par(mar = c(2,2,2,2), oma = c(2,3,4,0))
pplt = par("plt")
adjx = (0.1 - pplt[1]) / (pplt[2] - pplt[1])
for(pp_import in 1:length(importations_list)){
    n_fig = pp_import * 2 - 2
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~testonexit, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])
    
    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min], na.rm = T)})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25, na.rm = T)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75, na.rm = T)})
    outbreak_out_box = gather(as.data.frame(tmp_out),key = 'scenario',value = 'infections')
    outbreak_out_box$scenario = factor(outbreak_out_box$scenario, levels = test_list)

    br_size = boxplot(infections ~ scenario,data = outbreak_out_box, yaxt = 'n', xaxt = 'n',                      
                      lwd = 0.6, cex = 0.3, frame.plot = F, ylim = c(0, max(outbreak_out_box$infections)*1.2))
    
    ## axis(side = 1, at = br_size, labels = fm_list, cex.axis = 0.7)
    axis(side = 1, at = 1:length(test_list), labels = test_labels_list,cex.axis = 0.7, las = 1)    
    axis(side = 2, las = 2)
    
    mtext(LETTERS[n_fig + 1], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 1){
        mtext("Outbreak size", side = 2, line = 1.0, outer = T)
    }
    if(pp_import == 3){
        mtext("Specificity: 0.998", side = 4, line = 0.5, cex = 0.8)
    }
    mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))

    tmp_out_alt= inf.testDay.alt[inf.testDay.alt$importation == importations_list[pp_import],]
    tmp_out_alt = as.matrix(dcast(tmp_out_alt,rep ~testonexit, value.var = 'infections')[,-which(colnames(tmp_out_alt) == 'rep')])
    
    outbreak_size_array = apply(tmp_out_alt, 2, function(x){median(x[x>outbreak_min], na.rm = T)})
    outbreak_size_array_low = apply(tmp_out_alt, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25, na.rm = T)})
    outbreak_size_array_high = apply(tmp_out_alt, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75, na.rm = T)})
    outbreak_out_box = gather(as.data.frame(tmp_out_alt),key = 'scenario',value = 'infections')
    outbreak_out_box$scenario = factor(outbreak_out_box$scenario, levels = test_list)

    br_size = boxplot(infections ~ scenario,data = outbreak_out_box, yaxt = 'n', xaxt = 'n',                      
                      lwd = 0.6, cex = 0.3, frame.plot = F, ylim = c(0, max(outbreak_out_box$infections)*1.2))
    
    ## axis(side = 1, at = br_size, labels = fm_list, cex.axis = 0.7)
    axis(side = 1, at = 1:length(test_list), labels = test_labels_list,cex.axis = 0.7, las = 1)    
    axis(side = 2, las = 2)
    
    mtext(LETTERS[n_fig + 2], side = 3, line = 0.5, outer = F, cex = 1.0, adj = adjx)
    if(pp_import == 3){
        mtext("Specificity: 0.992", side = 4, line = 0.5, cex = 0.8)
    }
    ##mtext(importation_name[importations_list[pp_import]], side = 3, line = 2.5)
    
    print(sprintf("Importation %s. Probability of outbreak", importation_name[pp_import]))
}

##mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 2.5)
mtext("Testing scenario", side = 1, line = 1, outer = T)
dev.off()

