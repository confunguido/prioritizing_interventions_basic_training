##==================================================#
## load files--------------
##==================================================#
library(reshape2)
inf.testDay = read.csv('../output/output_tracing_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_tracing_time_scenarios.csv', stringsAsFactors = F)
importation_name = c(0,1,10)
importations_list = sort(unique(inf.testDay$importation))
quarantine_contact_list = sort(unique(inf.testDay$tracing))
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
jpeg('../figures/manuscript_figure_tracing_scenarios.jpeg',  width=7,height=9, units="in", res = 300)

layout(matrix(1:(2*length(importations_list)),nrow = length(importations_list), byrow = T))
par(mar = c(6,4,3,2))

for(pp_import in 1:length(importations_list)){
    tmp_out = inf.testDay[inf.testDay$importation == importations_list[pp_import],]
    tmp_out = as.matrix(dcast(tmp_out,rep ~ tracing, value.var = 'infections')[,-which(colnames(tmp_out) == 'rep')])
    
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})

    br_prob = barplot(outbreak_prob_array, yaxt = 'n')
    ##axis(side = 1, at = br_prob, labels = quarantine_contact_list,cex.axis = 1.0, las = 1)
    axis(side = 2, las = 2)

    mtext("Probability of outbreak", side = 2, line = 3)

    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min])})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75)})

    br_size = barplot(outbreak_size_array, yaxt = 'n',  ylim = c(0,max(outbreak_size_array_high)))

    error.bar(br_size, outbreak_size_array, outbreak_size_array_high, outbreak_size_array_low)
    ##axis(side = 1, at = br_size, labels = quarantine_contact_list, cex.axis = 1.0, las = 1)
    axis(side = 2, las = 2)
    mtext("Outbreak size", side = 2, line = 3)
    mtext(sprintf("Importation x %.0f", importation_name[importations_list[pp_import]]), side = 4, line = 3)    
}
mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Contacts quarantined", side = 1, line = -2, outer = T)

dev.off()


##==================================================#
## Infections over time figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/manuscript_figure_tracing_scenarios_time.jpeg',  width=10,height=6, units="in", res = 300)
layout(matrix(1:(length(importations_list) * length(quarantine_contact_list)),nrow = length(importations_list), byrow = T))
par(mar = c(0,0,0,0), oma = c(5,5,3,3))
for(pp_import in 1:length(importations_list)){
    tmp_out = inf.time[inf.time$importation == importations_list[pp_import],]    
    for(cc in 1:length(quarantine_contact_list)){
        tmp_out_scenario = tmp_out[tmp_out$tracing == quarantine_contact_list[cc],]
        for(nn in 1:nreps){
            tmp_inf = tmp_out_scenario[tmp_out_scenario$rep == nn,]
            if(nn == 1){
                plot(tmp_inf$time, tmp_inf$infections, type = "l", col = "#bcbddc80",
                     ylim = c(0,max(tmp_out$infections)), yaxt = 'n', xaxt = 'n')
                if(pp_import == 1){
                    mtext(sprintf("tracing: %.0f", quarantine_contact_list[cc]), side = 3, line = 1)
                }
                if(cc == 1){
                    mtext("Infections",side = 2, line = 3)
                    axis(2, las = 2)
                }
                if(pp_import == length(importations_list)){
                    axis(1)
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
    mtext(sprintf("Importation x %.0f", importation_name[importations_list[pp_import]]), side = 4, line = 1, cex = 0.6)    
}
mtext("Days", side = 1, outer = T, line = 3)
dev.off()
