##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
inf.testDay = read.csv('../output/output_varying_facemask_scenarios.csv', stringsAsFactors = F)
inf.time = read.csv('../output/output_varying_facemask_time_scenarios.csv', stringsAsFactors = F)
importations_list = sort(unique(inf.testDay[,ncol(inf.testDay)]))

##==================================================#
## Create figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/figure_manuscript_facemask_scenarios.jpeg',  width=7,height=9, units="in", res = 300)
layout(matrix(1:(2*length(importations_list)),nrow = length(importations_list), byrow = T))

for(pp_import in 1:length(importations_list)){
    tmp_out = output_list[[pp_import]]
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})
    br_prob = barplot(outbreak_prob_array, yaxt = 'n')
    axis(side = 1, at = br_prob, labels = fm_list,cex.axis = 0.7)
    axis(side = 2, las = 2)

    mtext("Probability of outbreak", side = 2, line = 3)

    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min], na.rm = T)})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25, na.rm = T)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75, na.rm = T)})

    br_size = barplot(outbreak_size_array, yaxt = 'n', ylim = c(0,max(outbreak_size_array_high, na.rm = T)))
    error.bar(br_size, outbreak_size_array, outbreak_size_array_high, outbreak_size_array_low)
    axis(side = 1, at = br_size, labels = fm_list, cex.axis = 0.7)
    axis(side = 2, las = 2)
    mtext("Outbreak size", side = 2, line = 3)
    mtext(sprintf("Importation x %.0f", importations_list[pp_import]), side = 4, line = 1)
}
mtext(sprintf("Outbreak > %d infections", outbreak_min), outer = T, line = -3)
mtext("Face-mask compliance", side = 1, line = -2, outer = T)

dev.off()

##==================================================#
## Infections over time figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/figure_varying_facemask_scenarios_time.jpeg',  width=10,height=6, units="in", res = 300)
layout(matrix(1:(length(importations_list) * length(fm_list)),nrow = length(importations_list), byrow = T))
par(mar = c(0,0,0,0), oma = c(5,5,3,3))
for(pp_import in 1:length(importations_list)){
    tmp_out = output_time_list[[pp_import]]
    for(cc in 1:length(fm_list)){
        for(nn in 1:nreps){
            tmp_inf = tmp_out[tmp_out[,length(fm_list) + 1] == nn,cc]
            if(nn == 1){
                plot(tmp_inf, type = "l", col = "#bcbddc05",
                     ylim = c(0,max(tmp_out[,1:length(fm_list)])), yaxt = 'n', xaxt = 'n')
                if(pp_import == 1){
                    mtext(sprintf("facemask: %.2f", fm_list[cc]), side = 3, line = 1)
                }
                if(cc == 1){
                    mtext("Infections",side = 2, line = 2)
                    axis(2, las = 2)
                }
                if(pp_import == length(importations_list)){
                    axis(1)
                }
            }else{
                lines(tmp_inf,   col = "#bcbddc05")
            }

        }
    }
    mtext(sprintf("Importation x %.0f", importations_list[pp_import]), side = 4, line = 1, cex = 0.6)
}
mtext("Days", side = 1, outer = T, line = 3)
dev.off()


