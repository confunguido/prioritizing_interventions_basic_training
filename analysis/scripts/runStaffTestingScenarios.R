##==================================================#
## load function to simulate outbreaks--------------
##==================================================#
source('simOutbreak.R')
nreps = 1e3


##==================================================#
## Functions----------
##==================================================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}

##==================================================#
## run sims across different FM compliance----------
##==================================================#
importations_list = c(0,1,10)
fm_list = c(0,0.2,0.4,0.6,0.8,1.0)
time_max = 84
output_list = list()
output_time_list = list()
for(pp_import in 1:length(importations_list)){
    init.testDay = inf.testDay = matrix(NA,nreps,length(fm_list))
    inf.time = matrix(NA,nreps*time_max,length(fm_list) + 1)
    inf.time[,length(fm_list) + 1] = rep(1:nreps, each = time_max)
    print(sprintf("Importaion multiplier = %.0f", importations_list[pp_import]))
    for(ii_param in 1:length(fm_list)){
        print(ii_param)
        source('paramDefaults.R')
        time = 1:time_max

        ## Importations
        importation = c(rep(0, numrecruits), rep(importations_list[pp_import] * 0.01/84, numdrillserg + numStaff))
        
        quarantine.contacts = 0

        ## Change values masks
        mask.compliance = fm_list[ii_param]
        compliance.avg = mask.protection * mask.compliance
        compliance = rbeta(
            numrecruits+numdrillserg+numStaff,
            compliance.avg * compliance.spread,
            (1 - compliance.avg) * compliance.spread
        )
        
        s = replicate(nreps,simOutbreak())
        ##s = replicate(nreps,simOutbreak()[[1]])
        ss_summary = matrix(unlist(s[1,]), ncol = nreps)
        ss_time_inf = do.call(rbind, s[2,])
        inf.time[,ii_param] = ss_time_inf[,1]
        init.testDay[,ii_param] = ss_summary[1,]
        inf.testDay[,ii_param] = ss_summary[4,]
    }
 
    output_list[[pp_import]] = inf.testDay
    output_time_list[[pp_import]] = inf.time
}

##==================================================#
## Create figures----------
##==================================================#
outbreak_min = 100
jpeg('../figures/figure_facemask_scenarios.jpeg',  width=7,height=9, units="in", res = 300)
layout(matrix(1:(2*length(importations_list)),nrow = length(importations_list), byrow = T))

for(pp_import in 1:length(importations_list)){
    tmp_out = output_list[[pp_import]]
    outbreak_prob_array = apply(tmp_out, 2, function(x){sum(x>outbreak_min)/length(x)})
    br_prob = barplot(outbreak_prob_array, yaxt = 'n')
    axis(side = 1, at = br_prob, labels = fm_list,cex.axis = 0.7)
    axis(side = 2, las = 2)

    mtext("Probability of outbreak", side = 2, line = 3)

    outbreak_size_array = apply(tmp_out, 2, function(x){median(x[x>outbreak_min])})
    outbreak_size_array_low = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.25)})
    outbreak_size_array_high = apply(tmp_out, 2, function(x){quantile(x[x>outbreak_min], probs = 0.75)})

    br_size = barplot(outbreak_size_array, yaxt = 'n', ylim = c(0,max(outbreak_size_array_high)))
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
jpeg('../figures/figure_facemask_scenarios_time.jpeg',  width=10,height=6, units="in", res = 300)
layout(matrix(1:(length(importations_list) * length(fm_list)),nrow = length(importations_list), byrow = T))
par(mar = c(4,4,4,4))
for(pp_import in 1:length(importations_list)){
    tmp_out = output_time_list[[pp_import]]
    for(cc in 1:length(fm_list)){
        for(nn in 1:nreps){
            tmp_inf = tmp_out[tmp_out[,length(fm_list) + 1] == nn,cc]
            if(nn == 1){
                plot(tmp_inf, type = "l", col = "#bcbddc05",
                     ylim = c(0,max(tmp_out[,1:length(fm_list)])),
                     ylab = "Infections", xlab = sprintf("facemask: %.2f", fm_list[cc]))
            }else{
                lines(tmp_inf,   col = "#bcbddc05")
            }
        }
    }
    mtext(sprintf("Importation x %.0f", importations_list[pp_import]), side = 4, line = 1, cex = 0.6)
}

dev.off()


