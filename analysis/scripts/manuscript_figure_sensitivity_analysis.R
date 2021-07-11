##===============================#
## School reopening cases/deaths
## FRED-COVID19
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)


##===============================#
## Inputs------------
##===============================#
min_outbreak = 100
inf.testDay = read_csv('../output/output_sensitivity_analysis_scenarios.csv') %>%
    group_by(importation, param, param_value) %>%
    summarize(prob_outbreak = sum(total_inf > min_outbreak) / n(),
              infections_median = median(total_inf[total_inf > min_outbreak], na.rm = T)) %>%
    ungroup() %>%
    rename(infections = infections_median) %>%
    group_by(importation, param) %>%
    mutate(param_status = ifelse(param_value == min(param_value), 'min', 'max')) %>%
    ungroup() 


importation_name = c('None','1%','10%')
importation_list = c(0,1,10)
param_names_list = list(
    "asymp.adjust" = "rel. infec. of asymp.",
    "R0.mag" = "R0",
    "symp.mean" = "duration of symp.",
    "mask.protection" = "protec. mask and dist.",
    "isolate.length" = "isolation length",
    "pcr.spec.commercial" = "PCR spec.",
    "pcr.sens.commercial.target" = "PCR sens.",
    "symp.mag" = "proportion symp.",
    "initial.infected" = "init. infectious",
    "initial.immune" = "init. immune",
    "mask.compliance" = "compl. mask and dist.",
    "asymp.adjust" = "rel. infec. of asymp.",
    "tost.mu" = "TOST $\\mu$",
    "tost.ke" = "TOST $\\kappa_E$",
    "incub.shape" = "incub. period (shape)",
    "incub.scale"  = "incub. period (scale)",
    "genint_shape" = "generation int. (shape)",
    "genint_scale" = "generation int. (scale)")


##===============================================================#
## 1. Create a tornado plot -------------
##===============================================================#
col_palette = brewer.pal(n = 7, name='Greys')
jpeg('../figures/manuscript_figure_sensitivity_analysis.jpeg', width=6.5,height=5, units="in", res = 300)
layout(
    matrix(1:6,2,3, byrow = F), widths = c(rep(2,2),2), heights = rep(1,1)
)

par(mar = c(3.5,0.5,1.1,0.5), oma = c(3,7.0,3,1.5))
for(pp in 1:length(importation_list)){
    pp_import = importation_list[pp]
    default_inf_value = inf.testDay$infections[inf.testDay$param == 'default.params' & inf.testDay$importation == pp_import]

    default_prob_value = inf.testDay$prob_outbreak[inf.testDay$param == 'default.params' & inf.testDay$importation == pp_import]

    tmp_inf_df = inf.testDay[inf.testDay$importation == pp_import & inf.testDay$param != 'default.params',c('param', 'param_value', 'infections', 'param_status', 'prob_outbreak')]
    tmp_inf_df$infections = tmp_inf_df$infections - default_inf_value
    tmp_inf_df$prob_outbreak = tmp_inf_df$prob_outbreak - default_prob_value

    min_sens_df = tmp_inf_df[tmp_inf_df$param_status == 'min',]
    min_sens_df = min_sens_df[order(min_sens_df$param),]
    max_sens_df = tmp_inf_df[tmp_inf_df$param_status == 'max',]
    max_sens_df = max_sens_df[order(max_sens_df$param),]

    bb = barplot(height = min_sens_df$infections, horiz = T, col = col_palette[1], xaxt = "n",
                 xlim = c(-500,500))
                 ##xlim = max(tmp_inf_df$infections)*c(-1.2,1.2))

    barplot(height = max_sens_df$infections, horiz = T, col = col_palette[2], xaxt = "n", add = T)
    axis(1)
    mtext(importation_name[pp], side = 3, line = 1)
    if(pp == 1){
        axis(2, las = 2, at = bb, labels = as.character(param_names_list[min_sens_df$param]), cex.axis = 0.7)
    }

    
    if(pp == 2){
        mtext('Change in infections from baseline scenario', side = 1, line =2.5, cex = 0.7)
    }
    ##par(mar = c(0.5,3.5,1.1,3.0))
    ## probability of outbreak
    bb = barplot(height = min_sens_df$prob_outbreak, horiz = T, col = col_palette[1], xaxt = "n",
                 xlim = c(-0.8, 0.8))
    ##xlim = max(tmp_inf_df$prob_outbreak)*c(-1.05,1.05))

    barplot(height = max_sens_df$prob_outbreak, horiz = T, col = col_palette[2], xaxt = "n", add = T)
    axis(1)
    if(pp == 1){
        axis(2, las = 2, at = bb, labels =  as.character(param_names_list[min_sens_df$param]), cex.axis = 0.7)
    }
    if(pp == 3){
        legend("topright", c('min', 'max'), fill = col_palette[1:2], cex = 1.2,box.lwd = 0.2, horiz = F)
    }

    ##axis(2, las = 2, at = bb, labels = min_sens_df$param, cex.axis = 0.9)
    
    mtext(sprintf('Change in outbreak probability from baseline scenario'), side = 1, line =0, cex = 0.7, outer = T)
}
mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 1.5)
dev.off()


