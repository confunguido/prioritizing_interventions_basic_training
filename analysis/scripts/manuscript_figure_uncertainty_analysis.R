##===============================#
## Figure of uncertainty analysis
## Basictraining COVID-19
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(epiR)
library(ppcor)
library(latex2exp)

##===============================#
## Functions------------
##===============================#
error.bar <- function(x, y, upper, lower, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}
sentence_word <- function(x){
    str_replace(x, "^([a-zA-Z])",toupper(substr(x, 1,1)))
}

##===============================#
## Inputs------------
##===============================#
min_outbreak = 100
sims_out_df = read_csv('../output/output_uncertainty_analysis_scenarios.csv') %>%
    drop_na() %>%
    dplyr::select(-incub.shape_dif)

importation_list = sort(unique(sims_out_df$importation))
param_names_list = list(
    "asymp.adjust" = "rel. infec. of asymp.",
    "R0.mag" = "R0",
    "mask.protection" = "protec. mask and dist.",
    "isolate.length" = "isolation length",
    "pcr.spec.commercial" = "PCR spec.",
    "pcr.sens.commercial.target" = "PCR sens.",
    "symp.mag" = "proportion symp.",
    "symp.mean" = "duration of symp.",
    "initial.infected" = "init. infectious",
    "tost.mu" = "TOST $\\mu$",
    "tost.ke" = "TOST $\\kappa_E$",
    "incub.shape" = "incub. period (shape)",
    "incub.scale"  = "incub. period (scale)")


psa_table_df = data.frame(stringsAsFactors = F)
for(ii in 1:length(importation_list)){
    tmp_out = filter(sims_out_df, importation == importation_list[ii]) %>%
    dplyr::select(-c('rep','importation'))
    tmp_out[,ncol(tmp_out) + 1] = tmp_out[,1]
    tmp_out = tmp_out[,-1]

    psa_table_tmp = epi.prcc(tmp_out)
    psa_table_tmp$parameter = colnames(tmp_out[,-ncol(tmp_out)])
    psa_table_tmp$gamma = psa_table_tmp$est
    psa_table_tmp$importation = importation_list[ii]
    psa_table_df = bind_rows(psa_table_df, psa_table_tmp)
}

colnames(psa_table_df) = sentence_word(colnames(psa_table_df))

psa_table_prob = data.frame(stringsAsFactors = F)
for(ii in 1:length(importation_list)){
    tmp_out = filter(sims_out_df, importation == importation_list[ii]) %>%
        mutate(outbreak = ifelse(total_inf > min_outbreak, 1, 0)) %>%
        dplyr::select(-c('rep','importation', 'total_inf'))
    tmp_out = as.data.frame(tmp_out)
    psa_table_tmp = epi.prcc(tmp_out)
    psa_table_tmp$parameter = colnames(tmp_out[,-ncol(tmp_out)])
    psa_table_tmp$gamma = psa_table_tmp$est
    psa_table_tmp$importation = importation_list[ii]
    psa_table_prob = bind_rows(psa_table_prob, psa_table_tmp)
}

colnames(psa_table_prob) = sentence_word(colnames(psa_table_prob))


##===============================#
## make figure------------
##===============================#
jpeg('../figures/manuscript_figure_uncertainty_analysis.jpeg', width=7,height=3.5, units="in", res = 300)
col_palette = brewer.pal(n = 7, name='Greys')

layout(matrix(c(rep(1,2),2:3),nrow = 2,ncol = 2, byrow = T), heights = c(1,2))
par(oma = c(3,10,0,1))
par(mar = c(0,1,0,1))
importation_name = c('None','1%','10%')
plot(0,0, lwd = 0, xaxt = "n", yaxt = "n", bty = "n")

mtext("Trainers and support staff exposed in community", side = 3, outer = T, line = 0.5)

tmp_df = psa_table_df %>%
    dplyr::select(Parameter, Importation,  Gamma) %>%
    spread(key = Parameter, value = Gamma) %>%
    column_to_rownames("Importation")
tmp_df = as.matrix(tmp_df[as.character(importation_list),])
legend_names = importation_name

legend("center", (legend_names), fill = rev(col_palette[1:nrow(tmp_df)]), cex = 1.2,box.lwd = 0.2, horiz = T )

par(mar = c(2,1,0,1))
tmp_df = psa_table_df %>%
    dplyr::select(Parameter, Importation,  Gamma) %>%
    spread(key = Parameter, value = Gamma) %>%
    column_to_rownames("Importation")
tmp_df = as.matrix(tmp_df[as.character(importation_list),])
psa_barplot <- barplot(tmp_df, beside = T, xaxt = "n", col = col_palette[1:nrow(tmp_df)],horiz = T, yaxt = "n",
                       xlim = c(min(tmp_df)*1.1,max(tmp_df)*1.1))
##axis(1,las = 2, at = psa_barplot[2,], labels = colnames(tmp_df), cex.axis = 0.6)
axis(1)
axis(2, las = 2, at = psa_barplot[2,], labels = TeX(as.character(param_names_list[colnames(tmp_df)])), cex.axis = 0.7)

##axis(2, las = 2, at = psa_barplot[2,], labels = colnames(tmp_df), cex.axis = 0.9)

mtext("Outbreak size", side = 3, cex = 0.8)

tmp_df = psa_table_prob %>%
    dplyr::select(Parameter, Importation,  Gamma) %>%
    spread(key = Parameter, value = Gamma) %>%
    column_to_rownames("Importation")
tmp_df = as.matrix(tmp_df[as.character(importation_list),])
psa_barplot <- barplot(tmp_df, beside = T, xaxt = "n", col = col_palette[1:nrow(tmp_df)],horiz = T, yaxt = "n")
##axis(1,las = 2, at = psa_barplot[2,], labels = colnames(tmp_df), cex.axis = 0.6)
axis(1)

mtext("Probability of outbreak", side = 3, cex = 0.8)


## PLOT HERE THE SENSITIVITY ANALYSIS OVER PROB. OUTBREAK 
mtext(TeX('Partial rank correlation coefficient, $\\gamma$'), side = 1, outer = T, line = 1)

dev.off()


