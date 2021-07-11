##===================================##
## Author: Guido España
##===================================##
## Libraries-------------
##===================================##
library(tidyverse)
output_dir = '../output'
output_dir_sims = '../output_experiments'

##===================================##
## Gather data-------------
##===================================##
parameters_df = read_csv(file.path(output_dir_sims,'sensitivity_params_file.csv'))
combined_output_file = file.path(output_dir, "output_sensitivity_analysis_scenarios.csv")
combined_output_df = data.frame()
sims_list = sort(unique(parameters_df$simID))
## For each parameter, determine if it finished or not
for(ii in sims_list){
    ## Check if simulation finished
    out_file = file.path(output_dir_sims,sprintf("output_sensitivity_analysis_sims_%d.csv",ii))
    if(file.exists(out_file)){
        output_df = read.csv(out_file, stringsAsFactors = F)
        combined_output_df = bind_rows(combined_output_df, output_df)
    }else{
        print(sprintf("File %s doesn't exist", out_file))
    }
}
write_csv(combined_output_df, combined_output_file)
