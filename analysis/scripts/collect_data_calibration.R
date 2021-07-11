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
parameters_df = read_csv(file.path(output_dir_sims,'params_file.csv'))
parameters_df$Finished = 0
combined_output_file = file.path(output_dir, "calibration_sim.csv")
combined_output_df = data.frame()
sims_list = sort(unique(parameters_df$simID))
## For each parameter, determine if it finished or not
for(ii in sims_list){
    ## Check if simulation finished
    out_file = file.path(output_dir_sims,sprintf("calibration_sim_%d.csv",ii))
    if(file.exists(out_file)){
        output_df = read.csv(out_file, stringsAsFactors = F)
        combined_output_df = bind_rows(combined_output_df, output_df)
    }else{
        print(sprintf("File %s doesn't exist", out_file))
    }
}
write_csv(combined_output_df, combined_output_file)
