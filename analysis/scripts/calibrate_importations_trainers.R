##=========================================#
## Fit importation rate using
## Estimates of 70-day prevalence
## Author: Guido España
##=========================================#
## Setup-------------------
##=========================================#
library(tidyverse)

##=========================================#
##Read data ----------
##=========================================#
imports_df = read_csv('../data/training_camp_exposure_probs_GA-MO.csv')

##=========================================#
## Estimate rates ----------
##=========================================#
## 1. Calibration
## For calibration, we sampled the average importation rate based on the months
## FB. -> Georgia
fb_df = filter(imports_df, loc == "GA") %>%
    mutate(exposure_daily = exposure_mean / as.numeric(end_date - start_date))

## FLW -> Missouri
flw_df = filter(imports_df, loc == "MO") %>%
        mutate(exposure_daily = exposure_mean / as.numeric(end_date - start_date))


calibration_df = data.frame(site = c('FB', 'FLW'), imports = c(mean(fb_df$exposure_daily), mean(flw_df$exposure_daily)), stringsAsFactors = F)
write_csv(calibration_df, '../output/calibrated_imports_site.csv')
