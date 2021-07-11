#!/usr/bin/env bash
Rscript ./run_model_calibration_scenarios.R > ./run_model_calibration_scenarios.Rout
Rscript ./run_model_baseline_scenarios.R > ./run_model_baseline_scenarios.Rout
Rscript ./run_model_testing_scenarios.R > ./run_model_testing_scenarios.Rout
Rscript ./run_model_testonexit_scenarios.R > ./run_model_testonexit_scenarios.Rout
Rscript ./run_model_tracing_scenarios.R > ./run_model_tracing_scenarios.Rout
Rscript ./run_model_immunity_scenarios.R > ./run_model_immunity_scenarios.Rout
Rscript ./run_model_immunity_baseline_scenarios.R > ./run_model_immunity_baseline_scenarios.Rout
Rscript ./run_model_facemask_scenarios.R > ./run_model_facemask_scenarios.Rout
Rscript ./run_model_varying_facemask_scenarios.R > ./run_model_varying_facemask_scenarios.Rout
Rscript ./run_model_importation_scenarios.R > ./run_model_importation_scenarios.Rout
