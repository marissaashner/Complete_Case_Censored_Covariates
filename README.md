# Complete Case Analysis for Censored Covariates

## Marissa C. Ashner and Tanya P. Garcia

Code for simulations, tables, and figures for the manuscript *Understanding the implications of a complete case analysis for regression models with a right-censored covariate* (arXiv link: https://arxiv.org/abs/2303.16119).

#### In the `R` subdirectory, you will find the following: 

- `simulation_study_functions_cc.R`: This script defines the functions used to generate data, run a simulation, and report the results for requested simulation settings.
- `main_cc.R`: This script defines the functions used to estimate the parameters and the variance of the parameters using a complete case analysis or an oracle analysis.
- `simulation_study_cc.R`: You can run a simulation study from inside this script. You can change the settings in the script to run with a certain number of simulations, sample size, number of fully observed covariates, true parameter values, starting values, mean function, censoring rate, censoring mechanism, sigma2, and output file name.
- `dag_cc.R`: This script recreates the Directed Acyclic Graph (DAG) that is in the manuscript.
- `cc_tables.R`: This script recreates the simulation results tables that are in the manuscript main text and supplementary materials.

*Note*: These scripts assume your working directory is inside the `R` subdirectory.

#### In the main directory, you will find the following: 

- `cc_simulation_study.sh`: This is a bash script, which includes an example of another way to run `simulation_study_cc.R` from the command line instead of running the simulations inside the R script itself. To use this on the command line, you would edit the parameters in the file, save, and run the command `bash cc_simulation_study.sh` in the terminal.

*Note:* Some syntax in this file may change depending on the environment you are running it in. This is just an example of a bash file based on the computing cluster that was used to run the simulations.

#### In the `simulation_output_may2023` subdirectory, you will find the simulation output files needed to recreate the tables in `R/cc_tables.R`.

#### In the `Figures and Tables` subdirectory, you will find the output files for the tables and figures from `R/cc_tables.R` and `R/cc_dag.R`.

