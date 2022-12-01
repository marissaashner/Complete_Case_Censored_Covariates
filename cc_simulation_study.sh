#!/bin/bash

# make directory for output files
mkdir -p simulation_output

# load R
module load r/4.1.0 

# variables including command line arguments
i=1000     # number of simulations
n=1200      # sample size
p=2       # number of Z covariates
s=2       # sigma2
b="1r-2r0.5"        # true beta
v="0r0r0"        # starting values
m="linear" # m function
c="0.25r0.5r0.75" # censoring rates
w="indep_holds"
 
job_call="Rscript R/simulation_study_cc.R "$i" "$n" "$p" "$b" "$v" "$m" "$c" "$w" "$s" ./simulation_output/simulation_numsims"$i"_n"$n"_p"$p"_sigma2-"$s"_"$m"_truebeta-"$b"_startingvals-"$v"_censoringrates-"$c"_censoringmech-"$w"
		sbatch -t 72:00:00  --ntasks=24 --mem=30g --wrap="$job_call"
