###########################################
# this script runs the simulation studies #
###########################################

# clear environment
rm(list = ls())

###############################################
###### COMMAND LINE ARGUMENTS FOR CLUSTER #####
###############################################

# This is primarily used when running this simulation script from the command line
## these arguments are inputs to the script

# if using command line, use this, and comment out the args below
args <- commandArgs(TRUE)

# if not using commmand line, use something like this
#args <- c(10, # number of sims
#         100, # sample size
#         1, # number of Z covariates
#         "1r1", # true parameter values, first one is for X, second one for Z
#         "0r0", # starting values for iterative procedure
#         "linear", # mean function
#         0.5, # censoring rate
#         "exog_broken", # censoring mechanism
#         1, # sigma2
#         "test_sims") # output file name

num_sims. <- as.numeric(args[1]) # number of simulations
n. <- as.numeric(args[2]) # sample size
p. <- as.numeric(args[3]) # number of Z covariates (including the intercept)
true_beta. <- as.numeric(strsplit(args[4], "r")[[1]]) # true value for beta entered as "r" separated list
starting_vals. <- as.numeric(strsplit(args[5], "r")[[1]]) # starting values entered as "r" separated list
m_func. <- as.character(args[6])
censoring_rate. <- as.numeric(strsplit(args[7], "r")[[1]]) # censoring rates entered as "r" separated list
censoring_mechanism. <- as.character(args[8])
sigma2. <- as.numeric(args[9])
output_file <- as.character(args[10]) # output file name


###############################################
###### packages and functions needed ##########
###############################################

# need functions from main_cc.R and simulation_study_functions_cc.R
source("R/main_cc.R")
source("R/simulation_study_functions_cc.R")

# packages needed
library(tidyverse)
library(numDeriv)
library(parallel)
library(xtable)


#####################################
###### Running the simulation #######
#####################################

# Defining the mean function
# some examples, but any mean function can be used

if(m_func. == "linear"){
  m. = function(beta, X, Z){
    if(length(Z)>1 & is.null(dim(Z))){
      c(X,Z)%*%beta
    }else{
      cbind(X,Z)%*%beta
    }
  }
}else if(m_func. == "log"){
  m. = function(beta, X, Z){
    log(cbind(X,Z)%*%beta)
  }
}else if(m_func. == "logit"){
  m. = function(beta, X, Z){
    exp(cbind(X,Z)%*%beta)/(1+exp(cbind(X,Z)%*%beta))
  }
}else if(m_func. == "logistic"){
  m. = function(beta, X, Z){
    # logistic function is L / (1 + e^(-k(x-x0)))
    # L is the maximum value (1 right now)
    # k is the steepness (1 right now)
    # x0 is the sigmoid point (negative intercept)
    if(length(Z)>1 & is.null(dim(Z))){
      1/(1+exp(-5*(c(X,Z)%*%beta)))
    }else{
      1/(1+exp(-5*(cbind(X,Z)%*%beta)))
    }
     #+ ifelse(ncol(Z) > 1, cbind(Z[,2:ncol(Z)])%*%beta[3:length(beta)], rep(0,nrow(Z)))
  }
}

# running the simulation
sim1 = generate_simulation_results(num_sims = num_sims., n = n., p = p., true_beta = true_beta.,
                                   starting_vals = starting_vals., censoring_rate = censoring_rate.,
                                   output_file = output_file, m = m.,
                                   censoring_mechanism = censoring_mechanism.,
                                   se = TRUE,
                                   sigma2 = sigma2.)

# saving the results
saveRDS(sim1, file = paste0(output_file, ".RDS"))









