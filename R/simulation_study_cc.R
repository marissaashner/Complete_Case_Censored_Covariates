###############################################################
##################### SIMULATION STUDY FOR CC PAPER ###########
###############################################################

# clear environment
rm(list = ls())

###############################################################
##################### COMMAND LINE ARGUMENTS FOR CLUSTER #####
###############################################################
args <- commandArgs(TRUE)

num_sims. <- as.numeric(args[1]) # number of simulations
n. <- as.numeric(args[2]) # sample size
p. <- as.numeric(args[3]) # number of Z covariates
true_beta. <- as.numeric(strsplit(args[4], "r")[[1]]) # true value for beta entered as "r" separated list
starting_vals. <- as.numeric(strsplit(args[5], "r")[[1]]) # starting values entered as "r" separated list
m_func. <- as.character(args[6]) # linear, log, or logit as options for now
censoring_rate. <- as.numeric(strsplit(args[7], "r")[[1]]) # censoring rates entered as "r" separated list
censoring_mechanism. <- as.character(args[8])
sigma2. <- as.numeric(args[9])
output_file <- as.character(args[10]) # output file name

###############################################################
##################### arguments when using local comp #########
###############################################################
# num_sims. <- 100
# n. <- 100
# p. <- 2
# true_beta. <- c(0.01, -0.02, 0.005)
# starting_vals. <- c(0, 0, 0)
# m_func. <- "logistic"
# censoring_rate. <- c(0.5)
# censoring_assumption. <- TRUE
# censoring_mechanism. <- "exog_broken"
# sigma2. = 0.08
# output_file <- "test"

###############################################################
##################### packages and functions needed ##########
###############################################################
source("R/main_cc.R")

# packages needed
library(tidyverse)
library(numDeriv)
# install.packages("geex")
library(geex)
library(parallel)
library(xtable)
library(survival)

###############################################################
##################### DATA GENERATION #########################
###############################################################

## Create a function that generates data for a linear/nonlinear regression model 
## y = m(x,z,beta) + e
## m() can be linear or nonlinear 
## X is potentially censored (one for now)
## Z there are no censored (allow p = 0, 1, ...)
## Generate e from normal distribution 

## generate_data takes the following inputs
### p is the number of covariates Z
### m is the mean function
### n is sample size
### true_beta is a p+1 vector of true values for beta 
### censoring_rate is the proportion of X that are censored, 0 when not censored 
### censoring_mechanism defines how the data will be simulated ...
generate_data <- function(n, p, m, true_beta, censoring_rate, 
                          censoring_mechanism, sigma2){
  ## ERROR CHECKS
  
  ## note == no intercepts right now ...
  # make this a command line argument at some point so people can choose to include an intercept or not 
  intercept <- TRUE
  
  
  ## DATA GENERATION FOR ALL CENSORING ASSUMPTONS 
  X <- runif(n, 0, 3)
  
  if(p == 0){
    Z <- NULL
  }else{
    Z <- matrix(rnorm(n*p), nrow = n)
  }
  
  if(p > 0 & intercept == TRUE){
    Z[,p] <- rep(1, n)
  }
  
  ## Initialize D 
  D <- rep(0, n)
  
  ## DATA GENERATION SPECIFIC TO CERTAIN CENSORING ASSUMPTIONS 
  
  ######################### 
  ## EXOG BROKEN      #####
  #########################
  
  if(censoring_mechanism == "exog_broken"){
   # X <- runif(n, 0, 3)

    D <- rbernoulli(n, 1-censoring_rate) %>% as.numeric()
    mult = sqrt(sigma2)/8
    e <- lapply(1:n, function(x) 
      ifelse(D[x]==1, rnorm(1, -censoring_rate*mult/(1-censoring_rate), sqrt(sigma2)), rnorm(1, mult, sqrt(sigma2)))) %>% unlist()
    y <- m(true_beta, X, Z) + e
  }
  
  ######################### 
  ## STRICT EXOG BROKEN ###
  #########################
  
  if(censoring_mechanism == "strict_exog_broken"){
   # X <- runif(n, 0, 3)

    e <- rnorm(n, 0, sqrt(sigma2))
    D <- ifelse(abs(e) >= quantile(abs(e), censoring_rate) , 1, 0)
    y <- m(true_beta, X, Z) + e
  }
  
  ######################### 
  ## COND INDEP BROKEN ####
  #########################
  
  if(censoring_mechanism == "cond_indep_broken"){
  #  X <- runif(n, 0, 3)
    
    if(censoring_rate >= 0.5){ 
      C <- runif(n, 0, 6*(1-censoring_rate))
      D <- ifelse(X <= C, 1, 0)
      e <- rnorm(n, 0, ifelse(C > quantile(C)[4] | C < quantile(C)[2], 
                              sqrt(1.5*sigma2), sqrt(0.5*sigma2)))
    }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- runif(n, 0, 3/(2*censoring_rate))
      D <- ifelse(X <= C, 1, 0)
      e <- rnorm(n, 0, ifelse(C > quantile(C)[4] | C < quantile(C)[2], 
                              sqrt(5*sigma2), sqrt(0.5*sigma2)))
      #   D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else{
      D <- rep(1, n)
      e <- rnorm(n, 0, sqrt(sigma2))
    }

    # if(censoring_rate > 0){
    #   mu <- qnorm(1-censoring_rate)
    #   C <- rnorm(n, X + mu, 1)
    #   D <- ifelse(X <= C, 1, 0)
    #   e <- sample(c(-sqrt(sigma2), sqrt(sigma2)), n, replace = TRUE)*abs(C-X)
    # }else{
    #   D <- rep(1, n)
    #   C <- rnorm(n)
    #   e <- sample(c(-sqrt(sigma2), sqrt(sigma2)), n, replace = TRUE)*abs(C-X)
    # }
    y <- m(true_beta, X, Z) + e
  }
  
  ############################
  ## NON-INFORMATIVE BROKEN ##
  ############################
  
  if(censoring_mechanism == "noninf_broken"){
   # X <- rexp(n)
   # X <- runif(n, 0, 3)
    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate > 0){
      C <- runif(n, 0, X/censoring_rate)  
      D <- ifelse(X <= C, 1, 0)
    # if(censoring_rate >= 0.5){ 
    #   C <- ifelse(X > median(X), runif(n, 0, 6*(1-censoring_rate)), 
    #               runif(n, (3-6*(censoring_rate))/(2*(1-censoring_rate)), 3))  
    #   D <- ifelse(X <= C, 1, 0)
    #   #  D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    # }else if(censoring_rate < 0.5 & censoring_rate > 0){
    #   C <- ifelse(X > median(X), runif(n, 0, 3/(2*censoring_rate)), 
    #               runif(n,6*(1-censoring_rate)-3,3))
    #   D <- ifelse(X <= C, 1, 0)
    #   #   D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else{
      D <- rep(1, n)
    }
    # if(censoring_rate > 0){
    #   C <- rexp(n, -log(1-censoring_rate)/X)
    #   D <- ifelse(X <= C, 1, 0)
    # }else{
    #   D <- rep(1, n)
    # }
    y <- m(true_beta, X, Z) + e
  }
  
  
  ######################### 
  ## INDEPENDENCE BROKEN ##
  #########################
  
  if(censoring_mechanism == "indep_broken"){
    # X <- rexp(n)
  #  X <- runif(n, 0, 3)
    if(p ==0){
      stop("Cannot use this censoring_mechanism without Z covariates.")
    }
    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate >= 0.5){ 
      C <- ifelse(Z[,1] > median(Z[,1]), runif(n, 0, 6*(1-censoring_rate)), 
                  runif(n, (3-6*censoring_rate)/(2*(1-censoring_rate)), 3))     
      D <- ifelse(X <= C, 1, 0)
      #  D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- ifelse(Z[,1] > median(Z[,1]), runif(n, 0, 3/(2*(censoring_rate))), 
                  runif(n,6*(1-censoring_rate)-3,3))
      D <- ifelse(X <= C, 1, 0)
      #   D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else{
      D <- rep(1, n)
    }
    # if(censoring_rate > 0){
    #   ## interesting way of doing this but it works
    #   C <- ifelse(Z[,1] > quantile(Z[,1], censoring_rate), 10, 0.01)
    #   D <- ifelse(X <= C, 1, 0)
    # }else{
    #   D <- rep(1, n)
    # }
    y <- m(true_beta, X, Z) + e
  }
  
  ######################### 
  ## INDEPENDENCE HOLDS ###
  #########################
  
  if(censoring_mechanism == "indep_holds"){
    # X <- rexp(n)
  #  X <- runif(n, 0, 3)

    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate >= 0.5){ 
         C <- runif(n, 0, 6*(1-censoring_rate))
         D <- ifelse(X <= C, 1, 0)
    #  D <- rbinom(n, size = 1, prob = 1- censoring_rate)
      }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- runif(n, 0, 3/(2*censoring_rate))
      D <- ifelse(X <= C, 1, 0)
   #   D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else{
      D <- rep(1, n)
    }
    y <- m(true_beta, X, Z) + e
  }
  
  
  ######################### 
  ## MCAR HOLDS ###
  #########################
  
  if(censoring_mechanism == "MCAR"){
    # X <- rexp(n)
    #  X <- runif(n, 0, 3)
    
    e <- rnorm(n, 0, sigma2)
    if(censoring_rate > 0){
      D <- rbinom(n, size = 1, prob = 1- censoring_rate)
    }else{
      D <- rep(1, n)
    }
    y <- m(true_beta, X, Z) + e
  }

  
  # return data in data frame, including indicator for X censored, D 
  data_yXZ <- cbind(y, X, Z, D) %>% as.data.frame()
  colnames(data_yXZ) <- if(is.null(Z)){c("y", "X", "D")} else{c("y", "X", paste0("Z", 1:p), "D")}
  return(data_yXZ)
}

###############################################################
##################### RUN SIMULATION ##########################
###############################################################

## Create another function that runs the simulation study
## i.e. generates data, estimates beta, and stores the estimate for beta 

## run_simulation takes the following inputs
### takes all inputs for generate_data and estimate_beta
# ignore CI band for now
run_simulation <- function(n, p, m, true_beta, starting_vals, censoring_rate, method, censoring_mechanism, se, sigma2){
  ## ERROR CHECKS
  
  # generate data
  data_yXZ = generate_data(n, p, m, true_beta, censoring_rate, censoring_mechanism, sigma2)
  
  # estimate beta (function in main_cc.R)
    est_output <- estimate_beta(data_yXZ, m, starting_vals, method)
  

    if(se){
      se_est <- estimate_se_sandwich(data_yXZ, m, est_output$beta_est, 
                                     method)
    }else{
      # ignore CI band for now 
      # Just set equal to identity matrix as a placeholder
      se_est <- diag(1, p+1)
    }
  
  # return list of estimated beta and se
  return(list(beta_est = est_output$beta_est, se_est = se_est))
}

###############################################################
##################### REPORT RESULTS ##########################
###############################################################

## Create another function that reports results from 1000 simulations
## i.e. bias and empirical standard errors
## ggplot when only have X (no Z) -- plot the curve m() with estimated beta and specific choices for X 
## run with different choices for m() -- start with linear 

## generate_simulation_results takes the following inputs
### num_sims is an integer of the number of simulations to be run
### all inputs for run_simulation
### default for p is 0
### default for m is linear
### default for starting_vals is NULL (will use lm), must be length p + 1
### default for censoring_rate is 0 (i.e. no censoring), can take a vector of multiple values 
### default for censoring_mechanism is strict_exog 
### default for ci_band is TRUE
### default for sigma2 is 1

generate_simulation_results <- function(num_sims, n, 
                                        p = 0,  
                                        m = function(beta, X, Z){
                                          cbind(X,Z)%*%beta
                                        }, 
                                        true_beta,
                                        starting_vals = NULL,
                                        censoring_rate = 0,
                                        method = c("cc", "oracle"),
                                        censoring_mechanism = "strict_exog",
                                        output_file = NULL,
                                        ci_band = TRUE, 
                                        sigma2 = 1){
  ## ERROR CHECKS
  
  # set seed for replicability
  set.seed(10582938)
  
  # let's time this
  start_time = Sys.time()
  
  # set up dummy function
  dummy_function <- function(x){
    run_simulation(n, p, m, true_beta, starting_vals, censoring_rate[i], method[j], censoring_mechanism, ci_band, sigma2)
  }
  
  # intialize list for beta estimate 
  sim_output <- vector(mode = "numeric", length = length(censoring_rate)*length(method)) %>% as.list()
  names(sim_output) <- expand_grid(censoring_rate, method) %>% unite(name) %>% unlist() %>% unname()
  
  # running in parallel 
  cl = makeCluster(24) # use detectCores()) on local # use 24) on the cluster 
  clusterExport(cl, c("generate_data", "estimate_beta", "estimate_se_sandwich",
                      "run_simulation", "m", "p",
                      "n", "true_beta", "starting_vals",
                      "censoring_mechanism", "ci_band", "sigma2"), 
                envir = environment())
  clusterEvalQ(cl, c(library(tidyverse), library(geex), library(numDeriv)))
  # run clusterApply for each censoring rate and method combo 
  k = 1
  for (i in 1:length(censoring_rate)){
    for (j in 1:length(method)){
      sim_output[[k]] <- clusterApply(cl, 1:num_sims, function(x) {set.seed(x);dummy_function(x)})
      print(k)
      k = k + 1
    }
  }
  on.exit(stopCluster(cl))
  
  # list into dataframe 
  sep <- enframe(sim_output) %>% unnest(cols = value) %>% unnest(cols = value)
  # return(sep)
  
  
  # save beta estimation
  beta_est = suppressWarnings(sep[rep(c(TRUE, FALSE), num_sims*length(method)*length(censoring_rate)), ])  %>%
    unnest(cols = value) %>% 
    separate(name, c("censoring_rate", "method"), sep = "_")
  
  # save standard error estimation 
  se_est = suppressMessages(suppressWarnings(sep[rep(!c(TRUE, FALSE), num_sims*length(method)*length(censoring_rate)), ]  %>% 
                                               separate(name, c("censoring_rate", "method"), sep = "_") %>% unnest_wider(value)))
  colnames(se_est) = c("censoring_rate", "method", paste0("se", 1:(p+1)))
  
  # find cover
  beta_est_cover = beta_est %>% group_by(censoring_rate, method) %>%
    summarise(temp = n()) %>% 
    select(censoring_rate, method)
  for (i in 1:(p+1)){
   cover_est_temp =  cbind(beta_est, se_est[,3:(p+3)]) %>% 
     select(censoring_rate, method, ends_with(i %>% as.character())) 
   colnames(cover_est_temp) = c("censoring_rate", "method", "beta", "se")
   cover_est_new = cover_est_temp %>% 
     group_by(censoring_rate, method) %>% 
     summarize(cov = mean(I(beta + qnorm(0.025)*se < true_beta[i] & 
                         beta + qnorm(0.975)*se > true_beta[i])))
   beta_est_cover = cbind(beta_est_cover, cover_est_new[,3])
  }
  colnames(beta_est_cover) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))

  # take the mean of the estimated beta from each simulation 
  beta_est_mean = beta_est %>% group_by(censoring_rate, method) %>% summarise_all(mean) %>% as.data.frame()
  #beta_est_mean = beta_est_mean[,-ncol(beta_est_mean)]
  colnames(beta_est_mean) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))
  
  # take mean of estimated std error from each simulation 
  beta_est_se = se_est %>% group_by(censoring_rate, method) %>% summarise_all(mean) %>% as.data.frame()
  #beta_est_se = beta_est_se[,-ncol(beta_est_se)]
  colnames(beta_est_se) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))
  
  # find the ABSOLUTE bias of each estimate for each censoring rate
  if(p == 0){
    beta_est_bias = cbind(beta_est_mean[,1:2],
                          apply(beta_est_mean, 1, function(x) x[-(1:2)] %>% as.numeric() - true_beta) 
                          %>% as.data.frame())
    
  }else{
    beta_est_bias = cbind(beta_est_mean[,1:2],
                          apply(beta_est_mean, 1, function(x) x[-(1:2)] %>% as.numeric() - true_beta) 
                          %>% t() %>% as.data.frame())
  }
  colnames(beta_est_bias) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))
  
  # find the PERCENT bias of each estimate for each censoring rate
  if(p == 0){
    beta_est_bias_per = cbind(beta_est_mean[,1:2],
                          apply(beta_est_mean, 1, function(x) (x[-(1:2)] %>% as.numeric() - true_beta) / true_beta * 100) 
                          %>% as.data.frame())
    
  }else{
    beta_est_bias_per = cbind(beta_est_mean[,1:2],
                          apply(beta_est_mean, 1, function(x) (x[-(1:2)] %>% as.numeric() - true_beta) / true_beta * 100) 
                          %>% t() %>% as.data.frame())
  }
  colnames(beta_est_bias_per) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))
  
  # find the empirical standard error of each estimate for each censoring rate 
  beta_est_std_error = beta_est %>% group_by(censoring_rate, method) %>% summarise_all(sd) %>% as.data.frame()
  colnames(beta_est_std_error) <- c("Censoring Rate", "Method", paste0("$\\alpha_{", 0:p, "}$"))
  
  end_time = Sys.time()
  
  ################## make latex table ##################
  # RIGHT NOW, latex table gives absolute bias, can switch to percent or do both 
  # the results object outputs both so we can see them now 
  
  # create data frame with "bias (std error)" format for each censoring rate and estimate
  if(length(censoring_rate) > 1 | length(method) > 1){
    beta_df <- cbind(beta_est_bias %>% select(`Censoring Rate`, `Method`), 
                     mapply(function(x, y) paste0(round(x,4), " (", round(y,3), ")"), 
                            beta_est_bias[(3)], beta_est_bias_per[(3)]) %>% as.data.frame(),
                     beta_est_std_error[(3)] %>% round(., 3))
  }else{
    beta_df <- cbind(beta_est_bias %>% select(`Censoring Rate`, `Method`), 
                     mapply(function(x, y) paste0(round(x,4), " (", round(y,3), ")"), 
                            beta_est_bias[(3)], beta_est_bias_per[(3)]) %>% t() %>% as.data.frame(),
                     beta_est_std_error[(3)] %>% round(., 3))
  }
  colnames(beta_df) <- c("Censoring Rate", "Method", "Bias (Percent)", "Empirical Standard Deviation")
  beta_df$Method <- suppressMessages(plyr::mapvalues(beta_df$Method, from = c("cc", "oracle"), 
                                                     to = c("Complete Cases", "Oracle")))
  
  
  beta_xtable <- xtable(beta_df,
                        align = rep("c", ncol(beta_df)+1),
                        digits = c(0, 2, 0, rep(4, ncol(beta_df)-2)),
                        type = "latex") 
  
  # add extra row on top of column names for Bias (Empirical Standard Error)
#  addtorow <- list()
#  addtorow$pos <- list(0, 0)
#  addtorow$command <- c(paste0("& & \\multicolumn{", ncol(beta_df)-2 , 
#                               "}{c}{Bias (Empirical Standard Error)} \\\\\n"),
#                        paste0(paste(colnames(beta_df), collapse = " & "), " \\\\\n"))
  
  ################### make ggplot ##################
 #  
 #  # increment X and Z 
 #  x_plot <- seq(-10, 10, 0.01)
 #  if(p > 0){
 #    z_plot <- as.data.frame(matrix(0, ncol = p, nrow = length(x_plot)))
 #    z_plot[,1] <- rep(1, length(x_plot))
 #    if(p > 1){
 #      for (i in 2:p){
 #        z_plot[,i] <- rep(0, length(x_plot))
 #        # seq(-10, 10, 0.01)
 #      }
 #    }
 #  }else{
 #    z_plot = NULL
 #  }
 #  
 #  cov_plot <- cbind(x_plot, z_plot)
 #  
 #  # gradient function for 95% CI Band using Delta Method 
 #  grad_delta_method <- function(X, Z, m, beta){
 #    # grad is a numerical approximation to the derivative of the mean function with respect to beta
 #    grad(func = m, x = beta, X=X, Z=Z)
 #  }
 #  
 #  # mean function corresponding to true beta and estimated beta for each censoring rate
 #  m_plot <- data.frame(m_vals = numeric(), type = character(), method = character(),
 #                       m_low = numeric(), m_high = numeric())
 #  for (cr in censoring_rate){
 #    m_plot_true <- data.frame(m_vals = apply(cov_plot, 1, function(x) m(true_beta, X = x[1], Z = t(x[-1])))
 #                              %>% unlist(),
 #                              type = paste0("Censoring Rate: ", cr), 
 #                              method = "True Value") %>%
 #      # for CI Band, none for true value, so m = m_low = m_high 
 #      mutate(m_low = m_vals, m_high = m_vals)
 #    m_plot <- rbind(m_plot, m_plot_true)
 #    for (met in method){
 #      # Sigma hat estimation -- average each entry to the Sigma matrix from all the simulations (as done in Matsouaka?)
 #      Sigma_hat_sep <- se_est %>% filter(censoring_rate == cr, method == met)
 #      Sigma_hat <- (Sigma_hat_sep$value %>% Reduce("+", .))/num_sims
 #      
 #      m_plot_est <- data.frame(m_vals = apply(cov_plot, 1, function(x) 
 #        m((beta_est_mean %>% filter(censoring_rate == cr, method == met))[,-(1:2)] 
 #          %>% as.numeric(), 
 #          X = x[1], Z = t(x[-1])))
 #        %>% unlist(), 
 #        type = paste0("Censoring Rate: ", cr),
 #        method = met) #%>% 
 #        # CI band estimated variance using Delta Method
 #        # mutate(
 #        #   est_vars = apply(cov_plot, 1, function(x)
 #        #     t(grad_delta_method(X = x[1], Z = t(x[-1]), m = m, 
 #        #                         beta = (beta_est_mean %>% 
 #        #                                   filter(censoring_rate == cr, method == met))[,-(1:2)] 
 #        #                         %>% as.numeric()))%*% 
 #        #       Sigma_hat%*%
 #        #       grad_delta_method(X = x[1], Z = t(x[-1]), m = m, 
 #        #                         beta = (beta_est_mean %>% 
 #        #                                   filter(censoring_rate == cr, method == met))[,-(1:2)] 
 #        #                         %>% as.numeric())) %>% unlist()
 #        # ) %>% 
 #        # # CI Band using classic formula for a normal distribution
 #        # mutate(
 #        #   m_low = m_vals - 1.96*sqrt(est_vars/n),
 #        #   m_high = m_vals + 1.96*sqrt(est_vars/n)) %>% 
 #        # select(-est_vars)
 #      m_plot <- rbind(m_plot, m_plot_est)
 #    }
 #  }
 #  
 #  m_plot$method <- suppressMessages(plyr::mapvalues(m_plot$method, from = c("cc", "oracle"), 
 #                                                    to = c("Complete Cases", "Oracle")))
 #  m_plot$method <- m_plot$method %>% as.factor()
 #  m_plot$method <- factor(m_plot$method, levels=rev(levels(m_plot$method)))
 #  
 #  # combine data and plot
 #  data_plot <- data.frame(x_plot, m_plot)
 # # if(!ci_band){
 #    results_plot <- ggplot(data = data_plot, aes(x_plot, m_vals, col = method)) + 
 #      facet_grid(cols = vars(type)) + 
 #      geom_smooth(se = FALSE) + # geom_ribbon(aes(ymin = m_low, ymax = m_high), alpha = 0.1) + 
 #      labs(x = "X", y = "m(X, Z, Beta)", col = "Beta") + 
 #      theme(legend.position = "bottom")
 #  # }else{
 #  #   results_plot <- ggplot(data = data_plot, aes(x_plot, m_vals, col = method, fill = method)) + 
 #  #     facet_grid(cols = vars(type)) + 
 #  #     # make sure each row of data_plot has a m_low and m_high
 #  #     geom_smooth(se = FALSE) + geom_ribbon(aes(ymin = m_low, ymax = m_high), alpha = 0.3, col = NA) + 
 #  #     labs(x = "X", y = "m(X, Z, Beta)", col = "Beta") + 
 #  #     theme(legend.position = "bottom") + 
 #  #     guides(size = "legend", fill = "none")
 #  #   
 #  # }
  
  
  return(list(bias = beta_est_bias, 
              bias_per = beta_est_bias_per,
              std_dev = beta_est_std_error, 
              se = beta_est_se,
              coverage = beta_est_cover,
              run_time = end_time-start_time,
              plot_table = beta_est %>% mutate(n = n, censoring = censoring_mechanism),
              results_table = print(beta_xtable, 
                                    sanitize.text.function = function(x) {x},
                                    include.rownames = FALSE,
                                    # add.to.row = addtorow,
                                    include.colnames = FALSE,
                                    file = paste0(output_file, ".tex") # output table into tex file 
              )))#,
       #       results_plot = results_plot))
}

###############################################################
##################### LET'S TRY IT :) #########################
###############################################################

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

sim1 = generate_simulation_results(num_sims = num_sims., n = n., p = p., true_beta = true_beta., 
                                   starting_vals = starting_vals., censoring_rate = censoring_rate.,
                                   output_file = output_file, m = m.,
                                   censoring_mechanism = censoring_mechanism.,
                                   ci_band = TRUE, 
                                   sigma2 = sigma2.)

saveRDS(sim1, file = paste0(output_file, ".RDS"))


######### OLD VERSION  ########## 
# if(censoring_mechanism == "strict_exog"){
#   
#   # for this type of censoring, we want e ind D given X
#   
#   # For now, no Z 
#   # Y = X*true_beta + eps 
#   if(p == 0){
#     Z <- NULL
#   }
#   
#   # generate X 
#   X <- runif(n, 0, 3)
#   
#   # generate eps
#   e <- rnorm(n)
#   
#   # generate Y 
#   y <- m(true_beta, X, Z) + e
#   
#   # if censoring assumption holds
#   if(censoring_assumption){
#     # this feels like it would cause bias because D is determined directly by
#     ## the possibly unknown value X. However, it fits the description of the 
#     ## assumption (I think?)
#     
#     ## UPDATE: This is limit of detection censoring, since X is observed only if 
#     ### greater than a certain value
#     D <- ifelse(X >= quantile(X, censoring_rate), 1, 0)
#     
#     # an alternative that is completely independent of X would be 
#     # D <- rbinom(n, 1, 1-censoring_rate)
#   }
#   # if censoring assumption doesn't hold
#   else{
#     # make D dependent on Y and therefore eps 
#     D <- ifelse(e >= quantile(e, censoring_rate), 1, 0)
#   }
# }









