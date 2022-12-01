####################################################################
# this script defines functions for running the simulation studies #
####################################################################

###############################################
###### packages and functions needed ##########
###############################################

# need functions from main_cc.R
source("main_cc.R")

# packages needed
library(tidyverse)
library(numDeriv)
library(parallel)
library(xtable)

#################################################
###### DATA GENERATION #########################
#################################################

## Create a function that generates data for a linear/nonlinear regression model 
## y = m(x,z,beta) + e
## m() can be linear or nonlinear 
## X is potentially censored (one for now)
## Z (allow p = 0, 1, ...)
## Generate e from normal distribution 

## generate_data takes the following inputs
### p is the number of covariates Z
### m is the mean function
### n is sample size
### true_beta is a p+1 vector of true values for beta 
### censoring_rate is the proportion of X that are censored, 0 when not censored 
### censoring_mechanism defines how the data will be simulated
###### options are exog_broken, strict_exog_broken, cond_indep_xz_broken,
######             cond_indep_z_broken, indep_broken, indep_holds
generate_data <- function(n, p, m, true_beta, censoring_rate, 
                          censoring_mechanism, sigma2){
  
  # can make this a command line argument at some point so people can choose to include an intercept or not 
  # for now, intercept defaults to true 
  intercept <- TRUE
  
  
  ## DATA GENERATION FOR ALL CENSORING ASSUMPTONS 
  
  # X generated from uniform distribution
  X <- runif(n, 0, 3)
  
  # If p = 0, no Z including intercept
  if(p == 0){
    Z <- NULL
  }else{
    # generated Z from standard normal
    Z <- matrix(rnorm(n*p), nrow = n)
  }
  
  # change first column of Z to intercept (column of ones)
  if(p > 0 & intercept == TRUE){
    Z[,p] <- rep(1, n)
  }
  
  ## Initialize D 
  D <- rep(0, n)
  
  ## DATA GENERATION SPECIFIC TO CERTAIN CENSORING ASSUMPTIONS 
  
  ######################### 
  ##### EXOG BROKEN #####
  #########################
  
  if(censoring_mechanism == "exog_broken"){
    
    D <- rbernoulli(n, 1-censoring_rate) %>% as.numeric()
    mult = sqrt(sigma2)/8
    e <- lapply(1:n, function(x) 
      ifelse(D[x]==1, 
             rnorm(1, -censoring_rate*mult/(1-censoring_rate), sqrt(sigma2)), 
             rnorm(1, mult, sqrt(sigma2)))) %>% 
      unlist()
    y <- m(true_beta, X, Z) + e
  }
  
  ##############################
  ##### STRICT EXOG BROKEN #####
  ##############################
  
  if(censoring_mechanism == "strict_exog_broken"){
    e <- rnorm(n, 0, sqrt(sigma2))
    D <- ifelse(abs(e) >= quantile(abs(e), censoring_rate) , 1, 0)
    y <- m(true_beta, X, Z) + e
  }
  
  ############################### 
  ##### COND INDEP XZ BROKEN ####
  ###############################
  
  if(censoring_mechanism == "cond_indep_xz_broken"){
    
    if(censoring_rate >= 0.5){ 
      C <- runif(n, 0, 6*(1-censoring_rate))
      D <- ifelse(X <= C, 1, 0)
      e <- rnorm(n, 0, ifelse(C > quantile(C)[4] | C < quantile(C)[2], 
                              sqrt(1.5*sigma2), 
                              sqrt(0.5*sigma2)))
    }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- runif(n, 0, 3/(2*censoring_rate))
      D <- ifelse(X <= C, 1, 0)
      e <- rnorm(n, 0, ifelse(C > quantile(C)[4] | C < quantile(C)[2], 
                              sqrt(5*sigma2), 
                              sqrt(0.5*sigma2)))
    }else{
      D <- rep(1, n)
      e <- rnorm(n, 0, sqrt(sigma2))
    }
    y <- m(true_beta, X, Z) + e
  }
  
  ###############################
  ##### COND INDEP Z BROKEN #####
  ###############################
  
  if(censoring_mechanism == "cond_indep_z_broken"){
    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate > 0){
      C <- runif(n, 0, X/censoring_rate)  
      D <- ifelse(X <= C, 1, 0)
    }else{
      D <- rep(1, n)
    }
    y <- m(true_beta, X, Z) + e
  }
  
  
  ################################ 
  ###### INDEPENDENCE BROKEN #####
  ################################
  
  if(censoring_mechanism == "indep_broken"){
    if(p ==0){
      stop("Cannot use this censoring_mechanism without Z covariates.")
    }
    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate >= 0.5){ 
      C <- ifelse(Z[,1] > median(Z[,1]), runif(n, 0, 6*(1-censoring_rate)), 
                  runif(n, (3-6*censoring_rate)/(2*(1-censoring_rate)), 3))     
      D <- ifelse(X <= C, 1, 0)
    }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- ifelse(Z[,1] > median(Z[,1]), runif(n, 0, 3/(2*(censoring_rate))), 
                  runif(n,6*(1-censoring_rate)-3,3))
      D <- ifelse(X <= C, 1, 0)
    }else{
      D <- rep(1, n)
    }
    y <- m(true_beta, X, Z) + e
  }
  
  ############################# 
  ##### INDEPENDENCE HOLDS ####
  #############################
  
  if(censoring_mechanism == "indep_holds"){
    e <- rnorm(n, 0, sqrt(sigma2))
    if(censoring_rate >= 0.5){ 
      C <- runif(n, 0, 6*(1-censoring_rate))
      D <- ifelse(X <= C, 1, 0)
    }else if(censoring_rate < 0.5 & censoring_rate > 0){
      C <- runif(n, 0, 3/(2*censoring_rate))
      D <- ifelse(X <= C, 1, 0)
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

#########################
##### RUN SIMULATION ####
#########################

## Create another function that runs the simulation study
## i.e. generates data, estimates beta, and stores the estimate for beta 

## run_simulation takes the following inputs
### takes all inputs for generate_data and estimate_beta
run_simulation <- function(n, p, m, true_beta, starting_vals, censoring_rate, method, censoring_mechanism, se, sigma2){
  
  # generate data
  data_yXZ = generate_data(n, p, m, true_beta, censoring_rate, censoring_mechanism, sigma2)
  
  # estimate beta (function in main_cc.R)
  est_output <- estimate_beta(data_yXZ, m, starting_vals, method)
  
  # se is boolean, true if want the sandwich variance estimator
  if(se){
    se_est <- estimate_se_sandwich(data_yXZ, m, est_output$beta_est, 
                                   method)
  }else{
    # if se is false, se_est is set equal to identity matrix as a placeholder
    # the se estimates from the simulation results will be meaningless
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
### default for starting_vals is NULL (will use zeros in this situation), must be length p + 1
### default for censoring_rate is 0 (i.e. no censoring), can take a vector of multiple values 
### default for censoring_mechanism is exog_broken 
### default for se is TRUE
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
                                        censoring_mechanism = "exog_broken",
                                        output_file = NULL,
                                        se = TRUE, 
                                        sigma2 = 1){
  
  # set seed for replicability
  set.seed(10582938)
  
  # let's time this
  start_time = Sys.time()
  
  # no messages
  options(dplyr.summarise.inform = FALSE)
  
  # set up dummy function
  dummy_function <- function(x){
    run_simulation(n, p, m, true_beta, starting_vals, censoring_rate[i], method[j], censoring_mechanism, se, sigma2)
  }
  
  # intialize list for beta estimate 
  sim_output <- vector(mode = "numeric", length = length(censoring_rate)*length(method)) %>% as.list()
  names(sim_output) <- expand_grid(censoring_rate, method) %>% unite(name) %>% unlist() %>% unname()
  
  # running in parallel 
  cl = makeCluster(detectCores()) 
  clusterExport(cl, c("generate_data", "estimate_beta", "estimate_se_sandwich",
                      "run_simulation", "m", "p",
                      "n", "true_beta", "starting_vals",
                      "censoring_mechanism", "se", "sigma2"), 
                envir = environment())
  clusterEvalQ(cl, c(library(tidyverse), library(numDeriv)))
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
  
  # save beta estimation
  beta_est = suppressWarnings(sep[rep(c(TRUE, FALSE), num_sims*length(method)*length(censoring_rate)), ])  %>%
    unnest(cols = value) %>% 
    separate(name, c("censoring_rate", "method"), sep = "_")
  
  # save standard error estimation 
  se_est = suppressMessages(suppressWarnings(sep[rep(!c(TRUE, FALSE), num_sims*length(method)*length(censoring_rate)), ]  %>% 
                                               separate(name, c("censoring_rate", "method"), sep = "_") %>% unnest_wider(value)))
  colnames(se_est) = c("censoring_rate", "method", paste0("se", 1:(p+1)))
  
  # find coverage probability
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
    beta_est_cover = suppressMessages(cbind(beta_est_cover, cover_est_new[,3]))
  }
  colnames(beta_est_cover) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  return(beta_est_cover)
  
  # take the mean of the estimated beta from each simulation 
  beta_est_mean = beta_est %>% group_by(censoring_rate, method) %>% summarise_all(mean) %>% as.data.frame()
  #beta_est_mean = beta_est_mean[,-ncol(beta_est_mean)]
  colnames(beta_est_mean) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  
  # take mean of estimated std error from each simulation 
  beta_est_se = se_est %>% group_by(censoring_rate, method) %>% summarise_all(mean) %>% as.data.frame()
  #beta_est_se = beta_est_se[,-ncol(beta_est_se)]
  colnames(beta_est_se) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  
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
  colnames(beta_est_bias) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  
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
  colnames(beta_est_bias_per) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  
  # find the empirical standard error of each estimate for each censoring rate 
  beta_est_std_error = beta_est %>% group_by(censoring_rate, method) %>% summarise_all(sd) %>% as.data.frame()
  colnames(beta_est_std_error) <- c("Censoring Rate", "Method", paste0("$\\beta_{", 0:p, "}$"))
  
  end_time = Sys.time()
  
  ################## make latex table ##################
  # resulting table not used in manuscript (see cc_tables.R instead)
  # this code left for completion
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
  
  # ggplot code not used for final version of simulations
  ## commented out but left in for completion 
  
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
  # # if(!se){
  #    results_plot <- ggplot(data = data_plot, aes(x_plot, m_vals, col = method)) + 
  #      facet_grid(cols = vars(type)) + 
  #      geom_smooth(se = FALSE) + # geom_ribbon(aes(ymin = m_low, ymax = m_high), beta = 0.1) + 
  #      labs(x = "X", y = "m(X, Z, Beta)", col = "Beta") + 
  #      theme(legend.position = "bottom")
  #  # }else{
  #  #   results_plot <- ggplot(data = data_plot, aes(x_plot, m_vals, col = method, fill = method)) + 
  #  #     facet_grid(cols = vars(type)) + 
  #  #     # make sure each row of data_plot has a m_low and m_high
  #  #     geom_smooth(se = FALSE) + geom_ribbon(aes(ymin = m_low, ymax = m_high), beta = 0.3, col = NA) + 
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
