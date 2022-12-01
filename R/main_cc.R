###############################################################################
# this script defines the functions used to estimate beta and the sandwich se #
###############################################################################

# these functions are used in simulation_study_cc.R

#################################################
##### Estimating the sandwich standard error ####
#################################################

# takes in 
## a data frame with columns y (outcome), X (censored covariate), 
#### Z1, ...,  Zp (fully observed covariates) and D (censoring indicator), 
## m (mean function), 
## beta_est (parameter estimate),
## and method (i.e. oracle, cc, or naive) (naive not used in manuscript)
estimate_se_sandwich <- function(data_yXZ, m, beta_est, method){
  ### if we have EE with function g, then sandwich estimator is 
  ### (-1/n sum partial g(betahat) / partial beta )^-1  
  ### *(1/n sum g(betahat)g(betahat)^T)
  ### *(-1/n sum partial g(betahat) / partial beta )^-T 
  
  beta_est = beta_est %>% as.numeric()
  
  A <- function(X, Z, m, beta){
    # grad is a numerical approximation to the derivative of the mean function with respect to beta
    if(length(X) > 1){
      lapply(X, function(dummy_var) grad(func = m, x = beta, X=dummy_var, Z=Z)) %>% 
        as.data.frame() %>% as.matrix() %>% unname()
    }else{
      jacobian(func = m, x = beta, X=X, Z=Z)
    }
  }
  
  ## we need some form of g 
  if(method == "cc"){
    g = function(data, A, m_func, beta_est){
      data["D"]*A(data["X"], data[grepl("^Z", names(data))], m, beta_est)*
        rep(data["y"]-m(beta_est, data["X"], data[grepl("^Z", names(data))]), length(beta_est)) %>% as.numeric()
    }
  }else if(method == "oracle"){
    g = function(data, A, m_func, beta_est){
      A(data["X"], data[grepl("^Z", names(data))], m, beta_est)*
        rep(data["y"]-m(beta_est, data["X"], data[grepl("^Z", names(data))]), length(beta_est)) %>% as.numeric()
    }
  }
    
  
  ## then need to get the first derivative of g at each observation, sum them all, and 
  # divide by n 
    first_der <- apply(data_yXZ, 1, function(temp){
      numDeriv::jacobian(func = g, x = beta_est, A = A, m_func = m, data = temp)
    })# %>% mean() 
    if(length(beta_est) > 1){
      first_der = first_der %>% rowMeans() %>% matrix(nrow = length(beta_est))
    }else{
      first_der = first_der %>% mean()
    }
  inv_first_der <- solve(first_der)
  
  ## then need to get the outer product of g at each observation, and take mean
    outer_prod = apply(data_yXZ, 1, function(temp) t(g(temp, A, m_func, beta_est)) %*% 
                         g(temp, A, m, beta_est)) #%>% mean()
    if(length(beta_est) > 1){
      outer_prod = outer_prod %>% rowMeans() %>% matrix(nrow = length(beta_est))
    }else{
      outer_prod = outer_prod %>% mean()
    }
  
  ## then need to put it all together
  se = sqrt((inv_first_der %*% outer_prod %*% t(inv_first_der) / nrow(data_yXZ)) %>% diag())
  return(se) 
}

#########################################
##### Estimating the beta parameters ####
#########################################

## estimate_beta takes the following inputs
### data_yXZ is the n x p+3 data frame with columns y, X, Z1, ..., Zp, D
### m is the mean function 
### starting_vals are the starting values for beta estimation, length p + 1
### method is the method of estimation, cc is complete case analysis, oracle knows the true values
estimate_beta <- function(data_yXZ, m, starting_vals, method){
  
  n = nrow(data_yXZ)
  q = ncol(data_yXZ %>% select(starts_with("Z"))) + 1
  
  if(starting_vals %>% is.null()){
    starting_vals = rep(0, q)
  }
  
  # complete cases, remove the cases that have no observed X 
  if (method == "cc"){
    data_yXZ <- data_yXZ %>% filter(D == 1) %>% select(-D)
  }
  
  ## define variables from the data frame
  y <- data_yXZ$y
  X <- data_yXZ$X
  # if Z is present, select those columns and order numerically, i.e. Z1, Z2, ..., Zp
  if(data_yXZ %>% select(starts_with("Z")) %>% ncol() >0){Z <- data_yXZ %>% 
    select(starts_with("Z")) %>% 
    select(sort(names(.))) %>%
    as.matrix()
  }else{Z<- NULL}
  D <- data_yXZ$D
  
  if(is.null(Z)){
    nls_output <- summary(nls(y ~ m(beta, X, Z = NULL),
                              start = list(beta = starting_vals),
                              control = nls.control(minFactor = 1/5096, warnOnly = TRUE)))
  
  }else{
    nls_output <- summary(nls(y ~ m(beta, X, Z),
                            start = list(beta = starting_vals),
                            control = nls.control(minFactor = 1/5096, warnOnly = TRUE)))
  }
  beta_est <- nls_output$coeff[,1] %>% t() %>% as.data.frame()
 # beta_est <- beta_est %>% cbind(nls_output$coeff[1, 2])
  
  # rename columns
  colnames(beta_est) <-  c(paste0("beta", seq(1:q)))#, "std_error")
  
  
  return(list(beta_est = beta_est))
}
