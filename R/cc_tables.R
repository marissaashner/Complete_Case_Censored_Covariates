#########################################################################
# This script creates tables with simulation results for the manuscript #
#########################################################################

###########################
##### Packages Needed #####
###########################

library(tidyverse)
library(latex2exp)
# install.packages("kableExtra")
library(kableExtra)

###########################
##### LINEAR n = 400 ######
###########################

# Corresponds with Table 2 in manuscript 

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_xz_broken", "cond_indep_z_broken", "indep_broken", "indep_holds")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, ".RDS")
    sim_new <- readRDS(string_new)
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n) %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n)  %>% 
                                  filter(`Censoring Rate` !=0.5))
    sim_table_std_dev <- rbind(sim_table_std_dev, sim_new$std_dev %>% 
                                 mutate(censoring = censoring,
                                        n = n) %>% 
                                 filter(`Censoring Rate` !=0.5))
    sim_table_se <- rbind(sim_table_se, sim_new$se %>% 
                            mutate(censoring = censoring,
                                   n = n) %>% 
                            filter(`Censoring Rate` !=0.5))
    sim_table_cov <- rbind(sim_table_cov, sim_new$coverage %>% 
                             mutate(censoring = censoring,
                                    n = n) %>% 
                             filter(`Censoring Rate` !=0.5))
  }
}

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\beta}_0$",
                               "$\\wh{\\beta}_1$",
                               "$\\wh{\\beta}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Percent Bias" = 3, 
                     #  "Empirical SD" = 3,
                     "Model SE" = 3,
                     "95% Coverage" = 3),
                   line_sep = 5, bold = T) %>% 
  pack_rows(index=c("(a) Exogenous Censoring Broken" = 4, 
                    "(b) Strict Exogenous Censoring Broken" = 4,
                    "(c) Conditional Independence given (X,Z) Broken" = 4, 
                    "(d) Conditional Independence given Z Broken" = 4, 
                    "(e) Independence Broken" = 4, 
                    "(f) Independence Holds"= 4),
            latex_align = "c") %>% 
  collapse_rows(columns = 1, latex_hline = "linespace", valign = "top")
tab1


write_file(tab1, "tab_cc_400_linear.txt")

###########################
##### LINEAR n = 1200 ######
###########################

# Corresponds with Table S.1 in manuscript 


sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()

for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_xz_broken", "cond_indep_z_broken", "indep_broken", "indep_holds")){
  for(n in c(1200)){
    string_new <- paste0("simulation_output/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, ".RDS")
    sim_new <- readRDS(string_new)
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n) %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n)  %>% 
                                  filter(`Censoring Rate` !=0.5))
    sim_table_std_dev <- rbind(sim_table_std_dev, sim_new$std_dev %>% 
                                 mutate(censoring = censoring,
                                        n = n) %>% 
                                 filter(`Censoring Rate` !=0.5))
    sim_table_se <- rbind(sim_table_se, sim_new$se %>% 
                            mutate(censoring = censoring,
                                   n = n) %>% 
                            filter(`Censoring Rate` !=0.5))
    sim_table_cov <- rbind(sim_table_cov, sim_new$coverage %>% 
                             mutate(censoring = censoring,
                                    n = n) %>% 
                             filter(`Censoring Rate` !=0.5))
  }
}

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\beta}_0$",
                               "$\\wh{\\beta}_1$",
                               "$\\wh{\\beta}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Percent Bias" = 3, 
                     #  "Empirical SD" = 3,
                     "Model SE" = 3,
                     "95% Coverage" = 3),
                   line_sep = 5, bold = T) %>% 
  pack_rows(index=c("(a) Exogenous Censoring Broken" = 4, 
                    "(b) Strict Exogenous Censoring Broken" = 4,
                    "(c) Conditional Independence given (X,Z) Broken" = 4, 
                    "(d) Conditional Independence given Z Broken" = 4, 
                    "(e) Independence Broken" = 4, 
                    "(f) Independence Holds"= 4),
            latex_align = "c") %>% 
  collapse_rows(columns = 1, latex_hline = "linespace", valign = "top")
tab1


write_file(tab1, "tab_cc_1200_linear.txt")


################################
##### LINEAR INTERCEPT/NOT #####
################################

# Corresponds with Table S.2 in manuscript 


sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()

for(censoring in c("exog_broken")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, ".RDS")
    sim_new <- readRDS(string_new)
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n, intercept = "Yes") %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n, intercept = "Yes")  %>% 
                                  filter(`Censoring Rate` !=0.5))
    
    string_new_no_int <- paste0("simulation_output/simulation_numsims1000_n", n, 
                                "_p1_sigma2-2_linear_truebeta-1r-2_startingvals-0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                                censoring, ".RDS")
    sim_new_no_int <- readRDS(string_new_no_int)
    colnames(sim_new_no_int$bias) = colnames(sim_new$bias)[-5]
    colnames(sim_new_no_int$bias_per) = colnames(sim_new$bias_per)[-5]
    sim_table_bias <- rbind(sim_table_bias, sim_new_no_int$bias %>% 
                              mutate( `$\\beta_{2}$` = 0,
                                      censoring = censoring,
                                      n = n, intercept = "No") %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new_no_int$bias_per %>% 
                                  mutate(`$\\beta_{2}$` = 0,
                                         censoring = censoring,
                                         n = n, intercept = "No")  %>% 
                                  filter(`Censoring Rate` !=0.5))
  }
}

table_full = sim_table_bias_per[,c(1:2,5,3:4)] 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\beta}_0$",
                               "$\\wh{\\beta}_1$",
                               "$\\wh{\\beta}_2$"), 1))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Percent Bias" = 3),
                   line_sep = 5, bold = T) %>% 
  pack_rows(index=c("Intercept Present" = 4, 
                    "No Intercept Present" = 4),
            latex_align = "c") %>% 
  collapse_rows(columns = 1, latex_hline = "linespace", valign = "top")
tab1


write_file(tab1, "tab_cc_400_linear_int.txt")




###########################
##### LOGISTIC n = 400 ######
###########################

# Corresponds with Table S.3 in manuscript 


sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()

for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_xz_broken", "cond_indep_z_broken", "indep_broken", "indep_holds")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, ".RDS")
    sim_new <- readRDS(string_new)   
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n) %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n)  %>% 
                                  filter(`Censoring Rate` !=0.5))
    sim_table_std_dev <- rbind(sim_table_std_dev, sim_new$std_dev %>% 
                                 mutate(censoring = censoring,
                                        n = n) %>% 
                                 filter(`Censoring Rate` !=0.5))
    sim_table_se <- rbind(sim_table_se, sim_new$se %>% 
                            mutate(censoring = censoring,
                                   n = n) %>% 
                            filter(`Censoring Rate` !=0.5))
    sim_table_cov <- rbind(sim_table_cov, sim_new$coverage %>% 
                             mutate(censoring = censoring,
                                    n = n) %>% 
                             filter(`Censoring Rate` !=0.5))
  }
}

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\beta}_0$",
                               "$\\wh{\\beta}_1$",
                               "$\\wh{\\beta}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Percent Bias" = 3, 
                     #  "Empirical SD" = 3,
                     "Model SE" = 3,
                     "95% Coverage" = 3),
                   line_sep = 5, bold = T) %>% 
  pack_rows(index=c("(a) Exogenous Censoring Broken" = 4, 
                    "(b) Strict Exogenous Censoring Broken" = 4,
                    "(c) Conditional Independence given (X,Z) Broken" = 4, 
                    "(d) Conditional Independence given Z Broken" = 4, 
                    "(e) Independence Broken" = 4, 
                    "(f) Independence Holds"= 4),
            latex_align = "c") %>% 
  collapse_rows(columns = 1, latex_hline = "linespace", valign = "top")
tab1


write_file(tab1, "tab_cc_400_logistic.txt")


###########################
##### LOGISTIC n = 1200 ######
###########################

# Corresponds with Table S.4 in manuscript 


sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()

for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_xz_broken", "cond_indep_z_broken", "indep_broken", "indep_holds")){
  for(n in c(1200)){
    string_new <- paste0("simulation_output/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, ".RDS")
    sim_new <- readRDS(string_new)
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n) %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n)  %>% 
                                  filter(`Censoring Rate` !=0.5))
    sim_table_std_dev <- rbind(sim_table_std_dev, sim_new$std_dev %>% 
                                 mutate(censoring = censoring,
                                        n = n) %>% 
                                 filter(`Censoring Rate` !=0.5))
    sim_table_se <- rbind(sim_table_se, sim_new$se %>% 
                            mutate(censoring = censoring,
                                   n = n) %>% 
                            filter(`Censoring Rate` !=0.5))
    sim_table_cov <- rbind(sim_table_cov, sim_new$coverage %>% 
                             mutate(censoring = censoring,
                                    n = n) %>% 
                             filter(`Censoring Rate` !=0.5))
  }
}

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\beta}_0$",
                               "$\\wh{\\beta}_1$",
                               "$\\wh{\\beta}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Percent Bias" = 3, 
                     #  "Empirical SD" = 3,
                     "Model SE" = 3,
                     "95% Coverage" = 3),
                   line_sep = 5, bold = T) %>% 
  pack_rows(index=c("(a) Exogenous Censoring Broken" = 4, 
                    "(b) Strict Exogenous Censoring Broken" = 4,
                    "(c) Conditional Independence given (X,Z) Broken" = 4, 
                    "(d) Conditional Independence given Z Broken" = 4, 
                    "(e) Independence Broken" = 4, 
                    "(f) Independence Holds"= 4),
            latex_align = "c") %>% 
  collapse_rows(columns = 1, latex_hline = "linespace", valign = "top")
tab1


write_file(tab1, "tab_cc_1200_logistic.txt")
















