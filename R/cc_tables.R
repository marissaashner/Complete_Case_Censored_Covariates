library(tidyverse)
library(latex2exp)
install.packages("kableExtra")
library(kableExtra)

#################
# LINEAR n = 100 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


  # for(censoring in c("indep_holds")){
  for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
    for(n in c(100)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
      #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_100_linear} Simulation results for n = 100 and a linear mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (95% Coverage).") %>% 
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


write_file(tab1, "tab_cc_100_linear.txt")


#################
# LINEAR n = 400 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_400_linear} Simulation results for n = 400 and a linear mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (\\95% Coverage).") %>% 
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

#################
# LINEAR n = 800 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(800)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_800_linear} Simulation results for n = 800 and a linear mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (\\95% Coverage).") %>% 
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


write_file(tab1, "tab_cc_800_linear.txt")

#################
# LINEAR n = 1200 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(1200)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_1200_linear} Simulation results for n = 1200 and a linear mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (\\95% Coverage).") %>% 
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



#################
# LOGISTIC n = 100 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(100)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_100_logistic} Simulation results for n = 100 and a logistic mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (95\\% Coverage).") %>% 
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


write_file(tab1, "tab_cc_100_logistic.txt")


#################
# LOGISTIC n = 400 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_400_logistic} Simulation results for n = 400 and a logistic mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (95\\% Coverage).") %>% 
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


#################
# LOGISTIC n = 800 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(800)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_800_logistic} Simulation results for n = 800 and a logistic mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (95\\% Coverage).") %>% 
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


write_file(tab1, "tab_cc_800_logistic.txt")



#################
# LOGISTIC n = 1200 ########
#################

sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()
sim_table_std_dev <- data.frame()
sim_table_se <- data.frame()
sim_table_cov <- data.frame()


# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken", "strict_exog_broken", "cond_indep_broken", "noninf_broken", "indep_broken", "indep_holds")){
  for(n in c(1200)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-0.08_logistic_truebeta-0.01r-0.02r0.005_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
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

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] %>% 
  cbind(sim_table_se[,c(5,3:4)],
        #  sim_table_se[,c(5,3:4)],
        sim_table_cov[,c(5,3:4)]) 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 3))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_1200_logistic} Simulation results for n = 1200 and a logistic mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC). We present the percent bias, the model standard error (SE), and the observed coverage probability (95\\% Coverage).") %>% 
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


########################
# LINEAR INTERCEPT/NOT #
########################


sim_table_bias <- data.frame()
sim_table_bias_per <- data.frame()

# for(censoring in c("indep_holds")){
for(censoring in c("exog_broken")){
  for(n in c(400)){
    string_new <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                         "_p2_sigma2-2_linear_truebeta-1r-2r0.5_startingvals-0r0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                         censoring, "_8-17.RDS")
    sim_new <- readRDS(string_new)
    
    sim_table_bias <- rbind(sim_table_bias, sim_new$bias %>% 
                              mutate(censoring = censoring,
                                     n = n, intercept = "Yes") %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new$bias_per %>% 
                                  mutate(censoring = censoring,
                                         n = n, intercept = "Yes")  %>% 
                                  filter(`Censoring Rate` !=0.5))
    
    string_new_no_int <- paste0("simulation_output/simulation_output_cc_83/simulation_numsims1000_n", n, 
                               "_p1_sigma2-2_linear_truebeta-1r-2_startingvals-0r0_censoringrates-0.25r0.5r0.75_censoringmech-", 
                               censoring, "_8-4.RDS")
    sim_new_no_int <- readRDS(string_new_no_int)
    colnames(sim_new_no_int$bias) = colnames(sim_new$bias)[-5]
    colnames(sim_new_no_int$bias_per) = colnames(sim_new$bias_per)[-5]
    sim_table_bias <- rbind(sim_table_bias, sim_new_no_int$bias %>% 
                              mutate( `$\\alpha_{2}$` = 0,
                                      censoring = censoring,
                                     n = n, intercept = "No") %>% 
                              filter(`Censoring Rate` !=0.5))
    sim_table_bias_per <- rbind(sim_table_bias_per, sim_new_no_int$bias_per %>% 
                                  mutate(`$\\alpha_{2}$` = 0,
                                        censoring = censoring,
                                         n = n, intercept = "No")  %>% 
                                  filter(`Censoring Rate` !=0.5))
  }
}

# table_full_bias = sim_table_bias[,c(1:2,5,3:4)] %>% 
#   cbind(sim_table_bias_per[,c(5,3:4)]) 
# colnames(table_full_bias) = c("Censoring Rate", "Method",
#                          "bias1", "bias2", "bias3",
#                          "per1", "per2", "per3")
# table_full_bias = table_full_bias %>% 
#   mutate(final_bias1 = paste0(round(bias1, 2), " (", round(per1, 2), ")"),
#          final_bias2 = paste0(round(bias2, 2), " (", round(per2, 2), ")"),
#          final_bias3 = paste0(round(bias3, 2), " (", round(per3, 2), ")")) %>% 
#   select(`Censoring Rate`, `Method`, final_bias1, final_bias2, final_bias3)

table_full = sim_table_bias_per[,c(1:2,5,3:4)] 
colnames(table_full) = c("Censoring Rate", "Method", 
                         rep(c("$\\wh{\\alpha}_0$",
                               "$\\wh{\\alpha}_1$",
                               "$\\wh{\\alpha}_2$"), 1))

table_full$Method = factor(table_full$Method,
                           labels = c("CC", "Oracle"))

tab1 = kbl(table_full, format = "latex", booktabs = T, digits = 2, escape = F,
           caption = "\\label{tab:sims_400_linear_int} Simulation results for n = 400 and a linear mean model, comparing the oracle method with the full set of observations (Oracle) to the complete case analysis (CC) when there is and is not an intercept in the true model and the specified model. We present the percent bias.") %>% 
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



















