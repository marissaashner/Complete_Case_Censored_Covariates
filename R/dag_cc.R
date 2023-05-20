##########################################################
# This script creates the DAG figures for the manuscript #
##########################################################

###########################
##### Packages Needed #####
###########################

library(tidyverse)
# install.packages("ggdag")
library(ggdag)
# install.packages("cowplot")
library(cowplot)
library(latex2exp)

########################
##### Coordinates ######
########################

coord_dag = list(
  x = c(X = 0, C = 0, Y = 0.05, D = 0.025, Z = -0.015),
  y = c(X = 0, Y = -0.05, C = -0.1, D = -0.035, Z = 0.05)
)

########################################
##### Defining direct paths/labels #####
########################################

dag1 <- dagify(Y ~ X,
              X ~ Z,
              Y ~ Z,
              C ~ Z,
              D ~ X,
              D ~ C,
            #  C ~ X,
              C ~ Y,
              coords = coord_dag,
              labels = c("X" = "Time to Diagnosis",
                         "C" = "Time to Censoring",
                         "Y" = "Total Motor Score",
                         "D" = "Censoring Indicator",
                         "Z" = "CAG Repeat Length & Age"))
dag2 <- dagify(Y ~ X,
               X ~ Z,
               Y ~ Z,
               C ~ Z,
               D ~ X,
               D ~ C,
            #   C ~ X,
             #  Y ~ C,
               coords = coord_dag,
               labels = c("X" = "Time to Diagnosis",
                          "C" = "Time to Censoring",
                          "Y" = "Apathy",
                          "D" = "Censoring Indicator",
                          "Z" = "CAG, Age, & Total Motor Score"))

####################
##### Plotting #####
####################

dag1_plot <- ggdag(dag1,
      text = FALSE,
      use_labels = "label",
      node_size = 8,
      text_size = 4) +
  theme_void() +
  geom_dag_text(parse = TRUE,
                label = c("C", TeX("\\Delta"), "X", "Y", "Z"))


dag2_plot <- ggdag(dag2,
      text = FALSE,
      use_labels = "label",
      node_size = 8,
      text_size = 4) + theme_void() +
  geom_dag_text(parse = TRUE,
                label = c("C", TeX("\\Delta"), "X", "Y", "Z"))

dag2_plot

###################################
##### Plotting on same figure #####
###################################

dags = cowplot::plot_grid(dag2_plot, dag1_plot,
                          align = "hv", axis = "tblr", labels = c("a)", "b)"),
                          nrow = 1, hjust = -10)
dags

