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
  x = c(X = -0.25, C = 0, Y = 2, D = 0.5, Z = -1),
  y = c(X = 0.15, Y = 0, C = -1, D = -0.25, Z = 0.5)
)

########################################
##### Defining direct paths/labels #####
########################################

dag1 <- dagify(Y ~ X,
              X ~ Z,
              Y ~ Z,
            #  C ~ Z,
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
             #  Y ~ Z,
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
                          "Z" = "CAG Repeat Length & Age"))

####################
##### Plotting #####
####################

dag1_plot <- ggdag(dag1,
      text = FALSE,
      use_labels = "label",
      text_size = 2.5) + 
  theme_void() + 
  geom_dag_text(parse = TRUE, 
                label = c("C", TeX("\\Delta"), "X", "Y", "Z"))
                      

dag2_plot <- ggdag(dag2,
      text = FALSE,
      use_labels = "label",
      text_size = 2.5) + theme_void() + 
  geom_dag_text(parse = TRUE, 
                label = c("C", TeX("\\Delta"), "X", "Y", "Z"))

###################################
##### Plotting on same figure #####
###################################

dags = cowplot::plot_grid(dag2_plot, dag1_plot,
                          align = "hv", axis = "tblr", labels = c("a)", "b)"),
                          nrow = 1, hjust = -10)
dags

