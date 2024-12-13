################################################################################
####################### Empirical Application - Analysis #######################
################################################################################

############################ Script Description ################################
#
# Author: Cameron
# 
# Date Created: 12/11/24
#
#
# Script Description:   [NEED TO UPDATE]
#   This R script performs the mediation analysis for the empirical application. 
#   First, it calculate the Intraclass Correlation Coefficients (ICC) for the 
#   mediator and outcome variables. Next, it computes propensity scores (PSs) and 
#   Inverse Probability of Treatment Weight (IPTW) using the Single-Level, 
#   Fixed-Effect, and Random-Effect PS models, with percentile bootstrap confidence 
#   intervals. Covariate balance is visualized. Finally, it visualizes the 
# estimated effects to facilitate interpretation of the results.
# 
# Last Updated: 12/11/2024 
#
#
# Notes:
# 
################################################################################


# Set Up (Load packages, functions, &/or data) ----------------------------

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    tidyverse, 
    ggplot2, 
    extrafont, 
    stringr, 
    mice, 
    cobalt, 
    WeightIt, 
    boot, 
    utils, 
    lme4, 
    WeMix, 
    parallel
)

library(glue)
library(tidyverse)
library(mvtnorm)
library(SuperLearner)
library(origami)
library(fastDummies)

# Load Functions & Packages
# source("Application/Functions/bootstrap_ci_paral_2.R")
# source("Application/Functions/bootstrap_ci_re_paral_2.R")
# source("Application/Functions/bootstrap_ci_re_mean_paral.R")
source("Application/Functions/analyze_clustered_mediation.R")
devtools::load_all("Application/Functions/MediatorCL")

# Import Data -----------------------------------------------------
# Load clean dataset 
data <- read_rds(file = "Application/Data/Cleaned/Empirical-Application-Data.rds")














# Test analysis functions -------------------------------------------------

learners_a <- learners_m <- learners_y <- c("SL.glm","SL.nnet") 

# Sname <- "CLuster2"
data$CLUSTER2 <- as.factor(data$CLUSTER2)
# data$CLUSTER2 <- as.integer(data$CLUSTER2)


Sname <- "CLUSTER2" # have to specify this outside of MediatorCL function to work

results <- MediatorCL(
    data = data,
    Sname = colnames(data[, "CLUSTER2"]),
    Wnames = colnames(data[, "n"]),
    Xnames = colnames(data[, c(
        "age_w1_sc",
        "sex_w1",
        "white_w1",
        "black_w1",
        "parentalEdu_w1_sc",
        "familyStruct_w1",
        "feelings_w1_sc",
        "selfEst_w1_sc"
    )]),
    Aname = "sportPartic_w1",
    Mnames = "selfEst_w3_sc",
    Yname = "depress_w4",
    learners_a = learners_a,
    learners_m = learners_m,
    learners_y = learners_y,
    cluster_opt = "cwc.FE",
    num_folds = 5
)

results



# ("cwc.FE" & 5 folds)
# Effect Individual.Average_Estimate Individual.Average_StdError CI.lower_Individual.Average CI.upper_Individual.Average Cluster.Average_Estimate Cluster.Average_StdError
# 1     DE                 -0.13512198                  0.18894446                 -0.51238173                  0.24213777               0.10508977               0.24626812
# 2     IE                 -0.01900388                  0.03766539                 -0.09420924                  0.05620147              -0.06424489               0.04711085
# CI.lower_Cluster.Average CI.upper_Cluster.Average
# 1               -0.3866264               0.59680596
# 2               -0.1583097               0.02981994
# Warning messages:
#     1: In fold_fun(n, ...) :
#     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 


# OLD ("cwc" & 4 folds)
# Effect Individual.Average_Estimate Individual.Average_StdError CI.lower_Individual.Average CI.upper_Individual.Average Cluster.Average_Estimate Cluster.Average_StdError
# 1     DE                 -0.21489473                   0.1735398                 -0.56139638                  0.13160692              -0.07325174               0.21439062
# 2     IE                 -0.01923879                   0.0381667                 -0.09544509                  0.05696751              -0.03684349               0.04346996
# CI.lower_Cluster.Average CI.upper_Cluster.Average
# 1               -0.5013191               0.35481559
# 2               -0.1236387               0.04995168




