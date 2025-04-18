################################################################################
####################### Empirical Application - Analysis #######################
################################################################################

############################ Script Description ################################
#
# Author: Cameron
# 
# Date Created: 2025-04-14
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
# Last Updated: 2025-04-14
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

# source("Application/Functions/analyze_clustered_mediation.R")
# devtools::load_all("Application/Functions/MediatorCL")



# ══════════════════════════════
#     Source Updated Functions
# ══════════════════════════════
# Functions are from Functions folder in directory 
function_names <- c(
    # # "generate_data", 
    # "generate_clusters", 
    # "generate_confounders", 
    # "generate_treatment", 
    # "generate_mediator", 
    # "generate_outcome", 
    # "pm1", 
    # "my", 
    # # "trueVals", 
    # 
    # # As of 01/01/2025 the two funcs below are the updated versions 
    # "generate_data2.0c", 
    # # "trueVals2.0c",
    # "trueVals2.0d",
    # "trueVals2.0f",
    
    # Estimation functions 
    "crossfit", 
    "make_fold_K", 
    "eif", 
    "a.c",
    "a.mc",
    "mu.mac",
    "v.ac", 
    "mu.ac",
    "get_inference", 
    "internal_estimate_mediation", 
    "bound", 
    "effect", 
    "estimate_mediation"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}



# Import Data -----------------------------------------------------
# Load clean dataset 
data <- read_rds(file = "Application/Data/Cleaned/Empirical-Application-Data.rds")

# # Drop clusters < 8
# data <- data |> 
#     filter(CLUSTER2 != 274)

# Need
## ICC
## Descriptives on cluster size & number of clusters

# ══════════════════════════════
#    Treatment Proportion 
# ══════════════════════════════
# check proportion of adolescents in treatment per cluster 
trt_df <- data |> 
    group_by(CLUSTER2) |> 
    reframe(size = max(n), 
            trt_num = sum(sportPartic_w1), 
            trt_prop = sum(sportPartic_w1) / n)
psych::describe(trt_df$trt_prop)

trt_df[trt_df$trt_prop > 0.99 | trt_df$trt_prop < 0.01, ]
# Drop cluster with extreme trt proportion 
data <- data |>
    filter(CLUSTER2 != 216)
# cluster_sizes[cluster_sizes$cluster_size < 10, ]

# ══════════════════════════════
#    Cluster Sizes 
# ══════════════════════════════
# cluster size description 
psych::describe(data)
# head(data)

# Calculate the size (i.e., number of observations) within each cluster
cluster_sizes <- data %>%
    group_by(CLUSTER2) %>%
    summarise(cluster_size = n()) %>% 
    ungroup()

# Compute overall descriptive statistics for cluster sizes
cluster_summary <- cluster_sizes %>%
    summarise(
        total_clusters   = n(),
        min_cluster_size = min(cluster_size),
        max_cluster_size = max(cluster_size),
        mean_cluster_size = mean(cluster_size),
        median_cluster_size = median(cluster_size),
        sd_cluster_size  = sd(cluster_size)
    )
psych::describe(cluster_sizes)

# Print the descriptive summary
print(cluster_summary)



# 
# # learners_a <- learners_m <- learners_y <- c("SL.glm","SL.nnet") 
# learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
# num_folds <- 5
# 
# learners_a <- learners_m <- learners_y <- c("SL.glm")
# num_folds <- 1
# 
# data$CLUSTER2 <- as.factor(data$CLUSTER2)
# 
# cluster_opt <- "cwc.FE" #c("cwc", "cwc.FE")
# 
# 
# result <- estimate_mediation(
#     data = data,
#     Sname = "CLUSTER2",
#     Wnames = NULL,
#     Xnames = colnames(data[, c(
#         "age_w1_sc",
#         "sex_w1",
#         "white_w1",
#         "black_w1",
#         "parentalEdu_w1_sc",
#         "familyStruct_w1",
#         "feelings_w1_sc",
#         "selfEst_w1_sc"
#     )]),
#     Aname = "sportPartic_w1",
#     Mnames = "selfEst_w3_sc",
#     Yname = "depress_w4",
#     learners_a = learners_a,
#     learners_m = learners_m,
#     learners_y = learners_y,
#     cluster_opt = cluster_opt, 
#     num_folds = num_folds
# )
# 
# # library(BRRR)
# BRRR::skrrrahh("biggie") #BRRR::skrrrahh_list()
# 
# result
# 
# # MLR cwc.FE 
# # Warning message:
# #     In fold_fun(n, ...) :
# #     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n
# #               Effect  EffectVersion    Estimate   StdError    CILower      CIUpper
# # 1   Direct Effect (DE) Individual-Avg -0.09030192 0.19032598 -0.4703201  0.289716265
# # 2 Indirect Effect (IE) Individual-Avg -0.08516870 0.04587713 -0.1767702  0.006432804
# # 3   Direct Effect (DE)    Cluster-Avg  0.13349486 0.24511423 -0.3559174  0.622907098
# # 4 Indirect Effect (IE)    Cluster-Avg -0.12056028 0.05179921 -0.2239862 -0.017134344
# 
# # MLR cwc
# # Warning message:
# #     In fold_fun(n, ...) :
# #     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n
# #               Effect  EffectVersion    Estimate   StdError     CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg -0.21205782 0.17531910 -0.56211222 0.13799657
# # 2 Indirect Effect (IE) Individual-Avg -0.01877642 0.03658185 -0.09181831 0.05426546
# # 3   Direct Effect (DE)    Cluster-Avg -0.08180177 0.21710613 -0.51529108 0.35168754
# # 4 Indirect Effect (IE)    Cluster-Avg -0.03215435 0.04100405 -0.11402592 0.04971722
# 
# 
# # GLM cwc 
# #             Effect  EffectVersion    Estimate   StdError     CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg -0.22356145 0.17399670 -0.57097545 0.12385255
# # 2 Indirect Effect (IE) Individual-Avg -0.01987773 0.03658629 -0.09292848 0.05317302
# # 3   Direct Effect (DE)    Cluster-Avg -0.06377357 0.21581915 -0.49469320 0.36714606
# # 4 Indirect Effect (IE)    Cluster-Avg -0.04836699 0.04264549 -0.13351596 0.03678198
# # GLM cwc.FE 
# #             Effect  EffectVersion    Estimate   StdError     CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg -0.22208727 0.17399755 -0.56950296 0.12532841
# # 2 Indirect Effect (IE) Individual-Avg -0.01963776 0.03665063 -0.09281697 0.05354144
# # 3   Direct Effect (DE)    Cluster-Avg -0.06152678 0.21593426 -0.49267625 0.36962269
# # 4 Indirect Effect (IE)    Cluster-Avg -0.04808004 0.04263300 -0.13320408 0.03704401





# Estimate Effects --------------------------------------------------------

# Define the four sets of parameters for the mediation analysis.
param_list <- list(
    # Setup 1: learners = c("SL.nnet", "SL.gam"), num_folds = 5, cluster_opt = "cwc"
    list(learners = c("SL.nnet", "SL.gam"), num_folds = 5, cluster_opt = "cwc"),
    
    # Setup 2: learners = c("SL.nnet", "SL.gam"), num_folds = 5, cluster_opt = "cwc.FE"
    list(learners = c("SL.nnet", "SL.gam"), num_folds = 5, cluster_opt = "cwc.FE"),
    
    # Setup 3: learners = c("SL.glm"), num_folds = 1, cluster_opt = "cwc"
    list(learners = c("SL.glm"), num_folds = 1, cluster_opt = "cwc"),
    
    # Setup 4: learners = c("SL.glm"), num_folds = 1, cluster_opt = "cwc.FE"
    list(learners = c("SL.glm"), num_folds = 1, cluster_opt = "cwc.FE")
)

# Prepare a list to hold the results for each run
results_list <- list()

# Loop through each set of parameters, run the mediation estimation, and save the outputs.
for (i in seq_along(param_list)) {
    
    # Extract current parameter set
    params <- param_list[[i]]
    
    # Run the mediation analysis
    tmp <- estimate_mediation(
        data = data,
        Sname = "CLUSTER2",
        Wnames = NULL,
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
        learners_a = params$learners,
        learners_m = params$learners,
        learners_y = params$learners,
        cluster_opt = params$cluster_opt, 
        num_folds = params$num_folds
    )
    
    # Attach the parameters as extra columns to the output data frame.
    tmp$learners <- paste(params$learners, collapse = ", ")
    tmp$num_folds <- params$num_folds
    tmp$cluster_opt <- params$cluster_opt
    
    # Save the updated result into the results list.
    results_list[[i]] <- tmp

}

# Option 1: Save each output as separate objects:
result1 <- results_list[[1]]
result2 <- results_list[[2]]
result3 <- results_list[[3]]
result4 <- results_list[[4]]

# Option 2: Combine them into one large data frame 
# by adding a 'setup' indicator to each result.
combined_results <- do.call(rbind, lapply(seq_along(results_list), function(i) {
    df <- results_list[[i]]
    df$setup <- paste0("Setup_", i)
    df
}))

combined_results <- combined_results |> 
    mutate(Method = ifelse(learners == "SL.glm", "Parametric", "Nonparametric"))

# If you want to view the combined results:
print(combined_results)


# library(BRRR)
BRRR::skrrrahh("biggie") #BRRR::skrrrahh_list()

# Display results ---------------------------------------------------------

# visual settings
gglayer_theme <- list(theme_bw(),
                      scale_color_manual(values = c("#BF5700", #Fixed-effect
                                                    "#A6CD57", #Random-effect
                                                    "#333F48")), #Single-level
                      scale_fill_manual(values = c("#BF5700", #Fixed-effect
                                                   "#A6CD57", #Random-effect
                                                   "#333F48")), #Single-level
                      #"#9CADB7" <-- light gray
                      # Used following website with university colors: https://projects.susielu.com/viz-palette?
                      theme(text = element_text(family = "arial", size = 12), # "Times New Roman", size = 12), #
                            axis.title = element_text(size = 12),  # Adjust axis title size
                            axis.text = element_text(size = 10),  # Adjust axis text size
                            legend.title = element_text(size = 14), #12),  # Legend title size
                            legend.text = element_text(size = 10),  # Legend text size
                            strip.text = element_text(size = 12),  # Facet labels
                            line = element_line(linewidth = 0.5),  # APA recommends thin lines
                            legend.position = "top" #"bottom" #"right"
                      ))

combined_results |> 
    # filter(EffectVersion == "Individual-Avg") |>
    filter(Effect == "Indirect Effect (IE)") |>
    ggplot(aes(x = Estimate, 
               y = cluster_opt, #y = reorder(Effect, Estimate), 
               color = Method)) +
    geom_vline(xintercept = 0) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.1, 
                   position = position_dodge(width = 0.2)) +
    facet_grid(EffectVersion~Effect, scales = "free_y") +
    # facet_wrap(~Effect, scales = "free_y") +  # separate panels by Method
    theme_minimal() +
    
    xlab("Estimate (95% CI)") +
    ylab("Effect Type and Version") +
    ggtitle("Forest Plot of Estimates and 95% Confidence Intervals")

    

combined_results |> 
    # filter(EffectVersion == "Individual-Avg") |>
    # filter(Effect == "Indirect Effect (IE)") |>
    ggplot(aes(x = Estimate, 
               y = cluster_opt, #y = reorder(Effect, Estimate), 
               color = Method)) +
    geom_vline(xintercept = 0) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.1, 
                   position = position_dodge(width = 0.2)) +
    facet_grid(EffectVersion~Effect, scales = "free_x") +
    # facet_wrap(~Effect, scales = "free_y") +  # separate panels by Method
    theme_minimal() +
    # coord_flip() +
    gglayer_theme +
    
    xlab("Estimate (95% CI)") +
    ylab("Effect Type and Version") +
    ggtitle("Forest Plot of Estimates and 95% Confidence Intervals")

combined_results |> 
    mutate(meth = paste0(Method, ": ", cluster_opt))

ggplot(all_results, aes(x = Estimate, 
                        y = reorder(EffectLabel, Estimate),  # You can reorder by Estimate or keep as is
                        color = Method)) +  # Optional coloring by Method
    geom_point() +
    geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.3) +
    facet_wrap(~ Method, scales = "free_y") +  # separate panels by Method
    theme_minimal() +
    xlab("Estimate (95% CI)") +
    ylab("Effect Type and Version") +
    ggtitle("Forest Plot of Estimates and 95% Confidence Intervals")


# ══════════════════════════════
#    Estimate Table 
# ══════════════════════════════
# install.packages(c("dplyr", "tidyr", "kableExtra"))  # if needed
library(dplyr)
library(tidyr)
library(kableExtra)

# Suppose your data frame is already called combined_results.
# (If not, create/modify it as needed.)

table_df <- combined_results %>%
    # Create a single 95% CI column: "[lower, upper]"
    mutate(`95% CI` = paste0("[", round(CILower, 3), ", ", round(CIUpper, 3), "]")) %>%
    
    # Select the columns that matter
    select(Effect, cluster_opt, Method, EffectVersion, Estimate, `95% CI`) %>%
    
    # Pivot from long to wide; Individual-Avg and Cluster-Avg go into columns
    pivot_wider(
        names_from  = EffectVersion,
        values_from = c(Estimate, `95% CI`)
    ) %>%
    
    # Arrange rows by Effect, cluster_opt, Method
    arrange(Effect, cluster_opt, Method) %>%
    
    # For repeated Effects, show it only on the first row (others blank)
    group_by(Effect) %>%
    mutate(Effect = ifelse(row_number() == 1, Effect, ""), 
           cluster_opt = ifelse(row_number() %in% c(1, 5), "cluster means", 
                                ifelse(row_number() %in% c(3, 7), "cluster means + dummies", ""))) %>%
    ungroup() %>%
    mutate(`Estimate_Individual-Avg` = round(`Estimate_Individual-Avg`, 3), 
           `Estimate_Cluster-Avg` = round(`Estimate_Cluster-Avg`, 3), 
           Effect = ifelse(Effect == "Direct Effect (DE)", "NDE", 
                           ifelse(Effect == "Indirect Effect (IE)", "NIE", ""))) |> 
    select("Effect":"Estimate_Individual-Avg", "95% CI_Individual-Avg", 
           "Estimate_Cluster-Avg", "95% CI_Cluster-Avg") |> 
    # Rename columns for clarity before we build the table
    rename(
        `Cluster-level adjustment` = `cluster_opt`,
        `Estimate` = `Estimate_Individual-Avg`,
        `95% CI`   = `95% CI_Individual-Avg`,
        `Estimate `= `Estimate_Cluster-Avg`,
        `95% CI `  = `95% CI_Cluster-Avg`
    )

# Now build a table with multi-level column headers
estimate_table <- kable(table_df,
          caption = "Table 2. Effect Estimates for Empirical Application",
          align    = "l",
          booktabs = TRUE) %>%
    # Add "Individual-Average" spanning 2 columns, "Cluster-Average" spanning 2 columns
    add_header_above(c(
        " " = 3,
        "Individual-Average" = 2,
        "Cluster-Average" = 2
    )) |>
    kableExtra::kable_styling(
        bootstrap_options = c("striped", "condensed"),
        stripe_color = "#F2F2F2", 
        full_width = FALSE, 
        font_size = 14 # Increase font size for posters
    ) |> 
    kableExtra::row_spec(0, bold = FALSE, align = "c") |> 
    kableExtra::footnote(general = "CI = Confidence Interval; NDE = Nautral Direct Effect; NIE = Nautral Indirect Effect") |> 
    kableExtra::kable_classic()

estimate_table

kableExtra::save_kable(estimate_table, 
                       file = "Application/Output/Estimate-Table.png", 
                       zoom = 2)






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




