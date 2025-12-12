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
# Last Updated: 2025-11-11
#
#
# Notes:
#   # Need to re-run empirical applicaiton (especially with updated functions). 
#       Also clarify the larger neg association btw T & Y in larger clusters statement in paper. 
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
    parallel, 
    kable
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

# Descriptives of treatment proportion 
psych::describe(trt_df$trt_prop)

# Display those in a cluster with extreme treatment proportion
trt_df[trt_df$trt_prop > 0.99 | trt_df$trt_prop < 0.01, ]

# Drop cluster with extreme trt proportion 
data <- data |>
    filter(CLUSTER2 != 216)

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

# Descriptives on cluster size 
psych::describe(cluster_sizes)

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

# Print the descriptive summary
print(t(cluster_summary))



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

# set depth constraint for xgboost
xgb_depth2 <- create.Learner(
    "SL.xgboost",
    tune = list(max_depth = 2), 
    detailed_names = TRUE, 
    name_prefix = "xgb_d2"
)

# Define the four sets of parameters for the mediation analysis.
param_list <- list(
    
    # Setup 1: nonparametric with cluster mean & dummies 
    "nonparametric-cwc.FE" = list(
        learners = c("SL.mean", "SL.glm",
                     "SL.glmnet", # lasso/elastic-net
                     "SL.gam", # non-linearity
                     "SL.ranger", # random forest
                     xgb_depth2$names), # boosted trees with max depth of 2
        num_folds = 5, 
        cluster_opt = "cwc.FE"
    ),
    
    # Setup 2: nonparametric with cluster mean 
    "nonparametric-cwc" = list(
        learners = c("SL.mean", "SL.glm",
                     "SL.glmnet", # lasso/elastic-net
                     "SL.gam", # non-linearity
                     "SL.ranger", # random forest
                     xgb_depth2$names), # boosted trees with max depth of 2
        num_folds = 5, 
        cluster_opt = "cwc"
    ),
    
    # Setup 3: parametric (glm) with cluster mean
    "parametric-cwc" = list(
        learners = c("SL.glm"), 
        num_folds = 1, 
        cluster_opt = "cwc"
    ),
    
    # Setup 4: parametric (glm) with cluster mean & dummies 
    "parametric-cwc.FE" = list(
        learners = c("SL.glm"), 
        num_folds = 1, 
        cluster_opt = "cwc.FE"
    ), 
    
    # Setup 5: parametric (RE.glm) with cluster mean
    "parametric.RE-cwc" = list(
        learners = c("RE.glm"), 
        num_folds = 1, 
        cluster_opt = "RE.glm"
    )
)

# Select models to run 
param_list <- param_list[names(param_list) %in% c(c("nonparametric-cwc.FE", "nonparametric-cwc", "parametric-cwc", "parametric-cwc.FE"))]
# param_list <- param_list[names(param_list) %in% c(c("parametric-cwc", "parametric-cwc.FE"))]
# param_list <- param_list[names(param_list) %in% c(c("parametric.RE-cwc"))]

# Prepare a list to hold the results for each run
results_list <- list()

# Loop through each set of parameters, run the mediation estimation, and save the outputs.
for (i in seq_along(param_list)) {
    
    # Extract current parameter set
    params <- param_list[[i]]
    
    # Run the mediation analysis
    set.seed(5897)
    start_time <- Sys.time()
    
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
    
    end_time <- Sys.time()
    tmp$duration_secs <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Attach the parameters as extra columns to the output data frame.
    tmp$learners <- paste(params$learners, collapse = ", ")
    tmp$num_folds <- params$num_folds
    tmp$cluster_opt <- params$cluster_opt
    
    # Save the updated result into the results list.
    results_list[[i]] <- tmp
    
    # Assign element name 
    names(results_list[i]) <- names(param_list[i])

}

# Save each output as separate objects:
# result1 <- results_list[[1]]
# result2 <- results_list[[2]]
# result3 <- results_list[[3]]
# result4 <- results_list[[4]]

# Add setup name 
names(results_list) <- names(param_list)

# Combine results into one large data frame with 'setup' indicator 
combined_results <- do.call(rbind, lapply(seq_along(results_list), function(i) {
    df <- results_list[[i]]
    # df$setup <- paste0("Setup_", i)
    df$setup <- names(results_list[i])
    df
}))

# combined_results <- combined_results |> 
#     mutate(Method = ifelse(learners == "SL.glm", "Parametric", "Nonparametric"))

# view results
print(combined_results)
# on 2025-11-09:
#                  Effect  EffectVersion    Estimate   StdError     CILower    CIUpper duration_secs                                                learners num_folds cluster_opt                setup
# 1    Direct Effect (DE) Individual-Avg -0.22984178 0.17243742 -0.57419650 0.11451294   1291.682369 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5      cwc.FE nonparametric-cwc.FE
# 2  Indirect Effect (IE) Individual-Avg -0.01763323 0.03665058 -0.09082385 0.05555739   1291.682369 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5      cwc.FE nonparametric-cwc.FE
# 3    Direct Effect (DE)    Cluster-Avg -0.08157478 0.21213742 -0.50520975 0.34206019   1291.682369 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5      cwc.FE nonparametric-cwc.FE
# 4  Indirect Effect (IE)    Cluster-Avg -0.03663675 0.04117814 -0.11886882 0.04559532   1291.682369 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5      cwc.FE nonparametric-cwc.FE
# 
# 5    Direct Effect (DE) Individual-Avg -0.21925001 0.17480952 -0.56834177 0.12984175    372.411315 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5         cwc    nonparametric-cwc
# 6  Indirect Effect (IE) Individual-Avg -0.02577498 0.03842482 -0.10250872 0.05095876    372.411315 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5         cwc    nonparametric-cwc
# 7    Direct Effect (DE)    Cluster-Avg -0.07282350 0.21616096 -0.50449342 0.35884642    372.411315 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5         cwc    nonparametric-cwc
# 8  Indirect Effect (IE)    Cluster-Avg -0.05283816 0.04359051 -0.13988770 0.03421138    372.411315 SL.mean, SL.glm, SL.glmnet, SL.gam, SL.ranger, xgb_d2_2         5         cwc    nonparametric-cwc
# 
# 9    Direct Effect (DE) Individual-Avg -0.22358566 0.17397045 -0.57100181 0.12383048      1.015441                                                  SL.glm         1         cwc       parametric-cwc
# 10 Indirect Effect (IE) Individual-Avg -0.01987190 0.03647735 -0.09271657 0.05297276      1.015441                                                  SL.glm         1         cwc       parametric-cwc
# 11   Direct Effect (DE)    Cluster-Avg -0.06406657 0.21578868 -0.49499304 0.36685990      1.015441                                                  SL.glm         1         cwc       parametric-cwc
# 12 Indirect Effect (IE)    Cluster-Avg -0.04829391 0.04251428 -0.13319424 0.03660642      1.015441                                                  SL.glm         1         cwc       parametric-cwc
# 
# 13   Direct Effect (DE) Individual-Avg -0.22212596 0.17414957 -0.56989981 0.12564788     10.191986                                                  SL.glm         1      cwc.FE    parametric-cwc.FE
# 14 Indirect Effect (IE) Individual-Avg -0.01968223 0.03667916 -0.09292991 0.05356546     10.191986                                                  SL.glm         1      cwc.FE    parametric-cwc.FE
# 15   Direct Effect (DE)    Cluster-Avg -0.06171545 0.21598918 -0.49304231 0.36961142     10.191986                                                  SL.glm         1      cwc.FE    parametric-cwc.FE
# 16 Indirect Effect (IE)    Cluster-Avg -0.04822679 0.04269630 -0.13349060 0.03703703     10.191986                                                  SL.glm         1      cwc.FE    parametric-cwc.FE
# 
# 17   Direct Effect (DE) Individual-Avg -0.22212072 0.17156458 -0.56473239 0.12049095      3.180026                                                  RE.glm         1      RE.glm  parametric.RE-cwc
# 18 Indirect Effect (IE) Individual-Avg -0.01641018 0.03501510 -0.08633477 0.05351441      3.180026                                                  RE.glm         1      RE.glm  parametric.RE-cwc
# 19   Direct Effect (DE)    Cluster-Avg -0.06153963 0.21288768 -0.48667286 0.36359360      3.180026                                                  RE.glm         1      RE.glm  parametric.RE-cwc
# 20 Indirect Effect (IE)    Cluster-Avg -0.04334094 0.04116805 -0.12555286 0.03887099      3.180026                                                  RE.glm         1      RE.glm  parametric.RE-cwc



# library(BRRR)
# BRRR::skrrrahh("waka")
BRRR::skrrrahh("biggie") #BRRR::skrrrahh_list()

# Save
readr::write_csv(
    combined_results, 
    file = paste0("Application/Output/Effect-Estimates_", Sys.Date(), ".csv"),
    col_names = TRUE
)


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
    # filter(Effect == "Indirect Effect (IE)") |>
    ggplot(aes(x = Estimate, 
               y = EffectVersion, #y = reorder(Effect, Estimate), 
               color = setup, 
               shape = cluster_opt)) +
    geom_vline(xintercept = 0) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.1, 
                   position = position_dodge(width = 0.2)) +
    facet_grid(~Effect, scales = "free_x") +
    # facet_wrap(~Effect, scales = "free_y") +  # separate panels by Method
    theme_minimal() #+
    # coord_flip() +
    # gglayer_theme 



combined_results |> 
    # filter(EffectVersion == "Individual-Avg") |>
    filter(Effect == "Indirect Effect (IE)") |>
    ggplot(aes(x = Estimate, 
               y = cluster_opt, #y = reorder(Effect, Estimate), 
               color = setup, 
               shape = cluster_opt)) +
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

    

# combined_results |> 
#     # filter(EffectVersion == "Individual-Avg") |>
#     # filter(Effect == "Indirect Effect (IE)") |>
#     ggplot(aes(x = Estimate, 
#                y = cluster_opt, #y = reorder(Effect, Estimate), 
#                color = Method)) +
#     geom_vline(xintercept = 0) +
#     geom_point(position = position_dodge(width = 0.2)) +
#     geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.1, 
#                    position = position_dodge(width = 0.2)) +
#     facet_grid(EffectVersion~Effect, scales = "free_x") +
#     # facet_wrap(~Effect, scales = "free_y") +  # separate panels by Method
#     theme_minimal() +
#     # coord_flip() +
#     gglayer_theme +
#     
#     xlab("Estimate (95% CI)") +
#     ylab("Effect Type and Version") +
#     ggtitle("Forest Plot of Estimates and 95% Confidence Intervals")
# 
# combined_results |> 
#     mutate(meth = paste0(Method, ": ", cluster_opt))
# 
# ggplot(all_results, aes(x = Estimate, 
#                         y = reorder(EffectLabel, Estimate),  # You can reorder by Estimate or keep as is
#                         color = Method)) +  # Optional coloring by Method
#     geom_point() +
#     geom_errorbarh(aes(xmin = CILower, xmax = CIUpper), height = 0.3) +
#     facet_wrap(~ Method, scales = "free_y") +  # separate panels by Method
#     theme_minimal() +
#     xlab("Estimate (95% CI)") +
#     ylab("Effect Type and Version") +
#     ggtitle("Forest Plot of Estimates and 95% Confidence Intervals")



# 
# # ══════════════════════════════
# #    Estimate Table 
# # ══════════════════════════════
# # install.packages(c("dplyr", "tidyr", "kableExtra"))  # if needed
# library(dplyr)
# library(tidyr)
# library(kableExtra)
# 
# # Suppose your data frame is already called combined_results.
# # (If not, create/modify it as needed.)
# 
# table_df <- combined_results %>%
#     # Create a single 95% CI column: "[lower, upper]"
#     mutate(`95% CI` = paste0("[", round(CILower, 3), ", ", round(CIUpper, 3), "]")) %>%
#     
#     # Select the columns that matter
#     select(Effect, #cluster_opt, Method, 
#            EffectVersion, Estimate, `95% CI`) %>%
#     
#     # Pivot from long to wide; Individual-Avg and Cluster-Avg go into columns
#     pivot_wider(
#         names_from  = EffectVersion,
#         values_from = c(Estimate, `95% CI`)
#     ) %>%
#     
#     # Arrange rows by Effect, cluster_opt, Method
#     # arrange(Effect, cluster_opt, Method) %>%
#     
#     # For repeated Effects, show it only on the first row (others blank)
#     group_by(Effect) %>%
#     mutate(Effect = ifelse(row_number() == 1, Effect, "")) |> #, 
#            # cluster_opt = ifelse(row_number() %in% c(1, 5), "cluster means", 
#            #                      ifelse(row_number() %in% c(3, 7), "cluster means + dummies", ""))) %>%
#     ungroup() %>%
#     mutate(`Estimate_Individual-Avg` = round(`Estimate_Individual-Avg`, 3), 
#            `Estimate_Cluster-Avg` = round(`Estimate_Cluster-Avg`, 3), 
#            Effect = ifelse(Effect == "Direct Effect (DE)", "NDE", 
#                            ifelse(Effect == "Indirect Effect (IE)", "NIE", ""))) |> 
#     select("Effect":"Estimate_Individual-Avg", "95% CI_Individual-Avg", 
#            "Estimate_Cluster-Avg", "95% CI_Cluster-Avg") |> 
#     # Rename columns for clarity before we build the table
#     rename(
#         # `Cluster-level adjustment` = `cluster_opt`,
#         `Estimate` = `Estimate_Individual-Avg`,
#         `95% CI`   = `95% CI_Individual-Avg`,
#         `Estimate `= `Estimate_Cluster-Avg`,
#         `95% CI `  = `95% CI_Cluster-Avg`
#     )
# 
# # Now build a table with multi-level column headers
# estimate_table <- kable(table_df,
#           caption = "Table 2. Effect Estimates for Empirical Application",
#           align    = "l",
#           booktabs = TRUE)  |> 
#     # Add "Individual-Average" spanning 2 columns, "Cluster-Average" spanning 2 columns
#     kableExtra::add_header_above(c(
#         " " = 1, #3,
#         "Individual-Average" = 2,
#         "Cluster-Average" = 2
#     )) |>
#     kableExtra::kable_styling(
#         bootstrap_options = c("striped", "condensed"),
#         stripe_color = "#F2F2F2", 
#         full_width = FALSE, 
#         font_size = 14 # Increase font size for posters
#     ) |> 
#     kableExtra::row_spec(0, bold = FALSE, align = "c") |> 
#     kableExtra::footnote(general = "CI = Confidence Interval; NDE = Nautral Direct Effect; NIE = Nautral Indirect Effect") |> 
#     kableExtra::kable_classic()
# 
# estimate_table
# 
# kableExtra::save_kable(estimate_table, 
#                        file = "Application/Output/Estimate-Table.png", 
#                        zoom = 2)
# 
# 
# 
# 
# 
# # check corr
# data |> 
#     group_by(CLUSTER2) |> 
#     summarise(
#         n            = n(),
#         correlation  = cor(sportPartic_w1, depress_w4, use = "complete.obs"),
#         .groups      = "drop"
#     ) |> 
#     arrange(n) |> 
#     print(n = 120) |> 
#     mutate(
#         n_quartile = ntile(n, 4),
#         quartile_lbl = case_when(
#             n_quartile == 1 ~ "bottom 25%",
#             n_quartile == 2 ~ "25–50%",
#             n_quartile == 3 ~ "50–75%",
#             n_quartile == 4 ~ "top 25%"
#         )
#     ) |> 
#     group_by(quartile_lbl) %>%
#     summarise(
#         clusters      = n(),          
#         min_n         = min(n),
#         max_n         = max(n),
#         mean_n        = mean(n),
#         mean_corr     = mean(correlation),
#         sd_corr       = sd(correlation),
#         median_corr   = median(correlation),
#         .groups       = "drop"
#     ) %>%
#     arrange(match(quartile_lbl,
#                   c("bottom 25%", "25–50%", "50–75%", "top 25%")))
# 
# # Test analysis functions -------------------------------------------------
# 
# learners_a <- learners_m <- learners_y <- c("SL.glm","SL.nnet") 
# 
# # Sname <- "CLuster2"
# data$CLUSTER2 <- as.factor(data$CLUSTER2)
# # data$CLUSTER2 <- as.integer(data$CLUSTER2)
# 
# 
# Sname <- "CLUSTER2" # have to specify this outside of MediatorCL function to work
# 
# results <- MediatorCL(
#     data = data,
#     Sname = colnames(data[, "CLUSTER2"]),
#     Wnames = colnames(data[, "n"]),
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
#     cluster_opt = "cwc.FE",
#     num_folds = 5
# )
# 
# results
# 


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




