################################################################################
####################### Empirical Application - Analysis #######################
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-04-14
#
#
# Script Description:   
#   This R script performs the mediation analysis for the empirical application. 
# 
# Last Updated: 2026-01-29
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
    psych, 
    boot, 
    utils, 
    parallel, 
    kable,
    glue, 
    mvtnorm, 
    SuperLearner, 
    origami, 
    fastDummies
)

# ══════════════════════════════
#     Source Updated Functions
# ══════════════════════════════
# Functions are from Functions folder in directory 
function_names <- c(
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
    source(file.path("Application/Functions", paste0(func, ".R")))
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
        cluster_opt = "cwc.FE",
        random_slope_vars_a = NULL,
        random_slope_vars_m = NULL,
        random_slope_vars_y = NULL
    ),
    
    # Setup 2: nonparametric with cluster mean 
    "nonparametric-cwc" = list(
        learners = c("SL.mean", "SL.glm",
                     "SL.glmnet", # lasso/elastic-net
                     "SL.gam", # non-linearity
                     "SL.ranger", # random forest
                     xgb_depth2$names), # boosted trees with max depth of 2
        num_folds = 5, 
        cluster_opt = "cwc",
        random_slope_vars_a = NULL,
        random_slope_vars_m = NULL,
        random_slope_vars_y = NULL
    ),
    
    # Setup 3: parametric (glm) with cluster mean
    "parametric-cwc" = list(
        learners = c("SL.glm"), 
        num_folds = 1, 
        cluster_opt = "cwc",
        random_slope_vars_a = NULL,
        random_slope_vars_m = NULL,
        random_slope_vars_y = NULL
    ),
    
    # Setup 4: parametric (glm) with cluster mean & dummies 
    "parametric-cwc.FE" = list(
        learners = c("SL.glm"), 
        num_folds = 1, 
        cluster_opt = "cwc.FE",
        random_slope_vars_a = NULL,
        random_slope_vars_m = NULL,
        random_slope_vars_y = NULL
    ), 
    
    # Setup 5: parametric (RE.glm) with cluster mean
    "parametric.RE.glm.rs-cwc" = list(
        learners = c("RE.glm.rs"), 
        num_folds = 1, 
        cluster_opt = "RE.glm.rs", 
        random_slope_vars_a = c("age_w1_sc", # covariates
                                "sex_w1",
                                "white_w1",
                                "black_w1",
                                "parentalEdu_w1_sc",
                                "familyStruct_w1",
                                "feelings_w1_sc",
                                "selfEst_w1_sc"), 
        random_slope_vars_m = c("age_w1_sc", # covariates
                                "sex_w1",
                                "white_w1",
                                "black_w1",
                                "parentalEdu_w1_sc",
                                "familyStruct_w1",
                                "feelings_w1_sc",
                                "selfEst_w1_sc",
                                "sportPartic_w1"), # trt
        random_slope_vars_y = c("age_w1_sc", # covariates
                                "sex_w1",
                                "white_w1",
                                "black_w1",
                                "parentalEdu_w1_sc",
                                "familyStruct_w1",
                                "feelings_w1_sc",
                                "selfEst_w1_sc",
                                "sportPartic_w1", # trt
                                "selfEst_w3_sc")
    )
    # "parametric.RE-cwc" = list(
    #     learners = c("RE.glm"), 
    #     num_folds = 1, 
    #     cluster_opt = "RE.glm"
    # )
)

# Select models to run 
param_list <- param_list[names(param_list) %in% c("nonparametric-cwc.FE", 
                                                  "nonparametric-cwc", 
                                                  "parametric-cwc", 
                                                  "parametric-cwc.FE"#, 
                                                  # "parametric.RE.glm.rs-cwc" # skipping due to convergence issues 
                                                  )]
# param_list <- param_list[names(param_list) %in% c(c("parametric.RE.glm.rs-cwc"))]

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
        num_folds = params$num_folds, 
        random_slope_vars_a = params$random_slope_vars_a,
        random_slope_vars_m = params$random_slope_vars_m,
        random_slope_vars_y = params$random_slope_vars_y
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

