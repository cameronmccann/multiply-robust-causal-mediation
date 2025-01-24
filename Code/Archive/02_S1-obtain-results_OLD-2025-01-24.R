################################################################################
####################### Simulation 1 - Obtain Results ##########################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       This code summarizes and reports the results for the 
#       first simulation study (i.e., obtains performance measures). 
#
#
# Last Updated: 2025-01-24
#
#
# Notes:
#   To-Do
#
# 
#   Done: 
# 
# 
################################################################################


# Set Up (Load packages, functions, &/or data) ----------------------------

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    dplyr,
    tidyverse, 
    ggplot2, 
    flextable, 
    stringr, 
    ggdag, 
    dagitty, 
    huxtable
)


# Set simulation conditions & methods  ------------------------------------

# ══════════════════════════════
#     Simulation Conditions 
# ══════════════════════════════
conditions_all <- data.frame(rbind(
    expand.grid(
        J = c(10, 20, 40),
        Nj_low = c(50),
        Nj_high = c(100), 
        quadratic = c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian"), 
        if.null = c(F,T)
    ),
    expand.grid(
        J = c(40, 70, 100),
        Nj_low = c(5),
        Nj_high = c(20), 
        quadratic = c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian"), 
        if.null = c(F,T)
    )
), 
icc = c(0.2))

# limit conditions for testing 
conditions <- conditions_all |> 
    filter(quadratic == F) |> 
    filter(if.null == F) |> 
    filter(J %in% c(40)) |> # c(20)) |> # c(70)) |>
    filter(Nj_low %in% c(50)) |>
    filter(Mfamily %in% c("binomial", "gaussian"), Yfamily %in% c("binomial")) #c("gaussian")) # c("binomial")) #, Yfamily %in% c("gaussian"))


# ══════════════════════════════
#    Methods  
# ══════════════════════════════
methds_all <- data.frame(expand.grid(
    cluster_a = "FE", #c("FE", "RE", "noncluster"), # "RE", # "noncluster", #
    cluster_m = "FE", # c("FE", "RE", "noncluster"), # "RE", #  "noncluster.mlr", #
    cluster_y =  "FE", #c("FE", "RE", "noncluster"), # "noncluster.mlr", ## "FE.mlr", #
    # interact_fitm2 =  c(T), # NULL, #
    # interact_fity = c(T), # NULL, #
    # Morder = c("21", "12"),
    Fit = c("mlr","glm"), # 
    # cluster_opt_a = c("sufficient_stats",  "cwc.FE"), # "FE.glm", #  
    # cluster_opt_m = c("sufficient_stats",  "cwc.FE"),  #"FE.glm", # 
    # cluster_opt_y = c("sufficient_stats",  "cwc.FE") # "cwc.FE"#c("sufficient_stats") #, 
    cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
)) %>% 
    mutate(
        cluster_opt_a = cluster_opt, 
        cluster_opt_m = cluster_opt, 
        cluster_opt_y = cluster_opt
    )

# (methds <- methds_all %>%
#     filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") ))
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr")) #filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("mlr", "glm"))


# Set date, reps, & folders ----------------------------------------------

# Date of simulation 
sim_date <- Sys.Date() #"2025-01-18" 

# Number of replications
reps <- 15 # 100 # 200 # 1000
# reps <- 200

# Create directory to store results 
## Results folder 
path <- "Output/S1_Results"
### Add subdirectory, if desired (e.g., for test runs)
additional_folder <- "2025-01-23-test_15-reps_looking-into-binomial-outcome-low-null-reject-issue_with-res" # NULL
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder)) {
    path <- file.path(path, additional_folder)
}
### Create directory 
if (!dir.exists(path)) {
    dir.create(path)
}
## Data, Figures, & Tables subfolders 
if (!dir.exists(paste0(path, "/Data"))) {
    dir.create(paste0(path, "/Data"))
}
if (!dir.exists(paste0(path, "/Figures"))) {
    dir.create(paste0(path, "/Figures"))
}
if (!dir.exists(paste0(path, "/Tables"))) {
    dir.create(paste0(path, "/Tables"))
}
# Simulation output path 
sim_output_path <- "Output/S1_Simulation-Output"
### Add subdirectory, if desired (e.g., for test runs)
additional_folder <- "2025-01-23-test_15-reps_looking-into-binomial-outcome-low-null-reject-issue_with-res" # NULL
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder)) {
    sim_output_path <- file.path(sim_output_path, additional_folder)
}


# Import data  ------------------------------------------------------------

# Note: Maybe pull in each conditions list and extract true values and estimates and put into one large dataframe? 

# readRDS(paste0("Output/S1_Simulation-Output/S1_condition-01_reps-200_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds"))
# readRDS(paste0("Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), "_reps-",))
# 
# 
# file_path <- "Output/S1_Simulation-Output"
# 
# # List all .rds files in the directory that match specific conditions
# rds_files <- list.files(
#     path = file_path, 
#     pattern = paste0(sprintf("%02d", cond), "_reps-200.*\\.rds$"),  # Only files with "reps-200" and ".rds"
#     full.names = TRUE
# )
# 
# # Read all .rds files into a list
# rds_data_list <- lapply(rds_files, readRDS)
# 
# # Optionally, assign names to the list based on file names for easy identification
# names(rds_data_list) <- basename(rds_files)
# 
# # View the list of loaded data
# str(rds_data_list)


## Extract simulation data -------------------------------------------------

sim1_data <- NULL  # Store all conditions' data

for (cond in 1:nrow(conditions)) {
    
    rds_files <- list.files(
        path = sim_output_path, #path = "Output/S1_Simulation-Output", 
        pattern = paste0(sprintf("%02d", cond), "_reps-", reps, "_quad-", conditions[cond, "quadratic"], "_M-", conditions[cond, "Mfamily"], "_Y-", conditions[cond, "Yfamily"], ".*\\.rds$"),  # Only files with "reps-200" and ".rds"
        full.names = TRUE
    )
    
    # Read all .rds files into a list
    # temp_data <- lapply(rds_files, readRDS)
    temp_data <- readRDS(rds_files)
    
    # # Load condition-specific data
    # temp_data <- readRDS(paste0(
    #     # "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), ".rds"
    #     "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), "_reps-", reps, "-.*\\.rds$"
    # ))
    
    # Create a large dataframe to store all iterations for the current condition
    overall_models <- NULL
    
    # Loop through all iterations in the `temp_data` list
    for (i in seq_along(temp_data)) {
        # Loop through models within each iteration
        for (mod in 1:nrow(methds)) {
            estimates <- temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$estimates
            
            # Create a named list for "individual" and "cluster" estimates
            individual_de <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) %>% slice(1)
            individual_ie <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect)) %>% slice(1)
            cluster_de <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) %>% slice(1)
            cluster_ie <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) %>% slice(1)
            
            # Extract individual and cluster effects
            individual_effects <- temp_data[[i]]$effects$individual
            cluster_effects <- temp_data[[i]]$effects$cluster
            
            # Create a single-row dataframe
            model_row <- tibble(
                condition = cond,  # Add condition as a column
                iteration = i,
                model = mod,
                warnings = ifelse(length(temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings) == 0, NA, temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings),
                num_folds = temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$num_folds, 
                
                # Individual DE (direct effect) estimates
                individual_de_Estimate = individual_de$Estimate,
                individual_de_StdError = individual_de$StdError,
                individual_de_CILower = individual_de$CILower,
                individual_de_CIUpper = individual_de$CIUpper,
                
                # Individual IE (indirect effect) estimates
                individual_ie_Estimate = individual_ie$Estimate,
                individual_ie_StdError = individual_ie$StdError,
                individual_ie_CILower = individual_ie$CILower,
                individual_ie_CIUpper = individual_ie$CIUpper,
                
                # Cluster DE (direct effect) estimates
                cluster_de_Estimate = cluster_de$Estimate,
                cluster_de_StdError = cluster_de$StdError,
                cluster_de_CILower = cluster_de$CILower,
                cluster_de_CIUpper = cluster_de$CIUpper,
                
                # Cluster IE (indirect effect) estimates
                cluster_ie_Estimate = cluster_ie$Estimate,
                cluster_ie_StdError = cluster_ie$StdError,
                cluster_ie_CILower = cluster_ie$CILower,
                cluster_ie_CIUpper = cluster_ie$CIUpper,
                
                # Individual effects (pnde, tnie, tnde, pnie)
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                # Cluster effects (pnde, tnie, tnde, pnie)
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie, 
                
                # PS overlap statement 
                ps_overlap = temp_data[[i]]$overlap$ps_summary
            )
            
            # Append to overall_models dataframe
            overall_models <- bind_rows(overall_models, model_row)
        }
    }
    
    # Append this condition's data to the main dataframe
    sim1_data <- bind_rows(sim1_data, overall_models)
}

# Save data 
saveRDS(sim1_data, file = paste0(path, "/Data/S1_simulation-data_", sim_date, ".rds"))




# Compute Performance Measures --------------------------------------------

## Bias, Coverage, & MSE (need type I error) -----------------------------------------------------------

sim1_data <- readRDS(file = paste0(path, "/Data/S1_simulation-data_", sim_date, #"2025-01-12", #sim_date, 
                                   ".rds"))

# Individual-avg summary 
ind_summary <-
    as.data.frame(sim1_data) |> 
    group_by(condition) |> # get one true value per condition 
    mutate(true_PNDE = mean(individual_pnde), 
           true_TNIE = mean(individual_tnie)) |> 
    group_by(condition, model) |> 
    mutate(if_cover_PNDE = (individual_de_CILower < true_PNDE) & (individual_de_CIUpper > true_PNDE), 
           if_cover_TNIE = (individual_ie_CILower < true_TNIE) & (individual_ie_CIUpper > true_TNIE) ) |> 
    summarize(cover_PNDE = mean(if_cover_PNDE), 
              cover_TNIE = mean(if_cover_TNIE), 
              bias_individual_PNDE = mean((individual_de_Estimate - true_PNDE)), 
              bias_individual_TNIE = mean((individual_ie_Estimate - true_TNIE)), 
              MSE_individual_PNDE = mean((individual_de_Estimate - true_PNDE)^2), 
              MSE_individual_TNIE = mean((individual_ie_Estimate - true_TNIE)^2), 
              rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))), 
              rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))), 
              true_PNDE = mean(true_PNDE), 
              true_TNIE = mean(true_TNIE)
    ) #|> 
    # view()

ind_summary

# add condition description 
ind_summary <- cbind(condition = 1:nrow(conditions), 
                    conditions) |> 
    left_join(ind_summary, 
              by = "condition") 
    
# add method description 
ind_summary <- cbind(method = 1:nrow(methds), 
      methds) |> 
    left_join(ind_summary, 
              by = c("method" = "model"))

ind_summary

saveRDS(ind_summary, file = paste0(path, "/Tables/S1_performance-measures_", sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))



## res
# method cluster_a cluster_m cluster_y Fit cluster_opt cluster_opt_a cluster_opt_m cluster_opt_y condition  J Nj_low Nj_high quadratic  Mfamily  Yfamily if.null
# 1      1        FE        FE        FE mlr         cwc           cwc           cwc           cwc         1 40     50     100     FALSE binomial binomial   FALSE
# 2      1        FE        FE        FE mlr         cwc           cwc           cwc           cwc         2 40     50     100     FALSE gaussian binomial   FALSE
# icc cover_PNDE cover_TNIE bias_individual_PNDE bias_individual_TNIE MSE_individual_PNDE MSE_individual_TNIE rejectnull_individual_PNDE
# 1 0.2  1.0000000          1          0.006999728          0.001104295        0.0003826039        1.319436e-05                  0.3333333
# 2 0.2  0.9333333          1          0.001425151         -0.002226532        0.0005755800        6.142529e-05                  0.3333333
# rejectnull_individual_TNIE  true_PNDE   true_TNIE
# 1                  0.5333333 0.02610082 0.006856211
# 2                  0.8666667 0.03795173 0.030752859

## No res 
# method cluster_a cluster_m cluster_y Fit cluster_opt cluster_opt_a cluster_opt_m cluster_opt_y condition  J Nj_low Nj_high quadratic  Mfamily  Yfamily if.null
# 1      1        FE        FE        FE mlr         cwc           cwc           cwc           cwc         1 40     50     100     FALSE binomial binomial   FALSE
# 2      1        FE        FE        FE mlr         cwc           cwc           cwc           cwc         2 40     50     100     FALSE gaussian binomial   FALSE
# icc cover_PNDE cover_TNIE bias_individual_PNDE bias_individual_TNIE MSE_individual_PNDE MSE_individual_TNIE rejectnull_individual_PNDE
# 1 0.2          1          1          0.002239648        -0.0005117464        9.155625e-05        2.575158e-05                  0.3333333
# 2 0.2          1          1          0.001857521         0.0013637777        2.755786e-04        1.117778e-04                  0.6666667
# rejectnull_individual_TNIE  true_PNDE   true_TNIE
# 1                  0.2666667 0.02610082 0.006856211
# 2                  0.7333333 0.03792543 0.030818700









# reporting TACC 100 reps 2025-01-23 results ------------------------------


# import results 
tacc_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/Data/S1_simulation-data_2025-01-23.rds")
tacc_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-23.rds")

# took about 49 mins to run 
readRDS(file = "Output/S1_Simulation-Output/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/S1_Computation-Time.rds") |> 
    view()

#     
tacc_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    view()

# summary of null rejection 
tacc_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    group_by(Mfamily, Yfamily, Fit, cluster_opt) |> 
    summarize(
        mean_rej_PNDE = mean(rejectnull_individual_PNDE), 
        sd_rej_PNDE = sd(rejectnull_individual_PNDE),
        median_rej_PNDE = median(rejectnull_individual_PNDE),
        
        mean_rej_TNIE = mean(rejectnull_individual_TNIE), 
        sd_rej_TNIE = sd(rejectnull_individual_TNIE),
        median_rej_TNIE = median(rejectnull_individual_TNIE)
        
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily) |> 
    print(n = 32)


# summary of true effects 
tacc_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE") |>
    group_by(Mfamily, Yfamily) |>
    summarize(
        mean_true_PNDE = mean(true_PNDE),
        sd_true_PNDE = sd(true_PNDE),
        median_true_PNDE = median(true_PNDE),
        mean_true_TNIE = mean(true_TNIE),
        sd_true_TNIE = sd(true_TNIE),
        median_true_TNIE = median(true_TNIE),
        .groups = "drop"
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily)


# viz for TNIE bias 
library(ggplot2)
library(dplyr)

tacc_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0, ) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ Mfamily) +
    labs(subtitle = "Bias by Mediator (horz) & Outcome (vert) type") +
    theme_minimal()



# warnings 
# unique(tacc_data$warnings)
table(tacc_data$warnings, tacc_data$model)
# method 1 = mlr  cwc.FE
# 2 = glm      cwc.FE
# 3 = mlr         cwc 
# 4 = glm         cwc






# names(tacc_data)
# names(tacc_perf_measures)

tacc_perf_measures |> 
    filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE")






# ══════════════════════════════
#    possible viz 
# ══════════════════════════════

library(ggplot2)
library(dplyr)


tacc_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0, ) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ Mfamily) +
    labs(subtitle = "Bias by Mediator (horz) & Outcome (vert) type") +
    theme_minimal()










# Assuming your data frame is named 'tacc_perf_measures'
plot_data <- tacc_perf_measures %>%
    filter(Nj_low == 50, Yfamily == "binomial") %>%
    mutate(bias = bias_individual_PNDE)

ggplot(plot_data, aes(x = factor(J), y = bias, color = cluster_opt, shape = Fit)) +
    geom_point(size = 3) +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Mfamily ~ ., scales = "free_y") +
    labs(title = "Bias Comparison Across J and Mfamily",
         x = "J", y = "Bias (PNDE)",
         color = "Cluster Option", shape = "Fit") +
    theme_minimal() +
    theme(legend.position = "bottom")



library(ggplot2)
library(dplyr)
library(tidyr)

plot_data <- tacc_perf_measures %>%
    # filter(cluster_opt == "cwc", Fit == "mlr") %>%
    mutate(MSE = MSE_individual_PNDE)

ggplot(plot_data, aes(x = factor(J), y = factor(Nj_low), fill = MSE)) +
    geom_tile() +
    facet_grid(Mfamily ~ Yfamily) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "MSE Heatmap for Different J and Nj_low Combinations",
         x = "J", y = "Nj_low",
         fill = "MSE (PNDE)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(ggplot2)
library(dplyr)

# Filter data as per the example
filtered_data <- tacc_perf_measures %>%
    filter(cluster_opt == "cwc" & Fit == "mlr") %>%
    select("Fit", "cluster_opt", "J", "Nj_low", "Mfamily", "Yfamily",
           "bias_individual_PNDE", "bias_individual_TNIE",
           "cover_PNDE", "cover_TNIE",
           "MSE_individual_PNDE", "MSE_individual_TNIE",
           "rejectnull_individual_PNDE", "rejectnull_individual_TNIE")

# Bias Visualization
ggplot(filtered_data, aes(x = factor(J), y = bias_individual_PNDE, fill = Mfamily)) +
    geom_boxplot() +
    facet_wrap(~Yfamily) +
    labs(title = "Bias of PNDE Across J and Mfamily", x = "J", y = "Bias (PNDE)") +
    theme_minimal()

# Coverage Rate Visualization
ggplot(filtered_data, aes(x = J, y = cover_PNDE, color = Mfamily)) +
    geom_line(aes(group = Mfamily), size = 1) +
    facet_wrap(~Yfamily) +
    labs(title = "Coverage Rate of PNDE Across J", x = "J", y = "Coverage (PNDE)") +
    theme_minimal()

# MSE Scatter Plot
ggplot(filtered_data, aes(x = J, y = MSE_individual_PNDE, color = Mfamily)) +
    geom_point(size = 3, alpha = 0.7) +
    facet_wrap(~Nj_low) +
    labs(title = "MSE of PNDE Across J and Nj_low", x = "J", y = "MSE (PNDE)") +
    theme_minimal()

# Reject Null Visualization
ggplot(filtered_data, aes(x = factor(J), fill = factor(rejectnull_individual_PNDE))) +
    geom_bar(position = "fill") +
    facet_wrap(~Mfamily + Yfamily) +
    labs(title = "Proportion of Rejecting Null (PNDE)", x = "J", y = "Proportion") +
    theme_minimal()

# Heatmap for Bias
library(reshape2)

heatmap_data <- filtered_data %>%
    group_by(J, Nj_low) %>%
    summarize(mean_bias = mean(bias_individual_PNDE, na.rm = TRUE))

ggplot(heatmap_data, aes(x = factor(J), y = factor(Nj_low), fill = mean_bias)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    labs(title = "Heatmap of Mean Bias (PNDE)", x = "J", y = "Nj_low", fill = "Mean Bias") +
    theme_minimal()


