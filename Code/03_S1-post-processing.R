################################################################################
############ Simulation 1 - Obtain Results part 3: post-processing #############
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       Part 3 takes processed simulation output (either done in part 1 or 2, 
#       depending on whether you want "problematic" cases replaced or not) and 
#       summarizes and reports the results for the first simulation study 
#       (i.e., obtains performance measures). 
# 
#       Note: set use_updated <- TRUE for files with "updated-" in name 
#               (includes replaced values) otherwise set use_updated <- FALSE 
#     
#
# Last Updated: 2026-01-06
#
#
# Notes:
#   To-Do
#       # add for updated-values to save to a with-updated-values folder (which contains Data, Figures, & Tables subfolders)
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
    huxtable, 
    glue, 
    purrr
)


# User Inputs / Global Options --------------------------------------------

# Choose which dataframe to postprocess:
use_updated <- FALSE#TRUE   # TRUE = uses S1_updated-overall-output-dataframe_*.rds
                      # FALSE = uses S1_overall-output-dataframe_*.rds

# Create prefix for file labels when saving 
prefix <- if (use_updated) "S1_updated" else "S1"


# Date of simulation 
sim_date <- "2025-10-22" 

# Number of replications
reps <- 1000 

# Results folder
results_root <- "Output/S1_Results" #path <- "Output/S1_Results"

# Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 

results_path <- file.path(results_root, additional_folder_results)

# Safety: check that folders exist
stopifnot(dir.exists(results_path))
stopifnot(dir.exists(file.path(results_path, "Data")))
stopifnot(dir.exists(file.path(results_path, "Tables")))


# Import data -------------------------------------------------------------

# Import sim dataframe (either with or without replacements, based on use_updated)
if (use_updated) {
    input_df_file <- file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds"))
} else {
    input_df_file <- file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds"))
}

if (!file.exists(input_df_file)) stop("Input dataframe not found: ", input_df_file)

sim1_data <- readRDS(input_df_file)

sim1_data_all <- sim1_data # save all cases version for later

# Import sim dataframe with only converging cases
if (use_updated) {
    sim1_data_converged <- readRDS(file.path(results_path, "Data", 
                                             paste0("S1_updated-simulation-data_", sim_date, "_converged-only.rds")))
} else {
    sim1_data_converged <- readRDS(file.path(results_path, "Data", 
                                             paste0("S1_simulation-data_", sim_date, "_converged-only.rds")))
}


# Compute Performance Measures --------------------------------------------

# ══════════════════════════════
#    For converging cases 
# ══════════════════════════════
# Import data 
## import data with only converging cases
# sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_updated-simulation-data_", 
#                                    sim_date, 
#                                    "_converged-only.rds"))
# Using sim1_data_converged instead of reimporting data

# Compute performance measures 
perf_summary <- as.data.frame(sim1_data_converged) |>
    mutate(ifnull = as.logical(ifnull)) |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj) |> 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |> 
    ungroup() |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
    mutate(
        if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
        if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
        if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
        if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
        
        sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
        sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
        sig_cluster_PNDE = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
        sig_cluster_TNIE = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
    ) |> 
    summarize(
        # Individual PNDE
        cover_individual_PNDE = mean(if_cover_ind_PNDE),
        bias_individual_PNDE = mean(individual_de_Estimate - true_individual_PNDE),
        MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2),
        power_individual_PNDE = mean(sig_individual_PNDE[ifnull == FALSE]),
        type1_individual_PNDE = mean(sig_individual_PNDE[ifnull == TRUE]),
        
        # Individual TNIE
        cover_individual_TNIE = mean(if_cover_ind_TNIE),
        bias_individual_TNIE = mean(individual_ie_Estimate - true_individual_TNIE),
        MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2),
        power_individual_TNIE = mean(sig_individual_TNIE[ifnull == FALSE]),
        type1_individual_TNIE = mean(sig_individual_TNIE[ifnull == TRUE]),
        
        # Cluster PNDE
        cover_cluster_PNDE = mean(if_cover_clust_PNDE),
        bias_cluster_PNDE = mean(cluster_de_Estimate - true_cluster_PNDE),
        MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2),
        power_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == FALSE]),
        type1_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == TRUE]),
        
        # Cluster TNIE
        cover_cluster_TNIE = mean(if_cover_clust_TNIE),
        bias_cluster_TNIE = mean(cluster_ie_Estimate - true_cluster_TNIE),
        MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2),
        power_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == FALSE]),
        type1_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == TRUE]),
        
        # True values
        true_individual_PNDE = mean(true_individual_PNDE),
        true_individual_TNIE = mean(true_individual_TNIE),
        true_cluster_PNDE = mean(true_cluster_PNDE),
        true_cluster_TNIE = mean(true_cluster_TNIE)
    )


# table(perf_summary$quadratic)
# 
# perf_summary

# Save performance measures 
## Only include converged cases 
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", 
#                                     sim_date, "_converged-only.rds")) 
saveRDS(perf_summary,
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_performance-measures_", sim_date, "_converged-only.rds")))

# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", #linear_",
#                                     sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))

# Save performance measures (with warnings excluded)
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", 
#                                     sim_date, "_excludes-warnings.rds")) 
# saveRDS(perf_summary,
#         file = file.path(results_path, "Tables",
#                          paste0(prefix, "_performance-measures_", sim_date, "_converged-only.rds")))


# ══════════════════════════════
#    For all cases 
# ══════════════════════════════

# Import data 
## Import original/overall data
# sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

# Use sim1_data_all for performance measures 

# Compute performance measures 
perf_summary <- as.data.frame(sim1_data_all) |>
    mutate(ifnull = as.logical(ifnull)) |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj) |> 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |> 
    ungroup() |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
    mutate(
        if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
        if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
        if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
        if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
        
        sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
        sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
        sig_cluster_PNDE    = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
        sig_cluster_TNIE    = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
    ) |> 
    summarize(
        # Individual PNDE
        cover_individual_PNDE = mean(if_cover_ind_PNDE),
        bias_individual_PNDE = mean(individual_de_Estimate - true_individual_PNDE),
        MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2),
        power_individual_PNDE = mean(sig_individual_PNDE[ifnull == FALSE]),
        type1_individual_PNDE = mean(sig_individual_PNDE[ifnull == TRUE]),
        
        # Individual TNIE
        cover_individual_TNIE = mean(if_cover_ind_TNIE),
        bias_individual_TNIE = mean(individual_ie_Estimate - true_individual_TNIE),
        MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2),
        power_individual_TNIE = mean(sig_individual_TNIE[ifnull == FALSE]),
        type1_individual_TNIE = mean(sig_individual_TNIE[ifnull == TRUE]),
        
        # Cluster PNDE
        cover_cluster_PNDE = mean(if_cover_clust_PNDE),
        bias_cluster_PNDE = mean(cluster_de_Estimate - true_cluster_PNDE),
        MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2),
        power_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == FALSE]),
        type1_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == TRUE]),
        
        # Cluster TNIE
        cover_cluster_TNIE = mean(if_cover_clust_TNIE),
        bias_cluster_TNIE = mean(cluster_ie_Estimate - true_cluster_TNIE),
        MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2),
        power_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == FALSE]),
        type1_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == TRUE]),
        
        # True values
        true_individual_PNDE = mean(true_individual_PNDE),
        true_individual_TNIE = mean(true_individual_TNIE),
        true_cluster_PNDE = mean(true_cluster_PNDE),
        true_cluster_TNIE = mean(true_cluster_TNIE)
    )

# Save performance measures 
## All cases
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", #linear_",
#                                     sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))
saveRDS(perf_summary,
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_performance-measures_", sim_date, ".rds")))


# Compute Convergence Rates -----------------------------------------------

# Using sim1_data_converged & sim1_data to compute convergence rates 

# Modify data for visuals & merging  
sim1_data_converged <- sim1_data_converged |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
# dim(sim1_data[sim1_data$cluster_opt != "RE.glm", ])

# Calculate convergence rates & merge dataframes
convergence_rates <- sim1_data |> 
    # filter(cluster_opt != "RE.glm") |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(total_attempts = n(), .groups = "drop") |> 
    # merge 
    left_join(
        sim1_data_converged |> 
            # filter(cluster_opt != "RE.glm") |> 
            # mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
            # Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
            group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
            summarize(converged = n(), .groups = "drop"),
        by = c("ifnull", "quad", "Mfamily", "Yfamily", "J", "Nj", "Fit", "cluster_opt")
    ) |> 
    # compute convergence rates
    mutate(
        converged = ifelse(is.na(converged), 0, converged),
        nonconverged = total_attempts - converged,
        nonconvergence_rate = nonconverged / total_attempts,
        convergence_rate = converged / total_attempts, 
        Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
    ) 


# Save convergence rates 
# saveRDS(convergence_rates, file = paste0(results_path, "/Tables/S1_convergence-rates_", 
#                                          sim_date, ".rds"))
saveRDS(convergence_rates, 
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_convergence-rates_", sim_date, ".rds")))


############################# END OF PROCESSING ################################






