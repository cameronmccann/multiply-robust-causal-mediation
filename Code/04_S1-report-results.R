################################################################################
####################### Simulation 1 - Report Results ##########################
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-07-01
#
#
# Script Description: 
#       Creates tables and figures summarizing the performance 
#       of estimation methods in Simulation 1, including metrics such as 
#       bias, MSE, coverage, and power for direct and indirect effects.
#
#
# Last Updated: 2026-01-14
#
#
# Notes:
#   To-Do
#     - Add additional visualizations for DE metrics
#     - Review facet labeling consistency across plots
#
#   Done:
#     - Added bias and coverage plots for individual- and cluster-average TNIE
#     - Saved all figures to /Figures directory
#     - Generated summary tables and exported to CSV
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
    purrr, 
    stringr, 
    glue,
    tools
)


# User Inputs / Global Options --------------------------------------------

# Date of simulation
sim_date <- "2025-10-22" # "2025-09-03" 

# # Choose which results to report
# use_updated <- TRUE    # FALSE = no values replaced
#                        # TRUE  = values replaced
# 
# # Create prefix for file labels when saving 
# prefix <- if (use_updated) "S1_updated" else "S1"

# Results folder path 
results_root <- "Output/S1_Results" 

# Add subdirectory for results, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- NULL #"2025-10-22_1000-reps" # set to NULL on final run


# Set up directory structure ----------------------------------------------

# ══════════════════════════════
#    Check Results folder structure  
# ══════════════════════════════

# Form file path 
if (is.null(additional_folder_results)) {
    results_path <- file.path(results_root)
} else {
    results_path <- file.path(results_root, additional_folder_results)
}

# Double check that folders exist: Results, Data, Figures, & Tables
stopifnot(dir.exists(results_path))
stopifnot(dir.exists(file.path(results_path, "Data")))
stopifnot(dir.exists(file.path(results_path, "Figures")))
stopifnot(dir.exists(file.path(results_path, "Tables")))

# ══════════════════════════════
#    Create subdirectories for figures 
# ══════════════════════════════

figures_path <- file.path(results_path, "Figures")

# Create Figures subfolders 
fig_subfolders <- c("NIE", "NDE", "Convergence", "In-text")
for (sf in fig_subfolders) {
    dir.create(file.path(figures_path, sf), showWarnings = FALSE, recursive = TRUE)
}

# Create Figures subsubfolders (nonnull, null)
for (ssf in c("nonnull", "null")) {
    dir.create(file.path(figures_path, "NIE", ssf), showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(figures_path, "NDE", ssf), showWarnings = FALSE, recursive = TRUE)
}


# Import results data ----------------------------------------------------------

# Import data 
perf_measures <- readRDS(file.path(
    results_path, "Tables",
    paste0("S1_performance-measures_", sim_date, "_converged-only.rds")
))

sim1_data_nowarnings <- readRDS(file.path(
    results_path, "Data",
    paste0("S1_simulation-data_", sim_date, "_excludes-warnings.rds")
))

sim1_data_converged <- readRDS(file.path(
    results_path, "Data",
    paste0("S1_simulation-data_", sim_date, "_converged-only.rds")
))

convergence_rates <- readRDS(file.path(
    results_path, "Tables", 
    paste0("S1_convergence-rates_", sim_date, ".rds")
))

# Set sim_data dataset for visuals
sim_data <- sim1_data_converged # use converged iterations 

# Modify data for visuals 
perf_measures <- perf_measures |>
    mutate(
        quad = ifelse(quadratic == TRUE, "nonlinear", "linear"),
        Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"),
        # Nj = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]"))
        cluster_opt2 = ifelse(cluster_opt == "cwc", "mean", "mean + dummies"),
        Fit = ifelse(Fit == "mlr", "Nonparametric", ifelse(cluster_opt == "RE.glm", "Parametric: RE", "Parametric: GLM")), 
        cluster_opt = ifelse(cluster_opt == "RE.glm", "cwc", cluster_opt)
    )

# Modify data for visuals 
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))

# Modify convergence 
convergence_rates <- convergence_rates |> 
    mutate(Fit = ifelse(cluster_opt == "RE.glm", "Parametric: RE", Fit), 
           cluster_opt = ifelse(cluster_opt == "RE.glm", "cwc", cluster_opt), 
           Fit = ifelse(Fit == "Parametric", "Parametric: GLM", Fit))

# # Drop RE.glm
# perf_measures <- perf_measures |>
#     filter(cluster_opt != "RE.glm")
# sim_data <- sim_data |> 
#     filter(cluster_opt != "RE.glm")

# Split null & nonull dataframes
perf_measures_null <- perf_measures[perf_measures$ifnull == TRUE, ]
perf_measures_nonnull <- perf_measures[perf_measures$ifnull == FALSE, ]


# Visual themes & settings ------------------------------------------------

# visual settings
gglayer_theme <- list(theme_bw(),
                      scale_color_manual(values = c("#BF5700", #Fixed-effect
                                                    "#579D42", #"#A6CD57", #Random-effect
                                                    "#333F48")), #Single-level
                      scale_fill_manual(values = c("#BF5700", #Fixed-effect
                                                   "#579D42", #"#A6CD57", #Random-effect
                                                   "#333F48")), #Single-level
                      #"#9CADB7" <-- light gray
                      # Used following website with university colors: https://projects.susielu.com/viz-palette?
                      theme(text = element_text(family = "arial", size = 12), # "Times New Roman", size = 12), #
                            title = element_text(size = 16),
                            axis.title = element_text(size = 14),  # Adjust axis title size
                            axis.text = element_text(size = 12),  # Adjust axis text size
                            legend.title = element_text(size = 14), #12),  # Legend title size
                            legend.text = element_text(size = 12),  # Legend text size
                            strip.text = element_text(size = 14),  # Facet labels
                            line = element_line(linewidth = 0.8), #element_line(linewidth = 0.5),  # APA recommends thin lines
                            legend.position = "top" #"bottom" #"right"
                      ))

# facet layer 
gglayer_facet <- facet_grid(
    Mfamily + Yfamily ~ quad + cluster_opt,
    labeller = labeller(
        Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
        Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
        quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
        cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
    )
)


# In-text visuals ---------------------------------------------------------

# ══════════════════════════════
#    Bias  
# ══════════════════════════════
# NIE Bias for Gaussian mediator & outcome (large clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "50-100") |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Bias \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    scale_shape_manual(values = c("50-100" = 17)) + # make data point triangle (for large clusters) to match other visuals
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_bias_large-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)

# NIE Bias for Gaussian mediator & outcome (small clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "5-20") |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Bias \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_bias_small-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)


# ══════════════════════════════
#    CI Coverage Rate 
# ══════════════════════════════
# NIE coverage rate for Gaussian mediator & outcome (large clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & nj == "50-100") |> 
    ggplot(aes(x = factor(J), y = coverage_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.95) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Coverage \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    scale_shape_manual(values = c("50-100" = 17)) + # make data point triangle (for large clusters) to match other visuals
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_coverage_large-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)

# NIE coverage rate for Gaussian mediator & outcome (small clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & nj == "5-20") |> 
    ggplot(aes(x = factor(J), y = coverage_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.95) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Coverage \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_coverage_small-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)


# ══════════════════════════════
#    MSE  
# ══════════════════════════════
# NIE MSE for Gaussian mediator & outcome (large clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "50-100") |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "MSE \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    scale_shape_manual(values = c("50-100" = 17)) + # make data point triangle (for large clusters) to match other visuals
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_mse_large-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)

# NIE MSE for Gaussian mediator & outcome (small clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "5-20") |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "MSE \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_mse_small-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)


# ══════════════════════════════
#    Power  
# ══════════════════════════════
# NIE Power for Gaussian mediator & outcome (large clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "50-100") |> 
    ggplot(aes(x = factor(J), y = power_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.80) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Power \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    scale_shape_manual(values = c("50-100" = 17)) + # make data point triangle (for large clusters) to match other visuals
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_power_large-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)

# NIE Power for Gaussian mediator & outcome (small clusters)
perf_measures_nonnull |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "5-20") |> 
    ggplot(aes(x = factor(J), y = power_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.80) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Power \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_power_small-cluster_nonnull_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)


# ══════════════════════════════
#    Type I error  
# ══════════════════════════════
# NIE Type 1 error for Gaussian mediator & outcome (large clusters)
perf_measures_null |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "50-100") |> 
    ggplot(aes(x = factor(J), y = type1_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.05) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Type I Error \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    scale_shape_manual(values = c("50-100" = 17)) + # make data point triangle (for large clusters) to match other visuals
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_type1_large-cluster_null_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)

# NIE Type 1 error for Gaussian mediator & outcome (small clusters)
perf_measures_null |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & nj == "5-20") |> 
    ggplot(aes(x = factor(J), y = type1_individual_TNIE, color = Fit, shape = nj, linetype = Fit)) +
    geom_hline(yintercept = 0.0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, nj)), linewidth = 0.8) +
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, # dropped Mfamily + Yfamily for rows
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme +
    labs(title = NULL,
         x = "\n Number of Clusters",
         y = "Type I Error \n",
         color = "Method",
         linetype = "Method", 
         shape = "Cluster size") +
    guides(shape = "none") # drop shape from legend 

# Save plot
ggsave(filename = paste0(figures_path, "/In-text/",
                         "S1_type1_small-cluster_null_ind_NIE.png"),
       plot = last_plot(), 
       width = 9, 
       height = 6, 
       units = "in", 
       dpi = 300)


# Functions to create & save visuals --------------------------------------

create_plot <- function(data, 
                        y_var, 
                        effect = "NIE",  # "NIE" or "NDE" 
                        effect_level = "individual",  # "individual" or "cluster"  
                        null_status = "nonnull",  # "null" or "nonnull"
                        outcome_type = "all",  # "gaussian", "binomial", or "all"
                        mediator_type = "all",  # "gaussian", "binomial", or "all"
                        title = NULL, 
                        y_label = NULL, 
                        add_reference_line = TRUE,
                        reference_value = NULL) {
    
    # Input validation
    if (!y_var %in% names(data)) {
        stop("y_var '", y_var, "' not found in data")
    }
    
    # Filter data based on null status
    if (null_status == "null") {
        plot_data <- data[data$ifnull == TRUE, ]
    } else if (null_status == "nonnull") {
        plot_data <- data[data$ifnull == FALSE, ]
    } else {
        plot_data <- data
    }
    
    # Filter by outcome family
    if (outcome_type != "all") {
        plot_data <- plot_data[plot_data$Yfamily == outcome_type, ]
    }
    
    # Filter by mediator family  
    if (mediator_type != "all") {
        plot_data <- plot_data[plot_data$Mfamily == mediator_type, ]
    }
    
    # Check if data is empty after filtering
    if (nrow(plot_data) == 0) {
        stop("No data remains after filtering")
    }
    
    # Create the base plot
    p <- ggplot(plot_data, aes(x = factor(J), y = .data[[y_var]], 
                               color = Fit, shape = Nj, linetype = Fit)) 
    
    # Add reference line if requested
    if (add_reference_line) {
        if (is.null(reference_value)) {
            # Determine reference value based on metric type
            if (grepl("bias|mse", tolower(y_var))) {
                reference_value <- 0
            } else if (grepl("type1", tolower(y_var))) {
                reference_value <- 0.05
            } else if (grepl("power", tolower(y_var))) {
                reference_value <- 1
            } else if (grepl("cover", tolower(y_var))) {
                reference_value <- 0.95
            } else {
                reference_value <- 0  # default
            }
        }
        p <- p + geom_hline(yintercept = reference_value)
    }

    # Add remaining of base plot
    p <- p + geom_point(size = 2) +
        geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8)
    
    # Set default labels if not provided
    if (is.null(y_label)) {
        y_label <- case_when(
            grepl("bias", tolower(y_var)) ~ "Bias",
            grepl("mse", tolower(y_var)) ~ "MSE",
            grepl("cover", tolower(y_var)) ~ "Coverage",
            grepl("power", tolower(y_var)) ~ "Power", 
            grepl("type1", tolower(y_var)) ~ "Type I Error",
            TRUE ~ tools::toTitleCase(gsub("_", " ", y_var))
        )
    }
    
    # Add labels
    p <- p + labs(
        title = title,
        x = "\n Number of Clusters",
        y = paste0(y_label, " \n"),
        color = "Method",
        linetype = "Method", 
        shape = "Cluster size"
    )
    
    # Add faceting and theme
    p <- p +
        gglayer_facet +
        gglayer_theme

    return(p)
}


# ═══════════════════
#    Plotting specs dataframe 
# ═══════════════════
# Create specs dataframe
plot_specs <- expand.grid(
    effect = c("NIE", "NDE"),
    metric = c("bias", "coverage", "MSE", "power", "type1"),
    level = c("individual", "cluster"),
    null_status = c("nonnull", "null"),
    outcome = c("gaussian", "binomial", "all"),
    stringsAsFactors = FALSE
)

# Filter specs
plot_specs <- subset(plot_specs, 
                     !(metric == "power" & null_status == "null") &
                     !(metric == "type1" & null_status == "nonnull") &
                     !(metric %in% c("bias", "mse") & outcome == "all") & 
                     !(metric == "coverage" & outcome == "gaussian") & 
                     !(metric == "coverage" & outcome == "binomial")
)

# Create function to create & save plot from specs
create_and_save_plot <- function(spec_row) {
    
    effect_var <- if (spec_row$effect == "NIE") "TNIE" else "PNDE"
    y_var <- paste0(spec_row$metric, "_", spec_row$level, "_", effect_var)
    
    data <- if (spec_row$null_status == "null") perf_measures_null else perf_measures_nonnull
    
    # Create plot
    p <- create_plot(
        data = data,
        y_var = y_var,
        null_status = spec_row$null_status,
        outcome_type = spec_row$outcome
    )
    
    # Filename
    level_abbr <- if (spec_row$level == "individual") "ind" else "clus"
    outcome_abbr <- if (spec_row$outcome == "all") "all" else substr(spec_row$outcome, 1, 5)
    
    filename <- file.path(figures_path, 
                          spec_row$effect, 
                          spec_row$null_status, 
                          sprintf("S1_%s_%s_%s_%s_%s.png", 
                                  spec_row$metric, 
                                  spec_row$null_status, 
                                  level_abbr, 
                                  spec_row$effect, 
                                  outcome_abbr))
    
    # Save
    ggsave(filename, p, width = 10, 
           height = if (spec_row$outcome == "all") 11 else 9, 
           units = "in", dpi = 300)
    
    cat(sprintf("Saved: %s\n", basename(filename)))

}


# Generate Visuals (other than convergence rate viz) ----------------------

# loop through each row of plot_specs to save visuals
invisible(lapply(1:nrow(plot_specs), function(i) {
    create_and_save_plot(plot_specs[i, ])
}))


# Convergence Rate Visuals ------------------------------------------------

# ═══════════════════
#    Convergence rate for nonnull 
# ═══════════════════
create_plot(convergence_rates, 
            y_var = "convergence_rate", 
            null_status = "nonnull", 
            add_reference_line = F)

# Save 
ggsave(filename = paste0(figures_path, "/Convergence/",
                         "S1_convergence_nonnull_all_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)

# ═══════════════════
#    Convergence rate for null 
# ═══════════════════
create_plot(convergence_rates, 
            y_var = "convergence_rate", 
            null_status = "null", 
            add_reference_line = F)

# Save 
ggsave(filename = paste0(figures_path, "/Convergence/",
                         "S1_convergence_null_all_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


# Function to create and print tables ------------------------------------

create_and_print_table <- function(spec_row) {
    
    # Determine effect variable
    effect_var <- if (spec_row$effect == "NIE") "TNIE" else "PNDE"
    y_var <- paste0(spec_row$metric, "_", spec_row$level, "_", effect_var)
    
    # Select data
    data <- if (spec_row$null_status == "null") perf_measures_null else perf_measures_nonnull
    
    # Filter by quadratic
    data <- data |> filter(quadratic == spec_row$quadratic)
    
    # Create table header message
    quad_label <- if (spec_row$quadratic) "Nonlinear" else "Linear"
    level_label <- tools::toTitleCase(spec_row$level)
    metric_label <- tools::toTitleCase(spec_row$metric)
    null_label <- tools::toTitleCase(spec_row$null_status)
    
    cat("\n")
    cat("==============================================================================\n")
    cat(sprintf("%s - %s - %s - %s - %s\n", 
                metric_label, 
                level_label, 
                spec_row$effect, 
                quad_label,
                null_label))
    cat("==============================================================================\n")
    
    # Create summary table
    summary_table <- data |> 
        filter(!is.na(.data[[y_var]])) |> # Remove rows with NA for metric (to avoid NAs generated from glm.RE-quadratic interaction)
        group_by(quadratic, Fit, Mfamily, Yfamily, Nj, J, cluster_opt) |> 
        summarize(
            avg = mean(.data[[y_var]], na.rm = TRUE), 
            .groups = "drop"
        ) |> 
        # arrange(Mfamily, Yfamily, Nj, J, avg)
        arrange(desc(avg))
    
    # Print table
    print(summary_table, n = Inf)
    
    cat("\n")
    
}

# ═══════════════════
#    Table specs dataframe 
# ═══════════════════
# Create specs dataframe 
table_specs <- expand.grid(
    effect = c("NIE"), 
    metric = c("bias", "coverage", "MSE", "power", "type1"),
    level = c("individual"), #, "cluster"),
    null_status = c("nonnull", "null"),
    quadratic = c(TRUE, FALSE), 
    stringsAsFactors = FALSE
)

# Filter specs
table_specs <- subset(table_specs,
                      !(metric == "power" & null_status == "null") & 
                          !(metric == "type1" & null_status == "nonnull")
)


# Generate Tables ---------------------------------------------------------

# Record log
sink(file.path(results_path, "Tables", "performance-metrics-breakdown.txt"), split = TRUE)

# loop through each row of table_specs to save visuals
invisible(lapply(1:nrow(table_specs), function(i) {
    create_and_print_table(table_specs[i, ])
}))

# Close log
sink()


# Record log
sink(file.path(results_path, "Tables", "true-values.txt"), split = TRUE)

# Generate table of true effects
cat("\n")
cat("==============================================================================\n")
cat("True values - Nonnull\n")
cat("==============================================================================\n")

perf_measures_nonnull |> 
    group_by(quad, Mfamily, Yfamily, J, Nj) |> 
    slice_head(n = 1) |>
    select(starts_with("true") & ends_with("TNIE")) |> 
    print(n = Inf)

# Close log
sink()

################################# END OF CODE ##################################
