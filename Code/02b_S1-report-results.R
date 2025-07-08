################################################################################
####################### Simulation 1 - Report Results ##########################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-07-01
#
#
# Script Description: 
#       This script creates tables and figures summarizing the performance 
#       of estimation methods in Simulation 1, including metrics such as 
#       bias, MSE, coverage, and power for direct and indirect effects.
#
#
# Last Updated: 2025-07-03
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
    flextable, 
    stringr, 
    ggdag, 
    dagitty, 
    huxtable, 
    glue
)


# Set date, reps, & folders ----------------------------------------------

# Date of simulation
sim_date <- "2025-07-01" # (Note: Simulations were ran around 2025-02-08 & 2025-05-05) #"2025-02-08" #"2025-01-25" #Sys.Date() #"2025-01-23" #"2025-01-18"

# Results folder 
path <- "Output/S1_Results"
if (!dir.exists(path)) {
    dir.create(path)
}
### Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- "2025-07-01-test_600-reps" #"2025-02-08-test_600-reps" #2025-02-05-test_200-reps-lowered-relationship-with-A_2" #additional_folder_results <- "2025-02-04-test_checking-overlap"#additional_folder_results <- "2025-02-02-test_current-ensemble-in-randomized-trial" #/mulitple-mlr" #"2025-01-25-test_300-rep"
# additional_folder <- "from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods" #additional_folder <- "2025-01-24-test_1-rep_quad" #additional_folder <- "2025-01-23-test_100-reps_all-linear-conditions-with-all-methods" # NULL
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder_results)) {
    results_path <- file.path(path, additional_folder_results)
}
### Create directory
if (!dir.exists(results_path)) {
    dir.create(results_path)
}
## Data, Figures, & Tables subfolders
if (!dir.exists(paste0(results_path, "/Data"))) {
    dir.create(paste0(results_path, "/Data"))
}
if (!dir.exists(paste0(results_path, "/Figures"))) {
    dir.create(paste0(results_path, "/Figures"))
}
if (!dir.exists(paste0(results_path, "/Tables"))) {
    dir.create(paste0(results_path, "/Tables"))
}




# Import results data ----------------------------------------------------------

# import data if needed 
perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, ".rds")) 
sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, ".rds"))

# Modify data for visuals 
perf_measures <- perf_measures |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"), # Nj = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) 
           cluster_opt2 = ifelse(cluster_opt == "cwc", "mean", "mean + dummies"), 
           Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
    )
# Modify data for visuals 
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))


# Visuals -----------------------------------------------------------------

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
                            title = element_text(size = 16),
                            axis.title = element_text(size = 14),  # Adjust axis title size
                            axis.text = element_text(size = 12),  # Adjust axis text size
                            legend.title = element_text(size = 14), #12),  # Legend title size
                            legend.text = element_text(size = 12),  # Legend text size
                            strip.text = element_text(size = 14),  # Facet labels
                            line = element_line(linewidth = 0.8), #element_line(linewidth = 0.5),  # APA recommends thin lines
                            legend.position = "top" #"bottom" #"right"
                      ))

## TNIE --------------------------------------------------------------------


### Individual-average TNIE -------------------------------------------------

# ═══════════════════
#    Bias 
# ═══════════════════

# TNIE Bias for Gaussian outcomes 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt2) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 2. Bias for individual-average NIE with Gaussian outcome \n",
        x = "\n Number of Clusters", # "J",
        y = "Bias \n", 
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_bias-individual-avg-TNIE-gaussian-outcome.png"),
#        plot = last_plot())
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-individual-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE Bias for Binomial outcomes 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 3. Bias for individual-average NIE with Binomial outcome \n",
        x = "\n Number of Clusters", # "J",
        y = "Bias \n", 
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_bias-individual-avg-TNIE-binomial-outcome.png"),
#        plot = last_plot())

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-individual-avg-TNIE-binomial-outcome.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════

# TNIE CI coverage rate 
perf_measures |> 
    ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    # ggplot2::geom_hline(yintercept = 0.95) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 4. CI coverage rate for individual-average NIE \n",
        x = "\n Number of Clusters", # "J",
        y = "Coverage \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_coverage-individual-avg-TNIE.png"),
#        plot = last_plot())

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_coverage-individual-avg-TNIE.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


# ═══════════════════
#    MSE
# ═══════════════════

# TNIE MSE for Gaussian outcomes 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "MSE for individual-average NIE with Gaussian outcome \n",
        x = "Number of Clusters", # "J",
        y = "MSE",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-individual-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())

# TNIE MSE for Binomial outcomes 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "MSE for individual-average NIE with Binomial outcome \n",
        x = "Number of Clusters", # "J",
        y = "MSE",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-individual-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())


# ═══════════════════
#    Power
# ═══════════════════

# perf_measures only included output with if.null = FALSE, so this computes power (not type 1 error rate)

# TNIE Power 
perf_measures |> 
    ggplot(aes(x = factor(J), y = rejectnull_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Power for individual-average TNIE",
        x = "J",
        y = "Power"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_power-individual-avg-TNIE.png"),
       plot = last_plot())



### Cluster-average TNIE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════

# TNIE Bias for Gaussian outcomes 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "Bias"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-cluster-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())

# TNIE Bias for Binomial outcomes 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with Binomial outcome",
        x = "J",
        y = "Bias"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-cluster-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())


#### CI Coverage Rate --------------------------------------------------------------------

# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════

# TNIE CI coverage rate 
perf_measures |> 
    ggplot(aes(x = factor(J), y = cover_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "CI coverage rate for cluster-average TNIE",
        x = "J",
        y = "Coverage"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_coverage-cluster-avg-TNIE.png"),
       plot = last_plot())


#### MSE ---------------------------------------------------------------------

# ═══════════════════
#    MSE
# ═══════════════════

# TNIE MSE for Gaussian outcomes 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "MSE"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-cluster-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())

# TNIE MSE for Binomial outcomes 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with Binomial outcome",
        x = "J",
        y = "MSE"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-cluster-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())


#### Power -------------------------------------------------------------------

# ═══════════════════
#    Power
# ═══════════════════

# perf_measures only included output with if.null = FALSE, so this computes power (not type 1 error rate)

# TNIE Power 
perf_measures |> 
    ggplot(aes(x = factor(J), y = rejectnull_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Power for cluster-average TNIE",
        x = "J",
        y = "Power"
    ) +
    gglayer_theme

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_power-cluster-avg-TNIE.png"),
       plot = last_plot())



# Tables ------------------------------------------------------------------
## TNIE --------------------------------------------------------------------


# ══════════════════════════════
#    True values 
# ══════════════════════════════
# True values 
perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj) |> 
    slice_head(n = 1) |> 
    select(starts_with("true") & ends_with("TNIE")) |> 
    print(n = Inf)

# ══════════════════════════════
#    Individual-average TNIE Bias  
# ══════════════════════════════
# Bias individual-average TNIE
perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    # pivot_wider(
    #     names_from = c(Fit, Nj),
    #     values_from = avg_bias,
    #     names_prefix = "bias_"
    # ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)

# ══════════════════════════════
#    CI coverage rates 
# ══════════════════════════════
perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(coverage = mean(cover_individual_TNIE)) |> 
    arrange(Yfamily, Mfamily, quad, Fit, cluster_opt, J) |> 
    # filter(quad == "linear" & Fit == "Nonparametric") |> 
    # arrange(coverage) |> 
    print(n = Inf) 
    
# ══════════════════════════════
#    MSE 
# ══════════════════════════════
perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(mse = MSE_individual_TNIE) |> 
    arrange(Yfamily, Mfamily, quad, Fit, cluster_opt, J) |> 
    # filter(quad == "linear" & Fit == "Nonparametric") |> 
    # arrange(coverage) |> 
    print(n = Inf) 


    



#######
library(dplyr)
library(tidyr)
library(gt)
library(scales)

perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) %>%
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) %>%
    gt() %>%
    tab_header(
        title = "Bias in Individual-Average TNIE",
        subtitle = "Grouped by Yfamily, Mfamily, quad, cluster_opt, and J"
    ) %>%
    fmt_number(
        columns = starts_with("bias_"),
        decimals = 4
    ) %>%
    cols_label(
        quad = "Quadratic",
        Mfamily = "M Family",
        Yfamily = "Y Family",
        J = "J",
        cluster_opt = "Cluster Option",
        `bias_mlr_U[5, 20]` = "MLR U[5, 20]",
        `bias_glm_U[5, 20]` = "GLM U[5, 20]"
    ) %>%
    tab_style(
        style = cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(2)
        ),
        locations = cells_body(
            rows = Yfamily != lead(Yfamily)
        )
    ) %>%
    # tab_style(
    #     style = cell_fill(color = "lightgrey"),
    #     locations = cells_body(
    #         rows = seq(1, nrow(.), by = 2)
    #     )
    # ) %>%
    opt_row_striping() %>%
    tab_options(
        table.font.size = px(12),
        column_labels.font.weight = "bold",
        table.border.top.color = "black",
        table.border.top.width = px(2),
        table.border.bottom.color = "black",
        table.border.bottom.width = px(2)
    )


ind_bias_tbl <- perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) |> 
    pivot_wider(names_from = c(quad, Yfamily), 
                values_from = avg_bias) |> 
    arrange(Mfamily, J, Fit) 

# Display values (Copy-paste values into generate table website to obtain latex code)
as_hux(ind_bias_tbl) 

# Save Table 
write_csv(
    ind_bias_tbl, 
    file = paste0(results_path, "/Tables/",
                  "S1_bias-individual-avg-TNIE-table.csv"), 
    col_names = TRUE
)
filename = paste0(results_path, "/Figures/",
                  "S1_coverage-cluster-avg-TNIE.png")
write_csv(
    ind_bias_tbl,
    file = paste0(path, "/Tables/S2_TNIE-RMSE.csv"),
    col_names = TRUE
)


# EXAMPLE
# Pivot wide
rmse_Tbl <- pivot_wider(
    perf_measure_DF[, c("ICC",
                        "clust_size",
                        "analysisCond",
                        "PNDE_RMSE",
                        "TNIE_RMSE")],
    names_from = c(ICC, clust_size),
    names_sep = "_",
    values_from = c("PNDE_RMSE", "TNIE_RMSE")
)

# drop common piece of column names
colnames(rmse_Tbl) <-
    stringr::str_remove(string = colnames(rmse_Tbl),
                        pattern = "_RMSE")

# RMSE TNIE Table   
rmse_TNIE_Table <- rmse_Tbl %>%
    select(analysisCond, starts_with("TNIE_")) %>%
    separate(
        col = c("analysisCond"),
        into = c("PS Model", "Mediator/Outcome Model"),
        sep = "_"
    ) %>%
    arrange(`Mediator/Outcome Model`)

# Display values (Copy-paste values into generate table website to obtain latex code)
as_hux(rmse_TNIE_Table) %>%
    set_number_format(
        row = 1:nrow(rmse_TNIE_Table) + 1,
        col = -c(1:2),
        value = 3
    )

# Save Table 
write_csv(
    rmse_TNIE_Table,
    file = paste0(path, "/Tables/S2_TNIE-RMSE.csv"),
    col_names = TRUE
)




# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Process the data: compute average bias and reshape
pub_table <- perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) %>% 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) %>%
    # Rename columns to be more descriptive for publication
    rename(
        "Quadratic"       = quad,
        "M Family"        = Mfamily,
        "Y Family"        = Yfamily,
        "Sample Size (J)" = J,
        "Cluster Option"  = cluster_opt,
        "Bias (MLR, U[5,20])" = `bias_mlr_U[5, 20]`,
        "Bias (GLM, U[5,20])" = `bias_glm_U[5, 20]`
    )

# Create and style the publishable table
pub_table %>%
    kable(format = "html", digits = 4,
          caption = "Average Bias in TNIE by Model Configuration") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) %>%
    add_header_above(c(" " = 5, "Model Bias" = 2))



######



# CI coverage individual-average TNIE
perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_coverage = mean(cover_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_coverage,
        names_prefix = "coverage_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)

# MSE individual-average TNIE
perf_measures %>%
    group_by(Yfamily, Mfamily, quad, cluster_opt, J, Nj, Fit) %>%
    summarise(
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_MSE,
        names_prefix = "MSE_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)


perf_measures |> 
    group_by(Yfamily, Mfamily, quad, cluster_opt, J, Nj, Fit) |> 
    summarise(
        min_MSE = min(MSE_individual_TNIE), 
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        med_MSE = median(MSE_individual_TNIE), 
        max = max(MSE_individual_TNIE),
        .groups = "drop"
    ) |> 
    print(n = Inf)


### Overlap  ----------------------------------------------------------------

# ══════════════════════════════
#    check overlap 
# ══════════════════════════════
## function to extract percentages
extract_psnum <- function(text, pattern) {
    number <- str_extract(text, pattern)
    as.numeric(str_extract(number, "\\d+\\.?\\d*"))
}
## extract percent above or below thresholds 
sim_data$psLowPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)")
sim_data$psHighPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)$")


# ═══════════════════
#    display extreme PSs for replications  
# ═══════════════════
# Calculate counts & pct of extremely low PSs for each facet
threshold <- 1
threshold2 <- 2
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic) |> 
    summarize(count = sum(psLowPct > threshold), 
              pct = (sum(psLowPct > threshold) / sum(length(psLowPct)))*100, 
              count2 = sum(psLowPct > threshold2), 
              pct2 = (sum(psLowPct > threshold2) / sum(length(psLowPct)))*100)
# Extreme low PSs viz 
low_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    # & psLowPct >= threshold) |>
    ggplot(aes(x = psLowPct, fill = factor(psLowPct > threshold))) +
    geom_histogram() +
    facet_grid(~ J + Nj) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Calculate counts & pct of extremely high PSs for each facet
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic) |> 
    summarize(count = sum(psHighPct > threshold), 
              pct = (sum(psHighPct > threshold) / sum(length(psHighPct)))*100, 
              count2 = sum(psHighPct > threshold2), 
              pct2 = (sum(psHighPct > threshold2) / sum(length(psHighPct)))*100)
# Extreme high PSs viz 
high_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    # & psHighPct >= threshold) |>
    ggplot(aes(x = psHighPct, fill = factor(psHighPct>threshold))) +
    geom_histogram() +
    facet_grid(~ J + Nj) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Plot visual 
high_ps_plot / low_ps_plot +
    patchwork::plot_annotation(title = "# of reps w/ % of PSs more extreme than < .01 or > .99", 
                               theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))) +
    patchwork::plot_annotation(caption = "Note: only reps with > 1% of PSs being extreme are displayed \n Ex: (36%)>1 = 36% of reps in that facet had 1% or more extreme PSs") 

# Save plot
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_overlap_extreme-PSs-by-cluster-size-and-number.png"),
       plot = last_plot())


# ═══════════════════
#    Extreme PSs tables  
# ═══════════════════
# Summary of percent under 0.01 PS 
sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    summarize(min = min(psLowPct),
              q25 = quantile(psLowPct, 0.25),
              median = median(psLowPct),
              mean = mean(psLowPct),
              q75 = quantile(psLowPct, 0.75),
              max = max(psLowPct)
    ) |> 
    print(n = Inf)
# Summary of percent over 0.99 PS 
sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    summarize(min = min(psHighPct),
              q25 = quantile(psHighPct, 0.25),
              median = median(psHighPct),
              mean = mean(psHighPct),
              q75 = quantile(psHighPct, 0.75),
              max = max(psHighPct)
    ) |> 
    print(n = Inf)







###################################### OLD CODE ########################################

