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
# Last Updated: 2025-10-22
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
sim_date <- "2025-09-03" 

# Results folder 
path <- "Output/S1_Results"
if (!dir.exists(path)) {
    dir.create(path)
}
### Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- "2025-09-03_200-reps" 
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder_results)) {
    results_path <- file.path(path, additional_folder_results)
}
### Create directory
if (!dir.exists(results_path)) {
    dir.create(results_path)
}
## Data subfolder
if (!dir.exists(paste0(results_path, "/Data"))) {
    dir.create(paste0(results_path, "/Data"))
}
## Figure subfolder 
if (!dir.exists(paste0(results_path, "/Figures"))) {
    dir.create(paste0(results_path, "/Figures"))
}
## Tables subfolder
if (!dir.exists(paste0(results_path, "/Tables"))) {
    dir.create(paste0(results_path, "/Tables"))
}

## Figures path 
figures_path <- paste0(results_path, "/Figures_2025-10-21_replaced-extreme-values")
## Create Figures folder  
if (!dir.exists(figures_path)) {
    dir.create(figures_path)
}

## Figures' subfolders
if (!dir.exists(paste0(figures_path, "/TNIE"))) {
    dir.create(paste0(figures_path, "/TNIE"))
}
if (!dir.exists(paste0(figures_path, "/PNDE"))) {
    dir.create(paste0(figures_path, "/PNDE"))
}
if (!dir.exists(paste0(figures_path, "/Convergence"))) {
    dir.create(paste0(figures_path, "/Convergence"))
}
## Figures' subsubfolder
### TNIE
if (!dir.exists(paste0(figures_path, "/TNIE/nonnull"))) {
    dir.create(paste0(figures_path, "/TNIE/nonnull"))
}
if (!dir.exists(paste0(figures_path, "/TNIE/null"))) {
    dir.create(paste0(figures_path, "/TNIE/null"))
}
### PNDE
if (!dir.exists(paste0(figures_path, "/PNDE/nonnull"))) {
    dir.create(paste0(figures_path, "/PNDE/nonnull"))
}
if (!dir.exists(paste0(figures_path, "/PNDE/null"))) {
    dir.create(paste0(figures_path, "/PNDE/null"))
}


# Import results data ----------------------------------------------------------

# import data if needed 
# perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, ".rds"))
perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, "_converged-only.rds")) # perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, "_excludes-warnings.rds")) 
# sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_excludes-warnings.rds")) #"_excludes-nonconvergence.rds"))
sim1_data_nowarnings <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_excludes-warnings.rds")) 
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 
convergence_rates <- readRDS(file = paste0(results_path, "/Tables/S1_convergence-rates_", sim_date, ".rds"))

## data with replacement values 
perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_updated-performance-measures_", sim_date, "_converged-only.rds")) 
sim1_data_nowarnings <- readRDS(file = paste0(results_path, "/Data/S1_updated-simulation-data_", sim_date, "_excludes-warnings.rds")) 
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_updated-simulation-data_", sim_date, "_converged-only.rds")) 

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
        # Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")#,
        # Mfamily = factor(
        #     Mfamily,
        #     levels = c("binomial", "gaussian"),
        #     labels = c("Mediator: Binomial", "Mediator: Gaussian")
        # ),
        # Yfamily = factor(
        #     Yfamily,
        #     levels = c("binomial", "gaussian"),
        #     labels = c("Outcome: Binomial", "Outcome: Gaussian")
        # ),
        # quad = factor(
        #     quad,
        #     levels = c("linear", "nonlinear"),
        #     labels = c("Linear", "Nonlinear")
        # ),
        # cluster_opt = factor(
        #     cluster_opt,
        #     levels = c("cwc", "cwc.FE"),
        #     labels = c("mean", "mean + dummies")
        # )
    )

# Modify data for visuals 
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))

# Modify convergence 
convergence_rates <- convergence_rates |> 
    mutate(Fit = ifelse(cluster_opt == "RE.glm", "Parametric: RE", Fit), 
           cluster_opt = ifelse(cluster_opt == "RE.glm", "cwc", cluster_opt))

# # Drop RE.glm
# perf_measures <- perf_measures |>
#     filter(cluster_opt != "RE.glm")
# sim_data <- sim_data |> 
#     filter(cluster_opt != "RE.glm")

# Split null & nonull dataframes
perf_measures_null <- perf_measures[perf_measures$ifnull == TRUE, ]
perf_measures_nonnull <- perf_measures[perf_measures$ifnull == FALSE, ]


# Visuals -----------------------------------------------------------------

#333f48, #bf5700, & #579d42

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

# Store facet layer for reuse
gglayer_facet <- facet_grid(
    Mfamily + Yfamily ~ quad + cluster_opt,
    labeller = labeller(
        Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
        Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
        quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
        cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
    )
)



# Function for visuals ----------------------------------------------------

create_plot <- function(data, 
                        y_var, 
                        effect = "TNIE",  # "TNIE" or "PNDE" 
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
    p <- ggplot(plot_data, aes(x = factor(J), y = !!sym(y_var), 
                               color = Fit, shape = Nj, linetype = Fit)) +
        geom_point(size = 2) +
        geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8)
    
    # Add reference line if requested
    if (add_reference_line) {
        if (is.null(reference_value)) {
            # Auto-determine reference value based on metric type
            if (grepl("bias|mse|type1", tolower(y_var))) {
                reference_value <- 0
            } else if (grepl("cover|power", tolower(y_var))) {
                reference_value <- 1
            } else {
                reference_value <- 0  # default
            }
        }
        p <- p + geom_hline(yintercept = reference_value)
    }
    
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
    # p <- p + 
    #     facet_grid(
    #         Mfamily + Yfamily ~ quad + cluster_opt,
    #         labeller = labeller(
    #             Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
    #             Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
    #             quad = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
    #             cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
    #         )
    #     ) +
    #     theme_bw() +
    #     scale_color_manual(values = c("#BF5700", "#579D42", "#333F48")) +
    #     scale_fill_manual(values = c("#BF5700", "#579D42", "#333F48")) +
    #     theme(
    #         text = element_text(family = "arial", size = 12),
    #         title = element_text(size = 16),
    #         axis.title = element_text(size = 14),
    #         axis.text = element_text(size = 12),
    #         legend.title = element_text(size = 14),
    #         legend.text = element_text(size = 12),
    #         strip.text = element_text(size = 14),
    #         line = element_line(linewidth = 0.8),
    #         legend.position = "top"
    #     )
    
    return(p)
}



## Convergence Rate --------------------------------------------------------

#### Nonnull -------------------------------------------------------------------
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

#### Null -------------------------------------------------------------------
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



## TNIE (non-null) ---------------------------------------------------------

### Individual-average TNIE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# TNIE Bias for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_individual_TNIE",
            outcome_type = "gaussian")
# Save plot
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_bias_nonnull_ind_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE Bias for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_individual_TNIE",
            outcome_type = "binomial")
# Save plot
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_bias_nonnull_ind_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════

# TNIE CI coverage rate 
create_plot(data = perf_measures_nonnull, 
            y_var = "cover_individual_TNIE",
            outcome_type = "all")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_coverage_nonnull_ind_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════

# TNIE MSE for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_individual_TNIE",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_mse_nonnull_ind_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE MSE for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_individual_TNIE",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_mse_nonnull_ind_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Power -------------------------------------------------------------------
# ═══════════════════
#    Power
# ═══════════════════

# perf_measures only included output with if.null = FALSE, so this computes power (not type 1 error rate)

# TNIE Power 
create_plot(data = perf_measures_nonnull, 
            y_var = "power_individual_TNIE",
            outcome_type = "all")
# Save plot     
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_power_nonnull_ind_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


### Cluster-average TNIE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# TNIE Bias for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_cluster_TNIE",
            outcome_type = "gaussian")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_bias_nonnull_clus_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE Bias for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_cluster_TNIE",
            outcome_type = "binomial")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_bias_nonnull_clus_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════
# TNIE CI coverage rate 
create_plot(data = perf_measures_nonnull, 
            y_var = "cover_cluster_TNIE",
            outcome_type = "all")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_coverage_nonnull_clus_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# TNIE MSE for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_cluster_TNIE",
            outcome_type = "gaussian")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_mse_nonnull_clus_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE MSE for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_cluster_TNIE",
            outcome_type = "binomial")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_mse_nonnull_clus_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Power -------------------------------------------------------------------
# ═══════════════════
#    Power
# ═══════════════════

# perf_measures only included output with if.null = FALSE, so this computes power (not type 1 error rate)

# TNIE Power 
create_plot(data = perf_measures_nonnull, 
            y_var = "power_cluster_TNIE",
            outcome_type = "all")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1_power_nonnull_clus_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


## TNIE (null) -------------------------------------------------------------

### Individual-average TNIE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# TNIE Bias for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_individual_TNIE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_bias_null_ind_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE Bias for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_individual_TNIE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_bias_null_ind_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════

# TNIE CI coverage rate 
create_plot(data = perf_measures_null, 
            y_var = "cover_individual_TNIE",
            null_status = "null",
            outcome_type = "all")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_coverage_null_ind_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════

# TNIE MSE for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_individual_TNIE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_mse_null_ind_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE MSE for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_individual_TNIE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_mse_null_ind_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Type I Error ------------------------------------------------------------
# ═══════════════════
#    Type I Error 
# ═══════════════════
# TNIE Type I Error
create_plot(data = perf_measures_null, 
            y_var = "type1_individual_TNIE",
            null_status = "null",
            outcome_type = "all")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_type1_null_ind_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


### Cluster-average TNIE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════

# TNIE Bias for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_cluster_TNIE",
            null_status = "null",
            outcome_type = "gaussian")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_bias_null_clus_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE Bias for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_cluster_TNIE",
            null_status = "null",
            outcome_type = "binomial")
#Save 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_bias_null_clus_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------

# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════

# TNIE CI coverage rate 
create_plot(data = perf_measures_null, 
            y_var = "cover_cluster_TNIE",
            null_status = "null",
            outcome_type = "all")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_coverage_null_clus_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# TNIE MSE for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_cluster_TNIE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_mse_null_clus_TNIE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# TNIE MSE for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_cluster_TNIE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_mse_null_clus_TNIE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Type I Error ------------------------------------------------------------
# ═══════════════════
#    Type I Error 
# ═══════════════════
# TNIE Type I Error
create_plot(data = perf_measures_null, 
            y_var = "type1_cluster_TNIE",
            null_status = "null",
            outcome_type = "all")
# Save 
ggsave(filename = paste0(figures_path, "/TNIE/null/",
                         "S1_type1_null_clus_TNIE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)



## PNDE --------------------------------------------------------------------

## PNDE (non-null) --------------------------------------------------------------------

### Individual-average PNDE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# PNDE Bias for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "gaussian")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_bias_nonnull_ind_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE Bias for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "binomial")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_bias_nonnull_ind_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════
# PNDE CI coverage rate 
create_plot(data = perf_measures_nonnull, 
            y_var = "cover_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "all")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_coverage_nonnull_ind_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# PNDE MSE for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_mse_nonnull_ind_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE MSE for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "binomial")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_mse_nonnull_ind_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Power -------------------------------------------------------------------
# ═══════════════════
#    Power
# ═══════════════════
# TNIE Power 
create_plot(data = perf_measures_nonnull, 
            y_var = "power_individual_PNDE",
            null_status = "nonnull",
            outcome_type = "all")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_power_nonnull_ind_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


### Cluster-average PNDE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# PNDE Bias for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_bias_nonnull_clus_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE Bias for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "bias_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_bias_nonnull_clus_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════
# PNDE CI coverage rate 
create_plot(data = perf_measures_nonnull, 
            y_var = "cover_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "all")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_coverage_nonnull_clus_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# PNDE MSE for Gaussian outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_mse_nonnull_clus_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE MSE for Binomial outcomes 
create_plot(data = perf_measures_nonnull, 
            y_var = "MSE_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_mse_nonnull_clus_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Power -------------------------------------------------------------------
# ═══════════════════
#    Power
# ═══════════════════
# TNIE Power 
create_plot(data = perf_measures_nonnull, 
            y_var = "power_cluster_PNDE",
            null_status = "nonnull",
            outcome_type = "all")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/nonnull/",
                         "S1_power_nonnull_clus_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


## PNDE (null) --------------------------------------------------------------------

### Individual-average PNDE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# PNDE Bias for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_individual_PNDE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_bias_null_ind_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE Bias for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_individual_PNDE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_bias_null_ind_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════
# PNDE CI coverage rate 
create_plot(data = perf_measures_null, 
            y_var = "cover_individual_PNDE",
            null_status = "null",
            outcome_type = "all")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_coverage_null_ind_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# PNDE MSE for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_individual_PNDE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_mse_null_ind_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE MSE for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_individual_PNDE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_mse_null_ind_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Type I Error -------------------------------------------------------------------
# ═══════════════════
#    Type I Error
# ═══════════════════
# TNIE Type I Error 
create_plot(data = perf_measures_null, 
            y_var = "type1_individual_PNDE",
            null_status = "null",
            outcome_type = "all")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_type1_null_ind_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


### Cluster-average PNDE -------------------------------------------------

#### Bias --------------------------------------------------------------------
# ═══════════════════
#    Bias 
# ═══════════════════
# PNDE Bias for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_cluster_PNDE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_bias_null_clus_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE Bias for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "bias_cluster_PNDE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_bias_null_clus_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### CI Coverage Rate --------------------------------------------------------------------
# ═══════════════════
#    CI Coverage Rate 
# ═══════════════════
# PNDE CI coverage rate 
create_plot(data = perf_measures_null, 
            y_var = "cover_cluster_PNDE",
            null_status = "null",
            outcome_type = "all")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_coverage_null_clus_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


#### MSE ---------------------------------------------------------------------
# ═══════════════════
#    MSE
# ═══════════════════
# PNDE MSE for Gaussian outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_cluster_PNDE",
            null_status = "null",
            outcome_type = "gaussian")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_mse_null_clus_PNDE_gaus.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# PNDE MSE for Binomial outcomes 
create_plot(data = perf_measures_null, 
            y_var = "MSE_cluster_PNDE",
            null_status = "null",
            outcome_type = "binomial")
# Save plot 
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_mse_null_clus_PNDE_binom.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)


#### Type I Error -------------------------------------------------------------------
# ═══════════════════
#    Type I Error
# ═══════════════════
# TNIE Type I Error 
create_plot(data = perf_measures_null, 
            y_var = "type1_cluster_PNDE",
            null_status = "null",
            outcome_type = "all")
# Save plot
ggsave(filename = paste0(figures_path, "/PNDE/null/",
                         "S1_type1_null_clus_PNDE_all.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)






# Check variability in TNIE estimates -------------------------------------

# ══════════════════════════════
#    gaussian outcome  
# ══════════════════════════════
sim1_data_converged |> 
    filter(ifnull == FALSE & nj == "5-20" & Yfamily == "gaussian") |> # & cluster_opt == "cwc.FE") |> 
    filter(cluster_opt != "RE.glm") |> 
    mutate(res = individual_ie_Estimate - individual_tnie) |> 
    ggplot(aes(x = as.factor(J), y = res, fill = Fit)) +
    geom_boxplot(outlier.shape = 21,
                 outlier.size = 1.5,
                 outlier.alpha = 0.9, 
                 aes(fill = Fit)) +   
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    geom_hline(yintercept = 0.5, color = "red") +
    geom_hline(yintercept = -0.5, color = "red") +
    gglayer_theme +
    scale_fill_manual(values = c("#579D42", # glm
                                 "#BF5700", # mlr
                                 "#333F48")) +
    scale_color_manual(values = c("#579D42", # glm
                                  "#BF5700", # mlr
                                  "#333F48")) +
    labs(
        x = "J",
        y = "estimate - tnie", 
        caption = "Note: red line = +/- 0.5"
    )

# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1-extra_bias_nonnull_ind_TNIE_gaus.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)


# ══════════════════════════════
#    binomial outcome  
# ══════════════════════════════
sim1_data_converged |> 
    filter(ifnull == FALSE & nj == "5-20" & Yfamily == "binomial") |> # & cluster_opt == "cwc.FE") |> 
    filter(cluster_opt != "RE.glm") |> 
    mutate(res = individual_ie_Estimate - individual_tnie) |> 
    ggplot(aes(x = as.factor(J), y = res, fill = Fit)) +
    geom_boxplot(outlier.shape = 21,
                 outlier.size = 1.5,
                 outlier.alpha = 0.9, 
                 aes(fill = Fit)) +   
    facet_grid(Mfamily + Yfamily ~ quadratic + cluster_opt, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quadratic    = c("FALSE" = "Linear", "TRUE" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    # geom_hline(yintercept = 0.5, color = "red") +
    # geom_hline(yintercept = -0.5, color = "red") +
    gglayer_theme +
    scale_fill_manual(values = c("#579D42", # glm
                                 "#BF5700", # mlr
                                 "#333F48")) +
    labs(
        x = "J",
        y = "estimate - tnie", 
    )

# Save 
ggsave(filename = paste0(figures_path, "/TNIE/nonnull/",
                         "S1-extra_bias_nonnull_ind_TNIE_binom.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)









## Overlap  ----------------------------------------------------------------

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



## Number of Simulations per condition -------------------------------------

# import data if needed 
# ## (excludes warnings)
# sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_excludes-warnings.rds"))
## (excludes nonconverging cases)
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 
sim_data <- sim1_data_converged

# Modify data for visuals 
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
# str(sim_data)

# Number of reps with no warnings 
sim_data |> 
    # drop null & linear cases
    # filter(ifnull == FALSE & quadratic == TRUE) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(n_simulations = n(), .groups = "drop") |> 
    mutate(condition_label = paste(Fit, cluster_opt, sep = " / ")) |> 
    ggplot(aes(x = condition_label, y = n_simulations, fill = condition_label)) +
    geom_col() +
    geom_text(aes(label = n_simulations), vjust = 1.75, size = 3) +
    # facet_grid(Mfamily + Yfamily ~ J + Nj) +
    facet_grid(
        Mfamily + Yfamily ~ J + Nj + quad,
        # Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Number of Replications per Condition for Non-null Scenario",
        x = "Fit / Cluster Option",
        y = "Number of Replications"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 50, hjust = 1), 
        legend.position = "none"
    )
# Save 
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_replications-by-simulation-condition.png"),
       plot = last_plot(), width = 12, height = 8)



# Number of reps for quadratic scenario 
sim_data |> 
    # drop null & linear cases
    filter(ifnull == FALSE & quadratic == TRUE) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(n_simulations = n(), .groups = "drop") |> 
    mutate(condition_label = paste(Fit, cluster_opt, sep = " / ")) |> 
    ggplot(aes(x = condition_label, y = n_simulations, fill = condition_label)) +
    geom_col() +
    geom_text(aes(label = n_simulations), vjust = 1.75, size = 3) +
    facet_grid(Mfamily + Yfamily ~ J + Nj) +
    labs(
        title = "Number of Replications per Condition for Non-null & Quad Scenario",
        x = "Fit / Cluster Option",
        y = "Number of Replications"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 50, hjust = 1), 
        legend.position = "none"
    )

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_replications-by-simulation-condition-for-quad.png"),
       plot = last_plot(), width = 12, height = 8)

# Number of reps for linear scenario 
sim_data |> 
    # drop null & quad cases
    filter(ifnull == FALSE & quadratic == FALSE) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(n_simulations = n(), .groups = "drop") |> 
    mutate(condition_label = paste(Fit, cluster_opt, sep = " / ")) |> 
    ggplot(aes(x = condition_label, y = n_simulations, fill = condition_label)) +
    geom_col() +
    geom_text(aes(label = n_simulations), vjust = 1.75, size = 3) +
    facet_grid(Mfamily + Yfamily ~ J + Nj) +
    labs(
        title = "Number of Replications per Condition for Non-null & Linear Scenario",
        x = "Fit / Cluster Option",
        y = "Number of Replications"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 50, hjust = 1), 
        legend.position = "none"
    )

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_replications-by-simulation-condition-for-linear.png"),
       plot = last_plot(), width = 12, height = 8)





# nonconv_plot <- nonconv_summary |> 
#     filter(ifnull == FALSE & quadratic == FALSE) |> 
#     mutate(
#         Fit_cluster = paste(Fit, cluster_opt, sep = " / "),
#         label = ifelse(pct_nonconv > 0, paste0(round(pct_nonconv, 1), "%\n(", count_nonconv, ")"), NA)
#     ) |> 
#     ggplot(aes(x = Fit_cluster, y = pct_nonconv, fill = Fit_cluster)) +# count_nonconv)) +
#     geom_col() +
#     # geom_text(aes(label = label), vjust = -0.25) +
#     geom_text(aes(label = label), nudge_y = -0.2) +
#     facet_grid(Mfamily + Yfamily ~ J + Nj) +
#     labs(
#         x = "Fit / Cluster Option",
#         y = "% Nonconverged",
#         title = "% of Simulations with Nonconvergence by Fit and Cluster Option"
#     ) +
#     theme_minimal(base_size = 12) +
#     theme(
#         axis.text.x = element_text(angle = -45, hjust = 1),
#         legend.position = "bottom"
#     )







## Model convergence rates -------------------------------------------------

# ══════════════════════════════
#    Number of converging cases  
# ══════════════════════════════

# import data if needed 
# ## (excludes warnings)
# sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_excludes-warnings.rds"))
## (excludes nonconverging cases)
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 
# sim_data <- sim1_data_converged

# Modify data for visuals 
sim1_data_converged <- sim1_data_converged |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
# str(sim_data)

# Number of reps with convergence 
sim1_data_converged |> 
    filter(cluster_opt != "RE.glm.rs") |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(n_simulations = n(), .groups = "drop") |> 
    mutate(condition_label = paste(Fit, cluster_opt, sep = " / ")) |> 
    ggplot(aes(x = condition_label, y = n_simulations, fill = condition_label)) +
    geom_col() +
    # geom_text(aes(label = n_simulations), vjust = 1.75, size = 3) +
    geom_text(aes(label = n_simulations), vjust = 0.5, size = 3, angle = -90, hjust = -0.25) +
    # geom_text(aes(x = condition_label, y = 0, label = n_simulations), angle = -90, hjust = 1.5, size = 3) +
    facet_grid(
        Mfamily + Yfamily ~ Nj + J + quad,
        # Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Number of Convergence per Condition for Non-null Scenario",
        x = "Fit / Cluster Option",
        y = "Number of Replications"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 50, hjust = 1), 
        legend.position = "none"
    )

# Save 
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_convergence-by-simulation-condition.pdf"),
       plot = last_plot(), width = 12, height = 8)


# ══════════════════════════════
#    Convergence rate visualization  
# ══════════════════════════════

# import overall data 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))
# import converged cases data 
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 

# Modify data for visuals & merging  
sim1_data_converged <- sim1_data_converged |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))

# dim(sim1_data[sim1_data$cluster_opt != "RE.glm", ])

# Calculate convergence rates
convergence_rates <- sim1_data |> 
    filter(cluster_opt != "RE.glm") |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(total_attempts = n(), .groups = "drop") |> 
    left_join(
        sim1_data_converged |> 
            filter(cluster_opt != "RE.glm") |> 
            # mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
                   # Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
            group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
            summarize(converged = n(), .groups = "drop"),
        by = c("ifnull", "quad", "Mfamily", "Yfamily", "J", "Nj", "Fit", "cluster_opt")
    ) |> 
    mutate(
        converged = ifelse(is.na(converged), 0, converged),
        nonconverged = total_attempts - converged,
        nonconvergence_rate = nonconverged / total_attempts,
        convergence_rate = converged / total_attempts, 
        Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
    ) 


# TNIE Convergence rate 
convergence_rates |> 
    ggplot(aes(x = factor(J), y = convergence_rate, color = Fit, shape = Nj, linetype = Fit)) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), size = 0.8) +
    # facet_grid(
    #     Mfamily + Yfamily ~ quad + cluster_opt,
    #     labeller = labeller(
    #         Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
    #         Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
    #         quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
    #         cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
    #     )
    # ) +
    labs(
        x = "\n Number of Clusters",
        y = "Convergence Rate \n", 
        color = "Method",
        linetype = "Method",
        shape = "Cluster size"
    ) +
    gglayer_facet +
    gglayer_theme #+
    # scale_y_continuous(limits = c(0, 1))
    # scale_y_continuous(labels = scales::percent_format())
    
# Save 
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_convergence-rates-linegraph.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)




## inspecting warnings & poor performing models ----------------------------

sim1_data_converged |> 
    filter(Yfamily == "binomial" & cluster_opt == "cwc.FE" & quadratic == TRUE) |> 
    count(Mfamily, nj, J, Fit, warnings, sort = T, name = "freq") |> 
    arrange(Mfamily, nj, J, Fit) |> 
    print(n = Inf)


# 
# sim1_data_converged |> 
#     mutate(ifnull = as.logical(ifnull)) |> 
#     group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj) |> 
#     mutate(true_individual_PNDE = mean(individual_pnde), 
#            true_individual_TNIE = mean(individual_tnie), 
#            true_cluster_PNDE = mean(cluster_pnde), 
#            true_cluster_TNIE = mean(cluster_tnie)) |> 
#     ungroup() |> 
#     group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
#     mutate(
#         if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
#         if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
#         if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
#         if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
#         
#         sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
#         sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
#         sig_cluster_PNDE    = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
#         sig_cluster_TNIE    = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
#     ) 



# Compute nonconvergence rate for each condition 
nonconv_summary <- sim1_data_converged |>
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic, Fit, cluster_opt) %>%
    summarize(
        count_nonconv = sum(nonconverged, na.rm = TRUE),
        total = n(),
        pct_nonconv = (count_nonconv / total) * 100,
        .groups = "drop"
    )

nonconv_plot <- nonconv_summary |> 
    filter(ifnull == FALSE & quadratic == FALSE) |> 
    mutate(
        Fit_cluster = paste(Fit, cluster_opt, sep = " / "),
        label = ifelse(pct_nonconv > 0, paste0(round(pct_nonconv, 1), "%\n(", count_nonconv, ")"), NA)
    ) |> 
    ggplot(aes(x = Fit_cluster, y = pct_nonconv, fill = Fit_cluster)) +# count_nonconv)) +
    geom_col() +
    # geom_text(aes(label = label), vjust = -0.25) +
    geom_text(aes(label = label), nudge_y = -0.2) +
    facet_grid(Mfamily + Yfamily ~ J + Nj) +
    labs(
        x = "Fit / Cluster Option",
        y = "% Nonconverged",
        title = "% of Simulations with Nonconvergence by Fit and Cluster Option"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = -45, hjust = 1),
        legend.position = "bottom"
    )

nonconv_plot 

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_nonconvergence-by-simulation-condition.png"),
       plot = nonconv_plot, width = 12, height = 8)





# import data with nonconvergence included 
sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_includes-nonconvergence.rds"))

# Modify data for visuals 
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))

str(sim_data)

# Add nonconvergence indicator variable 
sim_data <- sim_data |> 
    mutate(nonconverged = warnings == "glm.fit: algorithm did not converge")
# Compute nonconvergence rate for each condition 
nonconv_summary <- sim_data |>
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic, Fit, cluster_opt) %>%
    summarize(
        count_nonconv = sum(nonconverged, na.rm = TRUE),
        total = n(),
        pct_nonconv = (count_nonconv / total) * 100,
        .groups = "drop"
    )

nonconv_summary |> 
    arrange(desc(pct_nonconv)) |> 
    filter(Mfamily == "gaussian" & Yfamily == "gaussian" & J == 40 & ifnull == FALSE)
    print(n = Inf)


library(ggplot2)

nonconv_plot <- nonconv_summary |> 
    filter(ifnull == FALSE & quadratic == FALSE) |> 
    mutate(
        Fit_cluster = paste(Fit, cluster_opt, sep = " / "),
        label = ifelse(pct_nonconv > 0, paste0(round(pct_nonconv, 1), "%\n(", count_nonconv, ")"), NA)
    ) |> 
    ggplot(aes(x = Fit_cluster, y = pct_nonconv, fill = Fit_cluster)) +# count_nonconv)) +
    geom_col() +
    # geom_text(aes(label = label), vjust = -0.25) +
    geom_text(aes(label = label), nudge_y = -0.2) +
    facet_grid(Mfamily + Yfamily ~ J + Nj) +
    labs(
        x = "Fit / Cluster Option",
        y = "% Nonconverged",
        title = "% of Simulations with Nonconvergence by Fit and Cluster Option"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = -45, hjust = 1),
        legend.position = "bottom"
    )

nonconv_plot 

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_nonconvergence-by-simulation-condition.png"),
       plot = nonconv_plot, width = 12, height = 8)



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

# Compare cluster info method for nonlinear 
perf_measures_nonnull |> 
    filter(quadratic == TRUE & Fit == "Nonparametric") |> 
    group_by(Mfamily, Yfamily, Nj, J, cluster_opt) |> # J, 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        # med_bias = median(bias_individual_TNIE),
        # min_bias = min(bias_individual_TNIE),
        # max_bias = max(bias_individual_TNIE),
        .groups = "drop"
    ) |> 
    arrange(Mfamily, Yfamily, Nj, J, avg_bias) |> # Yfamily, Mfamily, J
    # arrange(avg_bias) |>
    print(n = Inf)

# dumb move 
perf_measures_nonnull %>%
    filter(quadratic == TRUE, Fit == "Nonparametric") %>%
    group_by(Mfamily, Yfamily, Nj, J, cluster_opt) %>%
    summarise(avg_bias = mean(bias_individual_TNIE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = cluster_opt, values_from = avg_bias) %>%
    tidyr::drop_na(cwc, `cwc.FE`) %>%
    mutate(
        diff = `cwc.FE` - cwc,
        mid  = (`cwc.FE` + cwc) / 2,   # <-- average of the two
        scenario = paste0("M: ", stringr::str_to_title(Mfamily),
                          " | Y: ", stringr::str_to_title(Yfamily),
                          " | Nj: ", Nj,
                          " | J: ", J),
        scenario = paste0("Nj: ", Nj,
                          " | J: ", J),
        scenario = forcats::fct_reorder(scenario, mid, .desc = TRUE) # order by average bias
    ) %>%
    ggplot(aes(y = reorder(scenario, mid, .desc = T))) +# scenario)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_segment(aes(x = cwc, xend = `cwc.FE`, yend = scenario),
                 linewidth = 0.8, color = "grey60") +
    geom_point(aes(x = cwc,     color = "mean"), size = 2) +
    geom_point(aes(x = `cwc.FE`, color = "mean + dummies"), size = 2) +
    # facet_grid(Mfamily ~ Yfamily, scales = "free_x") +
    facet_grid(Mfamily + Yfamily ~ 1) +
    scale_color_manual(
        name = "Cluster adjustment",
        values = c("mean" = "#BF5700", "mean + dummies" = "#A6CD57")
    ) +
    labs(x = "Average bias (individual TNIE)", 
         # y = "Scenario (M | Y | Nj | J)"
         ) +
    theme_bw() +
    theme(legend.position = "top", axis.text.y = element_text(size = 10))


# TNIE bias comparing cluster_opt 
perf_measures_nonnull |> 
    # only nonparametric 
    # filter(Fit == "Nonparametric") |>
    # filter(Fit == "Parametric") |>
    filter(quadratic == F) |>
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Nj, linetype = cluster_opt)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8) +
    labs(
        x = "\n Number of Clusters", # "J",
        y = "Bias \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    
    facet_grid(Mfamily + Yfamily ~ quad + Fit, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme


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


# TNIE CI coverage rate comparing cluster_opt 
perf_measures_nonnull |> 
    # only nonparametric 
    # filter(Fit == "Nonparametric") |> 
    filter(quadratic == F) |>
    ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = cluster_opt, shape = Nj, linetype = cluster_opt)) +
    ggplot2::geom_hline(yintercept = 1) +
    # ggplot2::geom_hline(yintercept = 0.95) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8) +
    labs(
        x = "\n Number of Clusters", # "J",
        y = "Coverage \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    facet_grid(Mfamily + Yfamily ~ quad + Fit, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme



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

# Compare cluster info method for nonlinear 
perf_measures_nonnull |> 
    filter(quadratic == TRUE) |> 
    group_by(Nj, Fit, cluster_opt) |> #Mfamily, Yfamily, J, 
    summarise(
        avg_cover = mean(cover_individual_TNIE, na.rm = TRUE),
        med_cover = median(cover_individual_TNIE),
        min_cover = min(cover_individual_TNIE),
        max_cover = max(cover_individual_TNIE),
        .groups = "drop"
    ) |> 
    # arrange(cluster_opt) |> # Yfamily, Mfamily, J
    arrange(desc(avg_cover)) |> 
    print(n = Inf)

# TNIE MSE comparing cluster_opt 
perf_measures_nonnull |> 
    # only nonparametric 
    # filter(Fit == "Nonparametric") |> 
    filter(quadratic == F) |>
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = cluster_opt, shape = Nj, linetype = cluster_opt)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8) +
    labs(
        x = "\n Number of Clusters", # "J",
        y = "MSE \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    facet_grid(Mfamily + Yfamily ~ quad + Fit, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme


# ══════════════════════════════
#    Type 1 error rates 
# ══════════════════════════════
perf_measures_null |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(type1 = mean(type1_individual_TNIE)) |> 
    arrange(Yfamily, Mfamily, quad, Fit, cluster_opt, J) |> 
    # filter(quad == "linear" & Fit == "Nonparametric") |> 
    # arrange(type1) |> 
    print(n = Inf) 

# Compare cluster info method for nonlinear 
perf_measures_null |> 
    filter(quadratic == TRUE) |> 
    group_by(Nj, Fit, cluster_opt) |> #Mfamily, Yfamily, J, 
    summarise(
        avg_type1 = mean(type1_individual_TNIE, na.rm = TRUE),
        med_type1 = median(type1_individual_TNIE),
        min_type1 = min(type1_individual_TNIE),
        max_type1 = max(type1_individual_TNIE),
        .groups = "drop"
    ) |> 
    # arrange(cluster_opt) |> # Yfamily, Mfamily, J
    arrange(avg_type1) |> 
    print(n = Inf)

# TNIE Type I error rate comparing cluster_opt 
perf_measures_null |> 
    # only nonparametric 
    # filter(Fit == "Nonparametric") |> 
    filter(quadratic == F) |>
    ggplot(aes(x = factor(J), y = type1_individual_TNIE, color = cluster_opt, shape = Nj, linetype = cluster_opt)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8) +
    labs(
        x = "\n Number of Clusters", # "J",
        y = "Type I error \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    facet_grid(Mfamily + Yfamily ~ quad + Fit, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme


# ══════════════════════════════
#    Power 
# ══════════════════════════════
perf_measures_nonnull |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(power = mean(power_individual_TNIE)) |> 
    arrange(Yfamily, Mfamily, quad, Fit, cluster_opt, J) |> 
    # filter(quad == "linear" & Fit == "Nonparametric") |> 
    # arrange(type1) |> 
    print(n = Inf) 

# Compare cluster info method for nonlinear 
perf_measures_nonnull |> 
    filter(quadratic == TRUE) |> 
    group_by(Nj, Fit, cluster_opt) |> #Mfamily, Yfamily, J, 
    summarise(
        avg_power = mean(power_individual_TNIE, na.rm = TRUE),
        med_power = median(power_individual_TNIE),
        min_power = min(power_individual_TNIE),
        max_power = max(power_individual_TNIE),
        .groups = "drop"
    ) |> 
    # arrange(cluster_opt) |> # Yfamily, Mfamily, J
    arrange(avg_power) |> 
    print(n = Inf)

# TNIE Power comparing cluster_opt 
perf_measures_nonnull |> 
    # only nonparametric 
    # filter(Fit == "Nonparametric") |> 
    filter(quadratic == F) |>
    ggplot(aes(x = factor(J), y = power_individual_TNIE, color = cluster_opt, shape = Nj, linetype = cluster_opt)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point(size = 2) +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj)), linewidth = 0.8) +
    labs(
        x = "\n Number of Clusters", # "J",
        y = "Power \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    facet_grid(Mfamily + Yfamily ~ quad + Fit, 
               labeller = labeller(
                   Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
                   Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
                   quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
                   cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
               )) +
    gglayer_theme

##########################################
#           Trying other tabels           #
##########################################




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







###################################### OLD CODE ########################################

