################################################################################
####################### Simulation 1 - Study Design ############################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-07-01
#
#
# Script Description: 
#       This script generates figures and tables that describe the overall 
#       design of Simulation 1, not any simulation outputs. This includes 
#       the data-generating DAG and a summary table of manipulated design variables.
#
#
# Last Updated: 2025-07-07
#
#
# Notes:
#   To-Do
#     - simulation design table
#     - 
#
#   Done:
#     - Created DAG diagram illustrating data generation
#     - Generated design summary table and saved to /Tables
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

# # Date of simulation 
# sim_date <- "2025-07-01" # (Note: Simulations were ran around 2025-02-08 & 2025-05-05) #"2025-02-08" #"2025-01-25" #Sys.Date() #"2025-01-23" #"2025-01-18" 
# 
# # Number of replications
# # reps <- 200 # 1000
# reps <- 600

# Create directory to store results 
## Results folder 
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
# ### Create directory 
# if (!dir.exists(results_path)) {
#     dir.create(results_path)
# }
# ## Data, Figures, & Tables subfolders 
# if (!dir.exists(paste0(results_path, "/Data"))) {
#     dir.create(paste0(results_path, "/Data"))
# }
# if (!dir.exists(paste0(results_path, "/Figures"))) {
#     dir.create(paste0(results_path, "/Figures"))
# }
# if (!dir.exists(paste0(results_path, "/Tables"))) {
#     dir.create(paste0(results_path, "/Tables"))
# }


# Sim Design Table --------------------------------------------------------

SimDesign_Table <- as.data.frame(cbind(
    Variable = c(
        # "Sample Size ",
        # "Cluster size", 
        # "Number of clusters", 
        "Number & size of clusters",
        
        "Effect ",
        "Covariate terms ",
        "Cluster-level adjustment ", 
        "Mediator type ", 
        "Outcome type ", 
        "Estimation approach"
    ),
    Levels = c(#"J = 40, 70, 100 with N_j = U[5, 20] vs J = 10, 20, 40 with N_j = U[50, 100]", 
        # "Uniform[5, 20]", 
        # "70, 100", 
        "J = 40, 70, 100 with N_j ∼ U[5, 20]; J = 10, 20, 40 with N_j ∼ U[50, 100]", 
        
        "Null, Non-null", "Linear (X), Non-linear (X²)", "Cluster means + dummies, cluster means only", "Binomial, Gaussian", "Binomial, Gaussian", "Parametric (GLM), Nonparametric Machine Learning (Super Learner with GAM & neural networks)")
)) |>
    knitr::kable(caption = "Table 1. Manipulated Variables in Simulation.",
                 align = "lc", "html") |>
    kableExtra::kable_styling(
        bootstrap_options = c("striped", "condensed"), 
        stripe_color = "#F2F2F2", 
        full_width = FALSE, 
        font_size = 14 # Increase font size for posters
    ) |> 
    # kableExtra::column_spec(1, width_min = "2em") |>
    # kableExtra::column_spec(1, width_min = "4em", font_size = 14) |> 
    # kableExtra::column_spec(2, width_min = "12em", font_size = 14) |> 
    kableExtra::footnote(general = "GLM = Generalized Linear Model; GAM = Generalized Additive Model; J = number of clusters; N_j = cluster size.") |> 
    kableExtra::kable_classic()

SimDesign_Table

kableExtra::save_kable(SimDesign_Table, 
                       file = paste0(results_path, "/Tables/Simulation-Desgin-Table.png"), 
                       zoom = 2
)







# Data Generation DAG -----------------------------------------------------

dag1 <- dagify(
    U ~ 1,
    x1 ~ 1, x2 ~ 1, x3 ~ 1,
    # x4 ~ 1, x5 ~ 1, x6 ~ 1,
    A ~ 1,
    M ~ 1,
    Y ~ 1,
    exposure = "A",
    outcome = "Y",
    latent = "U",
    coords = list(x = c(A = 1.5, M = 3.5, Y = 5.5, U = 1,
                        x1 = 2.75, x2 = 3.5, x3 = 4.25),
                  # x4 = 2.75, x5 = 3.5, x6 = 4.25),
                  y = c(A = 2, M = 3.5, Y = 2, U = 6.5,
                        x1 = 5, x2 = 5, x3 = 5)) #,
    # x4 = 0, x5 = 0, x6 = 0))
)

# Create DAG viz 
p1 <- ggdag_classic(dag1, size = 4) +
    
    # U section # 
    # U circle 
    xlim(0, 7) +
    ylim(1, 7) +
    # 
    ggplot2::coord_fixed() +
    ggforce::stat_circle(aes(x0 = 1, y0 = 6.5, r = 0.3)) +
    # U line to A 
    geom_curve(x = 0.65, y = 6.3, 
               xend = 1.25, yend = 2.3, 
               linetype = "dashed", 
               arrow = arrow(length = unit(0.2, "cm"), 
                             ends = "last", 
                             type = "closed"), 
               curvature = 0.4) +
    # U line to M
    geom_curve(x = 1, y = 6.1, 
               xend = 3.2, yend = 3.5, 
               linetype = "dashed", 
               arrow = arrow(length = unit(0.2, "cm"), 
                             ends = "last", 
                             type = "closed"), 
               curvature = 0.4) +
    # U line to Y
    geom_curve(x = 1.3, y = 6.75, 
               xend = 5.65, yend = 2.3, 
               linetype = "dashed", 
               arrow = arrow(length = unit(0.2, "cm"), 
                             ends = "last", 
                             type = "closed"), 
               curvature = -0.65) +
    
    
    # AMY section # 
    # A box 
    geom_rect(aes(xmin = 1.25, ymin = 1.75, 
                  xmax = 1.75, ymax = 2.25), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    # M box 
    geom_rect(aes(xmin = 3.25, ymin = 3.25, 
                  xmax = 3.75, ymax = 3.75), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    # Y box 
    geom_rect(aes(xmin = 5.25, ymin = 1.75, 
                  xmax = 5.75, ymax = 2.25), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) + 
    # AM path 
    geom_segment(aes(x = 1.8, y = 2.1, 
                     xend = 3.2, yend = 3.3), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    # MY path 
    geom_segment(aes(x = 3.8, y = 3.3, 
                     xend = 5.2, yend = 2.1), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    # AY path 
    geom_segment(aes(x = 1.8, y = 2, 
                     xend = 5.2, yend = 2), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    
    
    # x1-x3 section # 
    # x1 box 
    geom_rect(aes(xmin = 2.5, ymin = 5.25, 
                  xmax = 3, ymax = 4.75), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    # x2 box 
    geom_rect(aes(xmin = 3.25, ymin = 5.25, 
                  xmax = 3.75, ymax = 4.75), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    # x3 box 
    geom_rect(aes(xmin = 4, ymin = 5.25, 
                  xmax = 4.5, ymax = 4.75), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    
    # x1-x3 box 
    geom_rect(aes(xmin = 2.25, ymin = 4.6, 
                  xmax = 4.75, ymax = 5.4), 
              fill = "transparent", color = "black", linewidth = 0.4, size = 1) +
    # x1-x3 line to A 
    geom_segment(aes(x = 3.4, y = 4.55, 
                     xend = 1.5, yend = 2.3), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    # x1-x3 line to M 
    geom_segment(aes(x = 3.5, y = 4.55, 
                     xend = 3.5, yend = 3.8), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    # x1-x3 line to Y 
    geom_segment(aes(x = 3.6, y = 4.55, 
                     xend = 5.5, yend = 2.3), 
                 linewidth = 0.4,
                 linetype = "solid", 
                 color = "black", 
                 arrow = arrow(length = unit(0.2, "cm"), 
                               ends = "last", 
                               type = "closed")) +
    # Apply DAG theme
    theme_dag() 

p1 #+
    # labs(title = "Figure 1. Data Generation DAG") +
    # theme(title = element_text(size = 9, hjust = 20))

# gglayer_theme +
# theme_dag()
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_Data-Generation-DAG.png"),
       plot = last_plot(), 
       width = 6, 
       height = 4, 
       dpi = 300)

