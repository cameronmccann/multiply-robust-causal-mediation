################################################################################
##################### Building Doubly Robust Est. Functions ####################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 2025-01-07
#
#
# Script Description: 
#   This script is used to quickly build/put together the functions for doubly/multiply robust 
#   estimation & test it before using the functions in simulation. This script is similar 
#   to 000d_building-double-robust-functions.R but is less meant to make adjustments and learn 
#   and more for getting usable functions quickly. 
# 
# 
# Last Updated: 2025-01-08
#
#
# Notes:
#   To-Do:
#       + 
#       + 
#
#   Done: 
#
################################################################################






## Generate data -----------------------------------------------------------

# ══════════════════════════════
#     Load Packages
# ══════════════════════════════
if (!require("pacman"))
    install.packages("pacman")
pacman::p_load(
    doParallel,
    foreach,
    parallel,
    purrr,
    glue,
    dplyr,
    readr,
    ggplot2,
    fastDummies,
    stringr,
    tibble
)

# ══════════════════════════════
#     Load data generation functions 
# ══════════════════════════════
function_names <- c(
    # "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    # "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# ══════════════════════════════
#    generate data 
# ══════════════════════════════
# 
# # Load Packages 
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#     # Packages 
#     # doParallel, 
#     # foreach,
#     # parallel, 
#     purrr, # for map()
#     glue, # for glue()
#     dplyr, 
#     readr 
#     # ggplot2
# )
# 
# # Load Data gen functions 
# # Define the vector of function names
# function_names <- c(
#     # "generate_data2.0", 
#     "generate_clusters", 
#     "generate_confounders", 
#     "generate_treatment", 
#     "generate_mediator", 
#     "generate_outcome", 
#     "pm1",
#     "my",
#     # "trueVals2.0", 
#     
#     # As of 2025-01-02, these are the most up-to-date functions 
#     "generate_data2.0c", 
#     "trueVals2.0c"
# )
# 
# # Loop through the function names and source each file
# for (func in function_names) {
#     source(file.path("Functions", paste0(func, ".R")))
# }

# Generate data 
data_list <- generate_data2.0c(
    J = 100,
    njrange = c(50, 100),
    Mfamily = "binomial",
    Yfamily = "gaussian", # "binomial",
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 2,
    y_on_m = 15,
    y_on_am = 5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 5,
    int.XZ = FALSE 
)




# Testing internal_estimate_mediation() & estimate_mediation() ------------

# ══════════════════════════════
#     Load estimation functions 
# ══════════════════════════════
function_names <- c(
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




# ══════════════════════════════
#    Checking internal_estimate_mediation() 
# ══════════════════════════════

# Sname = "school" # Corrected error with passing Sname on to other functions like v.ac()

internal_estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    Yfamily = "binomial",
    cluster_opt_a = "cwc.FE",
    cluster_opt_m = "cwc.FE",
    cluster_opt_y = "cwc.FE",
    cluster_opt_v = "cwc",
    interaction_fity = c("AM"),
    num_folds = 1,
    learners_a = c("SL.glm"),
    learners_m = c("SL.glm"),
    learners_y = c("SL.glm"),
    contrast_a = c(a = 1, astar = 0)
)




# ══════════════════════════════
#    Checking estimate_mediation() 
# ══════════════════════════════

estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.glm"),
    learners_m = c("SL.glm"),
    learners_y = c("SL.glm"),
    cluster_opt = "cwc.FE",
    num_folds = 1
    # Yfamily = "gaussian" # "binomial"
)



data_list$effects$individual

















