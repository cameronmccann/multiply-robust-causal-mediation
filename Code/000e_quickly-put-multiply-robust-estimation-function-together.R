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
# Last Updated: 2025-01-10
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

data_list$overlap$ps_summary


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
)



# with M = binomial & Y = gaussian 
# Effect Individual.Average_Estimate Individual.Average_StdError CI.lower_Individual.Average
# 1   Direct Effect (DE)                    7.081709                   0.1377726                    6.805528
# 2 Indirect Effect (IE)                    7.282790                   0.4544098                    6.371873
# CI.upper_Individual.Average Cluster.Average_Estimate Cluster.Average_StdError CI.lower_Cluster.Average
# 1                    7.357890                 7.079874               0.04845729                 6.982736
# 2                    8.193706                 7.314564               0.42858125                 6.455424
# CI.upper_Cluster.Average
# 1                 7.177012
# 2                 8.173705
# Warning messages:
#     1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 4: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#                                                                 prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases

# Check with true values 
# Likely these are the true corresponding estimates 
data_list$effects$individual$tnde
data_list$effects$individual$pnie
## other effects 
data_list$effects$individual$pnde
data_list$effects$individual$tnie




# ═══════════════════
#    Checking estimate_mediation() again  
# ═══════════════════
# This time I will generate different data (changing relationship sizes) & check the estimates 


# Generate data 
data_list <- generate_data2.0c(
    J =  100,
    njrange = c(5, 20), #c(50, 100),
    Mfamily = "binomial",
    Yfamily = "binomial", # "gaussian", 
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

data_list$overlap$ps_summary

results1 <- estimate_mediation(
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
    cluster_opt = "FE.glm", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)

results1


# with M = binomial & Y = binomial 

## "FE.glm"
# Warning messages:
#     1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 2: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#                                                                 prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
#                                                                 3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde
results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie
# [1] -0.01082346
# [1] 0.008386287

## "RE.glm"
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# Warning messages:
#     1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                         unable to evaluate scaled gradient
#                     2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                         Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#                                     3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                         unable to evaluate scaled gradient
#                                                     4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                                         Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#                                                                     5: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                                                         Model is nearly unidentifiable: large eigenvalue ratio
#                                                                                     - Rescale variables?
results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde
results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie
# [1] -0.03079653
# [1] 0.02778017

## "noncluster.glm"
# Warning message:
#     In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#                                                                  prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde
results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie
# [1] -0.03630748
# [1] 0.03170313






# Investigating warning messages ------------------------------------------

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


# using the following values based on the possible data generation values script for M binomial & Y binomial 
## this is what ill use for now 
#   m_on_a m_on_anj m_on_az y_on_a y_on_m y_on_am y_on_az y_on_mz y_on_anj       tnde      pnie       pnde       tnie
# 3      2      0.5     0.2      1      2       3     0.2     0.2        1 0.02728740 0.1123586 0.09483227 0.04481371


# Test 1 ------------------------------------------------------------------

# Here I tested both Dr Liu's package & the new estimation for both FE.glm & cwc with c("SL.glm", "SL.nnet"). 
# I focus on M = binomial & Y = binomial.
# For FE.glm I recieved an warning about fit probabilities being numerically 0 or 1 & the relative bias was large (did not look at package due to small error)
# For cwc with c("SL.glm", "SL.nnet") the package & new function did not differ nor had warnings. However, the relative bias was large. 


# Generate data 
data_list <- generate_data2.0c(
    J =  100,
    njrange = c(5, 20), #c(50, 100),
    Mfamily = "binomial", # "gaussian", 
    Yfamily = "binomial", # "gaussian", 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 2, #2.5, #3, #5, #10, #15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 1, 
    y_on_m = 2, #2.5, #3, #5, #10, #15,
    y_on_am = 2, #3, #5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 1, #5,
    int.XZ = FALSE 
)

data_list$overlap$ps_summary
# [1] "Number of PSs < 0.01: 7 (0.54%); Number of PSs > 0.99: 18 (1.39%)"
data_list$effects$individual$tnde
data_list$effects$individual$pnie
# [1] 0.02728499
# [1] 0.1123586
data_list$effects$individual$pnde
data_list$effects$individual$tnie
# [1] 0.09483189
# [1] 0.04481169





# ══════════════════════════════
#    FE.glm
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
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
    cluster_opt = "FE.glm", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)

results1

## FE.glm 
# Warning message:
#     glm.fit: fitted probabilities numerically 0 or 1 occurred 
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 0.8441782
# [1] 0.3148084



# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
devtools::load_all("Functions/MediatorCL")

Sname = "school"
results2 <- MediatorCL::MediatorCL(data = data_list$data, 
                                   Sname = "school",
                                   Wnames = NULL,
                                   Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm"),
                                   learners_m = c("SL.glm"),
                                   learners_y = c("SL.glm"),
                                   cluster_opt = "FE.glm",
                                   num_folds = 1)

results2

## FE.glm 

abs(results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie

# skipping because of following error: 
# Error in make_folds(cluster_ids = data[[Sname]], fold_fun = folds_vfold,  : 
#                         could not find function "make_folds"




# ══════════════════════════════
#    cwc with SL.glm & SL.nnet 
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 4
)

results1

## SL.glm & SL.nnet with cwc 4 fold
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 1.65641
# [1] 0.5394522


# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
devtools::load_all("Functions/MediatorCL")

Sname = "school"
results2 <- MediatorCL::MediatorCL(data = data_list$data, 
                                   Sname = "school",
                                   Wnames = NULL,
                                   Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                   num_folds = 4)

results2

# SL.glm & SL.nnet with cwc 4 fold
abs(results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 1.65641
# [1] 0.5394522




# Test 2 ------------------------------------------------------------------

# Here I simply changed J & njrange. However, nothing differed from prior: large relative error & only warning with FE.glm

# Generate data 
data_list <- generate_data2.0c(
    J =  40,
    njrange = c(50, 100),
    Mfamily = "binomial", # "gaussian", 
    Yfamily = "binomial", # "gaussian", 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 2, #2.5, #3, #5, #10, #15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 1, 
    y_on_m = 2, #2.5, #3, #5, #10, #15,
    y_on_am = 2, #3, #5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 1, #5,
    int.XZ = FALSE 
)

data_list$overlap$ps_summary
# [1] "Number of PSs < 0.01: 0 (0%); Number of PSs > 0.99: 15 (0.48%)"
data_list$effects$individual$tnde
data_list$effects$individual$pnie
# [1] 0.01639596
# [1] 0.1046274
data_list$effects$individual$pnde
data_list$effects$individual$tnie
# [1] 0.08960243
# [1] 0.03142095




# ══════════════════════════════
#    FE.glm
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
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
    cluster_opt = "FE.glm", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)

results1

## FE.glm 
# Warning message:
#     glm.fit: fitted probabilities numerically 0 or 1 occurred 
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 5.703511
# [1] 0.834795


# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
# devtools::load_all("Functions/MediatorCL")
# 
# Sname = "school"
# results2 <- MediatorCL::MediatorCL(data = data_list$data, 
#                                    Sname = "school",
#                                    Wnames = NULL,
#                                    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
#                                    Aname = "A",
#                                    Mnames = "M",
#                                    Yname = "Y",
#                                    learners_a = c("SL.glm"),
#                                    learners_m = c("SL.glm"),
#                                    learners_y = c("SL.glm"),
#                                    cluster_opt = "FE.glm",
#                                    num_folds = 1)
# 
# results2
# 
# # FE.glm 
# (results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
# (results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie


# ══════════════════════════════
#    cwc with SL.glm & SL.nnet 
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 4
)

results1

## SL.glm & SL.nnet with cwc 4 fold
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 4.923132
# [1] 0.8155262


# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
devtools::load_all("Functions/MediatorCL")

Sname = "school"
results2 <- MediatorCL::MediatorCL(data = data_list$data, 
                                   Sname = "school",
                                   Wnames = NULL,
                                   Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                   num_folds = 4)

results2

# SL.glm & SL.nnet with cwc 4 fold
abs(results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 4.923132
# [1] 0.8155262



# Test 3 ------------------------------------------------------------------

# Here changing binomail M & Y to gaussian 
## I am speculating that the true value calculation may be off 
# Biases still seem large; no warning messages; package & new func did not differ

# Generate data 
data_list <- generate_data2.0c(
    J =  40,
    njrange = c(50, 100),
    Mfamily = "gaussian", # "binomial", #
    Yfamily = "gaussian", # "binomial", # 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 2, #2.5, #3, #5, #10, #15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 1, 
    y_on_m = 2, #2.5, #3, #5, #10, #15,
    y_on_am = 2, #3, #5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 1, #5,
    int.XZ = FALSE 
)

data_list$overlap$ps_summary
# [1] "Number of PSs < 0.01: 0 (0%); Number of PSs > 0.99: 15 (0.48%)"
data_list$effects$individual$tnde
data_list$effects$individual$pnie
# [1] 5.081349
# [1] 4.025618
data_list$effects$individual$pnde
data_list$effects$individual$tnie
# [1] 1.079575
# [1] 8.027392



# ══════════════════════════════
#    FE.glm
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
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
    cluster_opt = "FE.glm", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)

results1

## FE.glm 
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 0.807813
# [1] 0.9732278


# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
# devtools::load_all("Functions/MediatorCL")
# 
# Sname = "school"
# results2 <- MediatorCL::MediatorCL(data = data_list$data,
#                                    Sname = "school",
#                                    Wnames = NULL,
#                                    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
#                                    Aname = "A",
#                                    Mnames = "M",
#                                    Yname = "Y",
#                                    learners_a = c("SL.glm"),
#                                    learners_m = c("SL.glm"),
#                                    learners_y = c("SL.glm"),
#                                    cluster_opt = "FE.glm",
#                                    num_folds = 1)
# 
# results2
# 
# # FE.glm
# (results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
# (results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie



# ══════════════════════════════
#    cwc with SL.glm & SL.nnet 
# ══════════════════════════════

# estimate with new function/package 
results1 <- estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 4
)

results1

## SL.glm & SL.nnet with cwc 4 fold
abs(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 0.8052369
# [1] 0.971213


# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
devtools::load_all("Functions/MediatorCL")

Sname = "school"
results2 <- MediatorCL::MediatorCL(data = data_list$data, 
                                   Sname = "school",
                                   Wnames = NULL,
                                   Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                   num_folds = 4)

results2

# SL.glm & SL.nnet with cwc 4 fold
abs(results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
abs(results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 0.8052369
# [1] 0.971213










################################### END ##############################################


# look into warnings ------------------------------------------------------

## heatmap -----------------------------------------------------------------

# Heatmap of correlations between variables
library(ggplot2)
library(reshape2)
corr_matrix <- cor(data_list$data[, -c(1, 2)], use = "complete.obs")  # Exclude ID and school
melted_corr <- reshape2::melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
    # geom_tile() +
    geom_tile(color = "white") +
    
    # scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    viridis::scale_fill_viridis(option = "C") +
    scale_color_viridis_b() +
    labs(title = "Correlation Heatmap",
         fill = "Correlation")

# looks like A & M are strongly correlated
cor.test(data_list$data$A, data_list$data$M)


## VIF (not sure if its okay for clustered data) ---------------------------

library(car)
fit <- lm(Y ~ Z + A + M + X1 + X2 + X3, data = data_list$data)
vif(fit)


# library(lme4)
# model <- lmer(Y ~ Z + A + M + (1 | school), data = data_list$data)
# summary(model)




# check different data generation values ----------------------------------

# using the following values based on the possible data generation values script for M binomial & Y binomial 
## this is what ill use for now 
#   m_on_a m_on_anj m_on_az y_on_a y_on_m y_on_am y_on_az y_on_mz y_on_anj       tnde      pnie       pnde       tnie
# 3      2      0.5     0.2      1      2       3     0.2     0.2        1 0.02728740 0.1123586 0.09483227 0.04481371


# Generate data 
data_list <- generate_data2.0c(
    J =  100,
    njrange = c(5, 20), #c(50, 100),
    Mfamily = "binomial", # "gaussian", 
    Yfamily = "binomial", # "gaussian", 
    seed = 8675309,
    num_x = 3,
    # include_overlapMsg = FALSE,
    
    m_on_a = 2, #2.5, #3, #5, #10, #15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    y_on_a = 1, 
    y_on_m = 2, #2.5, #3, #5, #10, #15,
    y_on_am = 2, #3, #5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 1, #5,
    int.XZ = FALSE 
)

data_list$overlap$ps_summary
data_list$effects$individual$tnde
data_list$effects$individual$pnie

data_list$effects$individual$pnde
data_list$effects$individual$tnie

# Use regex to capture the percentages within parentheses
data.frame(pctPSbelow01 = as.numeric(str_match_all(data_list$overlap$ps_summary, "\\(([^)]+)%\\)")[[1]][, 2][1]), 
           pctPSabove99 = as.numeric(str_match_all(data_list$overlap$ps_summary, "\\(([^)]+)%\\)")[[1]][, 2][2]))




### heatmap -----------------------------------------------------------------

corr_matrix <- cor(data_list$data[, -c(1, 2)], use = "complete.obs")  # Exclude ID and school
melted_corr <- reshape2::melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
    # geom_tile() +
    geom_tile(color = "white") +
    
    # scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    viridis::scale_fill_viridis(option = "C") +
    scale_color_viridis_b() +
    labs(title = "Correlation Heatmap",
         fill = "Correlation")

# looks like A & M are strongly correlated
cor.test(data_list$data$A, data_list$data$M)



### test FE glm -------------------------------------------------------------

results1 <- estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = NULL,
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
    cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 4
)

results1


# with M = binomial & Y = binomial 

## "FE.glm"
# Warning message:
#     glm.fit: fitted probabilities numerically 0 or 1 occurred 
(results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
(results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 0.8441782
# [1] -0.3148084

# SL.glm & SL.nnet with cwc 4 fold
# [1] 1.65641
# [1] -0.5394522

## Kind of large bias

# ═══════════════════
#    use Dr Liu package to be safe  
# ═══════════════════
devtools::load_all("Functions/MediatorCL")

Sname = "school"
results2 <- MediatorCL::MediatorCL(data = data_list$data, 
                                   Sname = "school",
                                   Wnames = NULL,
                                   Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_m = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   learners_y = c("SL.glm", "SL.nnet"), # c("SL.glm"),
                                   cluster_opt = "cwc", # "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                   num_folds = 4)

results2

# with M = binomial & Y = binomial 

# SL.glm & SL.nnet with cwc 4 fold
# [1] 1.65641
# [1] -0.5394522
(results2[results2$Effect == "DE", "Individual.Average_Estimate"] - data_list$effects$individual$tnde) / data_list$effects$individual$tnde
(results2[results2$Effect == "IE", "Individual.Average_Estimate"] - data_list$effects$individual$pnie) / data_list$effects$individual$pnie
# [1] 1.65641
# [1] -0.5394522



