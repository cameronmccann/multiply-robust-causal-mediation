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
# Last Updated: 2025-01-09
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
    cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
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











# Effect Individual.Average_Estimate Individual.Average_StdError CI.lower_Individual.Average
# 1   Direct Effect (DE)                  -0.0394788                 0.004830252                 -0.04916159
# 2 Indirect Effect (IE)                   0.1932492                 0.022367744                  0.14841045
# CI.upper_Individual.Average Cluster.Average_Estimate Cluster.Average_StdError CI.lower_Cluster.Average
# 1                  -0.0297960              -0.03998655              0.004913223              -0.04983567
# 2                   0.2380879               0.19185554              0.020904177               0.14995072
# CI.upper_Cluster.Average
# 1              -0.03013743
# 2               0.23376036
# There were 32 warnings (use warnings() to see them)

# Check with true values 
# Likely these are the true corresponding estimates 
data_list$effects$individual$tnde
data_list$effects$individual$pnie
## other effects 
data_list$effects$individual$pnde
data_list$effects$individual$tnie


# data_list$effects$individual



# learners_a = c("SL.glm"),
# learners_m = c("SL.glm"),
# learners_y = c("SL.glm"),
# cluster_opt = "cwc.FE",
# num_folds = 1
results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$tnde
results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Individual-Avg", ]$Estimate - data_list$effects$individual$pnie

results1[results1$Effect == "Direct Effect (DE)" & results1$EffectVersion == "Cluster-Avg", ]$Estimate - data_list$effects$cluster$tnde
results1[results1$Effect == "Indirect Effect (IE)" & results1$EffectVersion == "Cluster-Avg", ]$Estimate - data_list$effects$cluster$pnie















# Generate data 
sim_data <- generate_data2.0c(
    J = 100,
    njrange = c(50, 100),
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
    cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)





# # Initialize variables to store warnings and errors
warnings_list <- character(0)
estimation_result <- list(estimate = NULL, error = NULL, warning = NULL)

tryCatch({
    # Use `withCallingHandlers` to capture warnings while still running the code
    withCallingHandlers({
        # Place your estimation function or code here
        # estimate <- estimation_function(sim_data)  # Replace with actual function
        estimate <- estimate_mediation(data = sim_data$data, 
                                       Sname = "school", 
                                       Wnames = NULL,
                                       Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
                                       Aname = "A",
                                       Mnames = "M",
                                       Yname = "Y",
                                       learners_a = c("SL.glm"),
                                       learners_m = c("SL.glm"),
                                       learners_y = c("SL.glm"),
                                       cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                       num_folds = 1)
        
        # If estimation succeeds, store the estimate
        estimation_result$estimate <- estimate
    }, warning = function(w) {
        # Append warning message to the warnings list
        warnings_list <- c(warnings_list, conditionMessage(w))
        invokeRestart("muffleWarning")  # Prevent warnings from stopping execution
    })
}, error = function(e) {
    # Handle errors
    estimation_result$error <- conditionMessage(e)
})

# Store warnings in the final result list
if (length(warnings_list) > 0) {
    estimation_result$warning <- paste(warnings_list, collapse = "; ")
}





# trying to test catching warnings ----------------------------------------

result <- list()

output <- withCallingHandlers(
    {
        estimate <- estimate_mediation(data = sim_data$data, 
                                       Sname = "school", 
                                       Wnames = NULL,
                                       Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
                                       Aname = "A",
                                       Mnames = "M",
                                       Yname = "Y",
                                       learners_a = c("SL.glm"),
                                       learners_m = c("SL.glm"),
                                       learners_y = c("SL.glm"),
                                       cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                       num_folds = 1)
        list(estimates = estimate)
    }, 
    warning = function(w) {
        if (is.null(result$warnings)) result$warnings <- list()
        result$warnings <- c(result$warnings, list(w$message))
        invokeRestart("muffleWarning")
    }
)

result$estimates <- output$estimates



warnings <- tryCatch({
    estimate <- estimate_mediation()
    result$estimates <- estimate 
})


warnings_list <- character(0)
estimation_result <- list(estimate = NULL, error = NULL, warning = NULL)

tryCatch(
    estimate <- estimate_mediation(data = sim_data$data, 
                                   Sname = "school", 
                                   Wnames = NULL,
                                   Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
                                   Aname = "A",
                                   Mnames = "M",
                                   Yname = "Y",
                                   learners_a = c("SL.glm"),
                                   learners_m = c("SL.glm"),
                                   learners_y = c("SL.glm"),
                                   cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                   num_folds = 1), 
    error = function(e){
        # messge
        e
    }, 
    warning = function(w){
        warnings_list <- w
    }
)

w
warnings_list


# # Initialize variables to store warnings and errors
warnings_list <- character(0)
estimation_result <- list(estimate = NULL, error = NULL, warning = NULL)

tryCatch({
    # Use `withCallingHandlers` to capture warnings while still running the code
    withCallingHandlers({
        # Place your estimation function or code here
        # estimate <- estimation_function(sim_data)  # Replace with actual function
        estimate <- estimate_mediation(data = sim_data$data, 
                                       Sname = "school", 
                                       Wnames = NULL,
                                       Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
                                       Aname = "A",
                                       Mnames = "M",
                                       Yname = "Y",
                                       learners_a = c("SL.glm"),
                                       learners_m = c("SL.glm"),
                                       learners_y = c("SL.glm"),
                                       cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
                                       num_folds = 1)
        
        # If estimation succeeds, store the estimate
        estimation_result$estimate <- estimate
    }, warning = function(w) {
        # Append warning message to the warnings list
        warnings_list <- c(warnings_list, conditionMessage(w))
        invokeRestart("muffleWarning")  # Prevent warnings from stopping execution
    })
}, error = function(e) {
    # Handle errors
    estimation_result$error <- conditionMessage(e)
})

# Store warnings in the final result list
if (length(warnings_list) > 0) {
    estimation_result$warning <- paste(warnings_list, collapse = "; ")
}



# in the code that follows, how can i properly collect warning messages from estimate_mediation() and store it in the list. if there is a warning i would like to have the warning message and estimates if both occur. if an error occurs i would like to save the error message. 
# ══════════════════════════════
#     Main Loop Over Conditions
# ══════════════════════════════
total_conditions <- nrow(conditions)
for (cond_idx in seq_len(total_conditions)) {
    
    cond <- conditions[cond_idx, ]
    isQuad <- cond[["quadratic"]]
    Mfamily <- cond[["Mfamily"]]
    Yfamily <- cond[["Yfamily"]]
    Nj_low <- cond[["Nj_low"]]
    Nj_high <- cond[["Nj_high"]]
    Jval <- cond[["J"]]
    
    cond_label <- glue(
        "quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}"
    )
    
    # --------------------------------
    #     Start Timing for Condition
    # --------------------------------
    start_time_cond <- Sys.time()
    seeds <- sample.int(1e7, reps)
    
    result_list <- foreach(
        rep_idx = seq_len(reps),
        .packages = c("dplyr", "ggplot2", "glue", "purrr"), 
        # .export = c("cond_idx")
        .export = c("get_inference") #, "internal_estimate_mediation")  # Explicitly include your custom functions
    ) %dopar% {
        start_time_iter <- Sys.time()
        set.seed(seeds[rep_idx])
        
        # Generate data
        sim_data <- generate_data2.0c(
            J = Jval, 
            njrange = c(Nj_low, Nj_high), 
            Mfamily = Mfamily,
            Yfamily = Yfamily,
            seed = seeds[rep_idx],
            quadratic.A = isQuad,
            quadratic.M = isQuad,
            quadratic.Y = isQuad,
            num_x = 3,
            include_overlapMsg = FALSE,
            
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
        
        # Save population data if applicable
        if (!is.null(sim_data$truevals$pop_data) && rep_idx == 1) { # drop && rep_idx == 1 to save all pop data
            # Save only for the first replication
            # pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx}-rep-{rep_idx}.rds"))
            # saveRDS(sim_data$truevals$pop_data, pop_data_file)
            
            # Zero-padded condition number
            cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
            pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx_padded}-rep-{rep_idx}.rds"))
            saveRDS(sim_data$truevals$pop_data, pop_data_file)
        }
        
        
        
        ########################################################################
        # INSERT ESTIMATION CODE HERE & ADJUST OUTPUT DATAFRAME (A FEW LINES BELOW)
        # 
        # Dumb run 
        estimates <- estimate_mediation(
            data = sim_data$data,
            Sname = "school",
            Wnames = NULL,
            Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
            Aname = "A",
            Mnames = "M",
            Yname = "Y",
            learners_a = c("SL.glm"),
            learners_m = c("SL.glm"),
            learners_y = c("SL.glm"),
            cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
            num_folds = 1
        )
        ## add info to list 
        
        
        # # Initialize variables to store warnings and errors
        # warnings_list <- character(0)
        # estimation_result <- list(estimate = NULL, error = NULL, warning = NULL)
        # 
        # tryCatch({
        #     # Use withCallingHandlers to capture warnings while still running the code
        #     withCallingHandlers({
        #         # Place your estimation function or code here
        #         # estimate <- estimation_function(sim_data)  # Replace with actual function
        #         estimate <- estimate_mediation(data = sim_data$data, 
        #                                        Sname = "school", 
        #                                        Wnames = NULL,
        #                                        Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
        #                                        Aname = "A",
        #                                        Mnames = "M",
        #                                        Yname = "Y",
        #                                        learners_a = c("SL.glm"),
        #                                        learners_m = c("SL.glm"),
        #                                        learners_y = c("SL.glm"),
        #                                        cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
        #                                        num_folds = 1)
        #         
        #         # If estimation succeeds, store the estimate
        #         estimation_result$estimate <- estimate
        #     }, warning = function(w) {
        #         # Append warning message to the warnings list
        #         warnings_list <- c(warnings_list, conditionMessage(w))
        #         invokeRestart("muffleWarning")  # Prevent warnings from stopping execution
        #     })
        # }, error = function(e) {
        #     # Handle errors
        #     estimation_result$error <- conditionMessage(e)
        # })
        # 
        # # Store warnings in the final result list
        # if (length(warnings_list) > 0) {
        #     estimation_result$warning <- paste(warnings_list, collapse = "; ")
        # }
        # 
        ########################################################################
        
        end_time_iter <- Sys.time()
        iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
        
        ########################################################################
        # Prep data for export (update code that follows this)
        ## Maybe just make one list per condition that has an element for each iteration
        ## we will have this in the list for each iteration:
        #   - iteration = rep_idx,
        #   - seed = seeds[rep_idx],
        #   - cond_idx # or cond
        #   - condition_details = as.character(cond_label)
        #   - iter_time_sec = round(iter_duration, 4),
        #   - data_list$truevals
        #   - data_list$effects
        #   - data_list$overlap
        #       # data_list$overlap$ps_summary
        #       # data_list$overlap$iptw_summary
        #   - data_list$parameters
        #       # data_list$parameters$J
        #       # data_list$parameters$njrange
        #       # data_list$parameters$nj_sizes
        # Prep data for export (update code that follows this)
        iteration_data <- list(
            iteration = rep_idx,
            seed = seeds[rep_idx],
            cond_idx = cond_idx,
            condition_details = as.character(cond_label),
            iter_time_sec = round(iter_duration, 4),
            truevals = list(
                truevals_individual = sim_data$truevals$truevals_individual, 
                truevals_cluster = sim_data$truevals$truevals_cluster
            ), 
            effects = sim_data$effects,
            overlap = list(
                ps_summary = sim_data$overlap$ps_summary, 
                iptw_summary = sim_data$overlap$iptw_summary
            ), 
            parameters = list(
                J = sim_data$parameters$J,
                njrange = sim_data$parameters$njrange,
                nj_sizes = sim_data$parameters$nj_sizes
            ), 
            # 
            estimates = estimates, 
            cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
            num_folds = 1
            
            # estimation_result = estimation_result # Store estimation results including warnings/errors
            
        )
        
        
        #  # THEN ANY ESTIMATIONS
        
        # 
        # 
        ########################################################################
        
        
        # Return the iteration data
        iteration_data
        
        # Commented out previous code:
        # data.frame(
        #     iteration = rep_idx,
        #     seed = seeds[rep_idx],
        #     sim_data$
        #     
        #     
        #     iter_time_sec = round(iter_duration, 4),
        #     stringsAsFactors = FALSE
        # )
        
    }
    
    
    # Extract relevant fields and convert to a data frame
    iteration_summary_df <- map_dfr(result_list, ~{
        tibble(
            iteration = .x$iteration,
            seed = .x$seed,
            cond_idx = .x$cond_idx,
            iter_time_sec = .x$iter_time_sec
        )
    })
    
    # iteration_summary_df <- do.call(rbind, result_list)
    mean_iter_time <- mean(iteration_summary_df$iter_time_sec)
    
    end_time_cond <- Sys.time()
    cond_duration <- as.numeric(difftime(end_time_cond, start_time_cond, units = "secs"))
    cond_time_formatted <- glue("{floor(cond_duration / 60)} min {round(cond_duration %% 60)} s")
    
    OverallPar_time <- rbind(
        OverallPar_time,
        data.frame(
            condition_index = cond_idx,
            condition_details = as.character(cond_label),
            n_reps = reps,
            total_time = cond_time_formatted,
            avg_iter_time = glue("{floor(mean_iter_time / 60)} min {round(mean_iter_time %% 60)} s"),
            stringsAsFactors  = FALSE
        )
    )
    
    # Save iteration summary in main folder
    # Commented out previous code:
    # saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx}.rds")))
    # cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
    # saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
    
    # New code to save the list for each condition
    cond_idx_padded <- sprintf("%02d", cond_idx)
    saveRDS(result_list, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
    
    
    message(glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
        "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
        "Time: {cond_time_formatted}"
    ))
}
















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


# ═══════════════════
#    test wrapper 
# ═══════════════════


catch_warnings <- function(f) {
    warnings_list <- c()  # Local list for warnings
    result <- withCallingHandlers(
        f(),
        warning = function(w) {
            warnings_list <<- c(warnings_list, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(result = result, warnings = warnings_list)
}

# Example usage:
outcome <- catch_warnings(risky_function)
print(outcome$result)
print(outcome$warnings)



catch_warnings <- function(f) {
    warnings_list <- c()  # Local list for warnings
    result <- withCallingHandlers(
        f(),
        warning = function(w) {
            warnings_list <<- c(warnings_list, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(result = result, warnings = warnings_list)
}

# Example usage:
outcome <- catch_warnings(f = estimate_mediation(
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
    cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
))
outcome <- catch_warnings(estimate_mediation(
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
    cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
))
print(outcome$result)
print(outcome$warnings)


# ═══════════════════
#    withCallingHandlers 
# ═══════════════════



result <- withCallingHandlers(
    {
        estimates <- risky_function()  # Function call
        list(result = estimates, warnings = character(0))  # Initialize warnings as an empty character vector
    },
    warning = function(w) {
        message("Warning caught: ", conditionMessage(w))
        # Add the warning message to the existing list without returning NULL for the result
        result$warnings <- c(result$warnings, conditionMessage(w))  # Append the warning to the warnings list
        invokeRestart("muffleWarning")  # Muffle the warning so it doesn't interrupt
    }
)

# Print both the result and warnings
print(result$result)  # Check estimates
print(result$warnings)  # Check captured warnings



result <- withCallingHandlers(
    {
        estimates <- estimate_mediation(
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
            cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
            num_folds = 1
        )  # Function call
        list(result = estimates, warnings = NULL) #character(0))  # Initialize warnings as an empty character vector
    },
    warning = function(w) {
        message("Warning caught: ", conditionMessage(w))
        # Add the warning message to the existing list without returning NULL for the result
        result$warnings <- c(result$warnings, conditionMessage(w))  # Append the warning to the warnings list
        invokeRestart("muffleWarning")  # Muffle the warning so it doesn't interrupt
    }
)

# Print both the result and warnings
print(result$result)  # Check estimates
print(result$warnings)  # Check captured warnings



result <- withCallingHandlers(
    {
        estimates <- estimate_mediation(
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
            cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
            num_folds = 1
        )
        # estimates <- risky_function()  # Function call
        list(result = estimates, warnings = NULL)  # Default warnings to NULL
    },
    warning = function(w) {
        message("Warning caught: ", conditionMessage(w))
        # Return both the result and the warning
        return(list(result = NULL, warnings = conditionMessage(w)))
    }
)

print(result$result)  # Check estimates
print(result$warnings)  # Check captured warnings


###
warnings_list <- list()

result <- withCallingHandlers(
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
        cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
        num_folds = 1
    ),
    # risky_function(),
    warning = function(w) {
        warnings_list <<- c(warnings_list, conditionMessage(w))
        invokeRestart("muffleWarning")  # Prevent the warning from interrupting
    }
)

# Print the result and any captured warnings
print(result)
print(warnings_list)


result <- list(estimates = NULL, warnings = NULL)

# Custom warning handler

result <- tryCatch(
    {
        result$estimates <- estimate_mediation(
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
            cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
            num_folds = 1
        )
        # result
    }, 
    warning = function(w) {
        # result$estimates <- estimate_mediation(
        #     data = data_list$data,
        #     Sname = "school",
        #     Wnames = NULL,
        #     Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
        #     Aname = "A",
        #     Mnames = "M",
        #     Yname = "Y",
        #     learners_a = c("SL.glm"),
        #     learners_m = c("SL.glm"),
        #     learners_y = c("SL.glm"),
        #     cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
        #     num_folds = 1
        # )
        result$warnings <- conditionMessage(w)
        # invokeRestart("muffleWarning")
    }
)

result



result <- withCallingHandlers(
    {
        fit <- glm(formula = formula, data = data, family = family)
        result$estimates <- coef(fit)  # Collect coefficients
        result  # Return result if no issues
    },
    warning = function(w) {
        result$warnings <- conditionMessage(w)  # Collect warning message
        invokeRestart("muffleWarning")  # Suppress warning in output
    },
    error = function(e) {
        result$warnings <- conditionMessage(e)  # Collect error message if any
    }
)

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
    cluster_opt = "noncluster.glm", # "RE.glm", # "FE.glm", # "cwc.FE",
    num_folds = 1
)

results1



