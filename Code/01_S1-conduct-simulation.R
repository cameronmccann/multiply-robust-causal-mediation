################################################################################
############################## Simulation(s) ###################################
################################################################################
############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-01-01
#
#
# Script Description: {Junk file to test project} 
# 
#       Note: data generation creates pop data (when Mfamily & Yfamily meet "gaussian" &. "binomial") for every iteration but only saves first iteration into pop data folder. 
#
#
# Last Updated: 2025-01-09
#
#
# Notes:
#   To-Do: 
#       - {Still need to Finish updating anlaysis functions (helper and overall)} 
#       - test run parametric analysis, then generate nonlinear data & test parameteric analysis 
# 
#       - update functions to handle: 
#           + varing cluster size, using uniform distribution? 
#           + nonlinear relations between z & xs with t, m, & y
#       - first test parametric model before using machine learning models 
#           + we will test 2 (dich./contin. mediator) x 2 (dich./contin. outcome) scenario 
#           + performance criteria: power, coverage, bias, RMSE, etc
#           + test 100 reps, 200, 500, and then 1,000 reps for final 
#
#   Done: 
#
################################################################################



# Set Up (Load packages, functions, &/or data) ----------------------------

# ══════════════════════════════
#    Load packages 
# ══════════════════════════════
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    doParallel, 
    foreach,
    parallel, 
    purrr, # for map()
    glue, # for glue()
    dplyr, 
    readr, 
    ggplot2, 
    fastDummies, # for Dr Liu's package/function (specifically, for dummy_cols() in fastDummies package)
    stringr, # for str_detect() in Dr Liu's package/function
    tibble # for rownames_to_column() in Dr Liu's package/function
)

# ══════════════════════════════
#    Load functions 
# ══════════════════════════════

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# # Load analysis functions 
# # Define the vector of function names
# function_names <- c(
#     "estimate_propensity_score",
#     "estimate_mediator_model",
#     "estimate_outcome_model",
#     "analyze_clustered_mediation"
# )
# 
# # Loop through the function names and source each file
# for (func in function_names) {
#     source(file.path("Functions", paste0(func, ".R")))
# }








# Simulation conditions  --------------------------------------------------

# conditions
conditions <- data.frame(rbind(
    expand.grid(
        J = c(10, 20, 40),
        Nj_low = c(50),
        Nj_high = c(100), 
        quadratic = c(F), #c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian")
        # binary.med = c(T, F), 
        # binary.out = c(T, F)
    ),
    expand.grid(
        J = c(40, 70, 100),
        Nj_low = c(5),
        Nj_high = c(20), 
        quadratic = c(F), #c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian")
        # binary.med = c(T, F), 
        # binary.out = c(T, F)
    )
))

conditions








# Set Parameters ----------------------------------------------------------

## Initialize DF to store results 
OverallPar_time <- NULL

## Set number of replications/repetitions 
reps <- 2 # 1000 

## Create directory to store output 
path <- "Output/S1_Simulation-Output"
if (!dir.exists(path)) {
    dir.create(path)
}







# # Simulation 1 ------------------------------------------------------------
# 
# # ══════════════════════════════
# #     Load Packages
# # ══════════════════════════════
# if (!require("pacman"))
#     install.packages("pacman")
# pacman::p_load(
#     doParallel,
#     foreach,
#     parallel,
#     purrr,
#     glue,
#     dplyr,
#     readr,
#     ggplot2,
#     fastDummies,
#     stringr,
#     tibble
# )
# 
# # ══════════════════════════════
# #     Source Updated Functions
# # ══════════════════════════════
# function_names <- c(
#     # "generate_data", 
#     "generate_clusters", 
#     "generate_confounders", 
#     "generate_treatment", 
#     "generate_mediator", 
#     "generate_outcome", 
#     "pm1", 
#     "my", 
#     # "trueVals", 
#     
#     # As of 01/01/2025 the two funcs below are the updated versions 
#     "generate_data2.0c", 
#     "trueVals2.0c"
# )
# for (func in function_names) {
#     source(file.path("Functions", paste0(func, ".R")))
# }
# 
# 
# # ══════════════════════════════
# #     Simulation Conditions
# # ══════════════════════════════
# conditions <- data.frame(rbind(
#     expand.grid(
#         J = c(10, 20, 40),
#         Nj_low = c(50),
#         Nj_high = c(100), 
#         quadratic = c(F), #c(T, F), 
#         Mfamily = c("binomial", "gaussian"),
#         Yfamily = c("binomial", "gaussian")
#     ),
#     expand.grid(
#         J = c(40, 70, 100),
#         Nj_low = c(5),
#         Nj_high = c(20), 
#         quadratic = c(F), #c(T, F), 
#         Mfamily = c("binomial", "gaussian"),
#         Yfamily = c("binomial", "gaussian")
#     )
# ))
# 
# # ══════════════════════════════
# #     Number of Replications and Output Paths
# # ══════════════════════════════
# # Number of replications
# reps <- 100 #2 # 100 # 200 # 1000
# 
# # Create parent output directory
# path <- "Output/S1_Simulation-Output"
# if (!dir.exists(path)) dir.create(path, recursive = TRUE)
# 
# # Create subfolder for pop_data
# pop_data_folder <- file.path(path, "pop-data")
# if (!dir.exists(pop_data_folder)) dir.create(pop_data_folder)
# 
# # ══════════════════════════════
# #     Set Up Parallel 
# # ══════════════════════════════
# n_cores <- parallel::detectCores(logical = TRUE) - 1
# cl <- parallel::makeCluster(n_cores)
# doParallel::registerDoParallel(cl)
# 
# # ══════════════════════════════
# #     Initialize Timing Table
# # ══════════════════════════════
# OverallPar_time <- data.frame(
#     condition_index = integer(0),
#     condition_details = character(0),
#     n_reps = integer(0),
#     total_time_min = character(0),
#     avg_iter_time_sec = numeric(0),
#     stringsAsFactors = FALSE
# )
# 
# 
# ### main loop ---------------------------------------------------------------
# 
# # ══════════════════════════════
# #     Main Loop Over Conditions
# # ══════════════════════════════
# total_conditions <- nrow(conditions)
# for (cond_idx in seq_len(total_conditions)) {
#     
#     cond <- conditions[cond_idx, ]
#     isQuad <- cond[["quadratic"]]
#     Mfamily <- cond[["Mfamily"]]
#     Yfamily <- cond[["Yfamily"]]
#     Nj_low <- cond[["Nj_low"]]
#     Nj_high <- cond[["Nj_high"]]
#     Jval <- cond[["J"]]
#     
#     cond_label <- glue(
#         "quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}"
#     )
#     
#     # --------------------------------
#     #     Start Timing for Condition
#     # --------------------------------
#     start_time_cond <- Sys.time()
#     seeds <- sample.int(1e7, reps)
#     
#     result_list <- foreach(
#         rep_idx = seq_len(reps),
#         .packages = c("dplyr", "ggplot2", "glue", "purrr"), 
#         .export = c("cond_idx")
#     ) %dopar% {
#         start_time_iter <- Sys.time()
#         set.seed(seeds[rep_idx])
#         
#         # Generate data
#         sim_data <- generate_data2.0c(
#             J = Jval, 
#             njrange = c(Nj_low, Nj_high), 
#             Mfamily = Mfamily,
#             Yfamily = Yfamily,
#             seed = seeds[rep_idx],
#             quadratic.A = isQuad,
#             quadratic.M = isQuad,
#             quadratic.Y = isQuad,
#             num_x = 3,
#             include_overlapMsg = FALSE,
#             
#             m_on_a = 15,
#             m_on_anj = 0.5,
#             m_on_az = 0.2,
#             y_on_a = 2,
#             y_on_m = 15,
#             y_on_am = 5,
#             y_on_az = 0.2,
#             y_on_mz = 0.2,
#             y_on_anj = 5,
#             int.XZ = FALSE 
#         )
#         
#         # Save population data if applicable
#         if (!is.null(sim_data$truevals$pop_data) && rep_idx == 1) { # drop && rep_idx == 1 to save all pop data
#             # Save only for the first replication
#             # pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx}-rep-{rep_idx}.rds"))
#             # saveRDS(sim_data$truevals$pop_data, pop_data_file)
#             
#             # Zero-padded condition number
#             cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
#             pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx_padded}-rep-{rep_idx}.rds"))
#             saveRDS(sim_data$truevals$pop_data, pop_data_file)
#         }
#         
#         
#         
#         ########################################################################
#         # INSERT ESTIMATION CODE HERE & ADJUST OUTPUT DATAFRAME (A FEW LINES BELOW)
#         # 
#         # 
#         # 
#         ########################################################################
#         
#         end_time_iter <- Sys.time()
#         iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
#         
#         ########################################################################
#         # Prep data for export (update code that follows this)
#         ## Maybe just make one list per condition that has an element for each iteration
#         ## we will have this in the list for each iteration:
#         #   - iteration = rep_idx,
#         #   - seed = seeds[rep_idx],
#         #   - cond_idx # or cond
#         #   - condition_details = as.character(cond_label)
#         #   - iter_time_sec = round(iter_duration, 4),
#         #   - data_list$truevals
#         #   - data_list$effects
#         #   - data_list$overlap
#         #       # data_list$overlap$ps_summary
#         #       # data_list$overlap$iptw_summary
#         #   - data_list$parameters
#         #       # data_list$parameters$J
#         #       # data_list$parameters$njrange
#         #       # data_list$parameters$nj_sizes
#         
#         #  # THEN ANY ESTIMATIONS
#         # 
#         # 
#         ########################################################################
#         
#         data.frame(
#             iteration = rep_idx,
#             seed = seeds[rep_idx],
#             sim_data$
#             
#             
#             iter_time_sec = round(iter_duration, 4),
#             stringsAsFactors = FALSE
#         )
#     }
#     
#     iteration_summary_df <- do.call(rbind, result_list)
#     mean_iter_time <- mean(iteration_summary_df$iter_time_sec)
#     
#     end_time_cond <- Sys.time()
#     cond_duration <- as.numeric(difftime(end_time_cond, start_time_cond, units = "secs"))
#     cond_time_formatted <- glue("{floor(cond_duration / 60)} min {round(cond_duration %% 60)} s")
#     
#     OverallPar_time <- rbind(
#         OverallPar_time,
#         data.frame(
#             condition_index = cond_idx,
#             condition_details = as.character(cond_label),
#             n_reps = reps,
#             total_time = cond_time_formatted,
#             avg_iter_time = glue("{floor(mean_iter_time / 60)} min {round(mean_iter_time %% 60)} s"),
#             stringsAsFactors  = FALSE
#         )
#     )
#     
#     # Save iteration summary in main folder
#     # saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx}.rds")))
#     cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
#     saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
#     
#     
#     message(glue(
#         "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
#         "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
#         "Time: {cond_time_formatted}"
#     ))
# }
# 
# # ══════════════════════════════
# #     Save Overall Timing and Total Time
# # ══════════════════════════════
# total_time_sum <- sum(as.numeric(gsub(" min.*", "", OverallPar_time$total_time)) * 60 + as.numeric(sub(".*min ", "", gsub(" s", "", OverallPar_time$total_time_min))))
# message(glue("Total computation time: {floor(total_time_sum / 60)} min {round(total_time_sum %% 60)} s"))
# saveRDS(OverallPar_time, file.path(path, "S1_Computation-Time.rds"))
# 
# # ══════════════════════════════
# #     Shutdown Parallel 
# # ══════════════════════════════
# stopCluster(cl)
# 



















# # Generate Data 
# set.seed(8675309)
# data <- generate_data2.0c(
#     J = test_condition[["J"]], 
#     njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
#     Mfamily = "binomial",
#     Yfamily = "binomial", 
#     seed = 8675309,
#     num_x = 3,
#     
#     m_on_a = 15,
#     m_on_anj = 0.5,
#     m_on_az = 0.2,
#     
#     y_on_a = 2,
#     y_on_m = 15,
#     y_on_am = 5,
#     y_on_az = 0.2,
#     y_on_mz = 0.2,
#     y_on_anj = 5,
#     int.XZ = FALSE
# )

# # Effects
# data.frame(
#     individual = unlist(data$effects$individual),
#     cluster = unlist(data$effects$cluster),
#     row.names = names(data$effects$individual)
# )
# 
# head(conditions)
# 
# 
# 
# # Generate Data 
# test <- generate_data2.0c(
#     J = conditions[1, "J"], 
#     njrange = c(conditions[1, "Nj_low"], conditions[1, "Nj_high"]), 
#     Mfamily = conditions[1, "Mfamily"], 
#     Yfamily = conditions[1, "Yfamily"], 
#     seed = 8675309, # CHANGE VALUE 
#     num_x = 3,
#     quadratic.A = conditions[1, "quadratic"], 
#     quadratic.M = conditions[1, "quadratic"], 
#     quadratic.Y = conditions[1, "quadratic"], 
#     
#     # MAYBE MODIFY VALUES BELOW
#     m_on_a = 15,
#     m_on_anj = 0.5,
#     m_on_az = 0.2,
#     
#     y_on_a = 2,
#     y_on_m = 15,
#     y_on_am = 5,
#     y_on_az = 0.2,
#     y_on_mz = 0.2,
#     y_on_anj = 5,
#     int.XZ = FALSE, 
#     include_overlapMsg = FALSE
# )
# 
# 
# 
# 
# 
# # Save 
# test$effects$individual
# test$effects$cluster
# test$truevals$truevals_individual
# test$truevals$truevals_cluster





# NEW~~~~~~ ---------------------------------------------------------------

# Simulation 1 ------------------------------------------------------------
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
#     Source Updated Functions
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
    "trueVals2.0c", 
    
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
    source(file.path("Functions", paste0(func, ".R")))
}


# ══════════════════════════════
#     Simulation Conditions
# ══════════════════════════════
conditions <- data.frame(rbind(
    expand.grid(
        J = c(10, 20, 40),
        Nj_low = c(50),
        Nj_high = c(100), 
        quadratic = c(F), #c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian")
    ),
    expand.grid(
        J = c(40, 70, 100),
        Nj_low = c(5),
        Nj_high = c(20), 
        quadratic = c(F), #c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian")
    )
))

# limit conditions for testing 
# conditions <- conditions[1:10, ]
## binomial M & Y 
# conditions <- conditions[1:3, ]


# ══════════════════════════════
#     Number of Replications and Output Paths
# ══════════════════════════════
# Number of replications
reps <- 2 # 100 # 200 # 1000

# Create parent output directory
path <- "Output/S1_Simulation-Output"
if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Create subfolder for pop_data
pop_data_folder <- file.path(path, "pop-data")
if (!dir.exists(pop_data_folder)) dir.create(pop_data_folder)

# ══════════════════════════════
#     Set Up Parallel 
# ══════════════════════════════
n_cores <- parallel::detectCores(logical = TRUE) - 1
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# ══════════════════════════════
#     Initialize Timing Table
# ══════════════════════════════
OverallPar_time <- data.frame(
    condition_index = integer(0),
    condition_details = character(0),
    n_reps = integer(0),
    total_time_min = character(0),
    avg_iter_time_sec = numeric(0),
    stringsAsFactors = FALSE
)


### main loop ---------------------------------------------------------------

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
            
            # free up space 
            # rm(sim_data$truevals$pop_data)
            # gc()
        }
        
        
        
        ########################################################################
        # INSERT ESTIMATION CODE HERE 
        clust_opt <- c("noncluster.glm", "FE.glm", "RE.glm") # "cwc", "cwc.FE"
        results <- list()
        
        for (opt in clust_opt) {
            warnings_list <- character(0)  # Reset warnings for each iteration
            
            estimates <- withCallingHandlers(
                {
                    estimate_mediation(
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
                        cluster_opt = opt,  
                        num_folds = 1
                    )
                },
                warning = function(w) {
                    warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
                    invokeRestart("muffleWarning")  # Muffle warning to continue
                }
            )
            
            # Store results and warnings in the list for this iteration
            results[[opt]] <- list(
                estimates = estimates,
                warnings = warnings_list, 
                num_folds = 1           # change later
            )
        }
        
        ########################################################################
        
        end_time_iter <- Sys.time()
        iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
        
        ########################################################################
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
            results = results
        )
        
        ########################################################################
        
        # clear up space
        rm(sim_data, results, estimates, warnings_list)
        
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
    
    # clear space
    rm(result_list)
    gc()
    
    message(glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
        "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
        "Time: {cond_time_formatted}"
    ))
}

# ══════════════════════════════
#     Save Overall Timing and Total Time
# ══════════════════════════════
# total_time_sum <- sum(as.numeric(gsub(" min.*", "", OverallPar_time$total_time)) * 60 + as.numeric(sub(".*min ", "", gsub(" s", "", OverallPar_time$total_time_min))))
# message(glue("Total computation time: {floor(total_time_sum / 60)} min {round(total_time_sum %% 60)} s"))
# saveRDS(OverallPar_time, file.path(path, "S1_Computation-Time.rds"))

total_seconds <- sum(
    OverallPar_time %>%
        mutate(
            minutes = as.numeric(str_extract(total_time, "\\d+(?= min)")),
            seconds = as.numeric(str_extract(total_time, "\\d+(?= s)"))
        ) %>%
        mutate(minutes = ifelse(is.na(minutes), 0, minutes),  # Handle cases with only seconds
               seconds = ifelse(is.na(seconds), 0, seconds)) %>%
        summarise(total_seconds = sum(minutes * 60 + seconds)) %>%
        pull(total_seconds)
)

# Convert total seconds to minutes and seconds
total_minutes <- floor(total_seconds / 60)
remaining_seconds <- total_seconds %% 60

message(glue("Total computation time: {total_minutes} min {remaining_seconds} s"))

# Add total time to Computation time file & save 
OverallPar_time <- rbind(OverallPar_time,
                         c(
                             NA,
                             NA,
                             sum(OverallPar_time$n_reps),
                             glue("{total_minutes} min {remaining_seconds} s"),
                             NA
                         ))
saveRDS(OverallPar_time, file.path(path, "S1_Computation-Time.rds"))

# ══════════════════════════════
#     Shutdown Parallel 
# ══════════════════════════════
stopCluster(cl)


