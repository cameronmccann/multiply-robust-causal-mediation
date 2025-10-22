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
# Script Description: 
# 
#       Note: data generation creates pop data (when Mfamily & Yfamily meet "gaussian" &. "binomial") for every iteration but only saves first iteration into pop data folder. 
#
#
# Last Updated: 2025-09-03
#
#
# Notes:
#   To-Do: 
#               + MAYBE SEE IF YOU CAN GET ITERATION NUMBER STORED PROPERLY INSTEAD OF SEED NUMBER 
# 
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

# Note:
#   code used for seeds in Dr Liu's code: 
        # set.seed(12)
        # datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

#
################################################################################


# # This can potentially speed up code
# library(compiler)
# enableJIT(3)

# set directory 
# setwd("/home1/10384/cameronmccann/multiply-robust-causal-mediation copy") # temp-2025-01-23-test_multiply-robust-causal-mediation")

# Load packages & functions ----------------------------------------------------

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
    # "trueVals2.0c",
    "trueVals2.0d",
    "trueVals2.0f",
    
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

# Select all of the simulation conditions you'd like to run (condition # will correspond to row #)
conditions <- conditions_all |> 
    # filter(quadratic == F) |> 
    # filter(if.null == T) |> 
    filter((J %in% c(10) & Nj_low %in% c(50)) | 
               J %in% c(40) & Nj_low %in% c(5)) |>  
    filter(!(J == 10 & Mfamily == "binomial" & Yfamily == "binomial" & if.null == TRUE)) # drop 2 rows we already ran
    # filter(J %in% c(10, 40)) #|> #c(100, 70)) |> # c(20)) |> # c(70)) |>
    # filter(Nj_low %in% c(50)) #|>
    # filter(!c(Mfamily == "gaussian" & Yfamily == "gaussian"))
    # filter(Mfamily %in% c("gaussian") & Yfamily %in% c("gaussian"))
    # filter(Mfamily %in% c("binomial", "gaussian"), Yfamily %in% c("binomial", "gaussian")) #, "gaussian")) # c("binomial")) #, Yfamily %in% c("gaussian"))

    # test run conditions: 
    # conditions_all |> tibble::rownames_to_column("condition_number") |> filter(if.null == F & J %in% c(40, 100)) |> filter((Mfamily == "binomial" & Yfamily == "binomial") | (Mfamily == "gaussian" & Yfamily == "gaussian"))

# limit conditions 
# conditions <- conditions[c(1, 18), ] #conditions[c(1:2, 17:18), ] #
conditions <- conditions_all |> #[c(58, 61, 64, 67, 70,  55, 56, 57, 60), , drop = FALSE] |> #c(1:2, 49:72) #1, 3, 6, 7, 9, 12, 13, 18, 19, 21, 24, 49, 51, 52, 54, 55, 56, 57, 58, 60, 67, 69, 70, 72, 61:66 #1, 7, 13, 19, 4, 10, 16, 22, 3, 9, 15, 21, 6, 12, 18, 24
    tibble::rownames_to_column("condition_number") |>
    # slice(1)
    filter(Nj_low == 50) |> 
    slice(c(3, 9, 30, 1:2, 4:8, 10:29))
    # slice(-c(1:85))
    # arrange(if.null) |>
    # slice(-c(1:25)) 
    
conditions

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
    Fit = c("mlr","glm"), #Fit = c("mlr3", "mlr2", "mlr","glm"), #
    # cluster_opt_a = c("sufficient_stats",  "cwc.FE"), # "FE.glm", #  
    # cluster_opt_m = c("sufficient_stats",  "cwc.FE"),  #"FE.glm", # 
    # cluster_opt_y = c("sufficient_stats",  "cwc.FE") # "cwc.FE"#c("sufficient_stats") #, 
    cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
)) |>  
    mutate(
        cluster_opt_a = cluster_opt, 
        cluster_opt_m = cluster_opt, 
        cluster_opt_y = cluster_opt
    )

# (methds <- methds_all %>%
#     filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") ))
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("glm", "mlr")) #, "mlr2", "mlr3")) #, "glm")) 
    
# Add RE & RE with random slopes 
## Add column for random-slope variables
methds <- data.frame(methds,
                     random_slope_vars_y = "NULL") 
## Add RE & RE with random slopes to dataframe 
methds <- bind_rows(
    methds,
    data.frame(
        cluster_a = c(NA, NA),
        cluster_m = c(NA, NA),
        cluster_y = c(NA, NA),
        Fit = c("glm", "glm"),
        cluster_opt = c("RE.glm", "RE.glm.rs"),
        cluster_opt_a = c("RE.glm", "RE.glm.rs"),
        cluster_opt_m = c("RE.glm", "RE.glm.rs"),
        cluster_opt_y = c("RE.glm", "RE.glm.rs"),
        random_slope_vars_y = c("NULL", "A")
    )
)
## Drop RE with random slopes 
methds <- methds |>
    filter(cluster_opt != "RE.glm.rs")



# Set desired reps, & folders ----------------------------------------------

# # Set seed & condition 
# set.seed(12)
# datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

backup_every <- 6#2#10#5#20#50          # Save after every 6 successful reps
# start_raw_iter <- 1         # Raw iteration to resume from (index in seed_pool)

# select starting & ending condition numbers (correspond to rows in conditions dataframe) 
strting_cond <- 1#6#1 #18                           # <------------------------- SELECT ROW AS STARTING CONDITION HERE
total_conditions <- nrow(conditions) 
# if (!exists(strting_cond) || is.null(strting_cond)) {
#     strting_cond <- 1
# }

# Number of desired replications (i.e., successful reps)
reps <- 200#12#600 #300 #200 # 1000

# Set max number of iterations attempted (avoid infinite loop)
max_attempts <- 500000#500#100#3000

# ══════════════════════════════
#    Set up folders 
# ══════════════════════════════
# Create parent output directory
path <- "Output/S1_Simulation-Output"
## Add subdirectory, if desired (e.g., for test runs)
additional_folder <- "2025-09-02_200-reps" #"2025-05-05-test-large-clusters_600-reps_TESTING" #"2025-02-08-test_600-reps"
## Check if additional_folder is not NULL to add to path
if (!exists("additional_folder") || !is.null(additional_folder)) {
    path <- file.path(path, additional_folder)
}
## Create directory, if not already done
if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Create subfolder for pop_data
pop_data_folder <- file.path(path, "pop-data")
if (!dir.exists(pop_data_folder)) dir.create(pop_data_folder)

# Create subfolder for back ups
backup_folder <- file.path(path, "backups")
if (!dir.exists(backup_folder)) dir.create(backup_folder)



# Simulation  -------------------------------------------------------------

# ══════════════════════════════
#     Set Up Parallel 
# ══════════════════════════════
Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)  # Avoid nested threads

# Detect available cores and dynamically allocate
min_cores <- 2#10  # Minimum cores to use
preferred_cores <- backup_every#3#6#25#0#50 #15 #20  #150 # Preferred minimum if available
available_cores <- parallel::detectCores(logical = TRUE)  # Detect all logical cores

n_cores <- max(min_cores, min(preferred_cores, available_cores))  # Use a reasonable number of cores
message(glue("Detected {available_cores} cores. Using {n_cores} cores for parallel computing."))

# n_cores <- 4#0 # parallel::detectCores(logical = TRUE) - 1
# cl <- parallel::makeCluster(n_cores)
# doParallel::registerDoParallel(cl)
# 
# message(glue(#"Detected {available_cores} cores. 
#              "Using {n_cores} cores for parallel computing."))

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

for (cond_idx in strting_cond:total_conditions) { #seq_len(total_conditions)) {
    
    cond <- conditions[cond_idx, ]
    isQuad <- cond[["quadratic"]]
    Mfamily <- cond[["Mfamily"]]
    Yfamily <- cond[["Yfamily"]]
    Nj_low <- cond[["Nj_low"]]
    Nj_high <- cond[["Nj_high"]]
    Jval <- cond[["J"]]
    isNull <- cond[["if.null"]]
    
    true_cond_number <- as.integer(conditions$condition_number[cond_idx]) # obtain cond number. 
    cond_idx_padded <- sprintf("%02d", true_cond_number)                  # change cond number: 1 => 01
    # cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
    
    # Create condition-specific backup folder
    cond_backup_folder <- file.path(backup_folder, glue("cond-{cond_idx_padded}_null-{isNull}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}"))
    if (!dir.exists(cond_backup_folder)) dir.create(cond_backup_folder, recursive = TRUE)
    
    # Check for existing backup for this condition
    backup_files <- list.files(
        path = cond_backup_folder,
        pattern = "progress-rep-.*\\.rds$",
        full.names = TRUE
    )
    
    if (length(backup_files) > 0) {
        latest_backup_file <- backup_files[which.max(file.mtime(backup_files))]
        successful_results <- readRDS(latest_backup_file)
        cat(glue("🔁 Resuming from {basename(latest_backup_file)} with {length(successful_results)} reps. \n\n"))
        
        # Determine the last used raw_iteration (i.e., seed index)
        # used_raw_iters <- sapply(successful_results, function(x) x$raw_iteration)
        # start_raw_iter <- max(used_raw_iters, na.rm = TRUE) + 1
        # Using robust version:
        used_raw_iters <- sapply(successful_results, function(x) {
            if (is.null(x$raw_iteration)) {
                return(NA_integer_)
            } else {
                return(as.integer(x$raw_iteration))
            }
        }, USE.NAMES = FALSE)
        
        # Remove any NA values and find the maximum
        valid_raw_iters <- used_raw_iters[!is.na(used_raw_iters)]
        
        if (length(valid_raw_iters) > 0) {
            start_raw_iter <- max(valid_raw_iters) + 1
        } else {
            # Fallback if no valid raw_iteration values found
            start_raw_iter <- 1
            warning("No valid raw_iteration values found in backup. Starting from 1.")
        }
        rep_counter <- length(successful_results) + 1
        
        # Create versioned resume log filename
        log_files <- list.files(cond_backup_folder, pattern = "^resume-log.*\\.txt$", full.names = FALSE)
        log_version <- length(log_files) + 1
        resume_log_file <- file.path(cond_backup_folder, glue("resume-log-{log_version}.txt"))
        
        # Write new log
        writeLines(
            glue(
                "Resumed from backup: {basename(latest_backup_file)}",
                "\nStarting at raw_iter = {start_raw_iter}",
                "\nTimestamp: {Sys.time()}"
            ),
            resume_log_file
        )
        # # Optional: Write log
        # writeLines(
        #     glue("Resumed from backup: {basename(latest_backup_file)}\nStarting at raw_iter = {start_raw_iter}"),
        #     file.path(cond_backup_folder, "resume-log.txt")
        # )
    } else {
        successful_results <- list()
        rep_counter <- 1
        start_raw_iter <- 1
    }
    
    # Create condition label for saving files 
    cond_label <- glue(
        "null={isNull}, quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}"
    )
    
    ### Start timing for condition ----------------------------------------------
    # start_time_cond <- Sys.time()
    # Set or reuse start time for duration calculation
    start_time_file <- file.path(cond_backup_folder, "start-time.txt")
    if (length(backup_files) == 0) {
        # First run — create start time and write it
        start_time_cond <- Sys.time()
        writeLines(as.character(start_time_cond), start_time_file)
    } else {
        # Resume — read original start time
        if (file.exists(start_time_file)) {
            start_time_cond <- as.POSIXct(readLines(start_time_file), tz = "")
        } else {
            start_time_cond <- Sys.time()  # fallback, shouldn't happen
        }
    }
    
    set.seed(12)
    seeds <- c(sample(1:1e6, 300000), sample(1:1e6+1e6, 200000)) #datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
    # seeds <- sample.int(1e7, reps)
    # seeds <- seeds[131:133] #[259:261]
    # seeds <- c(721442, 814237, 814286) #problematic seed nums
    
    # Define pool of seeds
    seed_pool <- seeds[1:max_attempts]
    
    # Prep before loop
    # successful_results <- list()
    # rep_counter <- 1
    seed_counter <- start_raw_iter  # Start at a specific raw iteration    #seed_counter <- 1
    batch_size <- n_cores
    
    # Loop until we have enough valid replications
    while (length(successful_results) < reps && seed_counter <= max_attempts) {
        
        # Select batch of seeds
        seeds_batch <- seed_pool[seed_counter:min(seed_counter + batch_size - 1, max_attempts)]
        
        # Parallel process the batch
        results_batch <- parallel::mclapply(seeds_batch, function(seed) {
            tryCatch({
                
                # Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1)  # Ensure settings per worker
                start_time_iter <- Sys.time()
                # set.seed(seeds[rep_idx])
                set.seed(seed)
                
                # message 
                # cat("Starting rep = ~", rep_counter, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Starting rep_idx =", rep_idx, "...\n")
                
                # Generate data
                sim_data <- generate_data2.0c(
                    J = Jval, 
                    njrange = c(Nj_low, Nj_high), 
                    Mfamily = Mfamily,
                    Yfamily = Yfamily,
                    if.null = isNull, 
                    seed = seed,
                    quadratic.A = isQuad,
                    quadratic.M = isQuad,
                    quadratic.Y = isQuad,
                    num_x = 3,
                    include_overlapMsg = FALSE,
                    plot_PSdiagnostics = FALSE,
                    randomize = FALSE,
                    ensure_cluster_positivity = TRUE,
                    a_on_x = sqrt(0.03 / 3),
                    a_on_z =  sqrt(0.05 / 1),
                    m_on_a = 0.2, 
                    m_on_az = 0, 
                    m_on_anj = 0.2, 
                    m_on_x = sqrt(0.15 / 3), 
                    m_on_z = sqrt(0.4), 
                    y_on_a = 0.2, 
                    y_on_m = 1, 
                    y_on_am = 0, 
                    y_on_az = 0, 
                    y_on_mz = 0, 
                    y_on_anj = 0.2, 
                    y_on_x = sqrt(0.15 / 3), 
                    y_on_z = sqrt(0.4), 
                    yintercept = 0.25, 
                    x_z = 0
                )
                
                if (is.null(sim_data)) return(NULL)
                
                
                # Save population data if applicable
                if (!is.null(sim_data$truevals$pop_data) && seed_counter == 1) {#rep_idx == 1) { # drop && rep_idx == 1 to save all pop data
                    # Save only for the first replication
                    # pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx}-rep-{rep_idx}.rds"))
                    # saveRDS(sim_data$truevals$pop_data, pop_data_file)
                    
                    # Zero-padded condition number
                    # cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
                    # pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}-rep-{rep_idx}.rds"))
                    pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"))
                    saveRDS(sim_data$truevals$pop_data, pop_data_file)
                    
                }
                
                ### Estimate effects --------------------------------------------------------
                results <- list()
                for (meth in 1:nrow(methds)) {
                    Fit <- as.character(methds$Fit[meth])
                    
                    if (Fit == "glm") {
                        learners_a <- learners_m <- learners_y <- c("SL.glm")
                        num_folds <- 1
                    }
                    
                    if (Fit == "mlr") {
                        learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
                        num_folds <- 5 #10 
                    }
                    
                    # More complex model 
                    if (Fit == "mlr2") {
                        learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam", "SL.ranger")
                        num_folds <- 5
                    }
                    
                    if (Fit == "mlr3") {
                        learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam", "SL.glmnet")
                        num_folds <- 5
                    }
                    
                    cluster_opt <- methds$cluster_opt[meth]
                    random_slope_vars_y <- methds$random_slope_vars_y[meth]
                    
                    # new 
                    warnings_list <- character(0)  # Reset warnings for each iteration
                    err_msg <- NULL
                    estimates <- NULL
                    
                    estimates <- tryCatch(
                        withCallingHandlers(
                            estimate_mediation(
                                data = sim_data$data, 
                                Sname = "school", 
                                Wnames = NULL,
                                Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))], 
                                Aname = "A", 
                                Mnames = "M",
                                Yname = "Y", 
                                learners_a = learners_a, 
                                learners_m = learners_m, 
                                learners_y = learners_y, 
                                cluster_opt = cluster_opt, 
                                num_folds = num_folds, 
                                random_slope_vars_y = random_slope_vars_y
                            ), 
                            warning = function(w) {
                                warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
                                invokeRestart("muffleWarning")  # Muffle warning to continue
                            }
                        ), 
                        error = function(e) {
                            err_msg <<- conditionMessage(e)
                            NULL
                        }
                    )
                    
                    # Obtain label 
                    label <- glue::glue("{methds$Fit[meth]}-{methds$cluster_opt[meth]}")
                    
                    # Store results and warnings in the list for this iteration
                    results[[label]] <- list(
                        Fit = methds$Fit[[meth]], 
                        cluster_opt = methds$cluster_opt[[meth]], 
                        num_folds = num_folds, 
                        estimates = estimates,
                        warnings = warnings_list, 
                        error = !is.null(err_msg), 
                        error_message = err_msg
                    )

                    
                    # old 
                    # warnings_list <- character(0)  # Reset warnings for each iteration
                    # 
                    # estimates <- withCallingHandlers(
                    #     {
                    #         estimate_mediation(
                    #             data = sim_data$data,
                    #             Sname = "school",
                    #             Wnames = NULL,
                    #             Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
                    #             Aname = "A",
                    #             Mnames = "M",
                    #             Yname = "Y",
                    #             learners_a = learners_a,
                    #             learners_m = learners_m,
                    #             learners_y = learners_y,
                    #             cluster_opt = cluster_opt,  
                    #             num_folds = num_folds, 
                    #             random_slope_vars_y = random_slope_vars_y
                    #         )
                    #     },
                    #     warning = function(w) {
                    #         warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
                    #         invokeRestart("muffleWarning")  # Muffle warning to continue
                    #     }
                    # )
                    # 
                    # # Store results and warnings in the list for this iteration
                    # results[[glue::glue("{methds$Fit[meth]}-{methds$cluster_opt[meth]}")]] <- list(
                    #     Fit = methds$Fit[[meth]], 
                    #     cluster_opt = methds$cluster_opt[[meth]], 
                    #     # methds[1, ], 
                    #     num_folds = num_folds, 
                    #     estimates = estimates,
                    #     warnings = warnings_list
                    # )
                }
                
                # ═══════════════════
                #    Decide if the iteration is keepable 
                # ═══════════════════
                any_success <- any(vapply(results, function(x) !is.null(x$estimates), logical(1)))
                # Record which methods succeeded/failed
                succ_methods <- names(Filter(function(x) !is.null(x$estimates), results))
                fail_methods <- names(Filter(function(x) is.null(x$estimates), results))
                # If everything failed, mark the iteration as an error 
                if (!any_success) {
                    return(list(
                        raw_iteration = match(seed, seed_pool),
                        iteration = seed_counter,
                        rep = rep_counter,
                        seed = seed,
                        cond_idx = true_cond_number,
                        condition_details = as.character(cond_label),
                        iter_time_sec = as.numeric(difftime(Sys.time(), start_time_iter, units = "secs")),
                        results = results,            # contains per-method error_message fields
                        error_all_methods = TRUE
                    ))
                }
                ########################################################################
                
                end_time_iter <- Sys.time()
                iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
                
                
                ########################################################################
                # Prep data for export (update code that follows this)
                iteration_data <- list(
                    raw_iteration = match(seed, seed_pool), # Add raw iteration number (not successful iteration number) 
                    iteration = seed_counter, #rep_idx,
                    rep = rep_counter, 
                    seed = seed, #seed_pool[rep_idx], #seeds[rep_idx],
                    cond_idx = true_cond_number, #cond_idx_padded, #cond_idx,
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
                        nj_sizes = sim_data$parameters$nj_sizes,
                        clust_trt_prop = as.vector(sim_data$parameters$clust_trt_prop)
                    ), 
                    # 
                    results = results,                 # <-- contains both successes and failures
                    summary_methods = list(
                        succeeded = succ_methods,
                        failed = fail_methods
                    ),
                    error_all_methods = FALSE
                )
                # 
                # ########################################################################
                # 
                # # clear up space
                # rm(sim_data, results, estimates, warnings_list)
                # 
                # message 
                # cat("Finished rep_idx =", rep_idx, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Finished rep_idx =", rep_idx, "...\n")
                # cat("Finished rep = ~", rep_counter, "... @", format(Sys.time(), "%H:%M:%S"), "\n") 
                # Return the iteration data
                return(iteration_data)
                
            }, error = function(e) {
                list(iteration = seed, #rep_idx, 
                     error = TRUE, 
                     message = conditionMessage(e))
            })
            
        }, mc.cores = n_cores)
        
        # Filter and store valid results
        # successful_batch <- Filter(Negate(is.null), results_batch)
        successful_batch <- Filter(function(x) {
            !is.null(x) && !isTRUE(x$error_all_methods) # drops iterations where all methods failed
        }, results_batch)
            
        if (length(successful_batch) > 0) {
            for (res in seq_along(successful_batch)) {
                # successful_results[[rep_counter]] <- res
                if (exists("successful_results") == TRUE) {
                    successful_batch[[res]]$rep <- length(successful_results) + res
                    # successful_batch[[res]]$iteration <- successful_batch[[res]]$raw_iteration
                    successful_results[[length(successful_results) + 1]] <- successful_batch[[res]]
                } else {
                    successful_results <- list(successful_batch[[res]])
                }
                rep_counter <- rep_counter + 1
                # if (length(successful_results) >= reps) break
                if (length(successful_results) %% backup_every == 0 || length(successful_results) >= reps) {
                    saveRDS(successful_results,
                            file.path(
                                cond_backup_folder,
                                glue("S1_condition-{cond_idx_padded}_progress-rep-{length(successful_results)}-of-{reps}.rds")
                            )
                        )
                    cat(glue("✔ Backup saved after {length(successful_results)} reps. \n"))
                }
            }
        }
        
        # Move forward in seed pool
        seed_counter <- seed_counter + batch_size
        cat(glue("Progress: {length(successful_results)} / {reps} reps completed. ... @ {format(Sys.time(), '%H:%M:%S')} \n\n "))
    
    }    
    
    
    # Message attempts ran out
    if (length(successful_results) < reps) {
        warning(glue("⚠️ Only {length(successful_results)} of {reps} reps completed for condition {cond_idx_padded} — max_attempts ({max_attempts}) exhausted."))
    }
    
    
    # Safely extract fields for the summary
    iteration_summary_df <- purrr::map_dfr(successful_batch, function(res) { #result_list, function(res) {
        if (is.null(res$iteration) || is.null(res$seed) || is.null(res$cond_idx)) {
            tibble(
                iteration = NA,
                seed = NA,
                cond_idx = NA,
                iter_time_sec = NA
            )
        } else {
            tibble(
                iteration = res$iteration,
                seed = res$seed,
                cond_idx = res$cond_idx,
                iter_time_sec = res$iter_time_sec
            )
        }
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
    # cond_idx_padded <- sprintf("%02d", cond_idx)
    # saveRDS(result_list, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
    saveRDS(successful_results, file.path( #result_list, file.path(
        path,
        glue(
            "S1_condition-{cond_idx_padded}_reps-{reps}_null-{isNull}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"
        )
    ))
    
    #Glue used in prior code or Dr Liu's
    # cond_label <- glue("quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}")
    # rname <- glue("RData/qA{qA}qM{qM}qY{qY}_null{null}_cluster_opt{cluster_opt}_intXZ{intXZ}_xz{xz}_{Yfamily}_Fit_{learner}_icc{icc}_J{J}_n{nj}_rep{jobseeds[1]}_{tail(jobseeds, 1)}.RData")
    
    # clear space
    rm(successful_batch) #rm(result_list)
    # gc()
    
    
    # ----- New Logging Info -----
    resume_logs <- list.files(cond_backup_folder, pattern = "^resume-log.*\\.txt$", full.names = TRUE)
    n_resumes <- length(resume_logs)
    resume_times <- if (n_resumes > 0) {
        sapply(resume_logs, function(file) {
            lines <- readLines(file)
            timestamp_line <- grep("^\\s*Timestamp:", lines, value = TRUE)
            if (length(timestamp_line)) sub("^\\s*Timestamp:\\s*", "", timestamp_line) else NA # if (length(timestamp_line)) timestamp_line else NA
        })
    } else {
        character(0)
    }
    
    # Build condition summary log
    summary_lines <- c(
        glue("Condition Index/Number: {cond_idx_padded}"),
        glue("Condition Details: {cond_label}"),
        glue("Start Time: {start_time_cond}"),
        glue("End Time: {Sys.time()}"),
        if (length(successful_results) >= reps) {
            glue("✅ Completed desired reps: {reps}")
        } else {
            glue("⚠️ Only {length(successful_results)} of {reps} reps completed — max_attempts exhausted.")
        },
        glue("Successful Reps: {length(successful_results)}"),
        glue("Raw Iterations Attempted: {seed_counter - 1}"),
        glue("Cores Used: {n_cores}"),
        glue("Total Duration This Session: {cond_time_formatted}"),
        glue("Average Iteration Time: {round(mean_iter_time, 2)} seconds"),
        glue("Number of Resumptions: {n_resumes}"),
        if (n_resumes > 0) {
            c("Resume Times:", paste0("    - ", resume_times))
        }
    )
    
    # Save log file
    summary_log_file <- file.path(
        cond_backup_folder,
        glue("condition-{cond_idx_padded}_summary-log_{format(Sys.time(), '%Y-%h-%d_%H-%M')}.txt")
    )
    writeLines(summary_lines, summary_log_file)
    
    
    cat(glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
        "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
        "Time: {cond_time_formatted}"
    ))
}




################################## END #########################################

# # ══════════════════════════════
# #     Main Loop Over Conditions
# # ══════════════════════════════
# # n_cores <- 1
# total_conditions <- nrow(conditions) #total_conditions <- 1 
# for (cond_idx in strting_cond:total_conditions) { #seq_len(total_conditions)) {
#     
#     cond <- conditions[cond_idx, ]
#     isQuad <- cond[["quadratic"]]
#     Mfamily <- cond[["Mfamily"]]
#     Yfamily <- cond[["Yfamily"]]
#     Nj_low <- cond[["Nj_low"]]
#     Nj_high <- cond[["Nj_high"]]
#     Jval <- cond[["J"]]
#     isNull <- cond[["if.null"]]
#     
#     cond_label <- glue(
#         "null={isNull}, quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}"
#     )
#     
# 
#     ### Start timing for condition ----------------------------------------------
#     start_time_cond <- Sys.time()
#     set.seed(12)
#     seeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200)) #datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
#     # seeds <- sample.int(1e7, reps)
#     # seeds <- seeds[131:133] #[259:261]
#     # seeds <- c(721442, 814237, 814286) #problematic seed nums
#     
#     result_list <- parallel::mclapply(seq_len(reps), function(rep_idx) {
#         # catching error 
#         tryCatch({
#             
#         Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1)  # Ensure settings per worker
#         start_time_iter <- Sys.time()
#         set.seed(seeds[rep_idx])
#         
#         # message 
#         cat("Starting rep_idx =", rep_idx, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Starting rep_idx =", rep_idx, "...\n")
#     # result_list <- foreach(
#     #     rep_idx = seq_len(reps),
#     #     .packages = c("dplyr", "ggplot2", "glue", "purrr"), 
#     #     # .export = c("cond_idx")
#     #     .export = c("get_inference") #, "internal_estimate_mediation")  # Explicitly include your custom functions
#     # ) %dopar% {
#     #     start_time_iter <- Sys.time()
#     #     set.seed(seeds[rep_idx])
#         
#         # tryCatch({#
#         ### Generate data -----------------------------------------------------------
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
#             plot_PSdiagnostics = FALSE, 
#             randomize = FALSE, # TRUE, 
#             
#             m_on_a = 0.2, 
#             m_on_az = 0.2, 
#             m_on_anj = 0.2, 
#             m_on_x = sqrt(0.15 / 3), #num_x
#             m_on_z = sqrt(0.4), 
#             y_on_a = 0.2, 
#             y_on_m = 1, 
#             y_on_am = 0, 
#             y_on_az = 0.2, 
#             y_on_mz = 0.2, 
#             y_on_anj = 0.2, 
#             y_on_x = sqrt(0.15 / 3), #num_x
#             y_on_z = sqrt(0.4), 
#             yintercept = 1, 
#             x_z = 0 
#             # include_truevals = TRUE, # FALSE, 
#             
#             # m_on_a = 2, 
#             # m_on_anj = 0.5,
#             # m_on_az = 0.2,
#             # y_on_a = 1, 
#             # y_on_m = 2, 
#             # y_on_am = 2, 
#             # y_on_az = 0.2,
#             # y_on_mz = 0.2,
#             # y_on_anj = 1
#             
#             # m_on_a = 15,
#             # m_on_anj = 0.5,
#             # m_on_az = 0.2,
#             # y_on_a = 2,
#             # y_on_m = 15,
#             # y_on_am = 5,
#             # y_on_az = 0.2,
#             # y_on_mz = 0.2,
#             # y_on_anj = 5,
#             # int.XZ = FALSE 
#         )
#         
#         
#         # Save population data if applicable
#         if (!is.null(sim_data$truevals$pop_data) && rep_idx == 1) { # drop && rep_idx == 1 to save all pop data
#             # Save only for the first replication
#             # pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx}-rep-{rep_idx}.rds"))
#             # saveRDS(sim_data$truevals$pop_data, pop_data_file)
#             
#             # Zero-padded condition number
#             cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
#             # pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}-rep-{rep_idx}.rds"))
#             pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"))
#             saveRDS(sim_data$truevals$pop_data, pop_data_file)
# 
#         }
#         
#         
#         
#         ########################################################################
#         # INSERT ESTIMATION CODE HERE 
# 
#         ### Estimate effects --------------------------------------------------------
#         results <- list()
#         for (meth in 1:nrow(methds)) {
#             Fit <- as.character(methds$Fit[meth])
#             
#             if (Fit == "glm") {
#                 learners_a <- learners_m <- learners_y <- c("SL.glm")
#                 num_folds <- 1
#             }
#             
#             if (Fit == "mlr") {
#                 learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
#                 num_folds <- 5
#             }
#             
#             # More complex model 
#             if (Fit == "mlr2") {
#                 learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam", "SL.ranger")
#                 num_folds <- 5
#             }
#             
#             if (Fit == "mlr3") {
#                 learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam", "SL.glmnet")
#                 num_folds <- 5
#             }
#             
#             cluster_opt <- methds$cluster_opt[meth]
#             
#             warnings_list <- character(0)  # Reset warnings for each iteration
#             
#             estimates <- withCallingHandlers(
#                 {
#                     estimate_mediation(
#                         data = sim_data$data,
#                         Sname = "school",
#                         Wnames = NULL,
#                         Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
#                         Aname = "A",
#                         Mnames = "M",
#                         Yname = "Y",
#                         learners_a = learners_a,
#                         learners_m = learners_m,
#                         learners_y = learners_y,
#                         cluster_opt = cluster_opt,  
#                         num_folds = num_folds
#                     )
#                 },
#                 warning = function(w) {
#                     warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
#                     invokeRestart("muffleWarning")  # Muffle warning to continue
#                 }
#             )
#             
#             # Store results and warnings in the list for this iteration
#             results[[glue::glue("{methds$Fit[meth]}-{methds$cluster_opt[meth]}")]] <- list(
#                 Fit = methds$Fit[[meth]], 
#                 cluster_opt = methds$cluster_opt[[meth]], 
#                 # methds[1, ], 
#                 num_folds = num_folds, 
#                 estimates = estimates,
#                 warnings = warnings_list
#             )
#         }
#         
#         
#         ###
#         # clust_opt <- c("noncluster.glm", "FE.glm", "RE.glm") # "cwc", "cwc.FE"
#         # results <- list()
#         # 
#         # for (opt in clust_opt) {
#         #     warnings_list <- character(0)  # Reset warnings for each iteration
#         #     
#         #     estimates <- withCallingHandlers(
#         #         {
#         #             estimate_mediation(
#         #                 data = sim_data$data,
#         #                 Sname = "school",
#         #                 Wnames = NULL,
#         #                 Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
#         #                 Aname = "A",
#         #                 Mnames = "M",
#         #                 Yname = "Y",
#         #                 learners_a = c("SL.glm"),
#         #                 learners_m = c("SL.glm"),
#         #                 learners_y = c("SL.glm"),
#         #                 cluster_opt = opt,  
#         #                 num_folds = 1
#         #             )
#         #         },
#         #         warning = function(w) {
#         #             warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
#         #             invokeRestart("muffleWarning")  # Muffle warning to continue
#         #         }
#         #     )
#         #     
#         #     # Store results and warnings in the list for this iteration
#         #     results[[opt]] <- list(
#         #         estimates = estimates,
#         #         warnings = warnings_list, 
#         #         num_folds = 1           # change later
#         #     )
#         # }
#         
#         ########################################################################
#         
#         end_time_iter <- Sys.time()
#         iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
#         
#         
#         ########################################################################
#         # Prep data for export (update code that follows this)
#         iteration_data <- list(
#             iteration = rep_idx,
#             seed = seeds[rep_idx],
#             cond_idx = cond_idx,
#             condition_details = as.character(cond_label),
#             iter_time_sec = round(iter_duration, 4),
#             truevals = list(
#                 truevals_individual = sim_data$truevals$truevals_individual, 
#                 truevals_cluster = sim_data$truevals$truevals_cluster
#             ), 
#             effects = sim_data$effects,
#             overlap = list(
#                 ps_summary = sim_data$overlap$ps_summary, 
#                 iptw_summary = sim_data$overlap$iptw_summary
#             ), 
#             parameters = list(
#                 J = sim_data$parameters$J,
#                 njrange = sim_data$parameters$njrange,
#                 nj_sizes = sim_data$parameters$nj_sizes
#             ), 
#             # 
#             results = results
#         )
#         # 
#         # ########################################################################
#         # 
#         # # clear up space
#         # rm(sim_data, results, estimates, warnings_list)
#         # 
#         # message 
#         cat("Finished rep_idx =", rep_idx, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Finished rep_idx =", rep_idx, "...\n")
#         # Return the iteration data
#         return(iteration_data)
#         }, error = function(e) {
#             list(iteration = rep_idx, 
#                  error = TRUE, 
#                  message = conditionMessage(e))
#         })
#     # })# 
#     }, mc.cores = n_cores)# End of mclapply
#     
#     # # Validate structure of result_list #
#     # if (!all(sapply(result_list, is.list))) { #
#     #     stop("Some elements of result_list are not lists. Check the worker output.") #
#     # } #
#     # 
#     # # Print structure for debugging #
#     # str(result_list[1:5])  # Inspect the first few elements #
#     
#     # # Extract relevant fields and convert to a data frame
#     # iteration_summary_df <- purrr::map_dfr(result_list, ~{
#     #     tibble(
#     #         iteration = .x$iteration,
#     #         seed = .x$seed,
#     #         cond_idx = .x$cond_idx,
#     #         iter_time_sec = .x$iter_time_sec
#     #     )
#     # })
#     
#     # Check structure
#     # str(result_list)
#     # purrr::map(result_list, names)
#     
#     
#     # Safely extract fields for the summary
#     iteration_summary_df <- purrr::map_dfr(result_list, function(res) {
#         if (is.null(res$iteration) || is.null(res$seed) || is.null(res$cond_idx)) {
#             tibble(
#                 iteration = NA,
#                 seed = NA,
#                 cond_idx = NA,
#                 iter_time_sec = NA
#             )
#         } else {
#             tibble(
#                 iteration = res$iteration,
#                 seed = res$seed,
#                 cond_idx = res$cond_idx,
#                 iter_time_sec = res$iter_time_sec
#             )
#         }
#     })
#     
#     
#     
#     # iteration_summary_df <- do.call(rbind, result_list)
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
#     # Commented out previous code:
#     # saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx}.rds")))
#     # cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
#     # saveRDS(iteration_summary_df, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
#     
#     # New code to save the list for each condition
#     cond_idx_padded <- sprintf("%02d", cond_idx)
#     # saveRDS(result_list, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
#     saveRDS(result_list, file.path(
#         path,
#         glue(
#             "S1_condition-{cond_idx_padded}_reps-{reps}_null-{isNull}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"
#         )
#     ))
#     
#     #Glue used in prior code or Dr Liu's
#     # cond_label <- glue("quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}")
#     # rname <- glue("RData/qA{qA}qM{qM}qY{qY}_null{null}_cluster_opt{cluster_opt}_intXZ{intXZ}_xz{xz}_{Yfamily}_Fit_{learner}_icc{icc}_J{J}_n{nj}_rep{jobseeds[1]}_{tail(jobseeds, 1)}.RData")
#     
#     # clear space
#     rm(result_list)
#     # gc()
#     
#     cat(glue(
#         "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
#         "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
#         "Time: {cond_time_formatted}"
#     ))
# }
# 
# # ══════════════════════════════
# #     Save Overall Timing and Total Time
# # ══════════════════════════════
# # total_time_sum <- sum(as.numeric(gsub(" min.*", "", OverallPar_time$total_time)) * 60 + as.numeric(sub(".*min ", "", gsub(" s", "", OverallPar_time$total_time_min))))
# # message(glue("Total computation time: {floor(total_time_sum / 60)} min {round(total_time_sum %% 60)} s"))
# # saveRDS(OverallPar_time, file.path(path, "S1_Computation-Time.rds"))
# 
# total_seconds <- sum(
#     OverallPar_time %>%
#         mutate(
#             minutes = as.numeric(str_extract(total_time, "\\d+(?= min)")),
#             seconds = as.numeric(str_extract(total_time, "\\d+(?= s)"))
#         ) %>%
#         mutate(minutes = ifelse(is.na(minutes), 0, minutes),  # Handle cases with only seconds
#                seconds = ifelse(is.na(seconds), 0, seconds)) %>%
#         summarise(total_seconds = sum(minutes * 60 + seconds)) %>%
#         pull(total_seconds)
# )
# 
# # Convert total seconds to minutes and seconds
# total_minutes <- floor(total_seconds / 60)
# remaining_seconds <- total_seconds %% 60
# 
# message(glue("Total computation time: {total_minutes} min {remaining_seconds} s"))
# 
# # Add total time to Computation time file & save 
# OverallPar_time <- rbind(OverallPar_time,
#                          c(
#                              NA,
#                              NA,
#                              sum(OverallPar_time$n_reps),
#                              glue("{total_minutes} min {remaining_seconds} s"),
#                              NA
#                          ))
# saveRDS(OverallPar_time, file.path(path, "S1_Computation-Time.rds"))
# 
# # ══════════════════════════════
# #     Shutdown Parallel 
# # ══════════════════════════════
# # stopCluster(cl)
# 
# 
# # END OF SCRIPT MESSAGE
# # BRRR::skrrrahh_list()
# # BRRR::skrrrahh("biggie")
# # BRRR::skrrrahh("kendrick")
