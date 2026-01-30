################################################################################
############################## Simulation(s) ###################################
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-01-01
#
#
# Script Description: 
#       This script runs the simulation study for all (or a select number of) 
#       conditions. Outputs are saved in Output/S1_Simulation-Output/. 
#
#
# Last Updated: 2026-01-30
#
#
# Notes:
#   To-Do: 
#
#   Done: 
#
################################################################################


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
    fastDummies, # for (specifically, for dummy_cols() in fastDummies package)
    stringr, # for str_detect() 
    tibble # for rownames_to_column() 
)

# ══════════════════════════════
#     Source Updated Functions
# ══════════════════════════════
function_names <- c(
    # Data Generation functions
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals",
    "generate_data", 
    
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

# Limit conditions: Select all of the simulation conditions you'd like to run
conditions <- conditions_all |> 
    tibble::rownames_to_column("condition_number") |>
    # filter(Nj_low == 50) |> 
    # arrange(if.null) |>
    slice(c(1:96)) #slice(c(1, 49, 68, 2:48, 50:67, 69:96))                   # <------------------------- SELECT CONDITIONS TO RUN HERE
    
conditions

# ══════════════════════════════
#    Methods  
# ══════════════════════════════
methds_all <- data.frame(expand.grid(
    cluster_a = "FE", 
    cluster_m = "FE", 
    cluster_y =  "FE", 
    Fit = c("mlr","glm"), 
    cluster_opt = c("cwc.FE", "cwc") 
)) |>  
    mutate(
        cluster_opt_a = cluster_opt, 
        cluster_opt_m = cluster_opt, 
        cluster_opt_y = cluster_opt
    )

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

# Set backup frequency
backup_every <- 10          # Save after every 10 successful reps
# start_raw_iter <- 1         # Raw iteration to resume from (index in seed_pool)

# select starting & ending condition numbers (correspond to rows in conditions dataframe) 
strting_cond <- 1                           # <------------------------- SELECT ROW AS STARTING CONDITION HERE
total_conditions <- nrow(conditions) 

# Number of desired replications (i.e., successful reps)
reps <- 1050 

# Set max number of iterations attempted (avoid infinite loop)
max_attempts <- 500000

# ══════════════════════════════
#    Set up folders 
# ══════════════════════════════
# Create parent output directory
path <- "Output/S1_Simulation-Output"
## Add subdirectory, if desired (e.g., for test runs)
additional_folder <- "2025-10-22_1000-reps" 
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
min_cores <- 2  # Minimum cores to use
preferred_cores <- backup_every # Preferred minimum if available
available_cores <- parallel::detectCores(logical = TRUE)  # Detect all logical cores

n_cores <- max(min_cores, min(preferred_cores, available_cores))  # Use a reasonable number of cores
message(glue("Detected {available_cores} cores. Using {n_cores} cores for parallel computing."))

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

for (cond_idx in strting_cond:total_conditions) { 
    
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
    seeds <- c(sample(1:1e6, 300000), sample(1:1e6+1e6, 200000)) 

    # Define pool of seeds
    seed_pool <- seeds[1:max_attempts]
    
    # Prep before loop
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
                set.seed(seed)
                
                # message 
                # cat("Starting rep = ~", rep_counter, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Starting rep_idx =", rep_idx, "...\n")
                
                # Generate data
                sim_data <- generate_data(
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
                
                # Prep data for export 
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
                return(iteration_data)
                
            }, error = function(e) {
                list(iteration = seed, #rep_idx, 
                     error = TRUE, 
                     message = conditionMessage(e))
            })
            
        }, mc.cores = n_cores)
        
        # Filter and store valid results
        successful_batch <- Filter(function(x) {
            !is.null(x) && !isTRUE(x$error_all_methods) # drops iterations where all methods failed
        }, results_batch)
        
        if (length(successful_batch) > 0) {
            for (res in seq_along(successful_batch)) {
                if (exists("successful_results") == TRUE) {
                    successful_batch[[res]]$rep <- length(successful_results) + res
                    successful_results[[length(successful_results) + 1]] <- successful_batch[[res]]
                } else {
                    successful_results <- list(successful_batch[[res]])
                }
                rep_counter <- rep_counter + 1
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
    
    # clear space
    rm(successful_batch) 
    
    # New Logging Info 
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
        glue("condition-{cond_idx_padded}_summary-log_{format(Sys.time(), '%Y-%h-%d_%H%M')}.txt")
    )
    writeLines(summary_lines, summary_log_file)
    
    
    cat(glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
        "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
        "Time: {cond_time_formatted}"
    ))
}

################################## END #########################################
