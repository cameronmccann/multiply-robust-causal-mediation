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
# Last Updated: 2025-07-06
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
setwd("/home1/10384/cameronmccann/multiply-robust-causal-mediation copy") # temp-2025-01-23-test_multiply-robust-causal-mediation")

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

# limit conditions for testing 
conditions <- conditions_all |> 
    # filter(quadratic == F) |> 
    # filter(if.null == T) |> 
    filter((J %in% c(10) & Nj_low %in% c(50)) | 
               J %in% c(40) & Nj_low %in% c(5)) %>% 
    filter(!(J == 10 & Mfamily == "binomial" & Yfamily == "binomial" & if.null == TRUE)) # drop 2 rows we already ran
    # filter(J %in% c(10, 40)) #|> #c(100, 70)) |> # c(20)) |> # c(70)) |>
    # filter(Nj_low %in% c(50)) #|>
    # filter(!c(Mfamily == "gaussian" & Yfamily == "gaussian"))
    # filter(Mfamily %in% c("gaussian") & Yfamily %in% c("gaussian"))
    # filter(Mfamily %in% c("binomial", "gaussian"), Yfamily %in% c("binomial", "gaussian")) #, "gaussian")) # c("binomial")) #, Yfamily %in% c("gaussian"))
conditions

# select starting condition 
strting_cond <- 1#6#1 #18              # <------------------------- SELECT ROW AS STARTING CONDITION HERE
# if (!exists(strting_cond) || is.null(strting_cond)) {
#     strting_cond <- 1
# }

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
)) %>% 
    mutate(
        cluster_opt_a = cluster_opt, 
        cluster_opt_m = cluster_opt, 
        cluster_opt_y = cluster_opt
    )

# (methds <- methds_all %>%
#     filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") ))
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("glm", "mlr")) #, "mlr2", "mlr3")) #, "glm")) 
    



# Set seeds, reps, & folders ----------------------------------------------

# # Set seed & condition 
# set.seed(12)
# datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
# 
# iseed <- 9
# cond <- 1

# Number of replications
# reps <- 2#00 # 10 # 200 # 1000
reps <- 600 #300 #200 #

# Create parent output directory
path <- "Output/S1_Simulation-Output"
## Add subdirectory, if desired (e.g., for test runs)
# additional_folder <- "2025-01-27_200-rep_all-linear-conditions-with-all-methods" #"2025-01-25-test_300-rep_all-quad-conditions-with-all-methods" #additional_folder <- "2025-01-24-test_100-rep_all-quad-conditions-with-all-methods" # NULL additional_folder <- "2025-01-23-test_200-reps_all-linear-conditions-with-all-methods" # NULL
additional_folder <- "2025-05-05-test-large-clusters_600-reps" #"2025-02-08-test_600-reps"
# additional_folder <- "2025-01-30-test_null-linear-scenario"
## Check if additional_folder is not NULL to add to path
if (!exists("additional_folder") || !is.null(additional_folder)) {
    path <- file.path(path, additional_folder)
}
## Create directory, if not already done
if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Create subfolder for pop_data
pop_data_folder <- file.path(path, "pop-data")
if (!dir.exists(pop_data_folder)) dir.create(pop_data_folder)


# Simulation  -------------------------------------------------------------

# ══════════════════════════════
#     Set Up Parallel 
# ══════════════════════════════
Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)  # Avoid nested threads

# Detect available cores and dynamically allocate
min_cores <- 2#10  # Minimum cores to use
preferred_cores <- 25#0#50 #15 #20  #150 # Preferred minimum if available
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
# n_cores <- 1
total_conditions <- nrow(conditions) #total_conditions <- 1 
for (cond_idx in strting_cond:total_conditions) { #seq_len(total_conditions)) {
    
    cond <- conditions[cond_idx, ]
    isQuad <- cond[["quadratic"]]
    Mfamily <- cond[["Mfamily"]]
    Yfamily <- cond[["Yfamily"]]
    Nj_low <- cond[["Nj_low"]]
    Nj_high <- cond[["Nj_high"]]
    Jval <- cond[["J"]]
    isNull <- cond[["if.null"]]
    
    cond_label <- glue(
        "null={isNull}, quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}"
    )
    

    ### Start timing for condition ----------------------------------------------
    start_time_cond <- Sys.time()
    set.seed(12)
    seeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200)) #datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
    # seeds <- sample.int(1e7, reps)
    # seeds <- seeds[131:133] #[259:261]
    # seeds <- c(721442, 814237, 814286) #problematic seed nums
    
    result_list <- parallel::mclapply(seq_len(reps), function(rep_idx) {
        # catching error 
        tryCatch({
            
        Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1)  # Ensure settings per worker
        start_time_iter <- Sys.time()
        set.seed(seeds[rep_idx])
        
        # message 
        cat("Starting rep_idx =", rep_idx, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Starting rep_idx =", rep_idx, "...\n")
    # result_list <- foreach(
    #     rep_idx = seq_len(reps),
    #     .packages = c("dplyr", "ggplot2", "glue", "purrr"), 
    #     # .export = c("cond_idx")
    #     .export = c("get_inference") #, "internal_estimate_mediation")  # Explicitly include your custom functions
    # ) %dopar% {
    #     start_time_iter <- Sys.time()
    #     set.seed(seeds[rep_idx])
        
        # tryCatch({#
        ### Generate data -----------------------------------------------------------
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
            plot_PSdiagnostics = FALSE, 
            randomize = FALSE, # TRUE, 
            
            m_on_a = 0.2, 
            m_on_az = 0.2, 
            m_on_anj = 0.2, 
            m_on_x = sqrt(0.15 / 3), #num_x
            m_on_z = sqrt(0.4), 
            y_on_a = 0.2, 
            y_on_m = 1, 
            y_on_am = 0, 
            y_on_az = 0.2, 
            y_on_mz = 0.2, 
            y_on_anj = 0.2, 
            y_on_x = sqrt(0.15 / 3), #num_x
            y_on_z = sqrt(0.4), 
            yintercept = 1, 
            x_z = 0 
            # include_truevals = TRUE, # FALSE, 
            
            # m_on_a = 2, 
            # m_on_anj = 0.5,
            # m_on_az = 0.2,
            # y_on_a = 1, 
            # y_on_m = 2, 
            # y_on_am = 2, 
            # y_on_az = 0.2,
            # y_on_mz = 0.2,
            # y_on_anj = 1
            
            # m_on_a = 15,
            # m_on_anj = 0.5,
            # m_on_az = 0.2,
            # y_on_a = 2,
            # y_on_m = 15,
            # y_on_am = 5,
            # y_on_az = 0.2,
            # y_on_mz = 0.2,
            # y_on_anj = 5,
            # int.XZ = FALSE 
        )
        
        
        # Save population data if applicable
        if (!is.null(sim_data$truevals$pop_data) && rep_idx == 1) { # drop && rep_idx == 1 to save all pop data
            # Save only for the first replication
            # pop_data_file <- file.path(pop_data_folder, glue("S1_pop-data-condition-{cond_idx}-rep-{rep_idx}.rds"))
            # saveRDS(sim_data$truevals$pop_data, pop_data_file)
            
            # Zero-padded condition number
            cond_idx_padded <- sprintf("%02d", cond_idx) # change condition number: 1 => 01
            # pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}-rep-{rep_idx}.rds"))
            pop_data_file <- file.path(pop_data_folder, glue::glue("S1_pop-data-condition-{cond_idx_padded}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"))
            saveRDS(sim_data$truevals$pop_data, pop_data_file)

        }
        
        
        
        ########################################################################
        # INSERT ESTIMATION CODE HERE 

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
                num_folds <- 5
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
                        learners_a = learners_a,
                        learners_m = learners_m,
                        learners_y = learners_y,
                        cluster_opt = cluster_opt,  
                        num_folds = num_folds
                    )
                },
                warning = function(w) {
                    warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
                    invokeRestart("muffleWarning")  # Muffle warning to continue
                }
            )
            
            # Store results and warnings in the list for this iteration
            results[[glue::glue("{methds$Fit[meth]}-{methds$cluster_opt[meth]}")]] <- list(
                Fit = methds$Fit[[meth]], 
                cluster_opt = methds$cluster_opt[[meth]], 
                # methds[1, ], 
                num_folds = num_folds, 
                estimates = estimates,
                warnings = warnings_list
            )
        }
        
        
        ###
        # clust_opt <- c("noncluster.glm", "FE.glm", "RE.glm") # "cwc", "cwc.FE"
        # results <- list()
        # 
        # for (opt in clust_opt) {
        #     warnings_list <- character(0)  # Reset warnings for each iteration
        #     
        #     estimates <- withCallingHandlers(
        #         {
        #             estimate_mediation(
        #                 data = sim_data$data,
        #                 Sname = "school",
        #                 Wnames = NULL,
        #                 Xnames = names(sim_data$data)[grep("^X", names(sim_data$data))],
        #                 Aname = "A",
        #                 Mnames = "M",
        #                 Yname = "Y",
        #                 learners_a = c("SL.glm"),
        #                 learners_m = c("SL.glm"),
        #                 learners_y = c("SL.glm"),
        #                 cluster_opt = opt,  
        #                 num_folds = 1
        #             )
        #         },
        #         warning = function(w) {
        #             warnings_list <<- c(warnings_list, conditionMessage(w))  # Append warning message
        #             invokeRestart("muffleWarning")  # Muffle warning to continue
        #         }
        #     )
        #     
        #     # Store results and warnings in the list for this iteration
        #     results[[opt]] <- list(
        #         estimates = estimates,
        #         warnings = warnings_list, 
        #         num_folds = 1           # change later
        #     )
        # }
        
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
        # 
        # ########################################################################
        # 
        # # clear up space
        # rm(sim_data, results, estimates, warnings_list)
        # 
        # message 
        cat("Finished rep_idx =", rep_idx, "... @", format(Sys.time(), "%H:%M:%S"), "\n") #cat("Finished rep_idx =", rep_idx, "...\n")
        # Return the iteration data
        return(iteration_data)
        }, error = function(e) {
            list(iteration = rep_idx, 
                 error = TRUE, 
                 message = conditionMessage(e))
        })
    # })# 
    }, mc.cores = n_cores)# End of mclapply
    
    # # Validate structure of result_list #
    # if (!all(sapply(result_list, is.list))) { #
    #     stop("Some elements of result_list are not lists. Check the worker output.") #
    # } #
    # 
    # # Print structure for debugging #
    # str(result_list[1:5])  # Inspect the first few elements #
    
    # # Extract relevant fields and convert to a data frame
    # iteration_summary_df <- purrr::map_dfr(result_list, ~{
    #     tibble(
    #         iteration = .x$iteration,
    #         seed = .x$seed,
    #         cond_idx = .x$cond_idx,
    #         iter_time_sec = .x$iter_time_sec
    #     )
    # })
    
    # Check structure
    # str(result_list)
    # purrr::map(result_list, names)
    
    
    # Safely extract fields for the summary
    iteration_summary_df <- purrr::map_dfr(result_list, function(res) {
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
    cond_idx_padded <- sprintf("%02d", cond_idx)
    # saveRDS(result_list, file.path(path, glue("S1_condition-{cond_idx_padded}.rds")))
    saveRDS(result_list, file.path(
        path,
        glue(
            "S1_condition-{cond_idx_padded}_reps-{reps}_null-{isNull}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"
        )
    ))
    
    #Glue used in prior code or Dr Liu's
    # cond_label <- glue("quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}")
    # rname <- glue("RData/qA{qA}qM{qM}qY{qY}_null{null}_cluster_opt{cluster_opt}_intXZ{intXZ}_xz{xz}_{Yfamily}_Fit_{learner}_icc{icc}_J{J}_n{nj}_rep{jobseeds[1]}_{tail(jobseeds, 1)}.RData")
    
    # clear space
    rm(result_list)
    # gc()
    
    cat(glue(
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
# stopCluster(cl)


# END OF SCRIPT MESSAGE
# BRRR::skrrrahh_list()
# BRRR::skrrrahh("biggie")
# BRRR::skrrrahh("kendrick")
