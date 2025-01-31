################################################################################
##################### Testing Data Generation Functions ####################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 2025-01-30
#
#
# Script Description: 
#   I am using this script to figure out what is causing the error(s) in 
# linear scenario for select iterations (4, 132, 260 for condition 1 only; 
# J=10, nj=[50,100] with binomial medaitor & outcome). 
# 
# 
# ══════════════════════════════
#    RESOLVED: since I am not getting this issue again after running the simulation (100 reps or less) mulitple times, I am considering it resolved.  
# ══════════════════════════════
# 
# Last Updated: 2025-01-30  
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
    filter(quadratic == F) |> 
    filter(if.null == F) |> 
    # filter(J %in% c(40)) |> # c(20)) |> # c(70)) |>
    # filter(Nj_low %in% c(50)) |>
    filter(Mfamily %in% c("binomial", "gaussian"), Yfamily %in% c("binomial", "gaussian")) #, "gaussian")) # c("binomial")) #, Yfamily %in% c("gaussian"))
# conditions

# select starting condition 
strting_cond <- 1 #18
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
    Fit = c("mlr","glm"), # 
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
    filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("mlr", "glm"))



# Set seeds & cond --------------------------------------------------------

# Set seed & condition 
set.seed(12)
datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

conditions

datseeds[c(4, 132, 260)]
# [1] 721442 814237 814286

# load simulation output to look at list
linear_c1 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-01_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")

# Below is there error I get for each of the 3 iterations 
# Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"\n"




# Sim code ----------------------------------------------------------------


# Number of replications
reps <- 2#200 #5 #10 #100 # 10 # 200 # 1000
# reps <- 300 #200 #100

# Create parent output directory
path <- "Output/S1_Simulation-Output"
## Add subdirectory, if desired (e.g., for test runs)
# additional_folder <- "2025-01-27_200-rep_all-linear-conditions-with-all-methods" #"2025-01-25-test_300-rep_all-quad-conditions-with-all-methods" #additional_folder <- "2025-01-24-test_100-rep_all-quad-conditions-with-all-methods" # NULL additional_folder <- "2025-01-23-test_200-reps_all-linear-conditions-with-all-methods" # NULL
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
# Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)  # Avoid nested threads
# 
# # Detect available cores and dynamically allocate
# min_cores <- 10  # Minimum cores to use
# preferred_cores <- 150 #20  # Preferred minimum if available
# available_cores <- parallel::detectCores(logical = TRUE)  # Detect all logical cores
# 
# n_cores <- max(min_cores, min(preferred_cores, available_cores))  # Use a reasonable number of cores
# message(glue("Detected {available_cores} cores. Using {n_cores} cores for parallel computing."))
n_cores <- parallel::detectCores()-1

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
total_conditions <- 1 #total_conditions <- nrow(conditions) #
for (cond_idx in strting_cond:total_conditions) { #seq_len(total_conditions)) {
    
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
    
    
    ### Start timing for condition ----------------------------------------------
    start_time_cond <- Sys.time()
    set.seed(12)
    seeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200)) #datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
    # seeds <- sample.int(1e7, reps)
    
    result_list <- parallel::mclapply(seq_len(reps)+3, function(rep_idx) {      # ADD 4 TO GET 4TH SEED
        Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1)  # Ensure settings per worker
        start_time_iter <- Sys.time()
        set.seed(seeds[rep_idx]) 
        
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
            
            # free up space 
            # rm(sim_data$truevals$pop_data)
            # gc()
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
        # # Return the iteration data
        # iteration_data
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
            "S1_condition-{cond_idx_padded}_reps-{reps}_quad-{isQuad}_M-{Mfamily}_Y-{Yfamily}_nj-[{Nj_low}-{Nj_high}]_J-{Jval}.rds"
        )
    ))
    
    #Glue used in prior code or Dr Liu's
    # cond_label <- glue("quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}")
    # rname <- glue("RData/qA{qA}qM{qM}qY{qY}_null{null}_cluster_opt{cluster_opt}_intXZ{intXZ}_xz{xz}_{Yfamily}_Fit_{learner}_icc{icc}_J{J}_n{nj}_rep{jobseeds[1]}_{tail(jobseeds, 1)}.RData")
    
    # clear space
    rm(result_list)
    # gc()
    
    message(glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_idx_padded}/{total_conditions} ",
        "({round((cond_idx / total_conditions) * 100)}%) completed. ({cond_label}) ",
        "Time: {cond_time_formatted}"
    ))
}


# Running the 4th iteration (with the same seed: 721442) completes with no error &, as seen below, results in estimates
# So there is no issue with the data generation or estimation processes. 
# look at result 
test1 <- readRDS(file = "Output/S1_Simulation-Output/S1_condition-01_reps-1_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds") 

view(test1)


# ran 5 reps for condition 1 in conduct sim script 
simtest2 <- readRDS(file = "Output/S1_Simulation-Output/testing-2025-01-30/S1_condition-01_reps-5_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds") 

# ran 100 reps for condition 1 in conduct sim script 
simtest3 <- readRDS(file = "Output/S1_Simulation-Output/testing-2025-01-30/S1_condition-01_reps-100_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds") 


# Test on 1 dataset -------------------------------------------------------

# problematic iteration 
seed132 <- datseeds[132] #datseeds[c(4, 132, 260)]

# select condition 1 (problematic condition) #conditions
cond_idx <- 1

cond <- conditions[cond_idx, ]
isQuad <- cond[["quadratic"]]
Mfamily <- cond[["Mfamily"]]
Yfamily <- cond[["Yfamily"]]
Nj_low <- cond[["Nj_low"]]
Nj_high <- cond[["Nj_high"]]
Jval <- cond[["J"]]


sim_data <- generate_data2.0c(
    J = Jval, 
    njrange = c(Nj_low, Nj_high), 
    Mfamily = Mfamily,
    Yfamily = Yfamily,
    seed = seed132, # seeds[rep_idx],
    quadratic.A = isQuad,
    quadratic.M = isQuad,
    quadratic.Y = isQuad,
    num_x = 3,
    include_overlapMsg = TRUE, # FALSE,
    plot_PSdiagnostics = FALSE, 
    
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

# Test Generate data  -----------------------------------------------------
