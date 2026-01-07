################################################################################
##################### Simulation 1 - Obtain Results part 1 #####################
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       This script processes and checks simulation output before saving it in 
#       a format to obtain results from
#
# Inputs:
#   - Simulation output .rds files in: "Output/S1_Simulation-Output/2025-10-22_1000-reps/"
# 
# Outputs: 
#   - 
# 
#
# Last Updated: 2026-01-07
#
#
# Notes:
#   To-Do
# 
#   Done: 
# 
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
    purrr, 
    stringr, 
    flextable, 
    huxtable, 
    ggdag, 
    dagitty, 
    glue
)


# User Inputs / Global Options --------------------------------------------

# Date of simulation 
sim_date <- "2025-10-22" #"2025-09-03"# "2025-07-30" #"2025-07-21" #"2025-07-01" # (Note: Simulations were ran around 2025-02-08 & 2025-05-05) #"2025-02-08" #"2025-01-25" #Sys.Date() #"2025-01-23" #"2025-01-18" 

# Number of replications
reps <- 1000 #600#200 

# # Results folder 
# results_root <- "Output/S1_Results" #path <- "Output/S1_Results"

# Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- NULL #"2025-10-22_1000-reps" # set to NULL on final run

# # Simulation output path 
# sim_output_path <- "Output/S1_Simulation-Output"

# Where to pull output from 
additional_folder_output <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 


# Set up directory structure ----------------------------------------------

# Create directory to store results 
results_root <- "Output/S1_Results"

if (!dir.exists(results_root)) {
    dir.create(results_root, recursive = TRUE)  
}

# Combine results_root + run-specific subfolder
if (is.null(additional_folder_results)) {
    results_path <- file.path(results_root)
} else {
    results_path <- file.path(results_root, additional_folder_results)
    # results_path <- file.path(results_root, paste0(sim_date, "_", reps, "-reps"))
}

# Add subdirectory
if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
}

# Create Data, Figures, and Tables subfolders
results_subfolders <- c("Data", "Figures", "Tables")
for (sf in results_subfolders) {
    dir_sf <- file.path(results_path, sf)
    if (!dir.exists(dir_sf)) dir.create(dir_sf, recursive = TRUE)
}

# Create logs subfolder in Data
logs_path <- file.path(results_path, "Data/logs")
if(!dir.exists(logs_path)) {
    dir.create(logs_path, recursive = TRUE)
}

# Simulation output path (where .rds files already live)
sim_output_root <- "Output/S1_Simulation-Output"

## Obtain simulation output path
if (!is.null(additional_folder_output)) {
    sim_output_path <- file.path(sim_output_root, additional_folder_output)
} else {
    sim_output_path <- file.path(sim_output_root, paste0(sim_date, "_", reps, "-reps"))   
}

## error message 
if (!dir.exists(sim_output_path)) {
    stop("Simulation output directory does not exist: ", sim_output_path)   
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


# Add condition number & limit conditions 
conditions <- conditions_all |> 
    tibble::rownames_to_column("condition_number") #|> 
    # dplyr::filter(condition_number %in% c(1:6)) # subset conditions

# ══════════════════════════════
#    Methods  
# ══════════════════════════════
methds_all <- data.frame(
    expand.grid(
        cluster_a = "FE", #c("FE", "RE", "noncluster"), # "RE", # "noncluster", #
        cluster_m = "FE", # c("FE", "RE", "noncluster"), # "RE", #  "noncluster.mlr", #
        cluster_y = "FE", #c("FE", "RE", "noncluster"), # "noncluster.mlr", ## "FE.mlr", #
        Fit = c("mlr3", "mlr2", "mlr", "glm"),
        cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
    )
) |>
    mutate(
        cluster_opt_a = cluster_opt,
        cluster_opt_m = cluster_opt,
        cluster_opt_y = cluster_opt
    )

# Limit to methods used in paper
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("glm", "mlr")) 

# Add RE & RE with random slopes 
methds <- data.frame(methds,
                     random_slope_vars_y = "NULL") 

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

# Drop RE with random slopes (not reported in paper)
methds <- methds |>
    filter(cluster_opt != "RE.glm.rs")


# Create overall list for all simulation output ---------------------------

# List all simulation output .rds files
rds_files_all <- list.files(
    path = sim_output_path,              # e.g., "Output/S1_Simulation-Output"
    pattern = "^S1_condition.*\\.rds$",   # only files starting with S1_condition and ending in .rds
    full.names = TRUE
) |> sort()

# Extract padded condition numbers 
cond_ids <- rds_files_all |> 
    basename() |> 
    str_extract("S1_condition-\\d{2}") |> 
    str_extract("\\d{2}") 

cond_ids <- paste0("cond_", cond_ids)

# ══════════════════════════════
#    Create overall list (dropping iterations with error) 
# ══════════════════════════════

# Record log
sink(file.path(logs_path, "processing-sim-output.txt"), split = TRUE)

# Define once (outside imap) for efficiency and clarity
error_msg <- "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\""
methd <- c("mlr-cwc.FE", "mlr-cwc", "glm-cwc.FE", "glm-cwc")

# Create overall simulation output list with padded condition numbers as element names (e.g., cond_01) & drop any iterations with errors
overall_list <- purrr::set_names(
    purrr::imap(rds_files_all, function(file, i) {
        # Load file 
        data <- readRDS(file)
        
        # # ORIGINAL CODE ###########################
        # # Identify iterations with error
        # prblm_iter <- which(vapply(data, function(x) {
        #     msg <- x$results$`mlr-cwc.FE`$error_message
        #     !is.null(msg) && grepl("Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"", 
        #                            msg, 
        #                            fixed = TRUE)
        # }, logical(1)))
        # 
        # # Drop iterations with error 
        # if (length(prblm_iter)) {
        #     data <- data[-prblm_iter]
        #     message(sprintf("File %d: Dropping %d iterations with error msg at indices: %s (%s)",
        #                     i, length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(file)))
        # }
        # ############################################

        # Identify iterations with error in ANY method
        prblm_iter <- which(vapply(data, function(x) {
            
            # Extract error messages for relevant methods
            msgs <- vapply(methd, function(k) {
                err_m <- x$results[[k]]$error_message
                if (is.null(err_m)) "" else err_m
            }, character(1))
            
            # Check for the specific fatal error
            any(grepl(error_msg, msgs, fixed = TRUE))
            
        }, logical(1)))
        
        # Drop iterations with error
        if (length(prblm_iter)) {
            data <- data[-prblm_iter]
            message(sprintf(
                "File %d: Dropping %d iterations with error msg at indices: %s (%s)",
                i, length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(file)
            ))
        }
        
        return(data)
    }), 
    nm = cond_ids
)
# File 9: Dropping 1 iterations with error msg at indices: 661 (S1_condition-09_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds)
# File 52: Dropping 1 iterations with error msg at indices: 198 (S1_condition-52_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds)
# File 56: Dropping 3 iterations with error msg at indices: 403, 502, 538 (S1_condition-56_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 57: Dropping 5 iterations with error msg at indices: 68, 238, 369, 458, 978 (S1_condition-57_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)
# File 58: Dropping 2 iterations with error msg at indices: 696, 721 (S1_condition-58_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds)
# File 59: Dropping 5 iterations with error msg at indices: 159, 330, 378, 616, 696 (S1_condition-59_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 60: Dropping 2 iterations with error msg at indices: 485, 666 (S1_condition-60_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)
# File 73: Dropping 4 iterations with error msg at indices: 79, 333, 875, 1024 (S1_condition-73_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds)
# File 74: Dropping 9 iterations with error msg at indices: 165, 205, 394, 399, 496, 595, 665, 771, 975 (S1_condition-74_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds)
# File 75: Dropping 4 iterations with error msg at indices: 699, 755, 852, 857 (S1_condition-75_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds)
# File 77: Dropping 2 iterations with error msg at indices: 145, 605 (S1_condition-77_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds)
# File 78: Dropping 2 iterations with error msg at indices: 199, 973 (S1_condition-78_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds)
# File 79: Dropping 6 iterations with error msg at indices: 18, 222, 686, 786, 814, 915 (S1_condition-79_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds)
# File 80: Dropping 3 iterations with error msg at indices: 295, 919, 1030 (S1_condition-80_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 81: Dropping 5 iterations with error msg at indices: 43, 297, 438, 772, 977 (S1_condition-81_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)
# File 82: Dropping 1 iterations with error msg at indices: 476 (S1_condition-82_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds)
# File 83: Dropping 3 iterations with error msg at indices: 168, 827, 913 (S1_condition-83_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 84: Dropping 1 iterations with error msg at indices: 651 (S1_condition-84_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)

# Save overall simulation output list for reference later
saveRDS(overall_list, 
        file = file.path(results_path, "Data", 
                         paste0("S1_overall-output-list_", sim_date, ".rds")))

# Close log
sink()


# Create dataframe version ------------------------------------------------

# Note: 
#   Simulation was conducted on TACC system. This script processes the simulation 
#   output. However, to replace the "problematic"/extreme cases, run the 
#   "02a2_S1-results-processing.R" script (which re-runs these cases on 
#   your computer, a different operating system than TACC). 


## Import data  ------------------------------------------------------------

# ══════════════════════════════
#    check for matches & duplicates  
# ══════════════════════════════
# Create a normalized ID column for matching
conditions_str <- conditions |> 
    mutate(
        file_pattern = paste0(
            "null-", toupper(as.character(if.null)),
            "_quad-", toupper(as.character(quadratic)),
            "_M-", Mfamily,
            "_Y-", Yfamily,
            "_nj-[", Nj_low, "-", Nj_high, "]",
            "_J-", J,
            ".rds"
        )
    )

# Normalize rds file names (get just the tail filename)
rds_filenames <- sub(".*(null-)", "\\1", basename(rds_files_all))

# Check for missing
conditions_str <- conditions_str |> 
    mutate(in_rds = file_pattern %in% rds_filenames)

missing_conditions <- filter(conditions_str, !in_rds)

if (nrow(missing_conditions) == 0) {
    message("No missing conditions!")
} else {
    message("Missing conditions: \n")
    print(missing_conditions)
}

# Check for duplicates 
## Extract normalized file patterns from rds_files_all
rds_pattern_extracted <- str_extract(
    rds_filenames,
    "null-[^_]+_quad-[^_]+_M-[^_]+_Y-[^_]+_nj-\\[[^\\]]+\\]_J-[0-9]+\\.rds"
)

duplicates_df <- tibble(file_pattern = rds_pattern_extracted[duplicated(rds_pattern_extracted)]) |>
    count(file_pattern, name = "count") |>
    filter(count > 1)

if (nrow(duplicates_df) == 0) {
    message("No duplicates!")
} else {
    message("Duplicate entries in rds_files_all:\n")
    print(duplicates_df)
}


# Read each file and attach its file name for later extraction of condition information
all_data_list <- lapply(rds_files_all, function(file) {
    data <- readRDS(file)
    data$file <- file  # store file name within the data object
    data
})

# Initialize the main data frame to store all simulation results
sim1_data <- NULL

## Helper to extract value (or insert NA)
get1 <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) default else x[[1]]
}

# Loop through each file's data and process the simulation output
for (file_data in all_data_list) {
    
    # Get the file name from the data (we will parse this to extract condition info)
    fname <- file_data$file
    
    message(sprintf("--------------------------------- \ncond_%s: ", 
                    str_extract(basename(fname), "(?<=condition-)[0-9]{2}")))
    
    # Remove the "file" element from file_data so that only simulation iterations remain
    iter_data <- file_data
    if ("file" %in% names(file_data)) {
        iter_data <- file_data[names(file_data) != "file"]
    }

    # Identify iterations with error
    error_iter <- which(vapply(iter_data, function(it) {
        msg <- it$results$`mlr-cwc.FE`$error_message
        !is.null(msg) && grepl(
            "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"",
            msg,
            fixed = TRUE
        )
    }, logical(1)))
    
    # Drop iterations with error 
    if (length(error_iter)) {
        iter_data <- iter_data[-error_iter]
    }
    message(sprintf("Dropping %d iterations with error msg at indices %s: %s", 
                    length(error_iter), paste(error_iter, collapse = ", "), basename(fname)))
    
    # # Identify iterations known to be problematic 
    # prblm_iter <- which(vapply(iter_data, function(iter) {
    #     iter$raw_iteration %in% c(1956, 3474, 760, 14436)
    # }, logical(1)))
    # 
    # # Drop known problematic iterations (1956 & 3474 for cond 68 and 760 & 14436 for cond 69)
    # if (length(prblm_iter)) {
    #     iter_data <- iter_data[-prblm_iter]
    # }
    # message(sprintf("Dropping %d iterations with known problematic at indices %s: %s", 
    #                 length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(fname)))
    
    # Extract condition number 
    condition_number <- str_extract(basename(fname), "(?<=condition-)[0-9]{2}")
    # Extract condition information from the file name using regex; expected pattern: "reps-200_quad-FALSE_M-gaussian_Y-gaussian_nj-[5,20]_J-100"
    pattern <- "reps-([0-9]+)_null-([A-Za-z]+)_quad-([A-Za-z]+)_M-([A-Za-z]+)_Y-([A-Za-z]+)_nj-\\[([^\\]]+)\\]_J-([0-9]+)"
    matches <- str_match(fname, pattern)
    
    if (is.na(matches[1,1])) {
        warning("File name ", fname, " does not match the expected pattern. Setting condition info to NA.")
        reps_val <- NA
        null_val <- NA
        quad_val <- NA
        M_val <- NA
        Y_val <- NA
        nj_val <- NA
        J_val <- NA
    } else {
        reps_val <- as.numeric(matches[1,2])
        # Convert null to logical if it is "TRUE" or "FALSE"
        null_val <- matches[1,3]
        if (tolower(null_val) == "true") {
            null_val <- TRUE
        } else if (tolower(null_val) == "false") {
            null_val <- FALSE
        }
        # Convert quad to logical if it is "TRUE" or "FALSE"
        quad_val <- matches[1,4]
        if (tolower(quad_val) == "true") {
            quad_val <- TRUE
        } else if (tolower(quad_val) == "false") {
            quad_val <- FALSE
        }
        M_val <- matches[1,5]
        Y_val <- matches[1,6]
        nj_val <- matches[1,7]   # this remains as a string (e.g., "5,20")
        J_val <- as.numeric(matches[1,8])
    }
    
    overall_models <- NULL  # to store processed results for the current file
    
    # Keep only the first desired number of replications (e.g., `reps`; 200) to avoid cases from overrun parallelization
    if (length(iter_data) > reps) {
        message(glue("Trimming to first {reps} iterations from {length(iter_data)} for file: {basename(fname)}"))
        iter_data <- iter_data[seq_len(reps)]
    }
    
    # Loop through each simulation iteration in the file
    for (i in seq_along(iter_data)) {
        # Loop through each model (each row in 'methds')
        for (mod in 1:nrow(methds)) {
            # Create a key to extract the correct results from the simulation object
            key <- glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")
            
            res <- iter_data[[i]]$results[[key]]
            if (is.null(res) || length(res) == 0) res <- list()
            
            estimates <- iter_data[[i]]$results[[key]]$estimates
            if (is.null(estimates) || length(estimates) == 0) {
                estimates <- tibble(
                    Effect = character(),
                    EffectVersion = character(),
                    Estimate = as.numeric(),
                    StdError = as.numeric(),
                    CILower = as.numeric(),
                    CIUpper = as.numeric()
                )
            } 
            
            # Extract individual and cluster estimates 
            individual_de <- estimates %>% 
                filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) %>% 
                slice(1)
            individual_ie <- estimates %>% 
                filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect)) %>% 
                slice(1)
            cluster_de <- estimates %>% 
                filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) %>% 
                slice(1)
            cluster_ie <- estimates %>% 
                filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) %>% 
                slice(1)
            
            # Extract individual effects & fix NAs
            individual_effects <- iter_data[[i]]$effects$individual
            # if (is.null(individual_effects) || length(individual_effects) == 0) {
            #     list(pnde = NA_real_, tnie = NA_real_, tnde = NA_real_, pnie = NA_real_)
            # } 
            if (is.null(individual_effects) || length(individual_effects) == 0) {
                individual_effects <- list(
                    pnde = NA_real_, tnie = NA_real_,
                    tnde = NA_real_, pnie = NA_real_
                )
            } 
            
            # Extract cluster effects & fix NAs
            cluster_effects <- iter_data[[i]]$effects$cluster
            # if (is.null(cluster_effects) || length(cluster_effects) == 0) {
            #     list(pnde = NA_real_, tnie = NA_real_, tnde = NA_real_, pnie = NA_real_)
            # } 
            if (is.null(cluster_effects) || length(cluster_effects) == 0) {
                cluster_effects <- list(
                    pnde = NA_real_, tnie = NA_real_,
                    tnde = NA_real_, pnie = NA_real_
                )
            } 
            
            # Warnings fields 
            warns <- res$warnings
            if (is.null(warns)) warns <- character(0)
            warnings_chr <- if (length(warns) == 0) NA_character_ else paste(warns, collapse="; ")
            n_warnings   <- length(warns)
            
            # Error fields
            error_flag <- res$error
            error_msg <- res$error_message
            if (is.null(error_msg)) error_msg <- NA_character_
            
            num_folds <- res$num_folds
            if (is.null(num_folds)) num_folds <- NA_integer_

            raw_it <- iter_data[[i]]$raw_iteration 
            if (is.null(raw_it)) raw_it <- NA_integer_
            
            ps_overlap   <- iter_data[[i]]$overlap$ps_summary 
            if (is.null(ps_overlap)) ps_overlap <- NA_character_
            
            clust_trt_prop_chr <- toString(round(iter_data[[i]]$parameters$clust_trt_prop, 2))
            if (is.null(clust_trt_prop_chr)) clust_trt_prop_chr <- NA_real_
            
            nj_sizes_chr <- toString(iter_data[[i]]$parameters$nj_sizes)
            if (is.null(nj_sizes_chr)) nj_sizes_chr <- NA
            
            # Create a single-row tibble for this model result, including condition info from the file name
            model_row <- tibble(
                file_name = fname,                # full file name for reference
                condition_num = condition_number, # extracted condition number 
                reps = reps_val,                  # extracted reps value
                ifnull = null_val, 
                quadratic = quad_val,             # extracted quadratic indicator
                Mfamily = M_val,                  # extracted M family
                Yfamily = Y_val,                  # extracted Y family
                nj = nj_val,                      # extracted nj information (as string)
                J = J_val,                        # extracted J value
                iteration = i,
                raw_iteration = iter_data[[i]]$raw_iteration, # raw_it
                model = mod,
                Fit = methds[mod, "Fit"],
                cluster_opt = methds[mod, "cluster_opt"],
                # warnings = if (length(iter_data[[i]]$results[[key]]$warnings) == 0) {
                #     NA
                # } else {
                #     paste(iter_data[[i]]$results[[key]]$warnings, collapse = "; ")
                # }, 
                warnings = warnings_chr, 
                n_warnings = n_warnings, # n_warnings = length(iter_data[[i]]$results[[key]]$warnings), 
                error = error_flag, # error = iter_data[[i]]$results[[key]]$error,
                error_message = error_msg, # error_message = iter_data[[i]]$results[[key]]$error_message,
                num_folds = num_folds, # num_folds = iter_data[[i]]$results[[key]]$num_folds,
                
                # Individual Direct Effect (DE) estimates
                # individual_de_Estimate = individual_de$Estimate,
                # individual_de_StdError = individual_de$StdError,
                # individual_de_CILower  = individual_de$CILower,
                # individual_de_CIUpper  = individual_de$CIUpper,
                individual_de_Estimate = get1(individual_de$Estimate),
                individual_de_StdError = get1(individual_de$StdError),
                individual_de_CILower  = get1(individual_de$CILower),
                individual_de_CIUpper  = get1(individual_de$CIUpper),
                
                # Individual Indirect Effect (IE) estimates
                # individual_ie_Estimate = individual_ie$Estimate,
                # individual_ie_StdError = individual_ie$StdError,
                # individual_ie_CILower  = individual_ie$CILower,
                # individual_ie_CIUpper  = individual_ie$CIUpper,
                individual_ie_Estimate = get1(individual_ie$Estimate),
                individual_ie_StdError = get1(individual_ie$StdError),
                individual_ie_CILower  = get1(individual_ie$CILower),
                individual_ie_CIUpper  = get1(individual_ie$CIUpper),
                
                # Cluster Direct Effect (DE) estimates
                # cluster_de_Estimate = cluster_de$Estimate,
                # cluster_de_StdError = cluster_de$StdError,
                # cluster_de_CILower  = cluster_de$CILower,
                # cluster_de_CIUpper  = cluster_de$CIUpper,
                cluster_de_Estimate = get1(cluster_de$Estimate),
                cluster_de_StdError = get1(cluster_de$StdError),
                cluster_de_CILower  = get1(cluster_de$CILower),
                cluster_de_CIUpper  = get1(cluster_de$CIUpper),
                
                # Cluster Indirect Effect (IE) estimates
                # cluster_ie_Estimate = cluster_ie$Estimate,
                # cluster_ie_StdError = cluster_ie$StdError,
                # cluster_ie_CILower  = cluster_ie$CILower,
                # cluster_ie_CIUpper  = cluster_ie$CIUpper,
                cluster_ie_Estimate = get1(cluster_ie$Estimate),
                cluster_ie_StdError = get1(cluster_ie$StdError),
                cluster_ie_CILower  = get1(cluster_ie$CILower),
                cluster_ie_CIUpper  = get1(cluster_ie$CIUpper),
                
                # Individual effects (e.g., pnde, tnie, tnde, pnie)
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                
                # Cluster effects (e.g., pnde, tnie, tnde, pnie)
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie,
                
                # Propensity score overlap summary
                # ps_overlap = iter_data[[i]]$overlap$ps_summary, 
                ps_overlap = ps_overlap,
                
                # Proportion of clusters in trt
                # clust_trt_prop = toString(round(iter_data[[i]]$parameters$clust_trt_prop, 2)), 
                clust_trt_prop = clust_trt_prop_chr,
                
                # Cluster sizes
                # nj_sizes = toString(iter_data[[i]]$parameters$nj_sizes)
                nj_sizes = nj_sizes_chr
            )
            
            # Append this row to the overall models for the current file
            overall_models <- bind_rows(overall_models, model_row)
        } # end loop over models
    }   # end loop over iterations
    
    # Append the current file's processed results to the master data frame
    sim1_data <- bind_rows(sim1_data, overall_models)
}

# --------------------------------- 
#     cond_01: 
#     Dropping 0 iterations with error msg at indices : S1_condition-01_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-01_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_02: 
#     Dropping 0 iterations with error msg at indices : S1_condition-02_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-02_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_03: 
#     Dropping 0 iterations with error msg at indices : S1_condition-03_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-03_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_04: 
#     Dropping 0 iterations with error msg at indices : S1_condition-04_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-04_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_05: 
#     Dropping 0 iterations with error msg at indices : S1_condition-05_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-05_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_06: 
#     Dropping 0 iterations with error msg at indices : S1_condition-06_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1055 for file: S1_condition-06_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_07: 
#     Dropping 0 iterations with error msg at indices : S1_condition-07_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-07_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_08: 
#     Dropping 0 iterations with error msg at indices : S1_condition-08_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-08_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_09: 
#     Dropping 1 iterations with error msg at indices 661: S1_condition-09_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1057 for file: S1_condition-09_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_10: 
#     Dropping 0 iterations with error msg at indices : S1_condition-10_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-10_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_11: 
#     Dropping 0 iterations with error msg at indices : S1_condition-11_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-11_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_12: 
#     Dropping 0 iterations with error msg at indices : S1_condition-12_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-12_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_13: 
#     Dropping 0 iterations with error msg at indices : S1_condition-13_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-13_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_14: 
#     Dropping 0 iterations with error msg at indices : S1_condition-14_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-14_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_15: 
#     Dropping 0 iterations with error msg at indices : S1_condition-15_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-15_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_16: 
#     Dropping 0 iterations with error msg at indices : S1_condition-16_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-16_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_17: 
#     Dropping 0 iterations with error msg at indices : S1_condition-17_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-17_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_18: 
#     Dropping 0 iterations with error msg at indices : S1_condition-18_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1057 for file: S1_condition-18_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_19: 
#     Dropping 0 iterations with error msg at indices : S1_condition-19_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-19_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_20: 
#     Dropping 0 iterations with error msg at indices : S1_condition-20_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-20_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_21: 
#     Dropping 0 iterations with error msg at indices : S1_condition-21_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-21_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_22: 
#     Dropping 0 iterations with error msg at indices : S1_condition-22_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-22_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_23: 
#     Dropping 0 iterations with error msg at indices : S1_condition-23_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-23_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_24: 
#     Dropping 0 iterations with error msg at indices : S1_condition-24_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1055 for file: S1_condition-24_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_25: 
#     Dropping 0 iterations with error msg at indices : S1_condition-25_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-25_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_26: 
#     Dropping 0 iterations with error msg at indices : S1_condition-26_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-26_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_27: 
#     Dropping 0 iterations with error msg at indices : S1_condition-27_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-27_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_28: 
#     Dropping 0 iterations with error msg at indices : S1_condition-28_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-28_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_29: 
#     Dropping 0 iterations with error msg at indices : S1_condition-29_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-29_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_30: 
#     Dropping 0 iterations with error msg at indices : S1_condition-30_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1056 for file: S1_condition-30_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_31: 
#     Dropping 0 iterations with error msg at indices : S1_condition-31_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-31_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_32: 
#     Dropping 0 iterations with error msg at indices : S1_condition-32_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-32_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_33: 
#     Dropping 0 iterations with error msg at indices : S1_condition-33_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-33_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_34: 
#     Dropping 0 iterations with error msg at indices : S1_condition-34_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-34_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_35: 
#     Dropping 0 iterations with error msg at indices : S1_condition-35_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-35_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_36: 
#     Dropping 0 iterations with error msg at indices : S1_condition-36_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1055 for file: S1_condition-36_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_37: 
#     Dropping 0 iterations with error msg at indices : S1_condition-37_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-37_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_38: 
#     Dropping 0 iterations with error msg at indices : S1_condition-38_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-38_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_39: 
#     Dropping 0 iterations with error msg at indices : S1_condition-39_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-39_reps-1050_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_40: 
#     Dropping 0 iterations with error msg at indices : S1_condition-40_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-40_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_41: 
#     Dropping 0 iterations with error msg at indices : S1_condition-41_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-41_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_42: 
#     Dropping 0 iterations with error msg at indices : S1_condition-42_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1055 for file: S1_condition-42_reps-1050_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_43: 
#     Dropping 0 iterations with error msg at indices : S1_condition-43_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1056 for file: S1_condition-43_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_44: 
#     Dropping 0 iterations with error msg at indices : S1_condition-44_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1052 for file: S1_condition-44_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_45: 
#     Dropping 0 iterations with error msg at indices : S1_condition-45_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1058 for file: S1_condition-45_reps-1050_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_46: 
#     Dropping 0 iterations with error msg at indices : S1_condition-46_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-46_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_47: 
#     Dropping 0 iterations with error msg at indices : S1_condition-47_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 1000 iterations from 1059 for file: S1_condition-47_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_48: 
#     Dropping 0 iterations with error msg at indices : S1_condition-48_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 1000 iterations from 1055 for file: S1_condition-48_reps-1050_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_49: 
#     Dropping 0 iterations with error msg at indices : S1_condition-49_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-49_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_50: 
#     Dropping 0 iterations with error msg at indices : S1_condition-50_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-50_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_51: 
#     Dropping 0 iterations with error msg at indices : S1_condition-51_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-51_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_52: 
#     Dropping 1 iterations with error msg at indices 198: S1_condition-52_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1049 for file: S1_condition-52_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_53: 
#     Dropping 0 iterations with error msg at indices : S1_condition-53_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-53_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_54: 
#     Dropping 0 iterations with error msg at indices : S1_condition-54_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-54_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_55: 
#     Dropping 0 iterations with error msg at indices : S1_condition-55_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-55_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_56: 
#     Dropping 3 iterations with error msg at indices 403, 502, 538: S1_condition-56_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1047 for file: S1_condition-56_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_57: 
#     Dropping 5 iterations with error msg at indices 68, 238, 369, 458, 978: S1_condition-57_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1045 for file: S1_condition-57_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_58: 
#     Dropping 1 iterations with error msg at indices 696: S1_condition-58_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1049 for file: S1_condition-58_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_59: 
#     Dropping 5 iterations with error msg at indices 159, 330, 378, 616, 696: S1_condition-59_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1046 for file: S1_condition-59_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_60: 
#     Dropping 2 iterations with error msg at indices 485, 666: S1_condition-60_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1048 for file: S1_condition-60_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_61: 
#     Dropping 0 iterations with error msg at indices : S1_condition-61_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-61_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_62: 
#     Dropping 0 iterations with error msg at indices : S1_condition-62_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-62_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_63: 
#     Dropping 0 iterations with error msg at indices : S1_condition-63_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-63_reps-1050_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_64: 
#     Dropping 0 iterations with error msg at indices : S1_condition-64_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-64_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_65: 
#     Dropping 0 iterations with error msg at indices : S1_condition-65_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-65_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_66: 
#     Dropping 0 iterations with error msg at indices : S1_condition-66_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-66_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_67: 
#     Dropping 0 iterations with error msg at indices : S1_condition-67_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-67_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_68: 
#     Dropping 0 iterations with error msg at indices : S1_condition-68_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-68_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_69: 
#     Dropping 0 iterations with error msg at indices : S1_condition-69_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-69_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_70: 
#     Dropping 0 iterations with error msg at indices : S1_condition-70_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-70_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_71: 
#     Dropping 0 iterations with error msg at indices : S1_condition-71_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 1000 iterations from 1051 for file: S1_condition-71_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_72: 
#     Dropping 0 iterations with error msg at indices : S1_condition-72_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 1000 iterations from 1050 for file: S1_condition-72_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds


# Save 
saveRDS(sim1_data, file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# ══════════════════════════════
#    DELETE CODE BELOW (in 02a3) 
# ══════════════════════════════
# Check warning & error messages ------------------------------------------

# Import sim dataframe 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# table(sim1_data[sim1_data$condition_num == "01" & sim1_data$cluster_opt == "RE.glm", "n_warnings"])
# table(sim1_data[sim1_data$condition_num == "01" & sim1_data$cluster_opt == "RE.glm", "error"])

# ══════════════════════════════
#    Error messages  
# ══════════════════════════════

# Record log
sink(file.path(logs_path, "error-messages.txt"), split = TRUE)

# How many errors occurred per method & condition combo
sim1_data |> 
    group_by(condition_num, Fit, cluster_opt) |> 
    summarize(n = n(), 
              n_error = sum(error)) |> 
    print(n = Inf)

# Most occurred for RE.glm (or mlr, cwc.FE)
sim1_data |> 
    group_by(condition_num, Fit, cluster_opt) |> 
    summarize(n = n(), 
              n_error = sum(error), 
              n_distinct_error = n_distinct(error_message, na.rm = TRUE), 
              error_messages = paste0(unique(na.omit(error_message)), collapse = " | ")) |> 
    filter(n_error > 0)

# A clearer look at the distinct error messages 
sim1_data |> 
    distinct(error_message)

# Close
sink()
    
# what iterations had the error: "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\""?
### used to be following conditions with errors: "cond_06", "cond_12", "cond_27"; cond_overall_list[[c("cond_06", "cond_12", "cond_27")]]
# cond_overall_list[[c("cond_30")]]

# conds <- c("cond_03") #c("cond_09") #c("cond_30") #c("cond_06", "cond_12", "cond_27")
# 
# # sim conditions & iteration numbers with error in cwc.FE to test after fixing fold function 
# lapply(conds, function(cond) {
#     which(vapply(
#         overall_list[[cond]], 
#         function(x) {
#             msg <- x$results$`mlr-cwc.FE`$error_message
#             !is.null(msg) && grepl("Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"", msg, fixed = T)
#         }, 
#         logical(1)
#     ))
# })
# 
# # [[1]]
# # [1] 57 # iteration 57 in condition 1 (2025-09-01) 
# 
# # [[1]]
# # [1] 77 # iteration 77 in condition 9 (2025-08-31)
# 
# # [[1]]
# # [1] 74 # iteration 74 in condition 30
# 
# ## used to have 3 errors: 
# # [[1]]
# # [1] 61
# # 
# # [[2]]
# # [1] 164
# # 
# # [[3]]
# # [1] 83



# ══════════════════════════════
#    Warning messages  
# ══════════════════════════════

# Record log
sink(file.path(logs_path, "warning-messages.txt"), split = TRUE)

# Create function to help clean warnings (to help detect near duplicates)
normalize_warning <- function(x) {
    x |> 
        str_to_lower() |> 
        # collapse whitespace
        str_replace_all("\\s+", " ") |> 
        # replace whitespaces with 1 space
        str_squish() |> 
        # replace numbers incl. decimals and scientific notation with a token
        str_replace_all("[-+]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?", "<num>") |> # detects number & replaces with <num>
        # optional: normalize common numeric-ish tokens
        str_replace_all("\\b(?:nan|inf|-inf)\\b", "<num>")
}

# Dataset with warning messages cleaned (i.e., formatted for easy use)
warnings_cleaned <- sim1_data  |> 
    filter(!is.na(warnings)) |> 
    separate_rows(warnings, sep = ";") |>     # split multiple warnings
    # distinct(warnings) |>                   # keep unique strings
    mutate(warnings = str_trim(warnings),     # clean whitespace @ beginning & end
           warning_template = normalize_warning(warnings)) 

# Distinct warnings across simulation conditions 
warnings_cleaned |> 
    distinct(warning_template) |> 
    arrange(warning_template)

# Frequency of unique warning messages across all simulation conditions 
warnings_cleaned |> 
    group_by(warning_template) |> 
    summarize(n = n(), 
              n_conditions = n_distinct(condition_num), 
              n_methods = n_distinct(Fit, cluster_opt),
              methods = paste(unique(paste(Fit, cluster_opt, sep = "-")), collapse = " | "), 
              example = dplyr::first(warnings),
              .groups = "drop") |> 
    arrange(desc(n)) |> 
    print(n = Inf)

# # Frequency of unique warning messages within each simulation conditions 
# warnings_cleaned |> 
#     group_by(condition_num, warning_template) |> 
#     summarize(n = n(), 
#               n_conditions = n_distinct(condition_num), 
#               example = dplyr::first(warnings),
#               .groups = "drop") |> 
#     arrange(desc(n)) |> 
#     print(n = Inf)

# Close log
sink()


# ══════════════════════════════
#    More cleaning of dataframe (drop unimportant warning messages)
# ══════════════════════════════

# Record log
sink(file.path(logs_path, "warning-messages.txt"), split = TRUE, append = TRUE)

# Import sim dataframe 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# Drop the RE custom messages (they simply say random intercept model was used in place of random slopes)
sim1_data_cleaned <- sim1_data |>
    mutate(
        warnings = ifelse(is.na(warnings), "", warnings), 
        # Split original warnings
        warning_list = str_split(warnings, ";\\s*"),
        
        # Remove RE.glm.rs messages
        warning_list_filtered = map(warning_list, ~ .x[!grepl("In .*\\(\\): RE\\.glm\\.rs specified but no random_slope_vars provided", .x)]),
        warning_list_filtered = map(warning_list_filtered, ~ .x[!grepl("using random intercept only \\(RE\\.glm\\)\\.", .x)]), 
        
        # Clean and recompute
        warning_list_filtered = map(warning_list_filtered, ~ unique(trimws(.x[.x != ""]))),
        
        warnings = map_chr(warning_list_filtered, ~ if (length(.x) > 0) paste(.x, collapse = "; ") else NA_character_),
        n_warnings = map_int(warning_list_filtered, length)
    ) |>
    select(-warning_list, -warning_list_filtered) 

# Table of number of warnings by model fitted
table(paste0(sim1_data_cleaned$Fit, "-", sim1_data_cleaned$cluster_opt), sim1_data_cleaned$n_warnings)

# Close log
sink()

# ══════════════════════════════
#    Look into warnings (clean to find distinct warning messages)
# ══════════════════════════════
# Drop NAs
warnings_raw <- sim1_data_cleaned$warnings[!is.na(sim1_data_cleaned$warnings)]

# Split compound warnings by semicolon, trim whitespace
warnings_split <- unlist(str_split(warnings_raw, pattern = ";\\s*"))

# Remove warnings like "In a.c(): RE.glm.rs specified but no random_slope_vars provided"
warnings_filtered <- lapply(warnings_split, function(wlist) {
    wlist[!grepl("In .*\\(\\): RE\\.glm\\.rs specified but no random_slope_vars provided", wlist)]
})

# Normalize repeated warnings and recombine into string
warnings_cleaned <- sapply(warnings_filtered, function(x) {
    if (length(x) == 0) return(NA_character_)  # preserve missing if all were filtered out
    paste(sort(unique(x)), collapse = "; ")
})

# Generalize numeric parts
warnings_generalized <- warnings_cleaned |>
    str_replace_all("max\\|grad\\| = [0-9\\.eE+-]+", "max|grad| = <number>") |>
    str_replace_all("tol = [0-9\\.eE+-]+", "tol = <number>") |>
    str_replace_all("Cluster '[0-9]+': minority count = [0-9]+ < V = [0-9]+", 
                    "Cluster '<number>': minority count = <number> < V = <number>") |> 
    str_replace_all("Cluster '[0-9]+': binary outcome only has one value", 
                    "Cluster '<number>': binary outcome only has one value") |> 
    trimws()

# Get distinct non-NA warning types
distinct_warnings <- unique(na.omit(warnings_generalized))

# Record log
sink(file.path(logs_path, "warning-messages.txt"), split = TRUE, append = TRUE)
# View results
print(distinct_warnings)
# Close log
sink()


# ══════════════════════════════
#    Saving different dataset versions 
# ══════════════════════════════

# Drop iterations where warnings != NA
sim1_data_nowarnings <- sim1_data_cleaned |>
    filter(n_warnings == 0) #|>
    # nrow()

# Save 
saveRDS(sim1_data_nowarnings, file = file.path(results_path, "Data", paste0("S1_simulation-data_", sim_date, "_excludes-warnings.rds")))


# Define set of convergence-related patterns to match any of the cases 
nonconvergence_patterns <- paste(
    c(
        # non-convergence
        "did not converge",
        "failure to converge",
        "failed to converge",
        # potetntially problematic (double check these messages)
        "Hessian is numerically singular",
        "degenerate.*Hessian"#,
        # "unable to evaluate scaled gradient",
        # "eigenvalue", # covers both large eigenvalue and eigenvalue ratio
        # "Residual degrees of freedom.*negative or zero"
    ),
    collapse = "|"
)

# Filter out rows with convergence-related warnings
sim1_data_converged <- sim1_data_cleaned |>
    filter(
        is.na(warnings) | !str_detect(warnings, regex(nonconvergence_patterns, ignore_case = TRUE))
    )

# Save the filtered dataset (excluding convergence warnings only)
saveRDS(sim1_data_converged, file = file.path(results_path, "Data", paste0("S1_simulation-data_", sim_date, "_converged-only.rds")))


# # ══════════════════════════════
# #    Examining specifically RE.glm nonconvergence/warnings 
# # ══════════════════════════════
# 
# sim1_data |> 
#     filter(cluster_opt == "RE.glm" & Mfamily == "binomial" & Yfamily == "binomial" & J == 40) |> # & nj == "50-100") |> 
#     count(condition_num, warnings, sort = T, name = "freq") |> 
#     arrange(condition_num) |> 
#     print(n = Inf)
#     

# 
# # 5a. Save the combined simulation data (including nonconverging cases)
# # saveRDS(sim1_data, file = file.path(results_path, "Data", paste0("S1_simulation-data_", sim_date, "_includes-nonconvergence.rds")))
# 
# # 5b. 
# ### [drop nonconverging cases]
# table(sim1_data$warnings, sim1_data$model)
# # Drop nonconverging cases 
# nrow(sim1_data[sim1_data$warnings != "glm.fit: algorithm did not converge", ])
# sim1_data_converg <- sim1_data |>
#     filter(warnings != "glm.fit: algorithm did not converge" | is.na(warnings))
# 
# ### [Save the combined simulation data (excluding nonconverging cases)]
# saveRDS(sim1_data_converg, file = file.path(results_path, "Data", paste0("S1_simulation-data_", sim_date, "_excludes-nonconvergence.rds")))
# 
# # 5c. 
# ### drop any cases with a warning (excluding warning about leave-one-out cross validation)
# nrow(sim1_data[!sim1_data$warnings %in% c("glm.fit: fitted probabilities numerically 0 or 1 occurred", "glm.fit: algorithm did not converge", "prediction from rank-deficient fit; attr(*, \"non-estim\") has doubtful cases"), ])
# sim1_data_nowarn <- sim1_data |> 
#     filter(!warnings %in% c("glm.fit: fitted probabilities numerically 0 or 1 occurred", "glm.fit: algorithm did not converge", "prediction from rank-deficient fit; attr(*, \"non-estim\") has doubtful cases"))
# ### Save 
# saveRDS(sim1_data_nowarn, file = file.path(results_path, "Data", paste0("S1_simulation-data_", sim_date, "_excludes-warnings.rds")))
# 
# 
# # Note: Maybe pull in each conditions list and extract true values and estimates and put into one large dataframe? 






# Compute Performance Measures --------------------------------------------

# ══════════════════════════════
#    For converging cases 
# ══════════════════════════════
# Import data 
## import data with only converging cases
sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", #linear_",
                                   sim_date, #"2025-01-12", #sim_date,
                                   "_converged-only.rds"))
# ## import data (with warnings excluded)
# sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", #linear_",
#                                    sim_date, #"2025-01-12", #sim_date,
#                                    "_excludes-warnings.rds"))
# ## Import original/overall data 
# sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# Compute performance measures 
perf_summary <- as.data.frame(sim1_data) |>
    mutate(ifnull = as.logical(ifnull)) |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj) |> 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |> 
    ungroup() |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
    mutate(
        if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
        if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
        if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
        if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
        
        sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
        sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
        sig_cluster_PNDE = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
        sig_cluster_TNIE = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
    ) |> 
    summarize(
        # Individual PNDE
        cover_individual_PNDE = mean(if_cover_ind_PNDE),
        bias_individual_PNDE = mean(individual_de_Estimate - true_individual_PNDE),
        MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2),
        power_individual_PNDE = mean(sig_individual_PNDE[ifnull == FALSE]),
        type1_individual_PNDE = mean(sig_individual_PNDE[ifnull == TRUE]),
        
        # Individual TNIE
        cover_individual_TNIE = mean(if_cover_ind_TNIE),
        bias_individual_TNIE = mean(individual_ie_Estimate - true_individual_TNIE),
        MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2),
        power_individual_TNIE = mean(sig_individual_TNIE[ifnull == FALSE]),
        type1_individual_TNIE = mean(sig_individual_TNIE[ifnull == TRUE]),
        
        # Cluster PNDE
        cover_cluster_PNDE = mean(if_cover_clust_PNDE),
        bias_cluster_PNDE = mean(cluster_de_Estimate - true_cluster_PNDE),
        MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2),
        power_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == FALSE]),
        type1_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == TRUE]),
        
        # Cluster TNIE
        cover_cluster_TNIE = mean(if_cover_clust_TNIE),
        bias_cluster_TNIE = mean(cluster_ie_Estimate - true_cluster_TNIE),
        MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2),
        power_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == FALSE]),
        type1_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == TRUE]),
        
        # True values
        true_individual_PNDE = mean(true_individual_PNDE),
        true_individual_TNIE = mean(true_individual_TNIE),
        true_cluster_PNDE = mean(true_cluster_PNDE),
        true_cluster_TNIE = mean(true_cluster_TNIE)
    )


# Save performance measures 
## Only include converged cases 
saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", 
                                    sim_date, "_converged-only.rds")) 
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", #linear_",
#                                     sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))
# Save performance measures (with warnings excluded)
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", 
#                                     sim_date, "_excludes-warnings.rds")) 


# ══════════════════════════════
#    For all cases 
# ══════════════════════════════

# Import data 
## Import original/overall data
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# Compute performance measures 
perf_summary <- as.data.frame(sim1_data) |>
    mutate(ifnull = as.logical(ifnull)) |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj) |> 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |> 
    ungroup() |> 
    group_by(ifnull, quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
    mutate(
        if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
        if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
        if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
        if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
        
        sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
        sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
        sig_cluster_PNDE    = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
        sig_cluster_TNIE    = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
    ) |> 
    summarize(
        # Individual PNDE
        cover_individual_PNDE = mean(if_cover_ind_PNDE),
        bias_individual_PNDE = mean(individual_de_Estimate - true_individual_PNDE),
        MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2),
        power_individual_PNDE = mean(sig_individual_PNDE[ifnull == FALSE]),
        type1_individual_PNDE = mean(sig_individual_PNDE[ifnull == TRUE]),
        
        # Individual TNIE
        cover_individual_TNIE = mean(if_cover_ind_TNIE),
        bias_individual_TNIE = mean(individual_ie_Estimate - true_individual_TNIE),
        MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2),
        power_individual_TNIE = mean(sig_individual_TNIE[ifnull == FALSE]),
        type1_individual_TNIE = mean(sig_individual_TNIE[ifnull == TRUE]),
        
        # Cluster PNDE
        cover_cluster_PNDE = mean(if_cover_clust_PNDE),
        bias_cluster_PNDE = mean(cluster_de_Estimate - true_cluster_PNDE),
        MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2),
        power_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == FALSE]),
        type1_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == TRUE]),
        
        # Cluster TNIE
        cover_cluster_TNIE = mean(if_cover_clust_TNIE),
        bias_cluster_TNIE = mean(cluster_ie_Estimate - true_cluster_TNIE),
        MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2),
        power_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == FALSE]),
        type1_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == TRUE]),
        
        # True values
        true_individual_PNDE = mean(true_individual_PNDE),
        true_individual_TNIE = mean(true_individual_TNIE),
        true_cluster_PNDE = mean(true_cluster_PNDE),
        true_cluster_TNIE = mean(true_cluster_TNIE)
    )

# Save performance measures 
## All cases
saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", #linear_",
                                    sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))


# Compute Convergence Rates -----------------------------------------------

# Import data 
## overall data 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))
## converged cases data 
sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 

# Modify data for visuals & merging  
sim1_data_converged <- sim1_data_converged |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
# dim(sim1_data[sim1_data$cluster_opt != "RE.glm", ])

# Calculate convergence rates & merge dataframes
convergence_rates <- sim1_data |> 
    # filter(cluster_opt != "RE.glm") |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
    group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarize(total_attempts = n(), .groups = "drop") |> 
    # merge 
    left_join(
        sim1_data_converged |> 
            # filter(cluster_opt != "RE.glm") |> 
            # mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
            # Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
            group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
            summarize(converged = n(), .groups = "drop"),
        by = c("ifnull", "quad", "Mfamily", "Yfamily", "J", "Nj", "Fit", "cluster_opt")
    ) |> 
    # compute convergence rates
    mutate(
        converged = ifelse(is.na(converged), 0, converged),
        nonconverged = total_attempts - converged,
        nonconvergence_rate = nonconverged / total_attempts,
        convergence_rate = converged / total_attempts, 
        Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
    ) 

# Save convergence rates 
saveRDS(convergence_rates, file = paste0(results_path, "/Tables/S1_convergence-rates_", 
                                         sim_date, ".rds"))

############################# END OF PROCESSING ################################


