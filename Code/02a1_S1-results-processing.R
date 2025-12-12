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
#       Summarize and report results for the simulation study 
#       (i.e., compute performance measures). 
#
# Inputs:
#   - Simulation output .rds files in: "Output/S1_Simulation-Output/2025-10-22_1000-reps/"
# 
# Outputs: 
#   - 
# 
# 
# To obtain output files from TACC, run a similar command in your terminal: 
#     
# scp -r "cameronmccann@ls6.tacc.utexas.edu:/home1/10384/cameronmccann/multiply-robust-causal-mediation copy/Output/S1_Simulation-Output/2025-10-22_1000-reps" \
# /Users/cameronmccann/Documents/Research-2025/multiply-robust-causal-mediation/Output/S1_Simulation-Output/
#
# Last Updated: 2025-12-11
#
#
# Notes:
#   To-Do
#       # filter out problematic iterations (1956 & 3474 for cond 68 and 760 & 14436 for cond 69) 
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
additional_folder_results <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 

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
if (!is.null(additional_folder_results)) {
    results_path <- file.path(results_root, additional_folder_results)
} else {
    results_path <- file.path(results_root, paste0(sim_date, "_", reps, "-reps"))
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







# Set date, reps, & folders ----------------------------------------------

# Date of simulation 
sim_date <- "2025-10-22" #"2025-09-03"# "2025-07-30" #"2025-07-21" #"2025-07-01" # (Note: Simulations were ran around 2025-02-08 & 2025-05-05) #"2025-02-08" #"2025-01-25" #Sys.Date() #"2025-01-23" #"2025-01-18" 

# Number of replications
reps <- 1000 #600#200 

# Create directory to store results 
## Results folder 
path <- "Output/S1_Results"
if (!dir.exists(path)) {
    dir.create(path)
}
### Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 
### Check if additional_folder_results is not NULL to add to path
if (!is.null(additional_folder_results)) {
    results_path <- file.path(path, additional_folder_results)
}
### Create directory 
if (!dir.exists(results_path)) {
    dir.create(results_path)
}
## Data, Figures, & Tables subfolders 
if (!dir.exists(paste0(results_path, "/Data"))) {
    dir.create(paste0(results_path, "/Data"))
}
if (!dir.exists(paste0(results_path, "/Figures"))) {
    dir.create(paste0(results_path, "/Figures"))
}
if (!dir.exists(paste0(results_path, "/Tables"))) {
    dir.create(paste0(results_path, "/Tables"))
}

# Simulation output path 
sim_output_path <- "Output/S1_Simulation-Output"
### where to pull output from 
additional_folder_output <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 
### Check if additional_folder_output is not NULL to add to path
if (!is.null(additional_folder_output)) {
    sim_output_path <- file.path(sim_output_path, additional_folder_output)
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
# Create overall simulation output list with padded condition numbers as element names (e.g., cond_01) & drop any iterations with errors
overall_list <- purrr::set_names(
    purrr::imap(rds_files_all, function(file, i) {
        # Load file 
        data <- readRDS(file)
        
        # Identify iterations with error
        prblm_iter <- which(vapply(data, function(x) {
            msg <- x$results$`mlr-cwc.FE`$error_message
            !is.null(msg) && grepl("Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"", 
                                   msg, 
                                   fixed = TRUE)
        }, logical(1)))
        
        # Drop iterations with error 
        if (length(prblm_iter)) {
            data <- data[-prblm_iter]
            message(sprintf("File %d: Dropping %d iterations with error msg at indices: %s (%s)",
                            i, length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(file)))
        }

        return(data)
    }), 
    nm = cond_ids
)
# File 9: Dropping 1 iterations with error msg at indices: 661 (S1_condition-09_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds)
# File 52: Dropping 1 iterations with error msg at indices: 198 (S1_condition-52_reps-1050_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds)
# File 56: Dropping 3 iterations with error msg at indices: 403, 502, 538 (S1_condition-56_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 57: Dropping 5 iterations with error msg at indices: 68, 238, 369, 458, 978 (S1_condition-57_reps-1050_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)
# File 58: Dropping 1 iterations with error msg at indices: 696 (S1_condition-58_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds)
# File 59: Dropping 5 iterations with error msg at indices: 159, 330, 378, 616, 696 (S1_condition-59_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds)
# File 60: Dropping 2 iterations with error msg at indices: 485, 666 (S1_condition-60_reps-1050_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds)

# Save overall simulation output list for reference later
saveRDS(overall_list, 
        file = file.path(results_path, "Data", 
                         paste0("S1_overall-output-list_", sim_date, ".rds")))


# Create dataframe version ------------------------------------------------

# Note: 
#   This does not include replacements for problematic cases. 
#   If you would like to include replacements, see: 
#   "Convert updated_overall_list to dataframe format" section in 
#   02a2_S1-results-processing.R.


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
    
    # Identify iterations known to be problematic 
    prblm_iter <- which(vapply(iter_data, function(iter) {
        iter$raw_iteration %in% c(1956, 3474, 760, 14436)
    }, logical(1)))
    
    # Drop known problematic iterations (1956 & 3474 for cond 68 and 760 & 14436 for cond 69)
    if (length(prblm_iter)) {
        iter_data <- iter_data[-prblm_iter]
    }
    message(sprintf("Dropping %d iterations with known problematic at indices %s: %s", 
                    length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(fname)))
    
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
#     Dropping 0 iterations with error msg at indices : S1_condition-01_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_02: 
#     Dropping 0 iterations with error msg at indices : S1_condition-02_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-02_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_03: 
#     Dropping 0 iterations with error msg at indices : S1_condition-03_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_04: 
#     Dropping 0 iterations with error msg at indices : S1_condition-04_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-04_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_05: 
#     Dropping 0 iterations with error msg at indices : S1_condition-05_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-05_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_06: 
#     Dropping 0 iterations with error msg at indices : S1_condition-06_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_07: 
#     Dropping 0 iterations with error msg at indices : S1_condition-07_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_08: 
#     Dropping 0 iterations with error msg at indices : S1_condition-08_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-08_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_09: 
#     Dropping 0 iterations with error msg at indices : S1_condition-09_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-09_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_10: 
#     Dropping 0 iterations with error msg at indices : S1_condition-10_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-10_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_11: 
#     Dropping 0 iterations with error msg at indices : S1_condition-11_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-11_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_12: 
#     Dropping 0 iterations with error msg at indices : S1_condition-12_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-12_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_13: 
#     Dropping 0 iterations with error msg at indices : S1_condition-13_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_14: 
#     Dropping 0 iterations with error msg at indices : S1_condition-14_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-14_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_15: 
#     Dropping 0 iterations with error msg at indices : S1_condition-15_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-15_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_16: 
#     Dropping 0 iterations with error msg at indices : S1_condition-16_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-16_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_17: 
#     Dropping 0 iterations with error msg at indices : S1_condition-17_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-17_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_18: 
#     Dropping 0 iterations with error msg at indices : S1_condition-18_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-18_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_19: 
#     Dropping 0 iterations with error msg at indices : S1_condition-19_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_20: 
#     Dropping 0 iterations with error msg at indices : S1_condition-20_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-20_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_21: 
#     Dropping 0 iterations with error msg at indices : S1_condition-21_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-21_reps-200_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_22: 
#     Dropping 0 iterations with error msg at indices : S1_condition-22_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-22_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_23: 
#     Dropping 0 iterations with error msg at indices : S1_condition-23_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-23_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_24: 
#     Dropping 0 iterations with error msg at indices : S1_condition-24_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-24_reps-200_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_25: 
#     Dropping 0 iterations with error msg at indices : S1_condition-25_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_26: 
#     Dropping 0 iterations with error msg at indices : S1_condition-26_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-26_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_27: 
#     Dropping 0 iterations with error msg at indices : S1_condition-27_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-27_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_28: 
#     Dropping 0 iterations with error msg at indices : S1_condition-28_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-28_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_29: 
#     Dropping 0 iterations with error msg at indices : S1_condition-29_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-29_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_30: 
#     Dropping 0 iterations with error msg at indices : S1_condition-30_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-30_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_31: 
#     Dropping 0 iterations with error msg at indices : S1_condition-31_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_32: 
#     Dropping 0 iterations with error msg at indices : S1_condition-32_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-32_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_33: 
#     Dropping 0 iterations with error msg at indices : S1_condition-33_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-33_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_34: 
#     Dropping 0 iterations with error msg at indices : S1_condition-34_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-34_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_35: 
#     Dropping 0 iterations with error msg at indices : S1_condition-35_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-35_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_36: 
#     Dropping 0 iterations with error msg at indices : S1_condition-36_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-36_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_37: 
#     Dropping 0 iterations with error msg at indices : S1_condition-37_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_38: 
#     Dropping 0 iterations with error msg at indices : S1_condition-38_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-38_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_39: 
#     Dropping 0 iterations with error msg at indices : S1_condition-39_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-39_reps-200_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_40: 
#     Dropping 0 iterations with error msg at indices : S1_condition-40_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-40_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_41: 
#     Dropping 0 iterations with error msg at indices : S1_condition-41_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-41_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_42: 
#     Dropping 0 iterations with error msg at indices : S1_condition-42_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-42_reps-200_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_43: 
#     Dropping 0 iterations with error msg at indices : S1_condition-43_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_44: 
#     Dropping 0 iterations with error msg at indices : S1_condition-44_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-44_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_45: 
#     Dropping 0 iterations with error msg at indices : S1_condition-45_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 207 for file: S1_condition-45_reps-200_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_46: 
#     Dropping 0 iterations with error msg at indices : S1_condition-46_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-46_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-10.rds
# --------------------------------- 
#     cond_47: 
#     Dropping 0 iterations with error msg at indices : S1_condition-47_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# Trimming to first 200 iterations from 208 for file: S1_condition-47_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-20.rds
# --------------------------------- 
#     cond_48: 
#     Dropping 0 iterations with error msg at indices : S1_condition-48_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# Trimming to first 200 iterations from 209 for file: S1_condition-48_reps-200_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[50-100]_J-40.rds
# --------------------------------- 
#     cond_49: 
#     Dropping 0 iterations with error msg at indices : S1_condition-49_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 202 for file: S1_condition-49_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_50: 
#     Dropping 0 iterations with error msg at indices : S1_condition-50_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_51: 
#     Dropping 0 iterations with error msg at indices : S1_condition-51_reps-200_null-FALSE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_52: 
#     Dropping 1 iterations with error msg at indices 198: S1_condition-52_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_53: 
#     Dropping 0 iterations with error msg at indices : S1_condition-53_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 201 for file: S1_condition-53_reps-200_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_54: 
#     Dropping 0 iterations with error msg at indices : S1_condition-54_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-54_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_55: 
#     Dropping 0 iterations with error msg at indices : S1_condition-55_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-55_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_56: 
#     Dropping 0 iterations with error msg at indices : S1_condition-56_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-56_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_57: 
#     Dropping 1 iterations with error msg at indices 68: S1_condition-57_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-57_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_58: 
#     Dropping 0 iterations with error msg at indices : S1_condition-58_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-58_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_59: 
#     Dropping 1 iterations with error msg at indices 159: S1_condition-59_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-59_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_60: 
#     Dropping 0 iterations with error msg at indices : S1_condition-60_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-60_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_61: 
#     Dropping 0 iterations with error msg at indices : S1_condition-61_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-61_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_62: 
#     Dropping 0 iterations with error msg at indices : S1_condition-62_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-62_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_63: 
#     Dropping 0 iterations with error msg at indices : S1_condition-63_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-63_reps-205_null-FALSE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_64: 
#     Dropping 0 iterations with error msg at indices : S1_condition-64_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-64_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_65: 
#     Dropping 0 iterations with error msg at indices : S1_condition-65_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-65_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_66: 
#     Dropping 0 iterations with error msg at indices : S1_condition-66_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-66_reps-205_null-FALSE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_67: 
#     Dropping 0 iterations with error msg at indices : S1_condition-67_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-67_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_68: 
#     Dropping 0 iterations with error msg at indices : S1_condition-68_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-68_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_69: 
#     Dropping 0 iterations with error msg at indices : S1_condition-69_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-69_reps-205_null-FALSE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_70: 
#     Dropping 0 iterations with error msg at indices : S1_condition-70_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-70_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_71: 
#     Dropping 0 iterations with error msg at indices : S1_condition-71_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-71_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_72: 
#     Dropping 0 iterations with error msg at indices : S1_condition-72_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-72_reps-205_null-FALSE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_73: 
#     Dropping 1 iterations with error msg at indices 79: S1_condition-73_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-73_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_74: 
#     Dropping 2 iterations with error msg at indices 165, 205: S1_condition-74_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 203 for file: S1_condition-74_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_75: 
#     Dropping 0 iterations with error msg at indices : S1_condition-75_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-75_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_76: 
#     Dropping 0 iterations with error msg at indices : S1_condition-76_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-76_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_77: 
#     Dropping 1 iterations with error msg at indices 145: S1_condition-77_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-77_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_78: 
#     Dropping 1 iterations with error msg at indices 199: S1_condition-78_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-78_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_79: 
#     Dropping 1 iterations with error msg at indices 18: S1_condition-79_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-79_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_80: 
#     Dropping 0 iterations with error msg at indices : S1_condition-80_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-80_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_81: 
#     Dropping 1 iterations with error msg at indices 43: S1_condition-81_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-81_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_82: 
#     Dropping 0 iterations with error msg at indices : S1_condition-82_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-82_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_83: 
#     Dropping 1 iterations with error msg at indices 168: S1_condition-83_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 204 for file: S1_condition-83_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_84: 
#     Dropping 0 iterations with error msg at indices : S1_condition-84_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-84_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-binomial_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_85: 
#     Dropping 0 iterations with error msg at indices : S1_condition-85_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-85_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_86: 
#     Dropping 0 iterations with error msg at indices : S1_condition-86_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-86_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_87: 
#     Dropping 0 iterations with error msg at indices : S1_condition-87_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-87_reps-205_null-TRUE_quad-TRUE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_88: 
#     Dropping 0 iterations with error msg at indices : S1_condition-88_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-88_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_89: 
#     Dropping 0 iterations with error msg at indices : S1_condition-89_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-89_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_90: 
#     Dropping 0 iterations with error msg at indices : S1_condition-90_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-90_reps-205_null-TRUE_quad-FALSE_M-binomial_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_91: 
#     Dropping 0 iterations with error msg at indices : S1_condition-91_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-91_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_92: 
#     Dropping 0 iterations with error msg at indices : S1_condition-92_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-92_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_93: 
#     Dropping 0 iterations with error msg at indices : S1_condition-93_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-93_reps-205_null-TRUE_quad-TRUE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 
#     cond_94: 
#     Dropping 0 iterations with error msg at indices : S1_condition-94_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# Trimming to first 200 iterations from 206 for file: S1_condition-94_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-40.rds
# --------------------------------- 
#     cond_95: 
#     Dropping 0 iterations with error msg at indices : S1_condition-95_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-95_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-70.rds
# --------------------------------- 
#     cond_96: 
#     Dropping 0 iterations with error msg at indices : S1_condition-96_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# Trimming to first 200 iterations from 205 for file: S1_condition-96_reps-205_null-TRUE_quad-FALSE_M-gaussian_Y-gaussian_nj-[5-20]_J-100.rds
# --------------------------------- 

# Save 
saveRDS(sim1_data, file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))


# Check warning & error messages ------------------------------------------

# Import sim dataframe 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# table(sim1_data[sim1_data$condition_num == "01" & sim1_data$cluster_opt == "RE.glm", "n_warnings"])
# table(sim1_data[sim1_data$condition_num == "01" & sim1_data$cluster_opt == "RE.glm", "error"])

# ══════════════════════════════
#    Error messages  
# ══════════════════════════════

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
    separate_rows(warnings, sep = ";") |>  # split multiple warnings
    # distinct(warnings) |>                   # keep unique strings
    mutate(warnings = str_trim(warnings), # clean whitespace @ beginning & end
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

# Frequency of unique warning messages within each simulation conditions 
warnings_cleaned |> 
    group_by(condition_num, warning_template) |> 
    summarize(n = n(), 
              n_conditions = n_distinct(condition_num), 
              example = dplyr::first(warnings),
              .groups = "drop") |> 
    arrange(desc(n)) |> 
    print(n = Inf)




#----------------------
## To Do:
# look at distinct error messages (& maybe frequency for each method)

### are these error messages concerning or just non-convergence 

# look at distinct warning messages 
# look at frequency of warning messages 
### are any warning messages concerning or related to convergence issues?
# address how you'll handle errors & warnings
# then create datasets (convergence & nonconvergence versions so you can compute convergence rates)
# graph results 
# 
# 
# 1. save version with nonconvergence cases
# 2. save version with noncoverging cases dropped 
# 3. visuals nonconvergence rate across conditions
# 4. create tables & figures of warning types & understand the warnings so we can address them 
# 5. create updated perforamnce measures & visualize results
# 6. write up updated results 
#----------------------


# ══════════════════════════════════════════════════════════════════════════════
# Next steps: 
# - write more code to look at unique warnings & errors 
# - then replace the duplicate messages that only differ by a value
# - last list out the truely unique warnings & errors
# - then work on dropping those considered non-converging 
# 
# ══════════════════════════════════════════════════════════════════════════════


# ══════════════════════════════
#    More cleaning of dataframe (drop unimportant warning messages)
# ══════════════════════════════
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
# table(sim1_data_cleaned$model, sim1_data_cleaned$n_warnings)
table(paste0(sim1_data_cleaned$Fit, "-", sim1_data_cleaned$cluster_opt), sim1_data_cleaned$n_warnings)
## rows (1-5) are models used; columns (0-4) are number of warnings
#       0     1     2     3     4
# 1     0     0 19169     6     0
# 2 15773  1497  1898     1     6
# 3 19175     0     0     0     0
# 4 19175     0     0     0     0
# 5 18562   248   320    42     3

# ══════════════════════════════
#    Look into warnings (clean to find distinct warning messages)
# ══════════════════════════════
# 1. Drop NAs
warnings_raw <- sim1_data_cleaned$warnings[!is.na(sim1_data_cleaned$warnings)]
# 2. Split compound warnings by semicolon, trim whitespace
warnings_split <- unlist(str_split(warnings_raw, pattern = ";\\s*"))
# 3. Remove warnings like "In a.c(): RE.glm.rs specified but no random_slope_vars provided"
warnings_filtered <- lapply(warnings_split, function(wlist) {
    wlist[!grepl("In .*\\(\\): RE\\.glm\\.rs specified but no random_slope_vars provided", wlist)]
})
# 4. Normalize repeated warnings and recombine into string
warnings_cleaned <- sapply(warnings_filtered, function(x) {
    if (length(x) == 0) return(NA_character_)  # preserve missing if all were filtered out
    paste(sort(unique(x)), collapse = "; ")
})
# 5. Generalize numeric parts
warnings_generalized <- warnings_cleaned |>
    str_replace_all("max\\|grad\\| = [0-9\\.eE+-]+", "max|grad| = <number>") |>
    str_replace_all("tol = [0-9\\.eE+-]+", "tol = <number>") |>
    str_replace_all("Cluster '[0-9]+': minority count = [0-9]+ < V = [0-9]+", 
                    "Cluster '<number>': minority count = <number> < V = <number>") |> 
    str_replace_all("Cluster '[0-9]+': binary outcome only has one value", 
                    "Cluster '<number>': binary outcome only has one value") |> 
    trimws()
# 6. Get distinct non-NA warning types
distinct_warnings <- unique(na.omit(warnings_generalized))

# 7. View results
print(distinct_warnings)
# [1] "Cluster '<number>': minority count = <number> < V = <number>"                                                                          
# [2] "not stratifying this cluster."                                                                                                         
# [3] "non-integer #successes in a binomial glm!"                                                                                             
# [4] "Cluster '<number>': binary outcome only has one value"                                                                                 
# [5] "glm.fit: fitted probabilities numerically 0 or 1 occurred"                                                                             
# [6] "glm.fit: algorithm did not converge"                                                                                                   
# [7] "Model failed to converge with max|grad| = <number> (tol = <number>, component 1)"                                                      
# [8] "prediction from rank-deficient fit"                                                                                                    
# [9] "attr(*, \"non-estim\") has doubtful cases"                                                                                             
# [10] "Model is nearly unidentifiable: large eigenvalue ratio\n - Rescale variables?"                                                         
# [11] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"                                                          
# [12] "unable to evaluate scaled gradient"                                                                                                    
# [13] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"                                                             
# [14] "iterations terminated prematurely because of singularities"                                                                            
# [15] "Error in algorithm SL.gam \n  The Algorithm will be removed from the Super Learner (i.e. given weight 0)"                              
# [16] "Model failed to converge: degenerate  Hessian with 2 negative eigenvalues"                                                             
# [17] "n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n"    
# [18] "Re-running estimation of coefficients removing failed algorithm(s)\nOriginal coefficients are: \n0.726183574907129, 0.273816425092871" 
# [19] "Re-running estimation of coefficients removing failed algorithm(s)\nOriginal coefficients are: \n0.00213038987352012, 0.99786961012648"

# Unique warnings by model 
sim1_data_cleaned |> 
    mutate(model_key = paste(Fit, cluster_opt, sep = "-")) |> 
    mutate(warning_list = str_split(warnings, ";\\s*")) |> 
    mutate(
        warning_list = map(warning_list, ~ .x[!grepl("RE\\.glm\\.rs specified.*|using random intercept only", .x)]),
        warning_list = map(warning_list, ~ trimws(.x[.x != ""])),
        warning_list = map(warning_list, unique),
        warning_string = map_chr(warning_list, ~ if (length(.x) > 0) paste(.x, collapse = "; ") else NA_character_)
    ) |> 
    mutate(
        warning_string = warning_string |>
            str_replace_all("max\\|grad\\| = [0-9\\.eE+-]+", "max|grad| = <number>") |>
            str_replace_all("tol = [0-9\\.eE+-]+", "tol = <number>")
    ) |> 
    filter(!is.na(warning_string)) |> 
    # # warnings by model 
    # count(model_key, warning_string, sort = TRUE, name = "frequency") |> 
    # warnings by model & data generation condition
    count(condition_num, model_key, warning_string, sort = TRUE, name = "frequency") |> 
    # count(model_key, warning_string, sort = TRUE, name = "frequency") |> 
    # group_by(model_key) |> 
    # summarise(unique_warnings = unique(warning_string), .groups = "drop") |> 
    arrange(condition_num, model_key) |> 
    # arrange(model_key) |> 
    print(n = Inf)

# Commented out section below is old


# unique(sim1_data$warnings)
# view(table(sim1_data$warnings, sim1_data$model))


# ══════════════════════════════
#    Saving different versions 
# ══════════════════════════════

# Drop iterations where warnings != NA
sim1_data_nowarnings <- sim1_data_cleaned |>
    filter(n_warnings == 0) #|>
    # nrow()
## Save 
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




# NEW SECTION TESTING -----------------------------------------------------

# working on looking at different warnings and probably dropping nonconvergent cases/iterations

# sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_includes-warnings.rds"))



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


# ══════════════════════════════
#    2025-08-11: 
# maybe start with this chunk of code to split null and nonnull results separately if you have issues again with the code below this chunk


# # Split data into null & non-null dataframes
# sim1_data_null <- filter(sim1_data, ifnull == TRUE)
# sim1_data_notnull <- filter(sim1_data, ifnull == FALSE)
# 
# # Compute performance measures for null cases 
# perf_summary_null <- as.data.frame(sim1_data_null) |> 
#     group_by(quadratic, Mfamily, Yfamily, J, nj) |> 
#     mutate(true_individual_PNDE = mean(individual_pnde), 
#            true_individual_TNIE = mean(individual_tnie), 
#            true_cluster_PNDE = mean(cluster_pnde), 
#            true_cluster_TNIE = mean(cluster_tnie)) |> 
#     ungroup() |> 
#     group_by(quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
#     mutate(
#         if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
#         if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
#         if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
#         if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE),
#         
#         sig_individual_PNDE = (individual_de_CILower > 0) | (individual_de_CIUpper < 0), # indicate rejection of null
#         sig_individual_TNIE = (individual_ie_CILower > 0) | (individual_ie_CIUpper < 0),
#         sig_cluster_PNDE    = (cluster_de_CILower > 0) | (cluster_de_CIUpper < 0),
#         sig_cluster_TNIE    = (cluster_ie_CILower > 0) | (cluster_ie_CIUpper < 0)
#     ) |> 
#     summarize(
#         # Individual PNDE
#         cover_individual_PNDE = mean(if_cover_ind_PNDE),
#         bias_individual_PNDE = mean(individual_de_Estimate - true_individual_PNDE),
#         MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2),
#         power_individual_PNDE = mean(sig_individual_PNDE[ifnull == FALSE]),
#         type1_individual_PNDE = mean(sig_individual_PNDE[ifnull == TRUE]),
#         
#         # Individual TNIE
#         cover_individual_TNIE = mean(if_cover_ind_TNIE),
#         bias_individual_TNIE = mean(individual_ie_Estimate - true_individual_TNIE),
#         MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2),
#         power_individual_TNIE = mean(sig_individual_TNIE[ifnull == FALSE]),
#         type1_individual_TNIE = mean(sig_individual_TNIE[ifnull == TRUE]),
#         
#         # Cluster PNDE
#         cover_cluster_PNDE = mean(if_cover_clust_PNDE),
#         bias_cluster_PNDE = mean(cluster_de_Estimate - true_cluster_PNDE),
#         MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2),
#         power_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == FALSE]),
#         type1_cluster_PNDE = mean(sig_cluster_PNDE[ifnull == TRUE]),
#         
#         # Cluster TNIE
#         cover_cluster_TNIE = mean(if_cover_clust_TNIE),
#         bias_cluster_TNIE = mean(cluster_ie_Estimate - true_cluster_TNIE),
#         MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2),
#         power_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == FALSE]),
#         type1_cluster_TNIE = mean(sig_cluster_TNIE[ifnull == TRUE]),
#         
#         # True values
#         true_individual_PNDE = mean(true_individual_PNDE),
#         true_individual_TNIE = mean(true_individual_TNIE),
#         true_cluster_PNDE = mean(true_cluster_PNDE),
#         true_cluster_TNIE = mean(true_cluster_TNIE)
#     )
# ══════════════════════════════

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


# # Compute performance measures 
# perf_summary <- as.data.frame(sim1_data) |>
#     group_by(quadratic, Mfamily, Yfamily, J, nj) |> # get one true value per condition (add if.null later)
#     mutate(true_individual_PNDE = mean(individual_pnde), 
#            true_individual_TNIE = mean(individual_tnie), 
#            true_cluster_PNDE = mean(cluster_pnde), 
#            true_cluster_TNIE = mean(cluster_tnie)) |> 
#     ungroup() |> 
    # group_by(quadratic, Mfamily, Yfamily, J, nj, Fit, cluster_opt) |>
#     mutate(if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
#            if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
#            if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
#            if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE) ) |> 
#     # filter(J == 100 & cluster_opt == "cwc.FE" & Fit == "mlr3") |> 
#     # view()
#     summarize(cover_individual_PNDE = mean(if_cover_ind_PNDE), 
#               cover_individual_TNIE = mean(if_cover_ind_TNIE), 
#               bias_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)), 
#               bias_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)), 
#               MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2), 
#               MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2), 
#               rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))), 
#               rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))),
#               true_individual_PNDE = mean(true_individual_PNDE), 
#               true_individual_TNIE = mean(true_individual_TNIE),
#               # cluster-avg
#               cover_cluster_PNDE = mean(if_cover_clust_PNDE), 
#               cover_cluster_TNIE = mean(if_cover_clust_TNIE), 
#               bias_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)), 
#               bias_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)), 
#               MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2), 
#               MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2), 
#               rejectnull_cluster_PNDE = mean((cluster_de_CILower > (0)) | (cluster_de_CIUpper < (0))), 
#               rejectnull_cluster_TNIE = mean((cluster_ie_CILower > (0)) | (cluster_ie_CIUpper < (0))),
#               true_cluster_PNDE = mean(true_cluster_PNDE), 
#               true_cluster_TNIE = mean(true_cluster_TNIE)
#     ) 



# |>
    # view()

    # group_by(if.null, quadratic, Mfamily, Yfamily, J, Nj_low, Nj_high) |> # get one true value per condition
    # # group_by(quadratic, condition) |> # get one true value per condition 
    # mutate(true_individual_PNDE = mean(individual_pnde), 
    #        true_individual_TNIE = mean(individual_tnie), 
    #        true_cluster_PNDE = mean(cluster_pnde), 
    #        true_cluster_TNIE = mean(cluster_tnie)) |>
    # group_by(if.null, quadratic, Mfamily, Yfamily, J, Nj_low, Nj_high, Fit, cluster_opt) |> 
    # # group_by(quadratic, condition, model) |> 
    # mutate(if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
    #        if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
    #        if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
    #        if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE) ) |> 
    # summarize(cover_individual_PNDE = mean(if_cover_ind_PNDE), 
    #           cover_individual_TNIE = mean(if_cover_ind_TNIE), 
    #           bias_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)), 
    #           bias_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)), 
    #           MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2), 
    #           MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2), 
    #           rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))), 
    #           rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))),
    #           true_individual_PNDE = mean(true_individual_PNDE), 
    #           true_individual_TNIE = mean(true_individual_TNIE), 
    #           # cluster-avg
    #           cover_cluster_PNDE = mean(if_cover_clust_PNDE), 
    #           cover_cluster_TNIE = mean(if_cover_clust_TNIE), 
    #           bias_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)), 
    #           bias_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)), 
    #           MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2), 
    #           MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2), 
    #           rejectnull_cluster_PNDE = mean((cluster_de_CILower > (0)) | (cluster_de_CIUpper < (0))), 
    #           rejectnull_cluster_TNIE = mean((cluster_ie_CILower > (0)) | (cluster_ie_CIUpper < (0))),
    #           true_cluster_PNDE = mean(true_cluster_PNDE), 
    #           true_cluster_TNIE = mean(true_cluster_TNIE)
    # )

# table(perf_summary$quadratic)
# 
# perf_summary

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






# Report results ----------------------------------------------------------

# import data if needed 
perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, ".rds")) 
sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, ".rds"))

# Modify data for visuals 
perf_measures <- perf_measures |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"), # Nj = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) 
           cluster_opt2 = ifelse(cluster_opt == "cwc", "mean", "mean + dummies"), 
           Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
    )
    
    
sim_data <- sim_data |> 
    mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
           # Nj = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]"))


## Visuals -----------------------------------------------------------------

# visual settings
gglayer_theme <- list(theme_bw(),
                      scale_color_manual(values = c("#BF5700", #Fixed-effect
                                                    "#A6CD57", #Random-effect
                                                    "#333F48")), #Single-level
                      scale_fill_manual(values = c("#BF5700", #Fixed-effect
                                                   "#A6CD57", #Random-effect
                                                   "#333F48")), #Single-level
                      #"#9CADB7" <-- light gray
                      # Used following website with university colors: https://projects.susielu.com/viz-palette?
                      theme(text = element_text(family = "arial", size = 12), # "Times New Roman", size = 12), #
                            title = element_text(size = 16),
                            axis.title = element_text(size = 14),  # Adjust axis title size
                            axis.text = element_text(size = 12),  # Adjust axis text size
                            legend.title = element_text(size = 14), #12),  # Legend title size
                            legend.text = element_text(size = 12),  # Legend text size
                            strip.text = element_text(size = 14),  # Facet labels
                            line = element_line(linewidth = 0.8), #element_line(linewidth = 0.5),  # APA recommends thin lines
                            legend.position = "top" #"bottom" #"right"
                      ))

### TNIE --------------------------------------------------------------------

# ══════════════════════════════
#    individual-average effects  
# ══════════════════════════════
# Bias for TNIE
## gaussian 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt2) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 2. Bias for individual-average NIE with Gaussian outcome \n",
        x = "\n Number of Clusters", # "J",
        y = "Bias \n", 
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_bias-individual-avg-TNIE-gaussian-outcome.png"),
#        plot = last_plot())
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-individual-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

## binomial 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 3. Bias for individual-average NIE with Binomial outcome \n",
        x = "\n Number of Clusters", # "J",
        y = "Bias \n", 
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_bias-individual-avg-TNIE-binomial-outcome.png"),
#        plot = last_plot())

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-individual-avg-TNIE-binomial-outcome.png"),
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300)

# CI coverage rate for TNIE
perf_measures |> 
    ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "Figure 4. CI coverage rate for individual-average NIE \n",
        x = "\n Number of Clusters", # "J",
        y = "Coverage \n",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme

# ggsave(filename = paste0(results_path, "/Figures/",
#                          "S1_coverage-individual-avg-TNIE.png"),
#        plot = last_plot())

ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_coverage-individual-avg-TNIE.png"),
       plot = last_plot(),
       width = 10,
       height = 11,
       units = "in",
       dpi = 300)

# MSE for TNIE
## gaussian
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "MSE for individual-average NIE with Gaussian outcome \n",
        x = "Number of Clusters", # "J",
        y = "MSE",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-individual-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    # facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    facet_grid(
        Mfamily + Yfamily ~ quad + cluster_opt,
        labeller = labeller(
            Mfamily = c("binomial" = "Mediator: Binomial", "gaussian" = "Mediator: Gaussian"),
            Yfamily = c("binomial" = "Outcome: Binomial", "gaussian" = "Outcome: Gaussian"),
            quad    = c("linear" = "Linear", "nonlinear" = "Nonlinear"),
            cluster_opt = c("cwc" = "mean", "cwc.FE" = "mean + dummies")
        )
    ) +
    labs(
        title = "MSE for individual-average NIE with Binomial outcome \n",
        x = "Number of Clusters", # "J",
        y = "MSE",
        color = "Method",
        linetype = "Method",
        shape = "Cluster size" 
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-individual-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())

# Power for TNIE
perf_measures |> 
    ggplot(aes(x = factor(J), y = rejectnull_individual_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Power for individual-average TNIE",
        x = "J",
        y = "Power"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_power-individual-avg-TNIE.png"),
       plot = last_plot())



# ══════════════════════════════
#    cluster-average effects  
# ══════════════════════════════
# Bias for TNIE
## gaussian 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "Bias"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-cluster-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())
## binomial 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with Binomial outcome",
        x = "J",
        y = "Bias"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_bias-cluster-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())

# CI coverage rate for TNIE
perf_measures |> 
    ggplot(aes(x = factor(J), y = cover_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "CI coverage rate for cluster-average TNIE",
        x = "J",
        y = "Coverage"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_coverage-cluster-avg-TNIE.png"),
       plot = last_plot())

# MSE for TNIE
## gaussian
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "MSE"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-cluster-avg-TNIE-gaussian-outcome.png"),
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with Binomial outcome",
        x = "J",
        y = "MSE"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_MSE-cluster-avg-TNIE-binomial-outcome.png"),
       plot = last_plot())

# Power for TNIE
perf_measures |> 
    ggplot(aes(x = factor(J), y = rejectnull_cluster_TNIE, color = Fit, shape = Nj, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj))) +
    facet_grid(Mfamily + Yfamily ~ quad + cluster_opt) + #, labeller = labeller())
    labs(
        title = "Power for cluster-average TNIE",
        x = "J",
        y = "Power"
    ) +
    gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_power-cluster-avg-TNIE.png"),
       plot = last_plot())


## Tables ------------------------------------------------------------------
### TNIE --------------------------------------------------------------------

# True values 
perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj) |> 
    slice_head(n = 1) |> 
    select(starts_with("true") & ends_with("TNIE")) |> 
    print(n = Inf)
    
# Bias individual-average TNIE
perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)

#######
library(dplyr)
library(tidyr)
library(gt)
library(scales)

perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) %>%
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) %>%
    gt() %>%
    tab_header(
        title = "Bias in Individual-Average TNIE",
        subtitle = "Grouped by Yfamily, Mfamily, quad, cluster_opt, and J"
    ) %>%
    fmt_number(
        columns = starts_with("bias_"),
        decimals = 4
    ) %>%
    cols_label(
        quad = "Quadratic",
        Mfamily = "M Family",
        Yfamily = "Y Family",
        J = "J",
        cluster_opt = "Cluster Option",
        `bias_mlr_U[5, 20]` = "MLR U[5, 20]",
        `bias_glm_U[5, 20]` = "GLM U[5, 20]"
    ) %>%
    tab_style(
        style = cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(2)
        ),
        locations = cells_body(
            rows = Yfamily != lead(Yfamily)
        )
    ) %>%
    # tab_style(
    #     style = cell_fill(color = "lightgrey"),
    #     locations = cells_body(
    #         rows = seq(1, nrow(.), by = 2)
    #     )
    # ) %>%
    opt_row_striping() %>%
    tab_options(
        table.font.size = px(12),
        column_labels.font.weight = "bold",
        table.border.top.color = "black",
        table.border.top.width = px(2),
        table.border.bottom.color = "black",
        table.border.bottom.width = px(2)
    )


ind_bias_tbl <- perf_measures |> 
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) |> 
    pivot_wider(names_from = c(quad, Yfamily), 
                values_from = avg_bias) |> 
    arrange(Mfamily, J, Fit) 

# Display values (Copy-paste values into generate table website to obtain latex code)
as_hux(ind_bias_tbl) 

# Save Table 
write_csv(
    ind_bias_tbl, 
    file = paste0(results_path, "/Tables/",
                  "S1_bias-individual-avg-TNIE-table.csv"), 
    col_names = TRUE
)
filename = paste0(results_path, "/Figures/",
                  "S1_coverage-cluster-avg-TNIE.png")
write_csv(
    ind_bias_tbl,
    file = paste0(path, "/Tables/S2_TNIE-RMSE.csv"),
    col_names = TRUE
)


# EXAMPLE
# Pivot wide
rmse_Tbl <- pivot_wider(
    perf_measure_DF[, c("ICC",
                        "clust_size",
                        "analysisCond",
                        "PNDE_RMSE",
                        "TNIE_RMSE")],
    names_from = c(ICC, clust_size),
    names_sep = "_",
    values_from = c("PNDE_RMSE", "TNIE_RMSE")
)

# drop common piece of column names
colnames(rmse_Tbl) <-
    stringr::str_remove(string = colnames(rmse_Tbl),
                        pattern = "_RMSE")

# RMSE TNIE Table   
rmse_TNIE_Table <- rmse_Tbl %>%
    select(analysisCond, starts_with("TNIE_")) %>%
    separate(
        col = c("analysisCond"),
        into = c("PS Model", "Mediator/Outcome Model"),
        sep = "_"
    ) %>%
    arrange(`Mediator/Outcome Model`)

# Display values (Copy-paste values into generate table website to obtain latex code)
as_hux(rmse_TNIE_Table) %>%
    set_number_format(
        row = 1:nrow(rmse_TNIE_Table) + 1,
        col = -c(1:2),
        value = 3
    )

# Save Table 
write_csv(
    rmse_TNIE_Table,
    file = paste0(path, "/Tables/S2_TNIE-RMSE.csv"),
    col_names = TRUE
)




# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Process the data: compute average bias and reshape
pub_table <- perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) %>% 
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) %>%
    # Rename columns to be more descriptive for publication
    rename(
        "Quadratic"       = quad,
        "M Family"        = Mfamily,
        "Y Family"        = Yfamily,
        "Sample Size (J)" = J,
        "Cluster Option"  = cluster_opt,
        "Bias (MLR, U[5,20])" = `bias_mlr_U[5, 20]`,
        "Bias (GLM, U[5,20])" = `bias_glm_U[5, 20]`
    )

# Create and style the publishable table
pub_table %>%
    kable(format = "html", digits = 4,
          caption = "Average Bias in TNIE by Model Configuration") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) %>%
    add_header_above(c(" " = 5, "Model Bias" = 2))



######



# CI coverage individual-average TNIE
perf_measures %>%
    group_by(quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
    summarise(
        avg_coverage = mean(cover_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_coverage,
        names_prefix = "coverage_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)

# MSE individual-average TNIE
perf_measures %>%
    group_by(Yfamily, Mfamily, quad, cluster_opt, J, Nj, Fit) %>%
    summarise(
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj),
        values_from = avg_MSE,
        names_prefix = "MSE_"
    ) %>%
    arrange(Yfamily, Mfamily, quad, cluster_opt, J) |> 
    print(n = Inf)


perf_measures |> 
    group_by(Yfamily, Mfamily, quad, cluster_opt, J, Nj, Fit) |> 
    summarise(
        min_MSE = min(MSE_individual_TNIE), 
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        med_MSE = median(MSE_individual_TNIE), 
        max = max(MSE_individual_TNIE),
        .groups = "drop"
    ) |> 
    print(n = Inf)


### Overlap  ----------------------------------------------------------------

# ══════════════════════════════
#    check overlap 
# ══════════════════════════════
## function to extract percentages
extract_psnum <- function(text, pattern) {
    number <- str_extract(text, pattern)
    as.numeric(str_extract(number, "\\d+\\.?\\d*"))
}
## extract percent above or below thresholds 
sim_data$psLowPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)")
sim_data$psHighPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)$")


# ═══════════════════
#    display extreme PSs for replications  
# ═══════════════════
# Calculate counts & pct of extremely low PSs for each facet
threshold <- 1
threshold2 <- 2
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic) |> 
    summarize(count = sum(psLowPct > threshold), 
              pct = (sum(psLowPct > threshold) / sum(length(psLowPct)))*100, 
              count2 = sum(psLowPct > threshold2), 
              pct2 = (sum(psLowPct > threshold2) / sum(length(psLowPct)))*100)
# Extreme low PSs viz 
low_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    # & psLowPct >= threshold) |>
    ggplot(aes(x = psLowPct, fill = factor(psLowPct > threshold))) +
    geom_histogram() +
    facet_grid(~ J + Nj) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Calculate counts & pct of extremely high PSs for each facet
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    group_by(Mfamily, Yfamily, J, Nj, ifnull, quadratic) |> 
    summarize(count = sum(psHighPct > threshold), 
              pct = (sum(psHighPct > threshold) / sum(length(psHighPct)))*100, 
              count2 = sum(psHighPct > threshold2), 
              pct2 = (sum(psHighPct > threshold2) / sum(length(psHighPct)))*100)
# Extreme high PSs viz 
high_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    # & psHighPct >= threshold) |>
    ggplot(aes(x = psHighPct, fill = factor(psHighPct>threshold))) +
    geom_histogram() +
    facet_grid(~ J + Nj) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Plot visual 
high_ps_plot / low_ps_plot +
    patchwork::plot_annotation(title = "# of reps w/ % of PSs more extreme than < .01 or > .99", 
                               theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))) +
    patchwork::plot_annotation(caption = "Note: only reps with > 1% of PSs being extreme are displayed \n Ex: (36%)>1 = 36% of reps in that facet had 1% or more extreme PSs") 

# Save plot
ggsave(filename = paste0(results_path, "/Figures/",
                         "S1_overlap_extreme-PSs-by-cluster-size-and-number.png"),
       plot = last_plot())


# ═══════════════════
#    Extreme PSs tables  
# ═══════════════════
# Summary of percent under 0.01 PS 
sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    summarize(min = min(psLowPct),
              q25 = quantile(psLowPct, 0.25),
              median = median(psLowPct),
              mean = mean(psLowPct),
              q75 = quantile(psLowPct, 0.75),
              max = max(psLowPct)
    ) |> 
    print(n = Inf)
# Summary of percent over 0.99 PS 
sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "gaussian" & Yfamily == "gaussian") |> # narrow to one condition (so total will equal reps #)
    summarize(min = min(psHighPct),
              q25 = quantile(psHighPct, 0.25),
              median = median(psHighPct),
              mean = mean(psHighPct),
              q75 = quantile(psHighPct, 0.75),
              max = max(psHighPct)
    ) |> 
    print(n = Inf)







###################################### OLD CODE ########################################






# readRDS(paste0("Output/S1_Simulation-Output/S1_condition-01_reps-200_quad-FALSE_M-binomial_Y-gaussian_nj-[50-100]_J-10.rds"))
# readRDS(paste0("Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), "_reps-",))
# 
# 
# file_path <- "Output/S1_Simulation-Output"
# 
# # List all .rds files in the directory that match specific conditions
# rds_files <- list.files(
#     path = file_path, 
#     pattern = paste0(sprintf("%02d", cond), "_reps-200.*\\.rds$"),  # Only files with "reps-200" and ".rds"
#     full.names = TRUE
# )
# 
# # Read all .rds files into a list
# rds_data_list <- lapply(rds_files, readRDS)
# 
# # Optionally, assign names to the list based on file names for easy identification
# names(rds_data_list) <- basename(rds_files)
# 
# # View the list of loaded data
# str(rds_data_list)


## Extract simulation data (quadratic) -------------------------------------------------

sim1_data <- NULL  # Store all conditions' data

for (cond in 1:nrow(conditions)) {
    
    rds_files <- list.files(
        path = sim_output_path, #path = "Output/S1_Simulation-Output", 
        pattern = paste0(sprintf("%02d", cond), "_reps-", reps, "_quad-", conditions[cond, "quadratic"], "_M-", conditions[cond, "Mfamily"], "_Y-", conditions[cond, "Yfamily"], ".*\\.rds$"),  # Only files with "reps-200" and ".rds"
        full.names = TRUE
    )
    
    # Read all .rds files into a list
    # temp_data <- lapply(rds_files, readRDS)
    temp_data <- readRDS(rds_files)
    
    # # Load condition-specific data
    # temp_data <- readRDS(paste0(
    #     # "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), ".rds"
    #     "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), "_reps-", reps, "-.*\\.rds$"
    # ))
    
    # Create a large dataframe to store all iterations for the current condition
    overall_models <- NULL
    
    # Loop through all iterations in the `temp_data` list
    for (i in seq_along(temp_data)) {
        # Loop through models within each iteration
        for (mod in 1:nrow(methds)) {
            estimates <- temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$estimates
            
            # Create a named list for "individual" and "cluster" estimates
            individual_de <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) %>% slice(1)
            individual_ie <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect)) %>% slice(1)
            cluster_de <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) %>% slice(1)
            cluster_ie <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) %>% slice(1)
            
            # Extract individual and cluster effects
            individual_effects <- temp_data[[i]]$effects$individual
            cluster_effects <- temp_data[[i]]$effects$cluster
            
            # Create a single-row dataframe
            model_row <- tibble(
                condition = cond,  # Add condition as a column
                iteration = i,
                model = mod,
                warnings = ifelse(length(temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings) == 0, NA, temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings),
                num_folds = temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$num_folds, 
                
                # Individual DE (direct effect) estimates
                individual_de_Estimate = individual_de$Estimate,
                individual_de_StdError = individual_de$StdError,
                individual_de_CILower = individual_de$CILower,
                individual_de_CIUpper = individual_de$CIUpper,
                
                # Individual IE (indirect effect) estimates
                individual_ie_Estimate = individual_ie$Estimate,
                individual_ie_StdError = individual_ie$StdError,
                individual_ie_CILower = individual_ie$CILower,
                individual_ie_CIUpper = individual_ie$CIUpper,
                
                # Cluster DE (direct effect) estimates
                cluster_de_Estimate = cluster_de$Estimate,
                cluster_de_StdError = cluster_de$StdError,
                cluster_de_CILower = cluster_de$CILower,
                cluster_de_CIUpper = cluster_de$CIUpper,
                
                # Cluster IE (indirect effect) estimates
                cluster_ie_Estimate = cluster_ie$Estimate,
                cluster_ie_StdError = cluster_ie$StdError,
                cluster_ie_CILower = cluster_ie$CILower,
                cluster_ie_CIUpper = cluster_ie$CIUpper,
                
                # Individual effects (pnde, tnie, tnde, pnie)
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                # Cluster effects (pnde, tnie, tnde, pnie)
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie, 
                
                # PS overlap statement 
                ps_overlap = temp_data[[i]]$overlap$ps_summary
            )
            
            # Append to overall_models dataframe
            overall_models <- bind_rows(overall_models, model_row)
        }
    }
    
    # Append this condition's data to the main dataframe
    sim1_data <- bind_rows(sim1_data, overall_models)
}

# Save data 
saveRDS(sim1_data, file = paste0(results_path, "/Data/S1_simulation-data_quad_", sim_date, ".rds"))



## Extract simulation data (linear; with non-list elements error --------

## skipping condition 1 (since it has the error)

sim1_data <- NULL  # Store all conditions' data

for (cond in 2:nrow(conditions)) {
    
    rds_files <- list.files(
        path = sim_output_path, #path = "Output/S1_Simulation-Output", 
        pattern = paste0(sprintf("%02d", cond), "_reps-", reps, "_quad-", conditions[cond, "quadratic"], "_M-", conditions[cond, "Mfamily"], "_Y-", conditions[cond, "Yfamily"], ".*\\.rds$"),  # Only files with "reps-200" and ".rds"
        full.names = TRUE
    )
    
    # Read all .rds files into a list
    # temp_data <- lapply(rds_files, readRDS)
    temp_data <- readRDS(rds_files)
    
    # # Load condition-specific data
    # temp_data <- readRDS(paste0(
    #     # "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), ".rds"
    #     "Output/S1_Simulation-Output/S1_condition-", sprintf("%02d", cond), "_reps-", reps, "-.*\\.rds$"
    # ))
    
    # Create a large dataframe to store all iterations for the current condition
    overall_models <- NULL
    
    # Loop through all iterations in the `temp_data` list
    for (i in seq_along(temp_data)) {
        # Loop through models within each iteration
        for (mod in 1:nrow(methds)) {
            estimates <- temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$estimates
            
            # Create a named list for "individual" and "cluster" estimates
            individual_de <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) %>% slice(1)
            individual_ie <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect)) %>% slice(1)
            cluster_de <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) %>% slice(1)
            cluster_ie <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) %>% slice(1)
            
            # Extract individual and cluster effects
            individual_effects <- temp_data[[i]]$effects$individual
            cluster_effects <- temp_data[[i]]$effects$cluster
            
            # Create a single-row dataframe
            model_row <- tibble(
                condition = cond,  # Add condition as a column
                iteration = i,
                model = mod,
                warnings = ifelse(length(temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings) == 0, NA, temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings),
                num_folds = temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$num_folds, 
                
                # Individual DE (direct effect) estimates
                individual_de_Estimate = individual_de$Estimate,
                individual_de_StdError = individual_de$StdError,
                individual_de_CILower = individual_de$CILower,
                individual_de_CIUpper = individual_de$CIUpper,
                
                # Individual IE (indirect effect) estimates
                individual_ie_Estimate = individual_ie$Estimate,
                individual_ie_StdError = individual_ie$StdError,
                individual_ie_CILower = individual_ie$CILower,
                individual_ie_CIUpper = individual_ie$CIUpper,
                
                # Cluster DE (direct effect) estimates
                cluster_de_Estimate = cluster_de$Estimate,
                cluster_de_StdError = cluster_de$StdError,
                cluster_de_CILower = cluster_de$CILower,
                cluster_de_CIUpper = cluster_de$CIUpper,
                
                # Cluster IE (indirect effect) estimates
                cluster_ie_Estimate = cluster_ie$Estimate,
                cluster_ie_StdError = cluster_ie$StdError,
                cluster_ie_CILower = cluster_ie$CILower,
                cluster_ie_CIUpper = cluster_ie$CIUpper,
                
                # Individual effects (pnde, tnie, tnde, pnie)
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                # Cluster effects (pnde, tnie, tnde, pnie)
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie, 
                
                # PS overlap statement 
                ps_overlap = temp_data[[i]]$overlap$ps_summary
            )
            
            # Append to overall_models dataframe
            overall_models <- bind_rows(overall_models, model_row)
        }
    }
    
    # Append this condition's data to the main dataframe
    sim1_data <- bind_rows(sim1_data, overall_models)
}

## Add simulation 1 to data (after dropping iterations that resulted in error)
linear_c1 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-01_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")

# create list of the error iterations 
c1_error_lst <- linear_c1[sapply(linear_c1, function(x) {
    !is.list(x)
})] # iterations 4, 132, & 260 for condition 1 (corresponding seeds: 721442, 814237, & 814286)

# drop error iterations from list 
linear_c1 <- Filter(is.list, linear_c1)

# ══════════════════════════════
#    repeat loop from above on condition 1 now  
# ══════════════════════════════
# change prior dataframe name & temp_data 
sim1_data1 <- sim1_data
temp_data <- linear_c1
cond <- 1 # set condition number 
# Create a large dataframe to store all iterations for the current condition
overall_models <- NULL

# Loop through all iterations in the `temp_data` list
for (i in seq_along(temp_data)) {
    # Loop through models within each iteration
    for (mod in 1:nrow(methds)) {
        estimates <- temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$estimates
        
        # Create a named list for "individual" and "cluster" estimates
        individual_de <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) %>% slice(1)
        individual_ie <- estimates %>% filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect)) %>% slice(1)
        cluster_de <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) %>% slice(1)
        cluster_ie <- estimates %>% filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) %>% slice(1)
        
        # Extract individual and cluster effects
        individual_effects <- temp_data[[i]]$effects$individual
        cluster_effects <- temp_data[[i]]$effects$cluster
        
        # Create a single-row dataframe
        model_row <- tibble(
            condition = cond,  # Add condition as a column
            iteration = i,
            model = mod,
            warnings = ifelse(length(temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings) == 0, NA, temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$warnings),
            num_folds = temp_data[[i]]$results[[glue::glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")]]$num_folds, 
            
            # Individual DE (direct effect) estimates
            individual_de_Estimate = individual_de$Estimate,
            individual_de_StdError = individual_de$StdError,
            individual_de_CILower = individual_de$CILower,
            individual_de_CIUpper = individual_de$CIUpper,
            
            # Individual IE (indirect effect) estimates
            individual_ie_Estimate = individual_ie$Estimate,
            individual_ie_StdError = individual_ie$StdError,
            individual_ie_CILower = individual_ie$CILower,
            individual_ie_CIUpper = individual_ie$CIUpper,
            
            # Cluster DE (direct effect) estimates
            cluster_de_Estimate = cluster_de$Estimate,
            cluster_de_StdError = cluster_de$StdError,
            cluster_de_CILower = cluster_de$CILower,
            cluster_de_CIUpper = cluster_de$CIUpper,
            
            # Cluster IE (indirect effect) estimates
            cluster_ie_Estimate = cluster_ie$Estimate,
            cluster_ie_StdError = cluster_ie$StdError,
            cluster_ie_CILower = cluster_ie$CILower,
            cluster_ie_CIUpper = cluster_ie$CIUpper,
            
            # Individual effects (pnde, tnie, tnde, pnie)
            individual_pnde = individual_effects$pnde,
            individual_tnie = individual_effects$tnie,
            individual_tnde = individual_effects$tnde,
            individual_pnie = individual_effects$pnie,
            
            # Cluster effects (pnde, tnie, tnde, pnie)
            cluster_pnde = cluster_effects$pnde,
            cluster_tnie = cluster_effects$tnie,
            cluster_tnde = cluster_effects$tnde,
            cluster_pnie = cluster_effects$pnie, 
            
            # PS overlap statement 
            ps_overlap = temp_data[[i]]$overlap$ps_summary
        )
        
        # Append to overall_models dataframe
        overall_models <- bind_rows(overall_models, model_row)
    }
}

# unique(sim1_data$condition)
# unique(overall_models$condition)

# Bind all conditions 
sim1_data <- bind_rows(sim1_data1, overall_models)

# Save data 
saveRDS(sim1_data, file = paste0(results_path, "/Data/S1_simulation-data_linear_", sim_date, ".rds"))



# Checking on issue with importing linear results -------------------------

c <- "01"
file_str <- glue::glue("Output/S1_Simulation-Output/from-tacc/", 
                       "2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/", 
                       "S1_condition-", c, "reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")

# lin_c1 <- readRDS(file = file_str)
lin100_c1 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/S1_condition-01_reps-100_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")
quad_c1 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/S1_condition-01_reps-300_quad-TRUE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")
lin_c1 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-01_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10.rds")
lin_c2 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-02_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-20.rds")
lin_c3 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-03_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-40.rds")
lin_c4 <- readRDS("Output/S1_Simulation-Output/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/S1_condition-04_reps-300_quad-FALSE_M-gaussian_Y-binomial_nj-[50-100]_J-10.rds")

sum(
    sapply(lin_c1, function(x) {
        !is.list(x)
    })
)

length(lin_c1)

2025-01-25-test_300-rep_all-linear-conditions-with-all-methods
S1_condition-01_reps-300_quad-FALSE_M-binomial_Y-binomial_nj-[50-100]_J-10
quad_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Data/S1_simulation-data_2025-01-25.rds")
quad_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-25.rds")





# Join quad & linear data -------------------------------------------------

# ══════════════════════════════
#    Simulation data 
# ══════════════════════════════
# import data 
quad_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", 
                                   "quad_", 
                                   sim_date, ".rds"))
linear_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", 
                                     "linear_", 
                                     sim_date, ".rds"))
# Stack data 
sim_data <- bind_rows(cbind(quadratic = TRUE, quad_data), 
                      cbind(quadratic = FALSE, linear_data))
# Add condition & method info to data
## condition 
sim_data <- sim_data |> 
    left_join(
        # conditions df in correct ordering 
        rbind(
            cbind(condition = 1:nrow(conditions_all[conditions_all$quadratic == FALSE & conditions_all$if.null == FALSE,]),
                  conditions_all[conditions_all$quadratic == FALSE & conditions_all$if.null == FALSE,]),
            cbind(condition = 1:nrow(conditions_all[conditions_all$ quadratic == TRUE & conditions_all$if.null == FALSE,]),
                  conditions_all[conditions_all$quadratic == TRUE & conditions_all$if.null == FALSE,])
        ), 
        by = c("condition", "quadratic"))
## methods
sim_data <- sim_data |> 
    left_join(cbind(model = 1:nrow(methds_all), 
                    methds_all[, c("Fit", "cluster_opt")])) # check matching: table(sim_data_t$Fit, sim_data_t$num_folds) 
# Save data 
saveRDS(sim_data,
        file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, ".rds")) 


# Compute Performance Measures --------------------------------------------
# import data (with both quad & linear scenarios included)
sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", #linear_", 
                                   sim_date, #"2025-01-12", #sim_date, 
                                   ".rds"))
# Compute performance measures 
perf_summary <- as.data.frame(sim1_data) |> 
    group_by(quadratic, condition) |> # get one true value per condition 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |>
    group_by(quadratic, condition, model) |> 
    mutate(if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
           if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
           if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
           if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE) ) |> 
    summarize(cover_individual_PNDE = mean(if_cover_ind_PNDE), 
              cover_individual_TNIE = mean(if_cover_ind_TNIE), 
              bias_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)), 
              bias_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)), 
              MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2), 
              MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2), 
              rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))), 
              rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))),
              true_individual_PNDE = mean(true_individual_PNDE), 
              true_individual_TNIE = mean(true_individual_TNIE), 
              # cluster-avg
              cover_cluster_PNDE = mean(if_cover_clust_PNDE), 
              cover_cluster_TNIE = mean(if_cover_clust_TNIE), 
              bias_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)), 
              bias_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)), 
              MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2), 
              MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2), 
              rejectnull_cluster_PNDE = mean((cluster_de_CILower > (0)) | (cluster_de_CIUpper < (0))), 
              rejectnull_cluster_TNIE = mean((cluster_ie_CILower > (0)) | (cluster_ie_CIUpper < (0))),
              true_cluster_PNDE = mean(true_cluster_PNDE), 
              true_cluster_TNIE = mean(true_cluster_TNIE)
    )
# Add condition & method info back
perf_summary <- perf_summary |> 
    left_join(sim_data[, c("quadratic", "condition", "model", "J", "Nj_low", "Nj_high", "Mfamily", "Yfamily", 
                           "if.null", "icc", "Fit", "cluster_opt")], 
              by = c("quadratic", "condition", "model"))

table(perf_summary$quadratic)

perf_summary

saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", #linear_", 
                                    sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))


## Bias, Coverage, & MSE (need type I error) -----------------------------------------------------------

sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_quad_", #linear_", 
                                   sim_date, #"2025-01-12", #sim_date, 
                                   ".rds"))

# 
perf_summary <- as.data.frame(sim1_data) |> 
    group_by(condition) |> # get one true value per condition 
    mutate(true_individual_PNDE = mean(individual_pnde), 
           true_individual_TNIE = mean(individual_tnie), 
           true_cluster_PNDE = mean(cluster_pnde), 
           true_cluster_TNIE = mean(cluster_tnie)) |>
    group_by(condition, model) |> 
    mutate(if_cover_ind_PNDE = (individual_de_CILower < true_individual_PNDE) & (individual_de_CIUpper > true_individual_PNDE), 
           if_cover_ind_TNIE = (individual_ie_CILower < true_individual_TNIE) & (individual_ie_CIUpper > true_individual_TNIE), 
           if_cover_clust_PNDE = (cluster_de_CILower < true_cluster_PNDE) & (cluster_de_CIUpper > true_cluster_PNDE), 
           if_cover_clust_TNIE = (cluster_ie_CILower < true_cluster_TNIE) & (cluster_ie_CIUpper > true_cluster_TNIE) ) |> 
    summarize(cover_individual_PNDE = mean(if_cover_ind_PNDE), 
              cover_individual_TNIE = mean(if_cover_ind_TNIE), 
              bias_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)), 
              bias_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)), 
              MSE_individual_PNDE = mean((individual_de_Estimate - true_individual_PNDE)^2), 
              MSE_individual_TNIE = mean((individual_ie_Estimate - true_individual_TNIE)^2), 
              rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))), 
              rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))),
              true_individual_PNDE = mean(true_individual_PNDE), 
              true_individual_TNIE = mean(true_individual_TNIE), 
              # cluster-avg
              cover_cluster_PNDE = mean(if_cover_clust_PNDE), 
              cover_cluster_TNIE = mean(if_cover_clust_TNIE), 
              bias_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)), 
              bias_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)), 
              MSE_cluster_PNDE = mean((cluster_de_Estimate - true_cluster_PNDE)^2), 
              MSE_cluster_TNIE = mean((cluster_ie_Estimate - true_cluster_TNIE)^2), 
              rejectnull_cluster_PNDE = mean((cluster_de_CILower > (0)) | (cluster_de_CIUpper < (0))), 
              rejectnull_cluster_TNIE = mean((cluster_ie_CILower > (0)) | (cluster_ie_CIUpper < (0))),
              true_cluster_PNDE = mean(true_cluster_PNDE), 
              true_cluster_TNIE = mean(true_cluster_TNIE)
    )
    

# add condition description 
perf_summary <- cbind(condition = 1:nrow(conditions), 
                     conditions) |> 
    left_join(perf_summary, 
              by = "condition") 

# add method description 
perf_summary <- cbind(method = 1:nrow(methds), 
                     methds) |> 
    left_join(perf_summary, 
              by = c("method" = "model"))

perf_summary

saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_quad_", #linear_", 
                                    sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))

# # Individual-avg summary
# ind_summary <- as.data.frame(sim1_data) |>
#     group_by(condition) |> # get one true value per condition
#     mutate(true_PNDE = mean(individual_pnde),
#            true_TNIE = mean(individual_tnie)) |>
#     group_by(condition, model) |>
#     mutate(if_cover_PNDE = (individual_de_CILower < true_PNDE) & (individual_de_CIUpper > true_PNDE),
#            if_cover_TNIE = (individual_ie_CILower < true_TNIE) & (individual_ie_CIUpper > true_TNIE) ) |>
#     summarize(cover_PNDE = mean(if_cover_PNDE),
#               cover_TNIE = mean(if_cover_TNIE),
#               bias_individual_PNDE = mean((individual_de_Estimate - true_PNDE)),
#               bias_individual_TNIE = mean((individual_ie_Estimate - true_TNIE)),
#               MSE_individual_PNDE = mean((individual_de_Estimate - true_PNDE)^2),
#               MSE_individual_TNIE = mean((individual_ie_Estimate - true_TNIE)^2),
#               rejectnull_individual_PNDE = mean((individual_de_CILower > (0)) | (individual_de_CIUpper < (0))),
#               rejectnull_individual_TNIE = mean((individual_ie_CILower > (0)) | (individual_ie_CIUpper < (0))),
#               true_PNDE = mean(true_PNDE),
#               true_TNIE = mean(true_TNIE)
#     ) #|>
# # view()
# 
# ind_summary
# 
# # add condition description
# ind_summary <- cbind(condition = 1:nrow(conditions),
#                     conditions) |>
#     left_join(ind_summary,
#               by = "condition")
# 
# # add method description
# ind_summary <- cbind(method = 1:nrow(methds),
#       methds) |>
#     left_join(ind_summary,
#               by = c("method" = "model"))
# 
# ind_summary
# 
# saveRDS(ind_summary, file = paste0(path, "/Tables/S1_performance-measures_", sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))


# Join quad & linear data -------------------------------------------------

# # ══════════════════════════════
# #    Simulation data 
# # ══════════════════════════════
# # import data 
# quad_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", 
#                                    "quad_", 
#                                    sim_date, ".rds"))
# linear_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", 
#                                      "linear_", 
#                                      sim_date, ".rds"))
# # Stack data 
# sim_data <- bind_rows(cbind(quadratic = TRUE, quad_data), 
#                       cbind(quadratic = FALSE, linear_data))
# # Add condition & method info to data
# ## condition 
# sim_data <- sim_data |> 
#     left_join(
#         # conditions df in correct ordering 
#         rbind(
#             cbind(condition = 1:nrow(conditions_all[conditions_all$quadratic == FALSE & conditions_all$if.null == FALSE,]),
#                   conditions_all[conditions_all$quadratic == FALSE & conditions_all$if.null == FALSE,]),
#             cbind(condition = 1:nrow(conditions_all[conditions_all$ quadratic == TRUE & conditions_all$if.null == FALSE,]),
#                   conditions_all[conditions_all$quadratic == TRUE & conditions_all$if.null == FALSE,])
#         ), 
#         by = c("condition", "quadratic"))
# ## methods
# sim_data <- sim_data |> 
#     left_join(cbind(model = 1:nrow(methds_all), 
#                     methds_all[, c("Fit", "cluster_opt")])) # check matching: table(sim_data_t$Fit, sim_data_t$num_folds) 
# # Save data 
# saveRDS(sim_data,
#         file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, ".rds")) 
# 
# # ══════════════════════════════
# #    Performance measures 
# # ══════════════════════════════
# # import & performance measures 
# quad_perf_measures <- readRDS(file = paste0(results_path, "/Tables/", 
#                                             "S1_performance-measures_",
#                                             "quad_", 
#                                             sim_date, ".rds"))
# linear_perf_measures <- readRDS(file = paste0(results_path, "/Tables/", 
#                                             "S1_performance-measures_",
#                                             "linear_", 
#                                             sim_date, ".rds"))
# # Stack performance measures 
# perf_measures <- bind_rows(quad_perf_measures, linear_perf_measures)
# # Save performance measures 
# saveRDS(perf_measures,
#         file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, ".rds")) 
# 
# 
# table(perf_measures$quadratic)
# table(linear_perf_measures$quadratic)
# table(quad_perf_measures$quadratic)
# 
# table(sim_data$quadratic)

# Report results (quad scenario) ------------------------------------------

# import data & performance measures 
quad_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Data/S1_simulation-data_2025-01-25.rds")
quad_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-25.rds")


# all performance measures 
quad_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    view()

# summary of true effects 
quad_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE") |>
    group_by(Mfamily, Yfamily) |>
    summarize(
        mean_true_PNDE = mean(true_PNDE),
        sd_true_PNDE = sd(true_PNDE),
        median_true_PNDE = median(true_PNDE),
        mean_true_TNIE = mean(true_TNIE),
        sd_true_TNIE = sd(true_TNIE),
        median_true_TNIE = median(true_TNIE),
        .groups = "drop"
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily)



# ══════════════════════════════
#    visuals 
# ══════════════════════════════

# CI coverage rate for TNIE
## U[5, 20]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [5, 20]") 
theme_minimal()
## U[50, 100]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 50) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [50, 100]") 
theme_minimal()

# Bias 
## U[5, 20]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |>
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "Bias for Individual-Average TNIE by Mediator (horz) & Outcome (vert) type for small clusters [5, 20]") +
    theme_minimal() #+
    # 
    # guides(
    #     color = guide_legend(
    #         override.aes = list(
    #             shape    = c(16, 17),        # match # of levels in color
    #             linetype = c("solid","dashed")
    #         )
    #     ),
    #     shape = "none",
    #     linetype = "none"
    # )
## U[50, 100]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 50) |>
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "Bias for Individual-Average TNIE by Mediator (horz) & Outcome (vert) type for small clusters [50, 100]") +
    theme_minimal()

# MSE
## U[5, 20]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |>
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    # ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "MSE for Individual-Average TNIE by Mediator (horz) & Outcome (vert) type for small clusters [5, 20]") 
## U[50, 100]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 50) |>
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    # ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "MSE for Individual-Average TNIE by Mediator (horz) & Outcome (vert) type for small clusters [50, 100]") 


# ══════════════════════════════
#     Major Section   
# ══════════════════════════════


# Report results (linear scenario) ------------------------------------------

# import data & performance measures 
linear_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/Data/S1_simulation-data_2025-01-25.rds")
linear_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-25.rds")

# all performance measures 
linear_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    view()

# summary of true effects 
linear_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE") |>
    group_by(Mfamily, Yfamily) |>
    summarize(
        mean_true_PNDE = mean(true_PNDE),
        sd_true_PNDE = sd(true_PNDE),
        median_true_PNDE = median(true_PNDE),
        mean_true_TNIE = mean(true_TNIE),
        sd_true_TNIE = sd(true_TNIE),
        median_true_TNIE = median(true_TNIE),
        .groups = "drop"
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily)


# ══════════════════════════════
#    visuals 
# ══════════════════════════════

# CI coverage rate for TNIE
## U[5, 20]
linear_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [5, 20]") 
theme_minimal()
## U[50, 100]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 50) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [50, 100]") 
theme_minimal()

# ══════════════════════════════
#    visuals 
# ══════════════════════════════

# CI coverage rate for TNIE
## U[5, 20]



# Report results (both quad & linear) -------------------------------------

## Visuals -----------------------------------------------------------------

# import data if needed 
perf_measures <- readRDS(file = paste0(results_path, "/Tables/S1_performance-measures_", sim_date, ".rds")) 

# visual settings
gglayer_theme <- list(theme_bw(),
                      scale_fill_manual(values = c("#BF5700", #Fixed-effect
                                                   "#A6CD57", #Random-effect
                                                   "#333F48")), #Single-level
                      #"#9CADB7" <-- light gray
                      # Used following website with university colors: https://projects.susielu.com/viz-palette?
                      theme(text = element_text(family = "Times New Roman", size = 12),
                            axis.title = element_text(size = 12),  # Adjust axis title size
                            axis.text = element_text(size = 10),  # Adjust axis text size
                            legend.title = element_text(size = 12),  # Legend title size
                            legend.text = element_text(size = 10),  # Legend text size
                            strip.text = element_text(size = 12),  # Facet labels
                            line = element_line(linewidth = 0.5),  # APA recommends thin lines
                            legend.position = "top"
                      ))

### TNIE --------------------------------------------------------------------

# ══════════════════════════════
#    individual-average effects  
# ══════════════════════════════
# Bias for TNIE
## gaussian 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for individual-average TNIE with Gaussian outcome",
        x = "J",
        y = "Bias"
    ) #+
    # gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-individual-avg-TNIE-gaussian-outcome.png"), 
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for individual-average TNIE with binomial outcome",
        x = "J",
        y = "Bias"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-individual-avg-TNIE-binomial-outcome.png"), 
       plot = last_plot())
# CI coverage rate for TNIE
perf_measures |> 
    # filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "CI coverage rate for individual-average TNIE",
        x = "J",
        y = "Coverage"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_coverage-individual-avg-TNIE.png"), 
       plot = last_plot())
# ## gaussian
# perf_measures |> 
#     filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 1) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
#     labs(
#         title = "CI coverage rate for individual-average TNIE with Gaussian outcome",
#         x = "J",
#         y = "Coverage"
#     ) 
# ## binomial 
# perf_measures |> 
#     filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = cover_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 1) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
#     labs(
#         title = "CI coverage rate for individual-average TNIE with binomial outcome",
#         x = "J",
#         y = "Coverage"
#     ) 


# MSE for TNIE
## gaussian
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "MSE for individual-average TNIE with Gaussian outcome",
        x = "J",
        y = "MSE"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_MSE-individual-avg-TNIE-gaussian-outcome.png"), 
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "MSE for individual-average TNIE with binomial outcome",
        x = "J",
        y = "MSE"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_MSE-individual-avg-TNIE-binomial-outcome.png"), 
       plot = last_plot())

# Power for TNIE
perf_measures |> 
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = rejectnull_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Power for individual-average TNIE",
        x = "J",
        y = "Power"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_power-individual-avg-TNIE.png"), 
       plot = last_plot())


# ══════════════════════════════
#    cluster-average effects  
# ══════════════════════════════
# Bias for TNIE
## gaussian 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "Bias"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-cluster-avg-TNIE-gaussian-outcome.png"), 
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for cluster-average TNIE with binomial outcome",
        x = "J",
        y = "Bias"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-cluster-avg-TNIE-binomial-outcome.png"), 
       plot = last_plot())

# CI coverage rate for TNIE
perf_measures |> 
    # filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = cover_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "CI coverage rate for cluster-average TNIE",
        x = "J",
        y = "Coverage"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_coverage-cluster-avg-TNIE.png"), 
       plot = last_plot())
# ## gaussian
# perf_measures |> 
#     filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = cover_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 1) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
#     labs(
#         title = "CI coverage rate for cluster-average TNIE with Gaussian outcome",
#         x = "J",
#         y = "Coverage"
#     ) 
# ## binomial 
# perf_measures |> 
#     filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = cover_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 1) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
#     labs(
#         title = "CI coverage rate for cluster-average TNIE with binomial outcome",
#         x = "J",
#         y = "Coverage"
#     ) 


# MSE for TNIE
## gaussian
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with Gaussian outcome",
        x = "J",
        y = "MSE"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_MSE-cluster-avg-TNIE-gaussian-outcome.png"), 
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "MSE for cluster-average TNIE with binomial outcome",
        x = "J",
        y = "MSE"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_MSE-cluster-avg-TNIE-binomial-outcome.png"), 
       plot = last_plot())

# Power for TNIE
perf_measures |> 
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = rejectnull_cluster_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Power for cluster-average TNIE",
        x = "J",
        y = "Power"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_power-cluster-avg-TNIE.png"), 
       plot = last_plot())



### PNDE --------------------------------------------------------------------

# ══════════════════════════════
#    individual-average effects  
# ══════════════════════════════
# Bias for PNDE
## gaussian 
perf_measures |> 
    filter(Yfamily %in% c("gaussian")) |> 
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_PNDE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for individual-average PNDE with Gaussian outcome",
        x = "J",
        y = "Bias"
    ) #+
# gglayer_theme
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-individual-avg-PNDE-gaussian-outcome.png"), 
       plot = last_plot())
## binomial
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_PNDE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "Bias for individual-average PNDE with binomial outcome",
        x = "J",
        y = "Bias"
    ) 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_bias-individual-avg-PNDE-binomial-outcome.png"), 
       plot = last_plot())






## Tables ------------------------------------------------------------------

# Bias individual-average TNIE
perf_measures %>%
    filter(Yfamily %in% c("gaussian", "binomial")) %>%
    mutate(
        quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"),
        Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")
    ) %>%
    group_by(Yfamily, Mfamily, quadratic, cluster_opt, J, Nj_low, Fit) %>%
    summarise(
        avg_bias = mean(bias_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj_low),
        values_from = avg_bias,
        names_prefix = "bias_"
    ) %>%
    arrange(Yfamily, Mfamily, quadratic, cluster_opt, J) |> 
    print(n = Inf)

# CI coverage individual-average TNIE
perf_measures %>%
    filter(Yfamily %in% c("gaussian", "binomial")) %>%
    mutate(
        quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"),
        Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")
    ) %>%
    group_by(Yfamily, Mfamily, quadratic, cluster_opt, J, Nj_low, Fit) %>%
    summarise(
        avg_coverage = mean(cover_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj_low),
        values_from = avg_coverage,
        names_prefix = "coverage_"
    ) %>%
    arrange(Yfamily, Mfamily, quadratic, cluster_opt, J) |> 
    print(n = Inf)
# MSE individual-average TNIE
perf_measures %>%
    filter(Yfamily %in% c("gaussian", "binomial")) %>%
    mutate(
        quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"),
        Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")
    ) %>%
    group_by(Yfamily, Mfamily, quadratic, cluster_opt, J, Nj_low, Fit) %>%
    summarise(
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = c(Fit, Nj_low),
        values_from = avg_MSE,
        names_prefix = "MSE_"
    ) %>%
    arrange(Yfamily, Mfamily, quadratic, cluster_opt, J) |> 
    # view()
    print(n = Inf)

perf_measures |> 
    filter(quadratic == "TRUE" & J == 40 & Nj_low == 5) |> 
    group_by(Yfamily, Mfamily, quadratic, cluster_opt, J, Nj_low, Fit) |> 
    summarise(
        min_MSE = min(MSE_individual_TNIE), 
        avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
        med_MSE = median(MSE_individual_TNIE), 
        max = max(MSE_individual_TNIE),
        .groups = "drop"
    ) 

perf_measures$MSE_individual_TNIE

# Checking extreme MSE values 
perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    filter(Mfamily %in% c("gaussian") & quadratic == TRUE & cluster_opt == "cwc") |> 
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    # facet_grid(interaction(Mfamily, Yfamily) ~ interaction(cluster_opt, quadratic)) + #, labeller = labeller())
    labs(
        title = "MSE for individual-average TNIE with binomial outcome",
        x = "J",
        y = "MSE"
    ) 


perf_measures |> 
    filter(Yfamily %in% c("binomial")) |> # & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ungroup() |> 
    filter(cluster_opt == "cwc" & Mfamily == "binomial" & quadratic == "nonlinear" & Nj_low == "U[50, 100]") |> 
    select(quadratic, J, Nj_low, Mfamily, Yfamily, Fit, cluster_opt, "MSE_individual_TNIE") |> # starts_with("MSE_"))
    # view()
    group_by(Fit) |> 
    summarize(min_MSE = min(MSE_individual_TNIE), 
              avg_MSE = mean(MSE_individual_TNIE, na.rm = TRUE),
              med_MSE = median(MSE_individual_TNIE), 
              max = max(MSE_individual_TNIE))




# Overlap  ----------------------------------------------------------------

sim_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_2025-01-25.rds"))

# ══════════════════════════════
#    check overlap 
# ══════════════════════════════
# check overlap 
## function to extract percentages
extract_psnum <- function(text, pattern) {
    number <- str_extract(text, pattern)
    as.numeric(str_extract(number, "\\d+\\.?\\d*"))
}
## extract percent above or below thresholds 
sim_data$psLowPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)")
sim_data$psHighPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)$")

# sim_data |> 
#     ggplot(aes(y = psLowPct, x = J)) +
#     geom_jitter(alpha = 0.5) +
#     geom_boxplot(aes(fill = factor(J))) +
#     # geom_violin() +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(Nj_low))


# 
# sim_data |> 
#     filter(model == 1 & quadratic == FALSE & Mfamily == "binomial" & Yfamily == "binomial") |> 
#     # filter(Mfamily == "binomial", Yfamily == "binomial", Nj_low == 50) |> 
#     ggplot(aes(x = psLowPct, color = condition)) +
#     geom_histogram() +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low))


# ═══════════════════
#    disply extreme PSs for replications  
# ═══════════════════
# Calculate counts & pct of extremely low PSs for each facet
threshold <- 1
threshold2 <- 2
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "binomial" & Yfamily == "binomial") |> 
    group_by(Mfamily, Yfamily, J, Nj_low) |>
    summarize(count = sum(psLowPct > threshold), 
              pct = (sum(psLowPct > threshold) / sum(length(psLowPct)))*100, 
              count2 = sum(psLowPct > threshold2), 
              pct2 = (sum(psLowPct > threshold2) / sum(length(psLowPct)))*100)
# Extreme low PSs viz 
low_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "binomial" & Yfamily == "binomial") |> 
    # & psLowPct >= threshold) |>
    ggplot(aes(x = psLowPct, fill = factor(psLowPct > threshold))) +
    geom_histogram() +
    facet_grid(~ interaction(J, Nj_low)) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Calculate counts & pct of extremely high PSs for each facet
facet_counts <- sim_data |>
    filter(model == 1 & quadratic == FALSE & Mfamily == "binomial" & Yfamily == "binomial") |> 
    group_by(Mfamily, Yfamily, J, Nj_low) |>
    summarize(count = sum(psHighPct > threshold), 
              pct = (sum(psHighPct > threshold) / sum(length(psHighPct)))*100, 
              count2 = sum(psHighPct > threshold2), 
              pct2 = (sum(psHighPct > threshold2) / sum(length(psHighPct)))*100)
# Extreme high PSs viz 
high_ps_plot <- sim_data |> 
    filter(model == 1 & quadratic == FALSE & Mfamily == "binomial" & Yfamily == "binomial") |> 
           # & psHighPct >= threshold) |>
    ggplot(aes(x = psHighPct, fill = factor(psHighPct>threshold))) +
    geom_histogram() +
    facet_grid(~ interaction(J, Nj_low)) + #facet_grid(interaction(Mfamily, Yfamily) ~ interaction(J, Nj_low)) +
    geom_text(
        data = facet_counts,
        aes(x = Inf, y = Inf, label = paste(count, "\n (", round(pct), "%) >", threshold, 
                                            "\n", count2, "\n (", round(pct2), "%) >", threshold2)),
        hjust = 1, vjust = 1.5, size = 3, color = "black",
        inherit.aes = FALSE
    ) +
    theme(legend.position = "none") +
    labs(y = "# of reps")

# Plot visual 
high_ps_plot / low_ps_plot +
    patchwork::plot_annotation(title = "# of reps w/ % of PSs more extreme than < .01 or > .99", 
                               theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))) +
    patchwork::plot_annotation(caption = "Note: only reps with > 1% of PSs being extreme are displayed \n Ex: (36%)>1 = 36% of reps in that facet had 1% or more extreme PSs") 
    # patchwork::plot_layout(guides = "collect") +
    # theme(legend.position = "bottom")
# Save plot 
ggsave(filename = paste0(results_path, "/Figures/", 
                         "S1_overlap_extreme-PSs-by-cluster-size-and-number.png"), 
       plot = last_plot())


# ═══════════════════
#    Extreme PSs tables  
# ═══════════════════
# Summary of percent under 0.01 PS 
sim_data |> 
    group_by(Mfamily, Yfamily, Nj_low, J) |> 
    summarize(min = min(psLowPct),
              q25 = quantile(psLowPct, 0.25),
              median = median(psLowPct),
              mean = mean(psLowPct),
              q75 = quantile(psLowPct, 0.75),
              max = max(psLowPct)
              ) |> 
    print(n = Inf)
# Summary of percent over 0.99 PS 
sim_data |> 
    group_by(Mfamily, Yfamily, Nj_low, J) |> 
    summarize(min = min(psHighPct),
              q25 = quantile(psHighPct, 0.25),
              median = median(psHighPct),
              mean = mean(psHighPct),
              q75 = quantile(psHighPct, 0.75),
              max = max(psHighPct)
    ) |> 
    print(n = Inf)




# reporting linear 100 reps 2025-01-23 results (from TACC) ------------------------------


# import results 
tacc_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/Data/S1_simulation-data_2025-01-23.rds")
tacc_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-23.rds")

# took about 49 mins to run 
# readRDS(file = "Output/S1_Simulation-Output/from-tacc/2025-01-23-test_100-reps_all-linear-conditions-with-all-methods/S1_Computation-Time.rds") |> 
#     view()

#     
tacc_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    view()

# summary of null rejection 
tacc_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    group_by(Mfamily, Yfamily, Fit, cluster_opt) |> 
    summarize(
        mean_rej_PNDE = mean(rejectnull_individual_PNDE), 
        sd_rej_PNDE = sd(rejectnull_individual_PNDE),
        median_rej_PNDE = median(rejectnull_individual_PNDE),
        
        mean_rej_TNIE = mean(rejectnull_individual_TNIE), 
        sd_rej_TNIE = sd(rejectnull_individual_TNIE),
        median_rej_TNIE = median(rejectnull_individual_TNIE)
        
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily) |> 
    print(n = 32)


# summary of true effects 
tacc_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE") |>
    group_by(Mfamily, Yfamily) |>
    summarize(
        mean_true_PNDE = mean(true_PNDE),
        sd_true_PNDE = sd(true_PNDE),
        median_true_PNDE = median(true_PNDE),
        mean_true_TNIE = mean(true_TNIE),
        sd_true_TNIE = sd(true_TNIE),
        median_true_TNIE = median(true_TNIE),
        .groups = "drop"
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily)


#
tacc_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    group_by(Mfamily, Yfamily, Fit) |>  #, cluster_opt) |> 
    summarize(
        min = min(cover_TNIE),
        mean = mean(cover_TNIE), 
        max = max(cover_TNIE)
        # mean_rej_PNDE = mean(rejectnull_individual_PNDE), 
        # sd_rej_PNDE = sd(rejectnull_individual_PNDE),
        # median_rej_PNDE = median(rejectnull_individual_PNDE),
        # 
        # mean_rej_TNIE = mean(rejectnull_individual_TNIE), 
        # sd_rej_TNIE = sd(rejectnull_individual_TNIE),
        # median_rej_TNIE = median(rejectnull_individual_TNIE)
        
    ) #|>
    # tidyr::pivot_longer(
    #     cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
    #     names_to = c(".value", "statistic"),
    #     names_pattern = "(.+)_(.+)"
    # ) |>
    # arrange(Mfamily, Yfamily) |> 
    # print(n = 32)



# ══════════════════════════════
#    Visualize results 
# ══════════════════════════════
# viz for TNIE bias 
library(ggplot2)
library(dplyr)

tacc_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0, ) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ Mfamily) +
    labs(subtitle = "Bias by Mediator (horz) & Outcome (vert) type") +
    theme_minimal()

# Add condition and method variables to iteration-level data 
sim_output <- tacc_data |> 
    left_join(tacc_perf_measures |> 
                  select("method", "Fit", "cluster_opt", "condition":"icc"), 
              by = c("condition", "model" = "method")) 

# 
sim_output |> 
    ggplot(aes(x = Fit, y = (individual_ie_Estimate - individual_tnie), 
               fill = cluster_opt)) +
    geom_boxplot() +
    facet_grid(Yfamily ~ Mfamily)

#
tacc_perf_measures |> 
    ggplot(aes(x = interaction(J, Nj_low), y = bias_individual_TNIE, 
               color = Fit, shape = cluster_opt)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ Mfamily) +
    theme_minimal()

#
tacc_perf_measures |> 
    ggplot(aes(x = interaction(J, Nj_low), y = cover_TNIE, 
               color = Fit, shape = cluster_opt)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ Mfamily) +
    theme_minimal()


names(tacc_perf_measures)





# warnings 
# unique(tacc_data$warnings)
table(tacc_data$warnings, tacc_data$model)
# method 1 = mlr  cwc.FE
# 2 = glm      cwc.FE
# 3 = mlr         cwc 
# 4 = glm         cwc





# reporting quadratic 100 reps 2025-01-24 results (from TACC) -------------

# import results 

quad_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-24-test_100-rep_all-quad-conditions-with-all-methods/Data/S1_simulation-data_2025-01-24.rds")
quad_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-24-test_100-rep_all-quad-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-24.rds")

# took about 49 mins to run 
# readRDS(file = "Output/S1_Simulation-Output/from-tacc/2025-01-24-test_100-rep_all-quad-conditions-with-all-methods/S1_Computation-Time.rds") |>
#     view()

#     
quad_perf_measures |> 
    # filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", ends_with("_PNDE"), ends_with("_TNIE")) |> 
    view()



# summary of true effects 
quad_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE") |>
    group_by(Mfamily, Yfamily) |>
    summarize(
        mean_true_PNDE = mean(true_PNDE),
        sd_true_PNDE = sd(true_PNDE),
        median_true_PNDE = median(true_PNDE),
        mean_true_TNIE = mean(true_TNIE),
        sd_true_TNIE = sd(true_TNIE),
        median_true_TNIE = median(true_TNIE),
        .groups = "drop"
    ) |>
    tidyr::pivot_longer(
        cols = c(starts_with("mean"), starts_with("sd"), starts_with("median")),
        names_to = c(".value", "statistic"),
        names_pattern = "(.+)_(.+)"
    ) |>
    arrange(Mfamily, Yfamily)


#
quad_perf_measures |>
    # filter(cluster_opt == "cwc" & Fit == "mlr") |>
    select("Fit", "cluster_opt", "J":"Yfamily", "cover_PNDE":"true_TNIE") |>
    group_by(J, Nj_low, Mfamily, Yfamily, cluster_opt, Fit) |>
    # filter(Nj_low == 50) |> 
    summarize(
        min_coverage = min(cover_TNIE), 
        mean_coverage = mean(cover_TNIE), 
        max_coverage = max(cover_TNIE),
              mean_bias = mean(bias_individual_TNIE), 
              mean_MSE = mean(MSE_individual_TNIE)) |> 
    print(n = 96)



# Bias 
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    # filter(Nj_low == 5) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0, ) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(interaction(Nj_low, Yfamily) ~ Mfamily) +
    labs(subtitle = "Bias by Mediator (horz) & Outcome (vert) type") +
    theme_minimal()

# Coverage 
## U[5, 20]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 5) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [5, 20]") 
    theme_minimal()
## U[50, 100]
quad_perf_measures |> 
    # filter(Yfamily == "gaussian" & Nj_low == 5) |> 
    filter(Nj_low == 50) |>
    ggplot(aes(x = factor(J), y = cover_TNIE, color = cluster_opt, shape = Fit, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit))) +
    facet_grid(Yfamily ~ interaction(Nj_low, Mfamily)) +
    labs(subtitle = "CI coverage rate by Mediator (horz) & Outcome (vert) type for small clusters [50, 100]") 
    theme_minimal()

    
names(quad_perf_measures)





# names(tacc_data)
# names(tacc_perf_measures)

tacc_perf_measures |> 
    filter(cluster_opt == "cwc" & Fit == "mlr") |> 
    # filter(Mfamily %in% c("gaussian", "binomial") & Yfamily %in% c("binomial")) |> 
    select("Fit", "cluster_opt", "J":"Yfamily", "rejectnull_individual_PNDE":"true_TNIE")




