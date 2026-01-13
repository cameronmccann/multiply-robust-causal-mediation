################################################################################
####################### Simulation 1 - Results Processing ######################
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
#       a format to obtain results
#
# Inputs:
#   - Simulation output .rds files in: "Output/S1_Simulation-Output/2025-10-22_1000-reps/"
# 
# Outputs: 
#   - 
# 
#
# Last Updated: 2026-01-12
#
#
# Notes:
#   To-Do
#       # Check & run error & warning sections 
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
    glue
)


# User Inputs / Global Options --------------------------------------------

# Date of simulation 
sim_date <- "2025-10-22" 

# Number of replications
reps <- 1000 #600#200 

# Add subdirectory for results, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- NULL #"2025-10-22_1000-reps" # set to NULL on final run

# Where to pull simulaiton output from 
additional_folder_output <- "2025-10-22_1000-reps" 


# Set up directory structure ----------------------------------------------

# Create directory to store results 
results_root <- "Output/S1_Results"

if (!dir.exists(results_root)) {
    dir.create(results_root, recursive = TRUE)  
}

## Combine results_root + run-specific subfolder & add subfolder
if (is.null(additional_folder_results)) {
    results_path <- file.path(results_root)
} else {
    results_path <- file.path(results_root, additional_folder_results)
}

if (!dir.exists(results_path)) {
    dir.create(results_path, recursive = TRUE)
}

## Create Data, Figures, and Tables subfolders
results_subfolders <- c("Data", "Figures", "Tables")
for (sf in results_subfolders) {
    dir_sf <- file.path(results_path, sf)
    if (!dir.exists(dir_sf)) dir.create(dir_sf, recursive = TRUE)
}

## Create logs subfolder in Data
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
    path = sim_output_path,               # e.g., "Output/S1_Simulation-Output"
    pattern = "^S1_condition.*\\.rds$",   # only files starting with S1_condition and ending in .rds
    full.names = TRUE
) |> sort()

# Extract padded condition numbers & add "cond_" prefix
cond_ids <- rds_files_all |> 
    basename() |> 
    str_extract("S1_condition-\\d{2}") |> 
    str_extract("\\d{2}") 

cond_ids <- paste0("cond_", cond_ids)

# ══════════════════════════════
#    Create overall list (dropping iterations with error) 
# ══════════════════════════════

# Record log
log_file <- file.path(logs_path, "processing-sim-output.txt")
sink(log_file, split = TRUE)

# Define once (outside imap) for efficiency and clarity
error_msg <- "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\""
methd <- c("mlr-cwc.FE", "mlr-cwc", "glm-cwc.FE", "glm-cwc")

# Create overall simulation output list with padded condition numbers as element names (e.g., cond_01) & drop any iterations with errors
overall_list <- purrr::set_names(
    purrr::imap(rds_files_all, function(file, i) {
        # Load file 
        data <- readRDS(file)

        # Identify iterations with error in any method
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
            cat(sprintf(
                "File %d: Dropping %d iterations with error msg at indices: %s (%s) \n",
                i, length(prblm_iter), paste(prblm_iter, collapse = ", "), basename(file)
            ))
        }
        
        return(data)
    }), 
    nm = cond_ids
)

# Save overall simulation output list for reference later
saveRDS(overall_list, 
        file = file.path(results_path, "Data", 
                         paste0("S1_overall-output-list_", sim_date, ".rds")))

# Close log
sink()


# Create dataframe version of simulation output ---------------------------

## Check for missing conditions & duplicate files --------------------------

# ═══════════════════
#    Missing conditions 
# ═══════════════════

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
    cat("No missing conditions!")
} else {
    cat("Missing conditions: \n")
    print(missing_conditions)
}

# ═══════════════════
#    Duplicate files 
# ═══════════════════

# Count duplicates
duplicates_df <- data.frame(file_pattern = rds_filenames[duplicated(rds_filenames)]) |>
    count(file_pattern, name = "count") |>
    filter(count > 1)

if (nrow(duplicates_df) == 0) {
    cat("No duplicates!")
} else {
    cat("Duplicate entries in rds_files_all:\n")
    print(duplicates_df)
}

## Create dataframe version  -----------------------------------------------

# Read each file and attach file name for later extraction of condition information
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

# ═══════════════════
#    Loop to create dataframe 
# ═══════════════════

# Record log
sink(log_file, split = TRUE, append = TRUE)

# Define error message & methods to efficiently drop those error cases 
error_msg <- "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\""
methd <- c("mlr-cwc.FE", "mlr-cwc", "glm-cwc.FE", "glm-cwc")

# Loop
sim1_data <- purrr::map_dfr(all_data_list, function(file_data) {
    
    # Get the file name
    fname <- file_data$file
    
    cat(sprintf("\n---------------------------------\ncond_%s:\n", 
                       str_extract(basename(fname), "(?<=condition-)[0-9]{2}")))
    
    # Remove "file" element
    iter_data <- file_data[names(file_data) != "file"]
    
    
    # Identify iterations with error in any method
    error_iter <- which(vapply(iter_data, function(iter) {
        
        # Extract error messages for relevant methods
        msgs <- vapply(methd, function(k) {
            m <- iter$results[[k]]$error_message
            if (is.null(m)) "" else m
        }, character(1))
        
        # Check for the specific error
        any(grepl(error_msg, msgs, fixed = TRUE))
    }, logical(1)))
    
    # Drop iterations with error
    if (length(error_iter)) {
        iter_data <- iter_data[-error_iter]
    }
    
    # Message information 
    cat(sprintf(
        "Dropping %d iterations with error msg at indices %s: %s\n", 
        length(error_iter), paste(error_iter, collapse = ", "), basename(fname)
    ))
    # # Identify and drop error iterations
    # error_iter <- which(vapply(iter_data, function(iter) {
    #     msg <- iter$results$`mlr-cwc.FE`$error_message
    #     !is.null(msg) && grepl(
    #         "Error in internal function `v.ac()`: no applicable method for 'predict' applied to an object of class \"NULL\"",
    #         msg, fixed = TRUE
    #     )
    # }, logical(1)))
    # 
    # if (length(error_iter)) {
    #     iter_data <- iter_data[-error_iter]
    # }
    # 
    # cat(sprintf("Dropping %d iterations with error msg at indices %s: %s\n", 
    #             length(error_iter), paste(error_iter, collapse = ", "), basename(fname)))
    
    # Extract condition number from filename
    condition_number <- str_extract(basename(fname), "(?<=condition-)[0-9]{2}")
    # Extract condition information from the file name; expected pattern: "reps-200_quad-FALSE_M-gaussian_Y-gaussian_nj-[5,20]_J-100"
    pattern <- "reps-([0-9]+)_null-([A-Za-z]+)_quad-([A-Za-z]+)_M-([A-Za-z]+)_Y-([A-Za-z]+)_nj-\\[([^\\]]+)\\]_J-([0-9]+)"
    matches <- str_match(fname, pattern)
    
    if (is.na(matches[1,1])) {
        warning("File name ", fname, " does not match expected pattern. Setting condition info to NA.")
        reps_val <- NA; null_val <- NA; quad_val <- NA
        M_val <- NA; Y_val <- NA; nj_val <- NA; J_val <- NA
    } else {
        reps_val <- as.numeric(matches[1,2])
        # Convert null & quad to logical if it is "TRUE" or "FALSE"
        null_val <- tolower(matches[1,3]) == "true"
        quad_val <- tolower(matches[1,4]) == "true"
        M_val <- matches[1,5]
        Y_val <- matches[1,6]
        nj_val <- matches[1,7] # this remains as a string (e.g., "5,20")
        J_val <- as.numeric(matches[1,8])
    }
    
    # Trim to desired number of reps
    if (length(iter_data) > reps) {
        cat(glue("Trimming to first {reps} iterations from {length(iter_data)} for file: {basename(fname)}\n"))
        iter_data <- iter_data[seq_len(reps)]
    }
    
    # Process iterations - nested map
    purrr::map_dfr(seq_along(iter_data), function(i) {
        purrr::map_dfr(1:nrow(methds), function(mod) {
            
            # Create a key to extract the correct results from the simulation object
            key <- glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")
            res <- iter_data[[i]]$results[[key]]
            if (is.null(res) || length(res) == 0) res <- list()
            
            estimates <- res$estimates
            if (is.null(estimates) || length(estimates) == 0) {
                estimates <- tibble(
                    Effect = character(), EffectVersion = character(),
                    Estimate = numeric(), StdError = numeric(),
                    CILower = numeric(), CIUpper = numeric()
                )
            }
            
            # Extract individual and cluster estimates 
            individual_de <- estimates|> 
                filter(EffectVersion == "Individual-Avg" & grepl("DE", Effect)) |> 
                slice(1)
            individual_ie <- estimates |> 
                filter(EffectVersion == "Individual-Avg" & grepl("IE", Effect))|> 
                slice(1)
            cluster_de <- estimates |> 
                filter(EffectVersion == "Cluster-Avg" & grepl("DE", Effect)) |>  
                slice(1)
            cluster_ie <- estimates |>  
                filter(EffectVersion == "Cluster-Avg" & grepl("IE", Effect)) |> 
                slice(1)
            
            # Extract effects
            individual_effects <- iter_data[[i]]$effects$individual
            if (is.null(individual_effects) || length(individual_effects) == 0) {
                individual_effects <- list(pnde = NA_real_, tnie = NA_real_,
                                           tnde = NA_real_, pnie = NA_real_)
            }
            
            cluster_effects <- iter_data[[i]]$effects$cluster
            if (is.null(cluster_effects) || length(cluster_effects) == 0) {
                cluster_effects <- list(pnde = NA_real_, tnie = NA_real_,
                                        tnde = NA_real_, pnie = NA_real_)
            }
            
            # Warnings & errors
            warns <- res$warnings
            if (is.null(warns)) warns <- character(0)
            warnings_chr <- if (length(warns) == 0) NA_character_ else paste(warns, collapse="; ")
            
            error_msg <- res$error_message
            if (is.null(error_msg)) error_msg <- NA_character_
            
            # Number of folds, raw iteration, PS overlap info, & proportion of treated in clusters
            num_folds <- res$num_folds
            if (is.null(num_folds)) num_folds <- NA_integer_
            
            raw_it <- iter_data[[i]]$raw_iteration
            if (is.null(raw_it)) raw_it <- NA_integer_
            
            ps_overlap <- iter_data[[i]]$overlap$ps_summary
            if (is.null(ps_overlap)) ps_overlap <- NA_character_
            
            clust_trt_prop_chr <- toString(round(iter_data[[i]]$parameters$clust_trt_prop, 2))
            nj_sizes_chr <- toString(iter_data[[i]]$parameters$nj_sizes)
            
            # Return single row
            tibble(
                file_name = fname,
                condition_num = condition_number,
                reps = reps_val,
                ifnull = null_val,
                quadratic = quad_val,
                Mfamily = M_val,
                Yfamily = Y_val,
                nj = nj_val,
                J = J_val,
                iteration = i,
                raw_iteration = raw_it,
                model = mod,
                Fit = methds[mod, "Fit"],
                cluster_opt = methds[mod, "cluster_opt"],
                warnings = warnings_chr,
                n_warnings = length(warns),
                error = res$error,
                error_message = error_msg,
                num_folds = num_folds,
                
                individual_de_Estimate = get1(individual_de$Estimate),
                individual_de_StdError = get1(individual_de$StdError),
                individual_de_CILower  = get1(individual_de$CILower),
                individual_de_CIUpper  = get1(individual_de$CIUpper),
                
                individual_ie_Estimate = get1(individual_ie$Estimate),
                individual_ie_StdError = get1(individual_ie$StdError),
                individual_ie_CILower  = get1(individual_ie$CILower),
                individual_ie_CIUpper  = get1(individual_ie$CIUpper),
                
                cluster_de_Estimate = get1(cluster_de$Estimate),
                cluster_de_StdError = get1(cluster_de$StdError),
                cluster_de_CILower  = get1(cluster_de$CILower),
                cluster_de_CIUpper  = get1(cluster_de$CIUpper),
                
                cluster_ie_Estimate = get1(cluster_ie$Estimate),
                cluster_ie_StdError = get1(cluster_ie$StdError),
                cluster_ie_CILower  = get1(cluster_ie$CILower),
                cluster_ie_CIUpper  = get1(cluster_ie$CIUpper),
                
                # Individual effects
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                # Cluster effects
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie,
                # Propensity score overlap summary
                ps_overlap = ps_overlap,
                # Proportion of clusters in trt
                clust_trt_prop = clust_trt_prop_chr,
                # Cluster sizes
                nj_sizes = nj_sizes_chr
            )
        }) # end models loop
    }) # end iterations loop
}) # end files loop

# Save 
saveRDS(sim1_data, file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

# Close log
sink()


# Check error messages ----------------------------------------------------

# Import sim dataframe 
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))

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


# Check warning messages --------------------------------------------------

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
    mutate(warnings = str_trim(warnings),     # trim whitespace
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

# ══════════════════════════════
#    More cleaning of dataframe (drop unimportant warning messages)
# ══════════════════════════════

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

# View results
print(distinct_warnings)

# Close log
sink()


# Saving different versions of dataframe ----------------------------------

# ═══════════════════
#    No warnings dataframe 
# ═══════════════════

# Drop iterations where warnings != NA
sim1_data_nowarnings <- sim1_data_cleaned[sim1_data_cleaned$n_warnings == 0, ]

# Save 
saveRDS(sim1_data_nowarnings, file = file.path(
    results_path,
    "Data",
    paste0("S1_simulation-data_", sim_date, "_excludes-warnings.rds")
))

# ═══════════════════
#    No nonconvergence dataframe 
# ═══════════════════

# Record log
sink(file.path(logs_path, "nonconvergence-messages.txt"), split = TRUE)
cat("\n~~~ Dropping nonconverging cases ~~~\n")

# Define set of convergence-related patterns to match any of the cases 
nonconvergence_patterns <- paste(
    c(
        # non-convergence
        "did not converge",
        "failure to converge",
        "failed to converge",
        # PIRLS/pwrss convergence issues
        "pwrssUpdate did not converge",
        "PIRLS step-halvings failed to reduce deviance",
        # potetntially problematic (double check these messages)
        "Hessian is numerically singular",
        "degenerate.*Hessian"#,
        # "unable to evaluate scaled gradient",
        # "eigenvalue", # covers both large eigenvalue and eigenvalue ratio
        # "Residual degrees of freedom.*negative or zero"
    ),
    collapse = "|"
)

# Collect those with convergence issues
convergence_issues <- sim1_data_cleaned[!is.na(sim1_data_cleaned$warnings) &
                                            str_detect(sim1_data_cleaned$warnings,
                                                       regex(nonconvergence_patterns, ignore_case = TRUE)), 
                                        c("condition_num", "iteration", "Fit", "cluster_opt", "warnings")]

# Filter out rows with convergence-related warnings
sim1_data_converged <- sim1_data_cleaned[is.na(sim1_data_cleaned$warnings) |
                                             !str_detect(sim1_data_cleaned$warnings,
                                                         regex(nonconvergence_patterns, ignore_case = TRUE)), ]

cat(sprintf("\nDropping %d rows due to convergence issues\n", nrow(convergence_issues)))
cat(sprintf("Keeping %d rows in converged dataset\n", nrow(sim1_data_converged)))

# See breakdown by method 
convergence_issues |>
    group_by(Fit, cluster_opt) |>
    summarize(n_dropped = n(), .groups = "drop") |>
    print(n = Inf)

# Save 
saveRDS(sim1_data_converged, file = file.path(
    results_path,
    "Data",
    paste0("S1_simulation-data_", sim_date, "_converged-only.rds")
))

# Close log
sink()

############################# END OF PROCESSING ################################

