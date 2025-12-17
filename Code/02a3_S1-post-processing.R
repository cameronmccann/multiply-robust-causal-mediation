################################################################################
############ Simulation 1 - Obtain Results part 3: post-processing #############
################################################################################

############################ Script Description ################################
#
# Author: 
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       Part 3 takes processed simulation output (either done in part 1 or 2, 
#       depending on whether you want "problematic" cases replaced or not) and 
#       summarizes and reports the results for the first simulation study 
#       (i.e., obtains performance measures). 
# 
#       Note: set use_updated <- TRUE for files with "updated-" in name 
#               (includes replaced values) otherwise set use_updated <- FALSE 
#     
#
# Last Updated: 2025-12-16
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
    flextable, 
    stringr, 
    ggdag, 
    dagitty, 
    huxtable, 
    glue, 
    purrr
)


# User Inputs / Global Options --------------------------------------------

# Choose which dataframe to postprocess:
use_updated <- TRUE   # TRUE = uses S1_updated-overall-output-dataframe_*.rds
                      # FALSE = uses S1_overall-output-dataframe_*.rds

# Create prefix for file labels when saving 
prefix <- if (use_updated) "S1_updated" else "S1"


# Date of simulation 
sim_date <- "2025-10-22" # "2025-09-03"

# Number of replications
reps <- 1000 #600#200 

# Results folder
results_root <- "Output/S1_Results" #path <- "Output/S1_Results"

# Add subdirectory, if desired (e.g., for test runs): where do you want results stored
additional_folder_results <- "2025-10-22_1000-reps" # "2025-09-03_200-reps" 

results_path <- file.path(results_root, additional_folder_results)

# Safety: check that folders exist
stopifnot(dir.exists(results_path))
stopifnot(dir.exists(file.path(results_path, "Data")))
stopifnot(dir.exists(file.path(results_path, "Tables")))


# Import data -------------------------------------------------------------

# Import sim dataframe (either with or without replacements, based on use_updated)
if (use_updated) {
    input_df_file <- file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds"))
} else {
    input_df_file <- file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds"))
}

if (!file.exists(input_df_file)) stop("Input dataframe not found: ", input_df_file)

sim1_data <- readRDS(input_df_file)

sim1_data_all <- sim1_data # save all cases version for later


# Check warning & error messages ------------------------------------------

# ══════════════════════════════
#    Error messages - should all be dropped 
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
# # Import sim dataframe (with replacements)
# sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

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
table(sim1_data_cleaned$n_warnings, paste0(sim1_data_cleaned$Fit, "-", sim1_data_cleaned$cluster_opt))
## rows (0-+) are number of warnings; columns are models used


# ══════════════════════════════
#    Look into warnings (clean to find distinct warning messages)
# ══════════════════════════════
# 1. Drop NAs
warnings_raw <- sim1_data_cleaned$warnings[!is.na(sim1_data_cleaned$warnings)]
# 2. Split compound warnings by semicolon, trim whitespace
warnings_split <- unlist(str_split(warnings_raw, pattern = ";\\s*"))
# 3. Remove warnings like "In a.c(): RE.glm.rs specified but no random_slope_vars provided"
warnings_filtered <- lapply(warnings_split, function(w) {
    w[!grepl("In .*\\(\\): RE\\.glm\\.rs specified but no random_slope_vars provided", w)]
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
# [10] "iterations terminated prematurely because of singularities"                                                                                                                                                                                                                                  
# [11] "Error in algorithm SL.gam \n  The Algorithm will be removed from the Super Learner (i.e. given weight 0)"                                                                                                                                                                                    
# [12] "Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables?"                                                                                                                                                                                                                
# [13] "Model is nearly unidentifiable: large eigenvalue ratio\n - Rescale variables?"                                                                                                                                                                                                               
# [14] "unable to evaluate scaled gradient"                                                                                                                                                                                                                                                          
# [15] "Model failed to converge: degenerate  Hessian with 2 negative eigenvalues"                                                                                                                                                                                                                   
# [16] "Model failed to converge: degenerate  Hessian with 1 negative eigenvalues"                                                                                                                                                                                                                   
# [17] "convergence code 3 from bobyqa: bobyqa -- a trust region step failed to reduce q"                                                                                                                                                                                                            
# [18] "convergence code -4 from nloptwrap: NLOPT_ROUNDOFF_LIMITED: Roundoff errors led to a breakdown of the optimization algorithm. In this case, the returned minimum may still be useful. (e.g. this error occurs in NEWUOA if one tries to achieve a tolerance too close to machine precision.)"
# [19] "n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n"                                                                                                                                                          
# [20] "Model failed to converge: degenerate  Hessian with 3 negative eigenvalues"                                                                                                                                                                                                                   
# [21] "Re-running estimation of coefficients removing failed algorithm(s)\nOriginal coefficients are: \n0.726183574907129, 0.273816425092871"                                                                                                                                                       
# [22] "Model failed to converge: degenerate  Hessian with 4 negative eigenvalues"                    

# # Unique warnings by model 
# sim1_data_cleaned |> 
#     mutate(model_key = paste(Fit, cluster_opt, sep = "-")) |> 
#     mutate(warning_list = str_split(warnings, ";\\s*")) |> 
#     mutate(
#         warning_list = map(warning_list, ~ .x[!grepl("RE\\.glm\\.rs specified.*|using random intercept only", .x)]),
#         warning_list = map(warning_list, ~ trimws(.x[.x != ""])),
#         warning_list = map(warning_list, unique),
#         warning_string = map_chr(warning_list, ~ if (length(.x) > 0) paste(.x, collapse = "; ") else NA_character_)
#     ) |> 
#     mutate(
#         warning_string = warning_string |>
#             str_replace_all("max\\|grad\\| = [0-9\\.eE+-]+", "max|grad| = <number>") |>
#             str_replace_all("tol = [0-9\\.eE+-]+", "tol = <number>")
#     ) |> 
#     filter(!is.na(warning_string)) |> 
#     # # warnings by model 
#     # count(model_key, warning_string, sort = TRUE, name = "frequency") |> 
#     # warnings by model & data generation condition
#     count(condition_num, model_key, warning_string, sort = TRUE, name = "frequency") |> 
#     # count(model_key, warning_string, sort = TRUE, name = "frequency") |> 
#     # group_by(model_key) |> 
#     # summarise(unique_warnings = unique(warning_string), .groups = "drop") |> 
#     arrange(condition_num, model_key) |> 
#     # arrange(model_key) |> 
#     print(n = Inf)
# 
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

# Save 
# saveRDS(sim1_data_nowarnings, file = file.path(results_path, "Data", paste0("S1_updated-simulation-data_", sim_date, "_excludes-warnings.rds")))
saveRDS(sim1_data_nowarnings,
        file = file.path(results_path, "Data",
                         paste0(prefix, "_simulation-data_", sim_date, "_excludes-warnings.rds")))

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
# saveRDS(sim1_data_converged, file = file.path(results_path, "Data", paste0("S1_updated-simulation-data_", sim_date, "_converged-only.rds")))
saveRDS(sim1_data_converged,
        file = file.path(results_path, "Data",
                         paste0(prefix, "_simulation-data_", sim_date, "_converged-only.rds")))



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





# sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_includes-warnings.rds"))


# Compute Performance Measures --------------------------------------------

# ══════════════════════════════
#    For converging cases 
# ══════════════════════════════
# Import data 
## import data with only converging cases
# sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_updated-simulation-data_", 
#                                    sim_date, 
#                                    "_converged-only.rds"))
# Using sim1_data_converged instead of reimporting data

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
perf_summary <- as.data.frame(sim1_data_converged) |>
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


# table(perf_summary$quadratic)
# 
# perf_summary

# Save performance measures 
## Only include converged cases 
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", 
#                                     sim_date, "_converged-only.rds")) 
saveRDS(perf_summary,
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_performance-measures_", sim_date, "_converged-only.rds")))

# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", #linear_",
#                                     sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))

# Save performance measures (with warnings excluded)
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_performance-measures_", 
#                                     sim_date, "_excludes-warnings.rds")) 
# saveRDS(perf_summary,
#         file = file.path(results_path, "Tables",
#                          paste0(prefix, "_performance-measures_", sim_date, "_converged-only.rds")))


# ══════════════════════════════
#    For all cases 
# ══════════════════════════════

# Import data 
## Import original/overall data
# sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

# Use sim1_data_all for performance measures 

# Compute performance measures 
perf_summary <- as.data.frame(sim1_data_all) |>
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
# saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", #linear_",
#                                     sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))
saveRDS(perf_summary,
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_performance-measures_", sim_date, ".rds")))


# Compute Convergence Rates -----------------------------------------------

# Using sim1_data_converged & sim1_data to compute convergence rates 

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
# saveRDS(convergence_rates, file = paste0(results_path, "/Tables/S1_convergence-rates_", 
#                                          sim_date, ".rds"))
saveRDS(convergence_rates, 
        file = file.path(results_path, "Tables",
                         paste0(prefix, "_convergence-rates_", sim_date, ".rds")))


############################# END OF PROCESSING ################################






