################################################################################
##################### Simulation 1 - Obtain Results part 2 #####################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       Part 2 replaces problematic cases with simulation output from personal computer.
# 
#       This code then summarizes and reports the results for the 
#       first simulation study (i.e., obtains performance measures). 
# 
#       Note: files with "updated-" in name are versions from this script
#
# To obtain output files from TACC, run a similar command in your terminal: 
# scp -r "cameronmccann@ls6.tacc.utexas.edu:/home1/10384/cameronmccann/multiply-robust-causal-mediation copy/Output/S1_Simulation-Output/2025-10-22_1000-reps" \
# /Users/cameronmccann/Documents/Research-2025/multiply-robust-causal-mediation/Output/S1_Simulation-Output/
#     
#
# Last Updated: 2026-01-06
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

# Date of simulation 
sim_date <- "2025-10-22" # "2025-09-03"

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


## Identify problematic iterations (very extreme estimates) ----------------

# Import overall simulation output list (created in 02a1_S1-results-processing.R)
overall_list <- readRDS(file = file.path(
    results_path, "Data", paste0("S1_overall-output-list_", sim_date, ".rds")
))

# Below we identify "problematic" iterations (i.e., extreme estimates with large 
# influence) by checking whether the raw bias is greater than the 75th percentile +
# 3xIQR or less than the 25th percentile - 3xIQR. If either is true, we flag the 
# iteration. This is repeated across all estimation methods. 

# ══════════════════════════════
#    Identify iterations with extreme estimates for each model across conditions
# ══════════════════════════════

# Helper function: difference between estimated & true tnie 
tnie_diff <- function(x, key) {
    res <- x$results[[key]]
    est <- res$estimates
    tnie <- x$effects$individual$tnie
    
    as.numeric(est[2, "Estimate"] - tnie)
}

# Helper function: identify extreme differences estimated & true tnie 
find_extreme_idx <- function(x) {
    q25 <- quantile(x, 0.25, na.rm = TRUE)
    q75 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- IQR(x, na.rm = TRUE)
    which(x < (q25 - 3 * iqr) | x > (q75 + 3 * iqr))
}

# Methods to check (single source of truth)
methods_key <- c(
    "mlr-cwc.FE",
    "mlr-cwc",
    "glm-cwc.FE",
    "glm-cwc"
)

# Create a list containing bias (estimate - tnie) and extreme estimates if present
extreme_list <- purrr::imap(overall_list, function(lst, cod_name) {
    purrr::set_names(
        lapply(methods_key, function(key) {
            # difference
            diffs <- vapply(lst, tnie_diff, numeric(1), key = key)
            
            # identify extreme estimates
            extreme_indx <- find_extreme_idx(diffs)
            
            # pull raw_iteration & seed number for extreme indices 
            if (length(extreme_indx)) {
                # iteration 
                iteration <- vapply(
                    lst[extreme_indx],
                    function(x) x$raw_iteration,
                    numeric(1)
                )
                # seed
                seed <- vapply(
                    lst[extreme_indx],
                    function(x) x$seed,
                    numeric(1)
                )
            } else {
                # empty vectors 
                iteration <- numeric(0)
                seed <- numeric(0)
            }
            
            # Return list
            list(
                tnie_diff = diffs, 
                extreme_indx = extreme_indx, 
                iteration = iteration, 
                seed = seed
                )
        }),
        method_keys
    )
})


# # Create a list containing bias (estimate - tnie) and extreme estimates if present
# extreme_list <- purrr::imap(overall_list, function(lst, nm) {
#     # mlr-cwc.FE
#     ## difference
#     mlrcwcFE_diff <- vapply(lst, function(x) {
#         x$results$`mlr-cwc.FE`$estimates[2, "Estimate"] - x$effects$individual$tnie
#     }, numeric(1))
#     ## identify extreme estimates
#     mlrcwcFE_extreme_indx <- which(
#         mlrcwcFE_diff > quantile(mlrcwcFE_diff, 0.75, na.rm = TRUE) + 3 * IQR(mlrcwcFE_diff)  |
#             mlrcwcFE_diff < quantile(mlrcwcFE_diff, 0.25, na.rm = TRUE) - 3 * IQR(mlrcwcFE_diff)
#     )
#     ## pull raw_iteration & seed number for extreme indices 
#     if (length(mlrcwcFE_extreme_indx)) {
#         # iteration 
#         mlrcwcFE_extreme_iter <- vapply(
#             lst[mlrcwcFE_extreme_indx],
#             function(x) x$raw_iteration,
#             numeric(1)
#         )
#         # seed
#         mlrcwcFE_extreme_seed <- vapply(
#             lst[mlrcwcFE_extreme_indx],
#             function(x) x$seed,
#             numeric(1)
#         )
#     } else {
#         # empty vectors 
#         mlrcwcFE_extreme_iter <- numeric(0)
#         mlrcwcFE_extreme_seed <- numeric(0)
#     }
#     
#     # mlr-cwc
#     ## difference
#     mlrcwc_diff <- vapply(lst, function(x) {
#         x$results$`mlr-cwc`$estimates[2, "Estimate"] - x$effects$individual$tnie
#     }, numeric(1))
#     ## identify extreme estimates
#     mlrcwc_extreme_indx <- which(
#         mlrcwc_diff > quantile(mlrcwc_diff, 0.75, na.rm = TRUE) + 3 * IQR(mlrcwc_diff)  |
#             mlrcwc_diff < quantile(mlrcwc_diff, 0.25, na.rm = TRUE) - 3 * IQR(mlrcwc_diff)
#     )
#     ## pull raw_iteration & seed number for extreme indices 
#     if (length(mlrcwc_extreme_indx)) {
#         # iteration 
#         mlrcwc_extreme_iter <- vapply(
#             lst[mlrcwc_extreme_indx],
#             function(x) x$raw_iteration,
#             numeric(1)
#         )
#         # seed
#         mlrcwc_extreme_seed <- vapply(
#             lst[mlrcwc_extreme_indx],
#             function(x) x$seed,
#             numeric(1)
#         )
#     } else {
#         # empty vectors 
#         mlrcwc_extreme_iter <- numeric(0)
#         mlrcwc_extreme_seed <- numeric(0)
#     }
#     
#     # glm-cwc.FE
#     ## difference
#     glmcwcFE_diff <- vapply(lst, function(x) {
#         x$results$`glm-cwc.FE`$estimates[2, "Estimate"] - x$effects$individual$tnie
#     }, numeric(1))
#     ## identify extreme estimates
#     glmcwcFE_extreme_indx <- which(
#         glmcwcFE_diff > quantile(glmcwcFE_diff, 0.75, na.rm = TRUE) + 3 * IQR(glmcwcFE_diff)  |
#             glmcwcFE_diff < quantile(glmcwcFE_diff, 0.25, na.rm = TRUE) - 3 * IQR(glmcwcFE_diff)
#     )
#     ## pull raw_iteration & seed number for extreme indices 
#     if (length(glmcwcFE_extreme_indx)) {
#         # iteration 
#         glmcwcFE_extreme_iter <- vapply(
#             lst[glmcwcFE_extreme_indx],
#             function(x) x$raw_iteration,
#             numeric(1)
#         )
#         # seed
#         glmcwcFE_extreme_seed <- vapply(
#             lst[glmcwcFE_extreme_indx],
#             function(x) x$seed,
#             numeric(1)
#         )
#     } else {
#         # empty vectors 
#         glmcwcFE_extreme_iter <- numeric(0)
#         glmcwcFE_extreme_seed <- numeric(0)
#     }
#     
#     # glm-cwc
#     ## difference
#     glmcwc_diff <- vapply(lst, function(x) {
#         x$results$`glm-cwc`$estimates[2, "Estimate"] - x$effects$individual$tnie
#     }, numeric(1))
#     ## identify extreme estimates
#     glmcwc_extreme_indx <- which(
#         glmcwc_diff > quantile(glmcwc_diff, 0.75, na.rm = TRUE) + 3 * IQR(glmcwc_diff)  |
#             glmcwc_diff < quantile(glmcwc_diff, 0.25, na.rm = TRUE) - 3 * IQR(glmcwc_diff)
#     )
#     ## pull raw_iteration & seed number for extreme indices 
#     if (length(glmcwc_extreme_indx)) {
#         # iteration 
#         glmcwc_extreme_iter <- vapply(
#             lst[glmcwc_extreme_indx],
#             function(x) x$raw_iteration,
#             numeric(1)
#         )
#         # seed
#         glmcwc_extreme_seed <- vapply(
#             lst[glmcwc_extreme_indx],
#             function(x) x$seed,
#             numeric(1)
#         )
#     } else {
#         # empty vectors 
#         glmcwc_extreme_iter <- numeric(0)
#         glmcwc_extreme_seed <- numeric(0)
#     }
#     
#     # Return list
#     list(
#         `mlr-cwc.FE` = list(tnie_diff = mlrcwcFE_diff, 
#                             extreme_indx = mlrcwcFE_extreme_indx, 
#                             iteration = mlrcwcFE_extreme_iter, 
#                             seed = mlrcwcFE_extreme_seed),
#         `mlr-cwc` = list(tnie_diff = mlrcwc_diff, 
#                          extreme_indx = mlrcwc_extreme_indx, 
#                          iteration = mlrcwc_extreme_iter, 
#                          seed = mlrcwc_extreme_seed),
#         `glm-cwc.FE` = list(tnie_diff = glmcwcFE_diff, 
#                             extreme_indx = glmcwcFE_extreme_indx, 
#                             iteration = glmcwcFE_extreme_iter, 
#                             seed = glmcwcFE_extreme_seed),
#         `glm-cwc` = list(tnie_diff = glmcwc_diff, 
#                          extreme_indx = glmcwc_extreme_indx, 
#                          iteration = glmcwc_extreme_iter, 
#                          seed = glmcwc_extreme_seed)
#     )
# })

# Show extreme iterations for each model & condition 
purrr::imap(extreme_list, function(cond, cond_name) {
    purrr::imap(cond, function(method, method_name) {
        if (length(method$extreme_indx) > 0) {
            paste0(
                cond_name, " | ", method_name, ": ",
                "indx=", toString(method$extreme_indx),
                " | iter=", toString(method$iteration),
                " | seed=", toString(method$seed)
            )
        } else {
            NULL
        }
    }) |> purrr::compact()
}) |> purrr::compact() |> unlist() |> cat(sep = "\n")
# 2025-12-15:
# cond_01 | mlr-cwc.FE: indx=230, 557 | iter=230, 559 | seed=229531, 19233
# cond_01 | mlr-cwc: indx=230, 858 | iter=230, 865 | seed=229531, 705983
# cond_01 | glm-cwc.FE: indx=191, 230, 858 | iter=191, 230, 865 | seed=426662, 229531, 705983
# cond_01 | glm-cwc: indx=191, 230, 403, 858 | iter=191, 230, 403, 865 | seed=426662, 229531, 232749, 705983
# cond_02 | mlr-cwc: indx=416 | iter=419 | seed=875999
# cond_04 | mlr-cwc.FE: indx=253, 510 | iter=254, 513 | seed=134127, 388028
# cond_06 | mlr-cwc.FE: indx=212 | iter=217 | seed=729113
# cond_10 | glm-cwc: indx=43 | iter=43 | seed=468871
# cond_16 | mlr-cwc: indx=463 | iter=465 | seed=119637
# cond_19 | mlr-cwc.FE: indx=580 | iter=583 | seed=478500
# cond_20 | mlr-cwc.FE: indx=418 | iter=421 | seed=948810
# cond_25 | mlr-cwc.FE: indx=677 | iter=682 | seed=965831
# cond_25 | mlr-cwc: indx=677 | iter=682 | seed=965831
# cond_25 | glm-cwc.FE: indx=69, 144, 677, 957 | iter=69, 144, 682, 965 | seed=832984, 898270, 965831, 356832
# cond_25 | glm-cwc: indx=69, 677, 957 | iter=69, 682, 965 | seed=832984, 965831, 356832
# cond_26 | glm-cwc.FE: indx=328 | iter=331 | seed=269470
# cond_26 | glm-cwc: indx=328 | iter=331 | seed=269470
# cond_28 | mlr-cwc.FE: indx=857 | iter=865 | seed=705983
# cond_28 | mlr-cwc: indx=377, 857 | iter=379, 865 | seed=678094, 705983
# cond_28 | glm-cwc.FE: indx=89, 377, 660, 667, 857, 875 | iter=89, 379, 666, 673, 865, 883 | seed=446888, 678094, 126620, 276025, 705983, 152190
# cond_28 | glm-cwc: indx=89, 377, 660, 857, 875 | iter=89, 379, 666, 865, 883 | seed=446888, 678094, 126620, 705983, 152190
# cond_29 | mlr-cwc: indx=965 | iter=975 | seed=589627
# cond_36 | mlr-cwc.FE: indx=767 | iter=778 | seed=589326
# cond_37 | mlr-cwc: indx=1036 | iter=1045 | seed=762384
# cond_37 | glm-cwc.FE: indx=288, 1036 | iter=288, 1045 | seed=666953, 762384
# cond_37 | glm-cwc: indx=288, 1036 | iter=288, 1045 | seed=666953, 762384
# cond_39 | mlr-cwc.FE: indx=819 | iter=828 | seed=806726
# cond_40 | mlr-cwc.FE: indx=172, 275 | iter=172, 276 | seed=296995, 346669
# cond_40 | mlr-cwc: indx=172, 738, 890, 985 | iter=172, 746, 898, 993 | seed=296995, 689371, 577587, 521569
# cond_40 | glm-cwc.FE: indx=676, 985 | iter=682, 993 | seed=965831, 521569
# cond_40 | glm-cwc: indx=676, 985 | iter=682, 993 | seed=965831, 521569
# cond_41 | glm-cwc.FE: indx=6 | iter=6 | seed=90493
# cond_43 | mlr-cwc: indx=765 | iter=772 | seed=266060
# cond_43 | glm-cwc.FE: indx=253 | iter=253 | seed=574717
# cond_43 | glm-cwc: indx=253, 954 | iter=253, 962 | seed=574717, 225595
# cond_44 | mlr-cwc.FE: indx=114 | iter=115 | seed=521817
# cond_46 | glm-cwc.FE: indx=252 | iter=253 | seed=574717
# cond_46 | glm-cwc: indx=252 | iter=253 | seed=574717
# cond_47 | mlr-cwc: indx=322 | iter=325 | seed=178893
# cond_48 | mlr-cwc: indx=100 | iter=104 | seed=167465
# cond_52 | mlr-cwc.FE: indx=788 | iter=5357 | seed=560671
# cond_54 | glm-cwc.FE: indx=681 | iter=83531 | seed=544981
# cond_61 | mlr-cwc.FE: indx=562, 748 | iter=3740, 5080 | seed=769694, 340305
# cond_62 | mlr-cwc.FE: indx=497, 502, 569, 582, 626, 708, 798, 879, 891 | iter=14053, 14101, 16070, 16315, 17545, 19966, 22642, 24934, 25448 | seed=369688, 698492, 206574, 23459, 55088, 244338, 499166, 564136, 572751
# cond_63 | mlr-cwc.FE: indx=713 | iter=79582 | seed=632257
# cond_64 | mlr-cwc.FE: indx=207, 236, 615 | iter=1314, 1558, 4115 | seed=362398, 794982, 686630
# cond_64 | mlr-cwc: indx=7, 549, 855 | iter=43, 3649, 5795 | seed=468871, 192223, 548728
# cond_65 | mlr-cwc.FE: indx=89 | iter=2541 | seed=115790
# cond_66 | mlr-cwc.FE: indx=20, 379, 496, 499, 554, 705, 811 | iter=2438, 46716, 60086, 60510, 68992, 87017, 101314 | seed=283639, 102303, 981085, 998560, 523160, 693732, 474692
# cond_67 | mlr-cwc.FE: indx=225, 327, 877, 996, 1043, 1045 | iter=1461, 2165, 6082, 6967, 7290, 7303 | seed=972867, 306937, 956523, 411707, 814013, 179127
# cond_68 | mlr-cwc.FE: indx=68, 118, 347, 604, 954 | iter=1956, 3474, 9441, 17006, 26887 | seed=608465, 562497, 84271, 868095, 702240
# cond_69 | mlr-cwc.FE: indx=5, 124, 287, 461, 543, 743, 891 | iter=760, 14436, 32272, 53052, 60397, 83002, 102298 | seed=654070, 947055, 818437, 433695, 157278, 624048, 171376
# cond_70 | mlr-cwc.FE: indx=184, 658, 765, 974, 1023 | iter=1204, 4517, 5194, 6637, 7034 | seed=473758, 727283, 364363, 709756, 238757
# cond_71 | mlr-cwc.FE: indx=14, 68, 113, 163, 386, 479, 555, 964, 1030 | iter=390, 1801, 3335, 4998, 10331, 13128, 15287, 27452, 29791 | seed=780975, 464385, 351844, 903911, 387499, 844063, 240313, 705046, 722676
# cond_72 | mlr-cwc.FE: indx=133, 157, 630, 664, 717, 978 | iter=17163, 21056, 77183, 81914, 88148, 121887 | seed=338062, 519951, 637586, 365684, 74783, 124095

# cond_01 | glm-cwc.FE: indx=191 | iter=191 | seed=426662
# cond_01 | glm-cwc: indx=191 | iter=191 | seed=426662
# cond_17 | mlr-cwc.FE: indx=196 | iter=198 | seed=35168
# cond_25 | glm-cwc.FE: indx=69 | iter=69 | seed=832984
# cond_25 | glm-cwc: indx=69 | iter=69 | seed=832984
# cond_40 | mlr-cwc.FE: indx=172 | iter=172 | seed=296995
# cond_40 | glm-cwc.FE: indx=172 | iter=172 | seed=296995
# cond_40 | glm-cwc: indx=172 | iter=172 | seed=296995
# cond_44 | mlr-cwc.FE: indx=114 | iter=115 | seed=521817
# cond_47 | mlr-cwc: indx=125 | iter=125 | seed=184546
# cond_47 | glm-cwc.FE: indx=125 | iter=125 | seed=184546
# cond_47 | glm-cwc: indx=125 | iter=125 | seed=184546
# cond_48 | mlr-cwc: indx=100 | iter=104 | seed=167465
# cond_64 | mlr-cwc: indx=7 | iter=43 | seed=468871
# cond_65 | mlr-cwc.FE: indx=89 | iter=2541 | seed=115790
# cond_66 | mlr-cwc.FE: indx=20 | iter=2438 | seed=283639
# cond_68 | mlr-cwc.FE: indx=68, 118 | iter=1956, 3474 | seed=608465, 562497
# cond_69 | mlr-cwc.FE: indx=5, 124 | iter=760, 14436 | seed=654070, 947055
# cond_70 | mlr-cwc.FE: indx=184 | iter=1204 | seed=473758
# cond_71 | mlr-cwc.FE: indx=14, 68, 113, 163 | iter=390, 1801, 3335, 4998 | seed=780975, 464385, 351844, 903911
# cond_72 | mlr-cwc.FE: indx=133 | iter=17163 | seed=338062
# cond_76 | glm-cwc.FE: indx=120 | iter=849 | seed=224309
# cond_76 | glm-cwc: indx=120 | iter=849 | seed=224309
# cond_85 | mlr-cwc: indx=92 | iter=605 | seed=745129
# cond_85 | glm-cwc.FE: indx=92 | iter=605 | seed=745129
# cond_85 | glm-cwc: indx=92 | iter=605 | seed=745129
# cond_86 | mlr-cwc.FE: indx=46 | iter=1167 | seed=280626
# cond_87 | mlr-cwc.FE: indx=123 | iter=14123 | seed=192121
# cond_88 | mlr-cwc.FE: indx=180 | iter=1192 | seed=593721
# cond_89 | mlr-cwc: indx=131 | iter=3866 | seed=349615
# cond_93 | mlr-cwc: indx=133 | iter=15295 | seed=16029
# cond_94 | mlr-cwc.FE: indx=74 | iter=550 | seed=944532
# cond_96 | mlr-cwc.FE: indx=27, 157 | iter=2899, 21056 | seed=351074, 519951

# 2025-11-18:
# cond_01 | mlr-cwc.FE: indx=557 | iter=559 | seed=19233
# cond_01 | mlr-cwc: indx=230 | iter=230 | seed=229531
# cond_01 | glm-cwc.FE: indx=191, 230 | iter=191, 230 | seed=426662, 229531
# cond_01 | glm-cwc: indx=191, 230, 403 | iter=191, 230, 403 | seed=426662, 229531, 232749
# cond_02 | mlr-cwc: indx=416 | iter=419 | seed=875999
# cond_02 | glm-cwc.FE: indx=274 | iter=277 | seed=800579
# cond_04 | mlr-cwc.FE: indx=253, 510 | iter=254, 513 | seed=134127, 388028
# cond_05 | mlr-cwc.FE: indx=480 | iter=484 | seed=299835
# cond_06 | mlr-cwc.FE: indx=212 | iter=217 | seed=729113
# cond_07 | mlr-cwc.FE: indx=449 | iter=449 | seed=657093
# cond_10 | glm-cwc.FE: indx=43 | iter=43 | seed=468871
# cond_10 | glm-cwc: indx=43 | iter=43 | seed=468871
# cond_16 | mlr-cwc: indx=463 | iter=465 | seed=119637
# cond_19 | mlr-cwc.FE: indx=580 | iter=583 | seed=478500
# cond_20 | mlr-cwc.FE: indx=418 | iter=421 | seed=948810
# cond_25 | glm-cwc.FE: indx=69, 144 | iter=69, 144 | seed=832984, 898270
# cond_25 | glm-cwc: indx=69 | iter=69 | seed=832984
# cond_26 | glm-cwc.FE: indx=328 | iter=331 | seed=269470
# cond_26 | glm-cwc: indx=328 | iter=331 | seed=269470
# cond_28 | mlr-cwc.FE: indx=515 | iter=518 | seed=988900
# cond_28 | mlr-cwc: indx=377 | iter=379 | seed=678094
# cond_28 | glm-cwc.FE: indx=89, 377 | iter=89, 379 | seed=446888, 678094
# cond_28 | glm-cwc: indx=89, 377 | iter=89, 379 | seed=446888, 678094
# cond_31 | glm-cwc.FE: indx=581 | iter=584 | seed=547459
# cond_37 | glm-cwc.FE: indx=288 | iter=288 | seed=666953
# cond_37 | glm-cwc: indx=288 | iter=288 | seed=666953
# cond_40 | mlr-cwc.FE: indx=172, 275 | iter=172, 276 | seed=296995, 346669
# cond_40 | mlr-cwc: indx=172 | iter=172 | seed=296995
# cond_40 | glm-cwc.FE: indx=172 | iter=172 | seed=296995
# cond_41 | glm-cwc.FE: indx=6 | iter=6 | seed=90493
# cond_41 | glm-cwc: indx=6 | iter=6 | seed=90493
# cond_43 | glm-cwc.FE: indx=253 | iter=253 | seed=574717
# cond_43 | glm-cwc: indx=253 | iter=253 | seed=574717
# cond_44 | mlr-cwc.FE: indx=114 | iter=115 | seed=521817
# cond_46 | glm-cwc.FE: indx=252 | iter=253 | seed=574717
# cond_46 | glm-cwc: indx=252 | iter=253 | seed=574717
# cond_47 | mlr-cwc: indx=322 | iter=325 | seed=178893
# cond_48 | mlr-cwc: indx=100 | iter=104 | seed=167465
# cond_49 | glm-cwc: indx=424 | iter=2805 | seed=133033
# cond_61 | mlr-cwc.FE: indx=562 | iter=3740 | seed=769694
# cond_62 | mlr-cwc.FE: indx=497, 502, 569, 582 | iter=14053, 14101, 16070, 16315 | seed=369688, 698492, 206574, 23459
# cond_64 | mlr-cwc.FE: indx=207, 236 | iter=1314, 1558 | seed=362398, 794982
# cond_65 | mlr-cwc.FE: indx=89 | iter=2541 | seed=115790
# cond_66 | mlr-cwc.FE: indx=20, 379, 496, 499 | iter=2438, 46716, 60086, 60510 | seed=283639, 102303, 981085, 998560
# cond_67 | mlr-cwc.FE: indx=225, 327 | iter=1461, 2165 | seed=972867, 306937
# cond_68 | mlr-cwc.FE: indx=68, 118, 347, 604 | iter=1956, 3474, 9441, 17006 | seed=608465, 562497, 84271, 868095
# cond_69 | mlr-cwc.FE: indx=5, 124, 287, 461, 543 | iter=760, 14436, 32272, 53052, 60397 | seed=654070, 947055, 818437, 433695, 157278
# cond_71 | mlr-cwc.FE: indx=68, 113, 386, 479, 555 | iter=1801, 3335, 10331, 13128, 15287 | seed=464385, 351844, 387499, 844063, 240313
# cond_72 | mlr-cwc.FE: indx=133, 157 | iter=17163, 21056 | seed=338062, 519951
# cond_73 | glm-cwc.FE: indx=418, 555 | iter=2778, 3730 | seed=168749, 72307
# cond_73 | glm-cwc: indx=418, 555 | iter=2778, 3730 | seed=168749, 72307
# cond_76 | glm-cwc.FE: indx=120, 224 | iter=849, 1472 | seed=224309, 439115
# cond_76 | glm-cwc: indx=120, 224 | iter=849, 1472 | seed=224309, 439115
# cond_77 | glm-cwc.FE: indx=312 | iter=8603 | seed=851825
# cond_78 | mlr-cwc: indx=331 | iter=41170 | seed=102420
# cond_79 | mlr-cwc: indx=188 | iter=1182 | seed=686620

# ══════════════════════════════
#     Source Updated Functions to re-run parts of simulation
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


# ══════════════════════════════
#    Create list for re-running simulation  
# ══════════════════════════════
stopifnot(exists("extreme_list"), exists("conditions_all"), exists("methds"))

# extreme_list$cond_01$`mlr-cwc.FE`$seed
# extreme_list[[69]]

targets_by_cond <- purrr::imap(extreme_list, function(cond, cond_name) {
    rows <- purrr::imap(cond, function(meth, method_name) {
        # NULL for those without extreme cases 
        if (is.null(meth$seed) || length(meth$seed) == 0) return(NULL)
        n <- min(length(meth$seed), length(meth$iteration), length(meth$extreme_indx))
        if (n == 0) return(NULL)
        # Flag missmatch in lengths
        if (length(meth$seed) != length(meth$iteration) || length(meth$seed) != length(meth$extreme_indx)) return("ERROR")
        
        # Create df
        data.frame(
            Fit = stringr::str_extract(method_name, pattern = "^[^-]+"), 
            cluster_opt = stringr::str_extract(method_name, pattern = "[^-]+$"), 
            # method = method_name, 
            seed = meth$seed, 
            raw_iteration = meth$iteration, 
            indx = meth$extreme_indx
        )
    })
    
    # Combine rows
    df <- dplyr::bind_rows(rows)
    df
    
}) |> 
    purrr::compact() # drop those conditions with null/no extreme cases 

# Check list
targets_by_cond


# ══════════════════════════════
#    Re-run simulation for iterations with extreme values  
# ══════════════════════════════
# create code to 
#   check what multiplier to use in IQR rule using cond 68 & 69
#   identify those estimates greater than IQR * 2.5 or 3 + Q3 & IQR * 2.5 or 3 - Q1
#   save the raw_iteration and seed for those cases 
#   re-run those cases on mac & save them
#   maybe compare those to prior estimates before replacing values? 

# Re-run simulation iterations 
extreme_rerun_list <- list()
for (cond_name in names(targets_by_cond)) {
    
    message(paste0("Running ", cond_name))
    
    true_cond_number <- as.integer(stringr::str_extract(cond_name, "\\d+"))
    cond_row <- conditions_all[true_cond_number, , drop = FALSE]
    
    isQuad <- cond_row[["quadratic"]]
    Mfamily <- cond_row[["Mfamily"]]
    Yfamily <- cond_row[["Yfamily"]]
    Nj_low <- cond_row[["Nj_low"]]
    Nj_high <- cond_row[["Nj_high"]]
    Jval <- cond_row[["J"]]
    isNull <- cond_row[["if.null"]]
    
    cond_label <- glue::glue("null={isNull}, quad={isQuad}, M={Mfamily}, Y={Yfamily}, nj=[{Nj_low},{Nj_high}], J={Jval}")
    
    df <- targets_by_cond[[cond_name]]
    
    # Group to avoid regenerating data for the same seed
    seeds <- unique(df$seed)
    iter_outputs <- vector("list", length(seeds))
    names(iter_outputs) <- as.character(seeds)
    
    
    for (i in seq_along(seeds)) {
        seed <- seeds[i]
        df_seed <- dplyr::filter(df, seed == !!seed)
        
        start_time_iter <- Sys.time()
        set.seed(seed)
        
        # Generate data ONCE per seed
        sim_data <- generate_data2.0c(
            J = Jval, 
            njrange = c(Nj_low, Nj_high),
            Mfamily = Mfamily, Yfamily = Yfamily,
            if.null = isNull, 
            seed = seed,
            quadratic.A = isQuad, quadratic.M = isQuad, quadratic.Y = isQuad,
            num_x = 3, 
            include_overlapMsg = FALSE, 
            plot_PSdiagnostics = FALSE,
            randomize = FALSE, 
            ensure_cluster_positivity = TRUE,
            a_on_x = sqrt(0.03 / 3), 
            a_on_z = sqrt(0.05 / 1),
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
        if (is.null(sim_data)) {
            iter_outputs[[i]] <- NULL
            next
        }
        
        # Run ONLY the flagged methods for this seed
        results <- list()
        for (j in seq_len(nrow(df_seed))) {
            Fit <- df_seed$Fit[j]
            cluster_opt <- df_seed$cluster_opt[j]
            
            # Learners/folds to match your main script
            if (Fit == "glm") {
                learners_a <- learners_m <- learners_y <- c("SL.glm")
                num_folds <- 1
            } else if (Fit == "mlr") {
                learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
                num_folds <- 5
            } else {
                next  # ignore any other Fits
            }
            
            warnings_list <- character(0)
            err_msg <- NULL
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
                        random_slope_vars_y = "NULL"
                    ),
                    warning = function(w) { warnings_list <<- c(warnings_list, conditionMessage(w)); invokeRestart("muffleWarning") }
                ),
                error = function(e) { err_msg <<- conditionMessage(e); NULL }
            )
            
            label <- paste0(Fit, "-", cluster_opt)  # e.g., "glm-cwc.FE"
            results[[label]] <- list(
                Fit = Fit,
                cluster_opt = cluster_opt,
                num_folds = num_folds,
                estimates = estimates,
                warnings = warnings_list,
                error = !is.null(err_msg),
                error_message = err_msg
            )
        }
        
        any_success <- any(vapply(results, function(x) !is.null(x$estimates), logical(1)))
        end_time_iter <- Sys.time()
        iter_duration <- as.numeric(difftime(end_time_iter, start_time_iter, units = "mins"))
        
        raw_iter <- unique(df_seed$raw_iteration)[1]
        
        iter_outputs[[i]] <- list(
            raw_iteration = raw_iter,
            iteration = NA_integer_,      # selective re-run
            rep = NA_integer_,
            seed = seed,
            cond_idx = true_cond_number,
            condition_details = as.character(cond_label),
            iter_time_sec = round(iter_duration, 4),
            truevals = list(
                truevals_individual = sim_data$truevals$truevals_individual,
                truevals_cluster = sim_data$truevals$truevals_cluster
            ),
            effects = sim_data$effects,
            overlap = list(
                ps_summary  = sim_data$overlap$ps_summary,
                iptw_summary = sim_data$overlap$iptw_summary
            ),
            parameters = list(
                J = sim_data$parameters$J,
                njrange = sim_data$parameters$njrange,
                nj_sizes = sim_data$parameters$nj_sizes,
                clust_trt_prop = as.vector(sim_data$parameters$clust_trt_prop)
            ),
            # RESULTS ONLY INCLUDE THE FLAGGED METHODS FOR THIS SEED
            results = results,
            summary_methods = list(
                succeeded = names(Filter(function(x) !is.null(x$estimates), results)),
                failed = names(Filter(function(x)  is.null(x$estimates), results))
            ),
            error_all_methods = !any_success
        )
    }
    
    extreme_rerun_list[[cond_name]] <- purrr::compact(iter_outputs)
}


# ══════════════════════════════
#    Replace estimate output & save list  
# ══════════════════════════════
# replace values in overall_list 
updated_overall_list <- imap(overall_list, function(iter_list, cond_name) {
    # If this condition wasn't re-run, return as is
    if (!cond_name %in% names(extreme_rerun_list)) return(iter_list)
    
    # Re-run entries are stored as a list named by seed
    reruns <- extreme_rerun_list[[cond_name]]
    
    # Make a lookup by raw_iteration (character key)
    rerun_by_iter <- set_names(
        reruns, 
        vapply(reruns, function(run) as.character(run$raw_iteration), character(1))
    )
    
    # Replace values
    patched <- imap(iter_list, function(orig_iter, idx) {
        key <- as.character(orig_iter$raw_iteration)
        rerun <- rerun_by_iter[[key]]
        if (is.null(rerun)) return(orig_iter)
        
        # Replace only the methods re-ran 
        orig_iter$results[names(rerun$results)] <- rerun$results
        
        # Update summary & diagnostics 
        orig_iter$summary_methods <- rerun$summary_methods
        orig_iter$error_all_methods <- rerun$error_all_methods
        orig_iter$iter_time_sec <- rerun$iter_time_sec
        
        # # Update data generation settings?
        # orig_iter$truevals <- rerun$truevals
        # orig_iter$effects <- rerun$effects
        # orig_iter$overlap <- rerun$overlap
        # orig_iter$parameters <- rerun$parameters
        
        orig_iter
    })
    
    message(paste0(cond_name, " Finished"))
    patched
})

# Save updated overall simulation output list (replaces extreme estimates with those obtained on personal computer)
saveRDS(updated_overall_list, 
        file = file.path(results_path, "Data", paste0("S1_updated-overall-output-list_", sim_date, ".rds")))


## Convert updated_overall_list to dataframe format ------------------------

# Note: this includes replacements for problematic cases (If you would not like 
# to include this skip to "Create dataframe version" section in other script)

# ══════════════════════════════
#    Convert updated_overall_list to dataframe format 
# ══════════════════════════════

# helper
get1 <- function(x, default = NA_real_) if (is.null(x) || length(x) == 0) default else x[[1]]

# ideal reps
cap_reps <- reps

# Create dataframe version of updated overall 
updated_overall_df <- purrr::imap_dfr(updated_overall_list, function(iter_list, cond_name) {
    
    # trim to ideal number of reps
    if (!is.null(cap_reps) && length(iter_list) > cap_reps) {
        iter_list <- iter_list[seq_len(cap_reps)]
    }
    
    # Loop iterations within condition 
    iter_res <- purrr::imap_dfr(iter_list, function(iter, i) {
        
        # 
        meth_row <- purrr::map_dfr(seq_len(nrow(methds)), function(mod) {
            
            key <- glue("{methds$Fit[mod]}-{methds$cluster_opt[mod]}")
            
            # results object for this method (may be NULL or absent)
            res <- iter$results[[key]]
            if (is.null(res)) res <- list()
            
            # estimates table (may be empty)
            estimates <- res$estimates
            if (is.null(estimates) || length(estimates) == 0) {
                estimates <- tibble::tibble(
                    Effect = character(),
                    EffectVersion = character(),
                    Estimate = numeric(),
                    StdError = numeric(),
                    CILower = numeric(),
                    CIUpper = numeric()
                )
            }
            
            # pick first matching rows by version & effect
            individual_de <- estimates %>% filter(EffectVersion == "Individual-Avg", grepl("DE", Effect)) %>% slice(1)
            individual_ie <- estimates %>% filter(EffectVersion == "Individual-Avg", grepl("IE", Effect)) %>% slice(1)
            cluster_de <- estimates %>% filter(EffectVersion == "Cluster-Avg", grepl("DE", Effect)) %>% slice(1)
            cluster_ie <- estimates %>% filter(EffectVersion == "Cluster-Avg", grepl("IE", Effect)) %>% slice(1)
            
            # effects (fallback to NA if missing)
            individual_effects <- iter$effects$individual
            if (is.null(individual_effects) || length(individual_effects) == 0) {
                individual_effects <- list(pnde = NA_real_, tnie = NA_real_, tnde = NA_real_, pnie = NA_real_)
            }
            
            cluster_effects <- iter$effects$cluster
            if (is.null(cluster_effects) || length(cluster_effects) == 0) {
                cluster_effects <- list(pnde = NA_real_, tnie = NA_real_, tnde = NA_real_, pnie = NA_real_)
            }
            
            # diagnostics
            warns <- res$warnings; if (is.null(warns)) warns <- character(0)
            warnings_chr <- if (length(warns) == 0) NA_character_ else paste(warns, collapse = "; ")
            n_warnings <- length(warns)
            
            error_flag <- res$error; if (is.null(error_flag)) error_flag <- NA
            error_msg <- res$error_message; if (is.null(error_msg)) error_msg <- NA_character_
            num_folds <- res$num_folds; if (is.null(num_folds)) num_folds <- NA_integer_
            
            # 
            ps_overlap <- iter$overlap$ps_summary
            if (is.null(ps_overlap)) ps_overlap <- NA_character_
            
            clust_trt_prop_chr <- toString(round(iter$parameters$clust_trt_prop, 2))
            if (is.null(clust_trt_prop_chr)) clust_trt_prop_chr <- NA_character_
            
            nj_sizes_chr <- toString(iter$parameters$nj_sizes)
            if (is.null(nj_sizes_chr)) nj_sizes_chr <- NA_character_
            
            tibble::tibble(
                condition_name = cond_name,
                condition_num  = stringr::str_extract(cond_name, "\\d+"),
                iteration = i,
                raw_iteration = iter$raw_iteration,
                seed = iter$seed,
                Fit = methds$Fit[mod],
                cluster_opt = methds$cluster_opt[mod],
                num_folds = num_folds,
                warnings = warnings_chr,
                n_warnings = n_warnings,
                error = error_flag,
                error_message = error_msg,
                
                individual_de_Estimate = get1(individual_de$Estimate),
                individual_de_StdError = get1(individual_de$StdError),
                individual_de_CILower = get1(individual_de$CILower),
                individual_de_CIUpper = get1(individual_de$CIUpper),
                
                individual_ie_Estimate = get1(individual_ie$Estimate),
                individual_ie_StdError = get1(individual_ie$StdError),
                individual_ie_CILower = get1(individual_ie$CILower),
                individual_ie_CIUpper = get1(individual_ie$CIUpper),
                
                cluster_de_Estimate = get1(cluster_de$Estimate),
                cluster_de_StdError = get1(cluster_de$StdError),
                cluster_de_CILower = get1(cluster_de$CILower),
                cluster_de_CIUpper = get1(cluster_de$CIUpper),
                
                cluster_ie_Estimate = get1(cluster_ie$Estimate),
                cluster_ie_StdError = get1(cluster_ie$StdError),
                cluster_ie_CILower = get1(cluster_ie$CILower),
                cluster_ie_CIUpper = get1(cluster_ie$CIUpper),
                
                individual_pnde = individual_effects$pnde,
                individual_tnie = individual_effects$tnie,
                individual_tnde = individual_effects$tnde,
                individual_pnie = individual_effects$pnie,
                
                cluster_pnde = cluster_effects$pnde,
                cluster_tnie = cluster_effects$tnie,
                cluster_tnde = cluster_effects$tnde,
                cluster_pnie = cluster_effects$pnie,
                
                ps_overlap = ps_overlap,
                clust_trt_prop = clust_trt_prop_chr,
                nj_sizes = nj_sizes_chr
            )
        })
        # message(sprintf("[%s] Iteration %d/%d done", cond_name, i, length(iter_list)))
        # meth_row
    })
    message(sprintf("[%s] Condition done (%d iterations)", cond_name, length(iter_list)))
    iter_res
})

# Add condition information 
## Add condition number to merge 
conditions_all <- conditions_all |> 
    rownames_to_column("condition_num") |> 
    mutate(condition_num = str_pad(condition_num, width = 2, side = "left", pad = "0")) 
## Merge condition information into updated dataframe & reformat variables 
updated_overall_df2 <- updated_overall_df |> 
    left_join(conditions_all, by = "condition_num") |> 
    rename("ifnull" = "if.null") |> 
    mutate(nj = paste0(Nj_low, "-", Nj_high), 
           Mfamily = as.character(Mfamily),
           Yfamily = as.character(Yfamily)) |> 
    select(-Nj_low, -Nj_high, -icc) |> 
    select(condition_name, condition_num, ifnull, quadratic, Mfamily, Yfamily, nj, J, iteration, 
           everything()) 

# Save 
saveRDS(updated_overall_df2, file = file.path(
    results_path,
    "Data",
    paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")
))






# ══════════════════════════════
#    DELETE CODE BELOW (in 02a3) 
# ══════════════════════════════
# Check warning & error messages ------------------------------------------

# Import sim dataframe (with replacements)
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

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
# Import sim dataframe (with replacements)
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

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
#       glm-cwc glm-cwc.FE glm-RE.glm mlr-cwc mlr-cwc.FE
# 0      9600       4868       9446       0          0
# 1      9599       4731       9211     520        496
# 2         0       4798        294       0          0
# 3         0       3657        243     859        832
# 4         0        591          5     842        790
# 5         0        554          0     656        661
# 6         0          0          0     542        542
# 7         0          0          0     383        389
# 8         0          0          0     265        289
# 9         0          0          0     192        201
# 10        0          0          0     155        160
# 11        0          0          0    1712       1647
# 12        0          0          0      93         97
# 13        0          0          0      67        153
# 14        0          0          0      49         63
# 15        0          0          0      32         36
# 16        0          0          0      15         17
# 17        0          0          0       7         13
# 18        0          0          0       7          9
# 19        0          0          0       2          2
# 20        0          0          0       2          3
# 21        0          0          0    1600       1461
# 22        0          0          0       1          0
# 23        0          0          0       0        139
# 24        0          0          0       5          1
# 25        0          0          0      11          0
# 26        0          0          0      20          5
# 27        0          0          0      35         11
# 28        0          0          0      55         20
# 29        0          0          0      90         35
# 30        0          0          0     105         55
# 31        0          0          0     175         89
# 32        0          0          0     175        105
# 33        0          0          0     206        175
# 34        0          0          0     197        175
# 35        0          0          0     166        204
# 36        0          0          0     145        196
# 37        0          0          0      94        168
# 38        0          0          0      68        148
# 39        0          0          0      35         94
# 40        0          0          0      12         67
# 41        0          0          0    2095       1439
# 42        0          0          0    1111         12
# 43        0          0          0       2        691
# 44        0          0          0       3       1111
# 45        0          0          0       6          2
# 46        0          0          0      14          3
# 47        0          0          0      16          6
# 48        0          0          0      27         14
# 49        0          0          0      44         15
# 50        0          0          0      52         27
# 51        0          0          0      83         45
# 52        0          0          0      86         52
# 53        0          0          0     116         81
# 54        0          0          0     112         83
# 55        0          0          0     117        116
# 56        0          0          0     123        109
# 57        0          0          0     125        118
# 58        0          0          0     122        127
# 59        0          0          0     145        124
# 60        0          0          0     116        122
# 61        0          0          0      99        148
# 62        0          0          0      68        117
# 63        0          0          0      58         98
# 64        0          0          0      38         66
# 65        0          0          0      16         58
# 66        0          0          0      20         38
# 67        0          0          0      15         17
# 68        0          0          0      24         22
# 69        0          0          0      31         15
# 70        0          0          0      50         23
# 71        0          0          0     246         31
# 72        0          0          0    1437         49
# 73        0          0          0      70        245
# 74        0          0          0      89       1439
# 75        0          0          0      80         71
# 76        0          0          0      96         87
# 77        0          0          0     103         79
# 78        0          0          0     105         96
# 79        0          0          0     105        100
# 80        0          0          0      98        102
# 81        0          0          0      90        106
# 82        0          0          0      97        103
# 83        0          0          0      84         93
# 84        0          0          0      82         96
# 85        0          0          0      73         83
# 86        0          0          0      51         82
# 87        0          0          0      49         74
# 88        0          0          0      42         52
# 89        0          0          0      25         48
# 90        0          0          0      26         41
# 91        0          0          0      15         26
# 92        0          0          0       4         27
# 93        0          0          0       1         15
# 94        0          0          0       1          4
# 95        0          0          0       1          1
# 96        0          0          0       0          1
# 97        0          0          0       0          1
# 101       0          0          0      88          0
# 102       0          0          0    1512          0
# 103       0          0          0       0         88
# 104       0          0          0       0       1512

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

# Save 
saveRDS(sim1_data_nowarnings, file = file.path(results_path, "Data", paste0("S1_updated-simulation-data_", sim_date, "_excludes-warnings.rds")))


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
saveRDS(sim1_data_converged, file = file.path(results_path, "Data", paste0("S1_updated-simulation-data_", sim_date, "_converged-only.rds")))




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
sim1_data <- readRDS(file = paste0(results_path, "/Data/S1_updated-simulation-data_", 
                                   sim_date, 
                                   "_converged-only.rds"))

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


# table(perf_summary$quadratic)
# 
# perf_summary

# Save performance measures 
## Only include converged cases 
saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", 
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
sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_updated-overall-output-dataframe_", sim_date, ".rds")))

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
saveRDS(perf_summary, file = paste0(results_path, "/Tables/S1_updated-performance-measures_", #linear_",
                                    sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))


# Compute Convergence Rates -----------------------------------------------

# NOTE: commented out because re-running some of the iterations on personal computer should not change convergence rates 

# # Import data 
# ## overall data 
# sim1_data <- readRDS(file = file.path(results_path, "Data", paste0("S1_overall-output-dataframe_", sim_date, ".rds")))
# ## converged cases data 
# sim1_data_converged <- readRDS(file = paste0(results_path, "/Data/S1_simulation-data_", sim_date, "_converged-only.rds")) 
# 
# # Modify data for visuals & merging  
# sim1_data_converged <- sim1_data_converged |> 
#     mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]"))
# # dim(sim1_data[sim1_data$cluster_opt != "RE.glm", ])
# 
# # Calculate convergence rates & merge dataframes
# convergence_rates <- sim1_data |> 
#     # filter(cluster_opt != "RE.glm") |> 
#     mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
#     group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
#     summarize(total_attempts = n(), .groups = "drop") |> 
#     # merge 
#     left_join(
#         sim1_data_converged |> 
#             # filter(cluster_opt != "RE.glm") |> 
#             # mutate(quad = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#             # Nj = ifelse(nj == "5-20", "U[5, 20]", "U[50, 100]")) |> 
#             group_by(ifnull, quad, Mfamily, Yfamily, J, Nj, Fit, cluster_opt) |> 
#             summarize(converged = n(), .groups = "drop"),
#         by = c("ifnull", "quad", "Mfamily", "Yfamily", "J", "Nj", "Fit", "cluster_opt")
#     ) |> 
#     # compute convergence rates
#     mutate(
#         converged = ifelse(is.na(converged), 0, converged),
#         nonconverged = total_attempts - converged,
#         nonconvergence_rate = nonconverged / total_attempts,
#         convergence_rate = converged / total_attempts, 
#         Fit = ifelse(Fit == "mlr", "Nonparametric", "Parametric")
#     ) 
# 
# # Save convergence rates 
# saveRDS(convergence_rates, file = paste0(results_path, "/Tables/S1_convergence-rates_", 
#                                          sim_date, ".rds"))


############################# END OF PROCESSING ################################





