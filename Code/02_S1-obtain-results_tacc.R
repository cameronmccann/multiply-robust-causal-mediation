################################################################################
####################### Simulation 1 - Obtain Results ##########################
################################################################################

############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 2025-01-09
#
#
# Script Description: 
#       This code summarizes and reports the results for the 
#       first simulation study (i.e., obtains performance measures). 
#
#
# Last Updated: 2025-01-28
#
#
# Notes:
#   To-Do
#
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
    huxtable
)


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
    filter(Mfamily %in% c("binomial", "gaussian"), Yfamily %in% c("binomial", "gaussian")) #c("gaussian")) # c("binomial")) #, Yfamily %in% c("gaussian"))


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


# Set date, reps, & folders ----------------------------------------------

# Date of simulation 
sim_date <- "2025-01-25" #Sys.Date() #"2025-01-23" #"2025-01-18" 

# Number of replications
# reps <- 100 # 100 # 200 # 1000
reps <- 300

# Create directory to store results 
## Results folder 
path <- "Output/S1_Results"
if (!dir.exists(path)) {
    dir.create(path)
}
### Add subdirectory, if desired (e.g., for test runs)
additional_folder <- "from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods" #additional_folder <- "2025-01-24-test_1-rep_quad" #additional_folder <- "2025-01-23-test_100-reps_all-linear-conditions-with-all-methods" # NULL
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder)) {
    path <- file.path(path, additional_folder)
}
### Create directory 
if (!dir.exists(path)) {
    dir.create(path)
}
## Data, Figures, & Tables subfolders 
if (!dir.exists(paste0(path, "/Data"))) {
    dir.create(paste0(path, "/Data"))
}
if (!dir.exists(paste0(path, "/Figures"))) {
    dir.create(paste0(path, "/Figures"))
}
if (!dir.exists(paste0(path, "/Tables"))) {
    dir.create(paste0(path, "/Tables"))
}
# Simulation output path 
sim_output_path <- "Output/S1_Simulation-Output"
### Check if additional_folder is not NULL to add to path
if (!is.null(additional_folder)) {
    sim_output_path <- file.path(sim_output_path, additional_folder)
}



# Import data  ------------------------------------------------------------

# Note: Maybe pull in each conditions list and extract true values and estimates and put into one large dataframe? 

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


## Extract simulation data -------------------------------------------------

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
saveRDS(sim1_data, file = paste0(path, "/Data/S1_simulation-data_", sim_date, ".rds"))



### Extract simulation data (when error with non-list elements are p --------

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
saveRDS(sim1_data, file = paste0(path, "/Data/S1_simulation-data_", sim_date, ".rds"))



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



# Compute Performance Measures --------------------------------------------

## Bias, Coverage, & MSE (need type I error) -----------------------------------------------------------

sim1_data <- readRDS(file = paste0(path, "/Data/S1_simulation-data_", sim_date, #"2025-01-12", #sim_date, 
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

saveRDS(perf_summary, file = paste0(path, "/Tables/S1_performance-measures_", sim_date, ".rds")) #paste0("Output/S1_Results/Tables/S1_performance-measures_", sim_date, ".rds"))

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

# import data if not done so 
# import data & performance measures 
quad_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Data/S1_simulation-data_2025-01-25.rds")
quad_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-quad-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-25.rds")
# import data & performance measures 
linear_data <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/Data/S1_simulation-data_2025-01-25.rds")
linear_perf_measures <- readRDS(file = "Output/S1_Results/from-tacc/2025-01-25-test_300-rep_all-linear-conditions-with-all-methods/Tables/S1_performance-measures_2025-01-25.rds")

# Stack data & performance measures 
sim_data <- bind_rows(cbind(quadratic = TRUE, quad_data), 
                      cbind(quadratic = FALSE, linear_data))
perf_measures <- bind_rows(quad_perf_measures, linear_perf_measures)
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



#

# ══════════════════════════════
#    individual-average effects  
# ══════════════════════════════

# Bias for TNIE
perf_measures |> 
    # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
    labs(
        title = "Bias for individual-average TNIE",
        x = "J",
        y = "Bias"
    ) 

# CI coverage rate for TNIE
perf_measures |> 
    # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = cover_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 1) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
    labs(
        title = "CI coverage rate for individual-average TNIE",
        x = "J",
        y = "Coverage"
    ) 
# theme_minimal()


# MSE for TNIE
perf_measures |> 
    # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
    # filter(cluster_opt == "cwc.FE") |> 
    # filter(Nj_low == 5) |>
    mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
           Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
    ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
    labs(
        title = "MSE for individual-average TNIE",
        x = "J",
        y = "MSE"
    ) 


# ══════════════════════════════
#    cluster-average effects  
# ══════════════════════════════

# # Bias for TNIE
# perf_measures |> 
#     # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = bias_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 0) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
#     labs(
#         title = "Bias for individual-average TNIE",
#         x = "J",
#         y = "Bias"
#     ) 
# 
# # CI coverage rate for TNIE
# perf_measures |> 
#     # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = cover_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 1) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
#     labs(
#         title = "CI coverage rate for individual-average TNIE",
#         x = "J",
#         y = "Coverage"
#     ) 
# # theme_minimal()
# 
# 
# # MSE for TNIE
# perf_measures |> 
#     # filter(Yfamily == "gaussian" & Mfamily == "binomial") |>
#     # filter(cluster_opt == "cwc.FE") |> 
#     # filter(Nj_low == 5) |>
#     mutate(quadratic = ifelse(quadratic == TRUE, "nonlinear", "linear"), 
#            Nj_low = ifelse(Nj_low == 5, "U[5, 20]", "U[50, 100]")) |> 
#     ggplot(aes(x = factor(J), y = MSE_individual_TNIE, color = Fit, shape = Nj_low, linetype = Fit)) +
#     ggplot2::geom_hline(yintercept = 0) +
#     geom_point() +
#     geom_line(aes(group = interaction(cluster_opt, Fit, Nj_low))) +
#     facet_grid(interaction(Mfamily, Yfamily) ~ interaction(quadratic, cluster_opt)) + #, labeller = labeller())
#     labs(
#         title = "MSE for individual-average TNIE",
#         x = "J",
#         y = "MSE"
#     ) 




# check overlap 
## function to extract percentages
extract_psnum <- function(text, pattern) {
    number <- str_extract(text, pattern)
    as.numeric(str_extract(number, "\\d+\\.?\\d*"))
}
## extract percent above or below thresholds 
sim_data$psLowPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)")
sim_data$psHighPct <- sapply(sim_data$ps_overlap, extract_psnum, pattern = "\\d+\\.?\\d*%\\)$")

sim_data |> 
    ggplot(aes(y = psLowPct, x = J)) +
    geom_jitter(alpha = 0.5) +
    geom_boxplot(aes(fill = factor(J))) +
    # geom_violin() +
    facet_grid(interaction(Mfamily, Yfamily) ~ interaction(Nj_low))






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




