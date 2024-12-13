################################################################################
############################ Test Simulation(s) ################################
################################################################################
############################ Script Description ################################
#
# Author: Cameron McCann
# 
# Date Created: 11/04/2024
#
#
# Script Description: {Junk file to test project} 
#
#
# Last Updated: 11/07/2024
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
#   Done: Completed tasks
#
################################################################################



# Set Up (Load packages, functions, &/or data) ----------------------------

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    doParallel, 
    foreach,
    parallel, 
    
    dplyr, 
    readr, 
    ggplot2
)


#----------------------
# Load Functions
#----------------------

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_level1_covariates",
    "generate_level1_mediator",
    "generate_level1_outcome",
    "generate_level1_treatment",
    "generate_level2_confounder",
    "generate_clustered_data", 
    # Nonlinear (quadratic) versions of data generation 
    "generate_level1_treatment_N", 
    "generate_level1_mediator_N",
    "generate_level1_outcome_N", 
    "generate_clustered_data_N"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}


# Load analysis func 
# Define the vector of function names
function_names <- c(
    "estimate_propensity_score",
    "estimate_mediator_model",
    "estimate_outcome_model",
    "analyze_clustered_mediation"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}




# source("Functions/AnalysisFunc_Sim1.R")
# # Load Functions 
# source("Functions/AnalysisFunc_Sim2.R")
# source("Functions/genOneData_Sim2.R")


# Simulation conditions  --------------------------------------------------

cond <- expand.grid(num_clust = 100,
                    clust_size = c(20, 40, 100),
                    num_x = 6,
                    icc = c(0.05, 0.2, 0.5))


# Set Parameters ----------------------------------------------------------

treat_m <- 1.17 # trt on med   
treat_y <- 1.35 # trt on outcome
med_y <- 1.2 # med on outcome  
NDE <- treat_y
NIE <- treat_m * med_y








# Testing functions -------------------------------------------------------


## Linear with continuous variables ----------------------------------------

# Generate Data 
set.seed(8675309)
data <- generate_clustered_data(
    num_clust = 100,
    clust_size = 50,
    binary_mediator = FALSE,
    binary_outcome = FALSE,
    z_confounds_t_m = TRUE,
    z_confounds_m_y = TRUE
)

# Analyze data 
results <- analyze_clustered_mediation(PS_model_type = "FE", 
                            Med_model_type = "FE", 
                            Out_model_type = "FE", 
                            Sname = "school", 
                            L1_baseline_cov = paste0("x", 1:3), 
                            treatment = "t", 
                            mediator = "m", 
                            outcome = "y", 
                            iptw_weights = "iptw",
                            L2_weight = "L2weight", 
                            data = data)

results
str(results)

results[1, "Individual.Average_Estimate"] # DE 
results[2, "Individual.Average_Estimate"] # IE 

# Bias 
(results[1, "Individual.Average_Estimate"] - NDE) / NDE
(results[2, "Individual.Average_Estimate"] - NIE) / NIE



## Nonlinear with continuous variables -------------------------------------


# Generate Data 
set.seed(8675309)
data <- generate_clustered_data_N(
    num_clust = 100,
    clust_size = 50,
    binary_mediator = FALSE,
    binary_outcome = FALSE,
    z_confounds_t_m = TRUE,
    z_confounds_m_y = TRUE, 
    nonlinear = TRUE
)

# Analyze data 
results <- analyze_clustered_mediation(PS_model_type = "FE", 
                                       Med_model_type = "FE", 
                                       Out_model_type = "FE", 
                                       Sname = "school", 
                                       L1_baseline_cov = paste0("x", 1:3), 
                                       treatment = "t", 
                                       mediator = "m", 
                                       outcome = "y", 
                                       iptw_weights = "iptw",
                                       L2_weight = "L2weight", 
                                       data = data)

results
str(results)

results[1, "Individual.Average_Estimate"] # DE 
results[2, "Individual.Average_Estimate"] # IE 

# Bias 
(results[1, "Individual.Average_Estimate"] - NDE) / NDE
(results[2, "Individual.Average_Estimate"] - NIE) / NIE







# Loop through linear relations & continuous med & out with FE -----------------------------------

# Create an empty dataframe to store results
results_df <- data.frame()

# Loop 5 times
for (i in 1:5) {
    # Set a new seed each time
    set.seed(8675309 + i)
    
    # Generate Data
    data <- generate_clustered_data(
        num_clust = 100,
        clust_size = 50,
        binary_mediator = FALSE,
        binary_outcome = FALSE,
        z_confounds_t_m = FALSE,
        z_confounds_m_y = FALSE, 
        z_confounds_t_y = TRUE
    )
    
    # Analyze data
    results <- analyze_clustered_mediation(
        PS_model_type = "FE", 
        Med_model_type = "FE", 
        Out_model_type = "FE", 
        Sname = "school", 
        L1_baseline_cov = paste0("x", 1:3), 
        treatment = "t", 
        mediator = "m", 
        outcome = "y", 
        iptw_weights = "iptw",
        L2_weight = "L2weight", 
        data = data
    )
    
    # Extract estimates
    DE_est <- results[1, "Individual.Average_Estimate"]
    IE_est <- results[2, "Individual.Average_Estimate"]
    
    # Calculate bias
    DE_bias <- (DE_est - NDE) / NDE
    IE_bias <- (IE_est - NIE) / NIE
    
    # Store results in dataframe
    iteration_results <- data.frame(
        Seed = 8675309 + i,
        DE_Estimate = DE_est,
        IE_Estimate = IE_est,
        DE_Bias = DE_bias,
        IE_Bias = IE_bias, 
        PS_model = results$PS_model, 
        Mediator_model = results$Mediator_model, 
        Outcome_model = results$Outcome_model
    )
    
    results_df <- bind_rows(results_df, iteration_results)
}

# View the results
print(results_df)
results_df_cont_med_out <- results_df




# Loop through linear relations & continuous med &  binary out wit --------

# Create an empty dataframe to store results
results_df <- data.frame()

# Loop 5 times
for (i in 1:5) {
    # Set a new seed each time
    set.seed(8675309 + i)
    
    # Generate Data
    data <- generate_clustered_data(
        num_clust = 100,
        clust_size = 50,
        binary_mediator = TRUE,
        binary_outcome = FALSE,
        z_confounds_t_m = FALSE,
        z_confounds_m_y = FALSE, 
        z_confounds_t_y = TRUE
    )
    
    # Analyze data
    results <- analyze_clustered_mediation(
        PS_model_type = "FE", 
        Med_model_type = "FE", 
        Out_model_type = "FE", 
        Sname = "school", 
        L1_baseline_cov = paste0("x", 1:3), 
        treatment = "t", 
        mediator = "m", 
        outcome = "y", 
        iptw_weights = "iptw",
        L2_weight = "L2weight", 
        data = data
    )
    
    # Extract estimates
    DE_est <- results[1, "Individual.Average_Estimate"]
    IE_est <- results[2, "Individual.Average_Estimate"]
    
    # Calculate bias
    DE_bias <- (DE_est - NDE) / NDE
    IE_bias <- (IE_est - NIE) / NIE
    
    # Store results in dataframe
    iteration_results <- data.frame(
        Seed = 8675309 + i,
        DE_Estimate = DE_est,
        IE_Estimate = IE_est,
        DE_Bias = DE_bias,
        IE_Bias = IE_bias, 
        PS_model = results$PS_model, 
        Mediator_model = results$Mediator_model, 
        Outcome_model = results$Outcome_model
    )
    
    results_df <- bind_rows(results_df, iteration_results)
}

# View the results
print(results_df)
results_df_binary_med_cont_out <- results_df








# small sim ---------------------------------------------------------------

# number of iterations 
reps <- 300

# Set up parallelization
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to run a single iteration
run_iteration <- function(seed, is_linear, is_binary_mediator) {
    set.seed(seed)
    
    if (is_linear) {
        data <- generate_clustered_data(
            num_clust = 100,
            clust_size = 50,
            binary_mediator = is_binary_mediator,
            binary_outcome = FALSE,
            z_confounds_t_m = FALSE,
            z_confounds_m_y = FALSE, 
            z_confounds_t_y = TRUE
        )
    } else {
        data <- generate_clustered_data_N(
            num_clust = 100,
            clust_size = 50,
            binary_mediator = is_binary_mediator,
            binary_outcome = FALSE,
            z_confounds_t_m = FALSE,
            z_confounds_m_y = FALSE, 
            z_confounds_t_y = TRUE, 
            nonlinear = TRUE
        )
    }
    
    results <- analyze_clustered_mediation(
        PS_model_type = "FE", 
        Med_model_type = "FE", 
        Out_model_type = "FE", 
        Sname = "school", 
        L1_baseline_cov = paste0("x", 1:3), 
        treatment = "t", 
        mediator = "m", 
        outcome = "y", 
        iptw_weights = "iptw",
        L2_weight = "L2weight", 
        data = data
    )
    
    de <- results[1, "Individual.Average_Estimate"]
    ie <- results[2, "Individual.Average_Estimate"]
    
    return(c(seed = seed, is_linear = is_linear, is_binary_mediator = is_binary_mediator, de = de, ie = ie))
}

# Modify the main simulation loop
set.seed(8675309)
seeds <- sample.int(1000000, reps)

results <- foreach(seed = seeds, .combine = rbind) %dopar% {
    linear_continuous <- run_iteration(seed, TRUE, FALSE)
    linear_binary <- run_iteration(seed, TRUE, TRUE)
    nonlinear_continuous <- run_iteration(seed, FALSE, FALSE)
    nonlinear_binary <- run_iteration(seed, FALSE, TRUE)
    rbind(linear_continuous, linear_binary, nonlinear_continuous, nonlinear_binary)
}

# Stop parallelization
stopCluster(cl)

# Convert results to dataframe
results_df <- as.data.frame(results)
colnames(results_df) <- c("seed", "is_linear", "is_binary_mediator", "DE", "IE")

# Calculate bias (assuming you have true NDE and NIE values for each scenario)
results_df <- results_df %>%
    mutate(
        DE_bias = (DE - NDE) / NDE,
        IE_bias = (IE - NIE) / NIE, 
        DE_abs_bias = abs((DE / NDE) - 1), 
        IE_abs_bias = abs((IE / NIE) - 1) 
    )

# Drop rownames 
rownames(results_df) <- NULL

# View results
print(results_df)

# Reshape the data for ggplot (long format)
results_df_long <- results_df %>%
    pivot_longer(cols = c(DE, IE), names_to = "effect_type", values_to = "effect_value") %>%
    pivot_longer(cols = c(DE_bias, IE_bias), names_to = "bias_type", values_to = "bias_value") %>%
    pivot_longer(cols = c(DE_abs_bias, IE_abs_bias), names_to = "abs_bias_type", values_to = "abs_bias_value") %>%
    mutate(bias_type = sub("_bias", "", bias_type), 
           abs_bias_type = sub("_abs_bias", "", abs_bias_type)) %>%
    filter(effect_type == bias_type, effect_type == abs_bias_type) |> 
    mutate(linearity = ifelse(is_linear == 1, "Linear", "Nonlinear"))

# Visualize results 
p <- ggplot(results_df_long, aes(x = linearity, y = abs_bias_value)) +
    geom_hline(yintercept = 0.1, color = "red") +
    geom_boxplot(aes(fill = linearity), position = position_dodge(width = 0.75), width = 0.6) +
    facet_grid(is_binary_mediator ~ effect_type, 
               labeller = labeller(
                   is_binary_mediator = c(`0` = "Continuous Mediator", `1` = "Binary Mediator"), 
                   effect_type = c("DE" = "Direct", "IE" = "Indirect")
               )) +
    labs(title = "Absolute Bias Comparison Across Linear/Nonlinear Relationships and Mediator Types",
         x = "", 
         y = "Absolute Bias",
         caption = "Fixed-Effect models were used on 100 clusters of size 50 with T-Y confounded by Z.") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0, size = 10, face = "italic"))

# Display the plot
print(p)


# Save the plot
ggsave("Output/absolute_bias_comparison.pdf", p, width = 12, height = 8, dpi = 300)

# Summarize results
summary_stats <- results_df_long |> 
    group_by(is_binary_mediator, effect_type, linearity) |> 
    summarize(mean = mean(abs_bias_value)) |> 
    ungroup() 

# Save the summary table as an Excel file
write_csv(summary_stats, "Output/summary_stats.csv")

# Print the summary table
print(summary_stats)








