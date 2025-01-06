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
# Last Updated: 01/01/2025
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
    purrr, # for map()
    glue, # for glue()
    dplyr, 
    readr, 
    ggplot2, 
    fastDummies, # for Dr Liu's package/function (specifically, for dummy_cols() in fastDummies package)
    stringr, # for str_detect() in Dr Liu's package/function
    tibble # for rownames_to_column() in Dr Liu's package/function
)


#----------------------
# Load Functions
#----------------------

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c"
    
    # # old 
    # "generate_level1_covariates",
    # "generate_level1_mediator",
    # "generate_level1_outcome",
    # "generate_level1_treatment",
    # "generate_level2_confounder",
    # "generate_clustered_data", 
    # 
    # # Nonlinear (quadratic) versions of data generation 
    # "generate_level1_treatment_N", 
    # "generate_level1_mediator_N",
    # "generate_level1_outcome_N", 
    # "generate_clustered_data_N"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}


# Load analysis functions 
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





# Test mediator generation 
# generate_mediator()

# # generate_data()
# J = 100                        # Number of clusters
# njrange = c(50, 100)            # Range for cluster sizes
# Mfamily = "gaussian"
# Yfamily = "binomial"            # Family for outcome ('gaussian' or 'binomial')
# if.null = FALSE
# num_x = 3                       # Number of individual-level confounders
# x_z = 0                         # Correlation between 'X' and 'Z'
# m_on_a = 0.2                    # Effect of 'A' on 'M'
# m_on_az = 0.2                   # Interaction effect of 'A' and 'Z' on 'M'
# m_on_anj = 0.2                  # Interaction effect of 'A' and cluster size on 'M'
# int.XZ = FALSE                  # Interaction between 'X' and 'Z'
# yintercept = 1                  # Intercept for outcome model
# y_on_a = 0.5                    # Effect of 'A' on 'Y'
# y_on_m = 1                      # Effect of 'M' on 'Y'
# y_on_am = 0                     # Interaction effect of 'A' and 'M' on 'Y'
# y_on_az = 0.2                   # Interaction effect of 'A' and 'Z' on 'Y'
# y_on_mz = 0.2                   # Interaction effect of 'M' and 'Z' on 'Y'
# y_on_anj = 0.2                  # Interaction effect of 'A' and cluster size on 'Y'
# quadratic.A = FALSE             # Include quadratic terms for 'A'
# quadratic.M = FALSE             # Include quadratic terms for 'M'
# quadratic.Y = FALSE             # Include quadratic terms for 'Y'
# iccx = 0.2                      # Intra-class correlation for 'X'
# icca = 0.2                      # Intra-class correlation for 'A'
# iccm = 0.2                      # Intra-class correlation for 'M'
# iccy = 0.2
# 
# set.seed(1234)
# generate_data(Mfamily = "gaussian")$data
# # ---------------------------
# # Step 1: Generate Clusters
# # ---------------------------
# data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)
# # data_list contains:
# # - data: Data frame with 'id', 'school', and 'W_nj'
# # - nj_sizes: Vector of cluster sizes
# # - njrange: Range of cluster sizes
# 
# # ---------------------------
# # Step 2: Generate Confounders
# # ---------------------------
# data_list <- generate_confounders(
#     data_list = data_list,
#     nj_sizes = data_list$nj_sizes,
#     num_x = num_x,
#     iccx = iccx,
#     x_z = x_z
# )
# # Confounders added:
# # - Z: Cluster-level unobserved confounder
# # - X: Matrix of individual-level confounders
# 
# # ---------------------------
# # Step 3: Generate Treatment
# # ---------------------------
# data_list <- generate_treatment(
#     data_list = data_list,
#     nj_sizes = data_list$nj_sizes,
#     icca = icca,
#     quadratic.A = quadratic.A,
#     num_x = num_x
# )
# # Treatment 'A' added to data_list$data
# 
# # ---------------------------
# # Step 4: Generate Mediator
# # ---------------------------
# data_list <- generate_mediator(
#     data_list = data_list,
#     nj_sizes = data_list$nj_sizes,
#     iccm = iccm,
#     num_x = num_x,
#     m_on_a = m_on_a,
#     m_on_az = m_on_az,
#     m_on_anj = m_on_anj,
#     quadratic.M = quadratic.M,
#     int.XZ = int.XZ,
#     Mfamily = Mfamily
# )
# 
# data_list$Mfamily
# data_list$data
# 
# generate_outcome(data_list = data_list)
# # [1] "gaussian"
# # > data_list$data
# # # A tibble: 7,185 × 7
# # id school  W_nj     Z X[,"x1"] [,"x2"] [,"x3"]     A      M
# # <int>  <int> <dbl> <dbl>    <dbl>   <dbl>   <dbl> <int>  <dbl>
# # 1     1      1  0.12 -1.81   0.156    1.31   1.96       1 -1.86 
# # 2     2      1  0.12 -1.81  -0.533    0.726  0.150      0 -4.14 
# # 3     3      1  0.12 -1.81  -1.49    -1.03   0.600      0 -1.05 
# # 4     4      1  0.12 -1.81   1.44     0.137  0.0148     0 -1.32 
# # 5     5      1  0.12 -1.81   0.0373   0.668 -0.426      0 -0.869
# # 6     6      1  0.12 -1.81  -1.11    -1.69  -0.403      0 -0.778
# # 7     7      1  0.12 -1.81  -0.962   -0.217  1.99       0 -1.33 
# # 8     8      1  0.12 -1.81   0.626    0.288  0.226      0 -1.86 
# # 9     9      1  0.12 -1.81  -0.205    0.533  0.282      0 -0.565
# # 10    10      1  0.12 -1.81   0.779    1.03  -0.310      1 -0.824
# 
# 
# # [1] "binary"
# # > data_list$data
# # # A tibble: 7,185 × 7
# # id school  W_nj     Z X[,"x1"] [,"x2"] [,"x3"]     A     M
# # <int>  <int> <dbl> <dbl>    <dbl>   <dbl>   <dbl> <int> <int>
# # 1     1      1  0.12 -1.81   0.156    1.31   1.96       1     0
# # 2     2      1  0.12 -1.81  -0.533    0.726  0.150      0     0
# # 3     3      1  0.12 -1.81  -1.49    -1.03   0.600      0     0
# # 4     4      1  0.12 -1.81   1.44     0.137  0.0148     0     0
# # 5     5      1  0.12 -1.81   0.0373   0.668 -0.426      0     0
# # 6     6      1  0.12 -1.81  -1.11    -1.69  -0.403      0     0
# # 7     7      1  0.12 -1.81  -0.962   -0.217  1.99       0     0
# # 8     8      1  0.12 -1.81   0.626    0.288  0.226      0     0
# # 9     9      1  0.12 -1.81  -0.205    0.533  0.282      0     0
# # 10    10      1  0.12 -1.81   0.779    1.03  -0.310      1     0
# 
# # 
# 
# generate_data(J = 100, 
#               njrange = c(50, 100), 
#               seed = 1234)

# Simulation conditions  --------------------------------------------------

# conditions
conditions <- data.frame(rbind(
    expand.grid(
        J = c(10, 20, 40),
        Nj_low = c(50),
        Nj_high = c(100), 
        quadratic = c(T, F), 
        binary.med = c(T, F), 
        binary.out = c(T, F)
    ),
    expand.grid(
        J = c(40, 70, 100),
        Nj_low = c(5),
        Nj_high = c(20), 
        quadratic = c(T, F), 
        binary.med = c(T, F), 
        binary.out = c(T, F)
    )
))

conditions



# Testing 1 dataset -------------------------------------------------------

# Condition 
test_condition <- conditions |> 
    filter(J == 40, Nj_low == 50, quadratic == FALSE, binary.med == TRUE, binary.out == TRUE)

# Generate Data 
set.seed(8675309)
data <- generate_data2.0c(
    J = test_condition[["J"]], 
    njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
    Mfamily = "binomial",
    Yfamily = "binomial", 
    seed = 8675309,
    num_x = 3,
    
    m_on_a = 15,
    m_on_anj = 0.5,
    m_on_az = 0.2,
    
    y_on_a = 2,
    y_on_m = 15,
    y_on_am = 5,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 5,
    int.XZ = FALSE
)

# Effects
data.frame(
    individual = unlist(data$effects$individual),
    cluster = unlist(data$effects$cluster),
    row.names = names(data$effects$individual)
)
# individual    cluster
# pnde 0.117075635 0.12222966
# tnie 0.009099977 0.01098372
# tnde 0.000000000 0.00000000 # A's direct effect only manifests in the absence of mediator change
# pnie 0.126175612 0.13321338


# Try different parameters 
set.seed(8675309)
data <- generate_data2.0c(#include_truevals = FALSE, 
    J = test_condition[["J"]], 
    njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
    # Mfamily = "binomial",
    Yfamily = "binomial",
    Mfamily = "gaussian",
    # Yfamily = "gaussian",
    seed = 8675309,
    num_x = 3,
    
    m_on_a = 3.5,
    m_on_anj = 0.2,
    m_on_az = 0.2,
    
    y_on_a = 2,
    y_on_m = 5,
    y_on_am = 2,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 0.2,
    int.XZ = FALSE
)

# Effects
data.frame(
    individual = unlist(data$effects$individual),
    cluster = unlist(data$effects$cluster),
    row.names = names(data$effects$individual)
) |> 
    format(scientific = FALSE)
# individual      cluster
# pnde 0.1221682092 0.1264475446
# tnie 0.0061939999 0.0070084034
# tnde 0.0007435363 0.0008976396
# pnie 0.1276186728 0.1325583084

# PNDE + TNIE = TNDE + PNIE
(data$effects$individual$pnde + data$effects$individual$tnie) == (data$effects$individual$tnde + data$effects$individual$pnie)
 


# Analyze data 
results <- analyze_clustered_mediation(PS_model_type = "FE", 
                                       Med_model_type = "FE", 
                                       Out_model_type = "RE", 
                                       Sname = "school", 
                                       L1_baseline_cov = paste0("X", 1:3), 
                                       treatment = "A", 
                                       mediator = "M", 
                                       outcome = "Y", 
                                       iptw_weights = "iptw",
                                       L2_weight = "L2weight", 
                                       data = data$data)

results
# data$effects$individual

# Load Dr Liu ML estimation package
devtools::load_all("Application/Functions/MediatorCL")


# Analyze data with ML approach 
learners_a <- learners_m <- learners_y <- c("SL.glm","SL.nnet") 

# data$CLUSTER2 <- as.factor(data$CLUSTER2)
Sname <- "school" # have to specify this outside of MediatorCL function to work

results_ml <- MediatorCL::MediatorCL(
    data = data$data,
    Sname = "school",
    Wnames = "W_nj",
    Xnames = paste0("X", 1:3),
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = learners_a,
    learners_m = learners_m,
    learners_y = learners_y,
    cluster_opt = "cwc",
    num_folds = 5
    
)

results_ml



# # Set Parameters ----------------------------------------------------------
# 
# treat_m <- 1.17 # trt on med   
# treat_y <- 1.35 # trt on outcome
# med_y <- 1.2 # med on outcome  
# NDE <- treat_y
# NIE <- treat_m * med_y
# 
# 






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








