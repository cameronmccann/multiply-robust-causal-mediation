################################################################################
##################### Fixing & Testing trueVals Function  ######################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 
#
#
# Script Description: this is used to fix, test, and properly build the trueVals funciton 
#
#
# Last Updated: 12/31/2024
#
#
# Notes:
#   To-Do: 
#       + probably examine more thoroughly (& check with Dr Liu) about the proper computation of true values with continuous med &/or out
#
#   Done: 
# 
#
################################################################################


# ══════════════════════════════
#    Creating part of trueVals function to handle continuous mediator & binary outcome  
# ══════════════════════════════

# Note:
#   Here I am working on updating the trueVals2.0 function & will be testing the code here. 
#   Specifically, I am working on correctly generate a large sample and computing the potential outcomes and then inserting this code into the trueVals2.0 function. 
# 
#   NEED TO: 
#       + NEXT STEP: work on the line 610 area. Ignore fitting it in a function for now. Just correctly run it in this script, then you can create another section to convert it into the function. 
#       I will also need to test that using generate_data2.0() within trueVals(), which itself is in generate_data2.0(), will work properly on referring to the set parameters. 
#       Maybe make truVals_contM_binY() to specifically re-generate data but large (take the existing data's parameters in a list as an argument & use the calc_m_latent & calc_y_latent functions inside)
#



# Set Up (Load packages, functions, &/or data) ----------------------------

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    # doParallel, 
    # foreach,
    # parallel, 
    purrr, # for map()
    glue, # for glue()
    dplyr, 
    readr, 
    ggplot2
)




# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data2.0b",
    "generate_data2.0", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals2.0b", 
    "trueVals2.0"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}


# ═══════════════════
#    generate data 
# ═══════════════════

# Generate Data 
set.seed(8675309)
data_list <- generate_data2.0b(
    J = 100, 
    njrange = c(50, 100), 
    Mfamily = "gaussian", # "binomial", 
    Yfamily = "binomial", # "gaussian", # 
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
    int.XZ = TRUE
)

# data_list






# 1. run the compute_expected_y for cont med & binary out --------------------
# Here I will run the part I am trying to develop outside of a function 


# ═══════════════════
#    run beginning of trueVals2.0 func 
# ═══════════════════

library(dplyr)
library(glue)

# Extract necessary components
data <- data_list$data
nj_sizes <- data_list$parameters$nj_sizes
Mfamily <- "gaussian" #data_list$Mfamily
Yfamily <- data_list$parameters$Yfamily
iccm <- data_list$parameters$iccm
iccy <- data_list$parameters$iccy

# Extract mediator parameters
gen_m <- list(
    iccm = data_list$parameters$iccm,
    m_on_a = data_list$parameters$m_on_a,
    m_on_x = data_list$parameters$m_on_x,
    m_on_z = data_list$parameters$m_on_z,
    m_on_az = data_list$parameters$m_on_az,
    m_on_anj = data_list$parameters$m_on_anj
)

# Extract outcome parameters
gen_y <- list(
    iccy = data_list$parameters$iccy,
    yintercept = data_list$parameters$yintercept,
    y_on_a = data_list$parameters$y_on_a,
    y_on_m = data_list$parameters$y_on_m,
    y_on_am = data_list$parameters$y_on_am,
    y_on_az = data_list$parameters$y_on_az,
    y_on_mz = data_list$parameters$y_on_mz,
    y_on_anj = data_list$parameters$y_on_anj,
    y_on_x = data_list$parameters$y_on_x,
    y_on_z = data_list$parameters$y_on_z
)

m_given <- data_list$parameters$m_given
y_given <- data_list$parameters$y_given

data <- data  |> 
    mutate(nj = nj_sizes[school])

# Helper functions:
## Calculate M latent mean
calc_m_latent <- function(a, z, nj, given) {
    # Latent M: includes main effects and interactions handled in data generation:
    # M distribution:
    # For binary M: p(M=1)=pnorm(latent)
    # For gaussian M: M ~ Normal(latent, 1 - iccm)
    latent_m <- gen_m$m_on_a * a +
        gen_m$m_on_az * a * z +
        gen_m$m_on_anj * a * nj +
        given
    return(latent_m)
}

## Calculate Y latent
calc_y_latent <- function(m, a, z, nj, given) {
    latent_y <- gen_y$y_on_m * m +
        gen_y$y_on_a * a +
        gen_y$y_on_am * a * m +
        gen_y$y_on_az * a * z +
        gen_y$y_on_mz * m * z +
        gen_y$y_on_anj * a * nj +
        given
    return(latent_y)
}



# ═══════════════════
#    replace compute_expected_y with code for cont med & binary out (below) <- MAYBE MAKE THIS PART INTO truVals_contM_binY()
# ═══════════════════

## Comment below out b/c i dont think i need it anymore 
# z <- data$Z
# nj <- data$nj
# given_m <- m_given
# given_y <- y_given
# 
# # Compute mediator parameters
# m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)


## values set in original data generation 
J = 100 
njrange = c(50, 100) 
Mfamily = "gaussian" # "binomial", 
Yfamily = "binomial" # "gaussian", # 
seed = 8675309

num_x = 3
m_on_a = 3.5
m_on_anj = 0.2
m_on_az = 0.2
y_on_a = 2
y_on_m = 5
y_on_am = 2
y_on_az = 0.2
y_on_mz = 0.2
y_on_anj = 0.2
int.XZ = TRUE

# ══════════════════════════════
#    Below is the part to add to trueVals function  
# ══════════════════════════════

# Set a0 & a1 values 
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
truevals_individual <- list()
truevals_cluster <- list()

# for (j in 1:nrow(a_vals)) {
#     if (Mfamily == "gaussian" & Yfamily == "binomial") {
#         
#         
#     }
# }
j = 4
a0_val <- a_vals$a0[j]
a1_val <- a_vals$a1[j]
label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")

## Part to add to function 

# generate 10,000 
large_J <- 10000
big_njrange <- c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()

pop_result <- generate_data2.0b( # generate_data2.0(
    J = large_J,
    njrange = big_njrange,
    Mfamily = "gaussian",
    Yfamily = "binomial",
    
    if.null = FALSE, # CHANGE
    seed = seed,
    num_x = 3,
    # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
    # a_z = sqrt(0.4 / 1),
    x_z = 0,
    m_on_a = 0.2,
    m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
    m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
    # m_on_x = sqrt(0.15 / num_x),
    # m_on_z = sqrt(0.4),
    int.XZ = TRUE,       # DELETE
    yintercept = 1,
    y_on_a = 0.5,
    y_on_m = 1,
    y_on_am = 0,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 0.2,
    # y_on_x = sqrt(0.15 / num_x),
    # y_on_z = sqrt(0.4),
    quadratic.A = FALSE,
    quadratic.M = FALSE,
    quadratic.Y = FALSE,
    iccx = 0.2,
    icca = 0.2,
    iccm = 0.2,
    iccy = 0.2
)

# add njs to data 
pop_result$data <- pop_result$data |> 
    mutate(nj = nj_sizes[school])




# M is continuous
# # M ~ Normal(m_latent, var=1 - iccm)
# # var_m <- 1 - iccm
# Ma0 <-
#     calc_m_latent(
#         a = a0_val,
#         z = pop_result$data$Z,
#         nj = pop_result$data$nj, 
#         given = pop_result$parameters$m_given
#     ) # M is continuous, so m_latent for a0_val = Ma0 
# 
# Ma1 <-
#     calc_m_latent(
#         a = a1_val,
#         z = pop_result$data$Z,
#         nj = pop_result$data$nj,
#         given = pop_result$parameters$m_given
#     ) # M is continuous, so m_latent for a1_val = Ma1 
m_mu <- calc_m_latent(a = a1_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$m_given)


# # Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]: Y is binary so E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
# y_latent_m0 <-
#     calc_y_latent(
#         m = Ma0,
#         a = a0_val,
#         z = pop_result$data$Z,
#         nj = pop_result$data$nj, #pop_result$parameters$nj_sizes,
#         given = pop_result$parameters$y_given
#     )
# y_latent_m1 <-
#     calc_y_latent(
#         m = Ma1,
#         a = a0_val,
#         z = pop_result$data$Z,
#         nj = pop_result$data$nj, #pop_result$parameters$nj_sizes,
#         given = pop_result$parameters$y_given
#     )
y_latent <- calc_y_latent(m = m_mu, a = a0_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$y_given)

# E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
# E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))


# individual
truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)

# cluster 
cluster_means <- pop_result$data  |>
    mutate(E_y = E_y_each) |>
    group_by(school) |>
    summarize(cluster_avg = mean(E_y, na.rm = TRUE))
truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)


# Check output 
truevals_individual; truevals_cluster


# for (j in 1:nrow(a_vals)) {
#     if (Mfamily == "gaussian" & Yfamily == "binomial") {
# 
# 
#     }
# }
# # j = 1
# a0_val <- a_vals$a0[j]
# a1_val <- a_vals$a1[j]
# label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")












# 2. put 1. part into compute_expected_y() for cont med & binary out --------------------
# Here I will put the part I ran above (1.) into compute_expected_y() 

# These parameters will already be set in func
seed = 8675309
# Mfamily = "gaussian" # "binomial", 
# Yfamily = "binomial" # "gaussian", # 

# Set a0 & a1 values 
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
truevals_individual <- list()
truevals_cluster <- list()

for (j in 1:nrow(a_vals)) {
    if (Mfamily == "gaussian" & Yfamily == "binomial") {
        
        a0_val <- a_vals$a0[j]
        a1_val <- a_vals$a1[j]
        label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
        
        # Generate pop data (J = 10,000) 
        large_J <- 10000
        big_njrange <- c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()
        
        pop_result <- generate_data2.0b( # generate_data2.0(
            J = large_J,
            njrange = big_njrange,
            Mfamily = "gaussian",
            Yfamily = "binomial",
            
            if.null = FALSE, # CHANGE
            seed = seed,
            num_x = 3,
            # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
            # a_z = sqrt(0.4 / 1),
            x_z = 0,
            m_on_a = 0.2,
            m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
            m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
            # m_on_x = sqrt(0.15 / num_x),
            # m_on_z = sqrt(0.4),
            int.XZ = TRUE,       # DELETE
            yintercept = 1,
            y_on_a = 0.5,
            y_on_m = 1,
            y_on_am = 0,
            y_on_az = 0.2,
            y_on_mz = 0.2,
            y_on_anj = 0.2,
            # y_on_x = sqrt(0.15 / num_x),
            # y_on_z = sqrt(0.4),
            quadratic.A = FALSE,
            quadratic.M = FALSE,
            quadratic.Y = FALSE,
            iccx = 0.2,
            icca = 0.2,
            iccm = 0.2,
            iccy = 0.2
        )
        
        # add njs to data 
        pop_result$data <- pop_result$data |> 
            mutate(nj = nj_sizes[school])
        
        
        m_mu <- calc_m_latent(a = a1_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$m_given)
        
        y_latent <- calc_y_latent(m = m_mu, a = a0_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$y_given)
        
        E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
        
        
        # individual
        truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
        
        # cluster 
        cluster_means <- pop_result$data  |>
            mutate(E_y = E_y_each) |>
            group_by(school) |>
            summarize(cluster_avg = mean(E_y, na.rm = TRUE))
        truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
        
    }
}

list(
    # truevals_individual = NULL, 
    # truevals_cluster = NULL
    truevals_individual = truevals_individual,
    truevals_cluster = truevals_cluster
)




# 3. Test run full trueVals function (trueVals2.0c) --------------------------------------
# Test trueVals2.0c

# ══════════════════════════════
#    load functions & set arguments for generate_data2.0b
# ══════════════════════════════

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data2.0b",
    "generate_data2.0", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals2.0b", 
    "trueVals2.0"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# load new/updated func 
source(file.path("Functions/trueVals2.0c.R"))
source(file.path("Functions/generate_data2.0c.R"))


# Arguments 
J = 100                        # Number of clusters
njrange = c(50, 100)            # Range for cluster sizes
Mfamily = "gaussian"            # Family for mediator ('gaussian' or 'binomial')
Yfamily = "binomial"            # Family for outcome ('gaussian' or 'binomial')
if.null = FALSE 
seed = 123456                   # Seed for reproducibility
num_x = 3                       # Number of individual-level confounders
# a_x = 0.15, # sqrt(0.15 * 1 / num_x)
# a_z = sqrt(0.4 / 1), 
x_z = 0                         # Correlation between 'X' and 'Z'
m_on_a = 0.2                    # Effect of 'A' on 'M'
m_on_az = 0.2                   # Interaction effect of 'A' and 'Z' on 'M'
m_on_anj = 0.2                  # Interaction effect of 'A' and cluster size on 'M'
m_on_x = sqrt(0.15 / num_x) 
m_on_z = sqrt(0.4)
int.XZ = TRUE                  # Interaction between 'X' and 'Z'
yintercept = 1                  # Intercept for outcome model
y_on_a = 0.5                    # Effect of 'A' on 'Y'
y_on_m = 1                      # Effect of 'M' on 'Y'
y_on_am = 0                     # Interaction effect of 'A' and 'M' on 'Y'
y_on_az = 0.2                   # Interaction effect of 'A' and 'Z' on 'Y'
y_on_mz = 0.2                   # Interaction effect of 'M' and 'Z' on 'Y'
y_on_anj = 0.2                  # Interaction effect of 'A' and cluster size on 'Y'
y_on_x = sqrt(0.15 / num_x)
y_on_z = sqrt(0.4)
quadratic.A = FALSE             # Include quadratic terms for 'A'
quadratic.M = FALSE             # Include quadratic terms for 'M'
quadratic.Y = FALSE             # Include quadratic terms for 'Y'
iccx = 0.2                      # Intra-class correlation for 'X'
icca = 0.2                      # Intra-class correlation for 'A'
iccm = 0.2                      # Intra-class correlation for 'M'
iccy = 0.2                       # Intra-class correlation for 'Y'



# ══════════════════════════════
#    run interior of generate_data2.0b 
# ══════════════════════════════

# Set Seed
set.seed(seed)

# Generate Clusters
data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)

# Generate Confounders
data_list <- generate_confounders(
    data_list = data_list,
    nj_sizes = data_list$nj_sizes,
    num_x = num_x,
    iccx = iccx,
    x_z = x_z
)

# Generate Treatment
data_list <- generate_treatment(
    data_list = data_list,
    nj_sizes = data_list$nj_sizes,
    icca = icca,
    quadratic.A = quadratic.A,
    num_x = num_x# , 
    # a_x = data_list$a_x,
    # a_z = a_z, 
)

# SKIPP OVERLAP PART OF FUNCTION 
# # Create Overlap Plot
# overlap_plot <- ggplot(data_list$data, aes(x = ps_true, color = factor(A), fill = factor(A))) +
#     geom_density(alpha = 0.5) +
#     labs(
#         title = "Density Plot of ps_true by Treatment Group (A)",
#         x = "True Propensity Score (ps_true)",
#         y = "Density",
#         fill = "Treatment (A)"
#     ) +
#     theme_minimal() +
#     theme(
#         legend.position = "top",
#         plot.title = element_text(hjust = 0.5, face = "bold")
#     )
# # Create Overlap Plot with Logit PS
# overlap_plot_logit <- ggplot(data_list$data, aes(x = qlogis(ps_true), fill = factor(A))) +
#     geom_density(alpha = 0.5) +
#     labs(
#         title = "Density Plot of Logit(ps_true) by Treatment Group (A)",
#         x = "Logit of the True Propensity Score",
#         y = "Density",
#         fill = "Treatment (A)"
#     ) +
#     theme_minimal()
# 
# # Summarize extreme PS values
# n_ps_below_001 <- sum(data_list$data$ps_true < 0.01, na.rm = TRUE)
# n_ps_above_099 <- sum(data_list$data$ps_true > 0.99, na.rm = TRUE)
# 
# # Calculate percentages
# pct_ps_below_001 <- 100 * n_ps_below_001 / nrow(data_list$data)
# pct_ps_above_099 <- 100 * n_ps_above_099 / nrow(data_list$data)
# 
# # Combine into a message string
# ps_msg <- paste0(
#     "Number of PSs < 0.01: ", n_ps_below_001, " (", 
#     round(pct_ps_below_001, 2), "%); ",
#     "Number of PSs > 0.99: ", n_ps_above_099, " (",
#     round(pct_ps_above_099, 2), "%)"
# )
# # Print to console
# message(ps_msg)
# 
# # Create an IPTW variable
# data_list$data <- data_list$data %>%
#     mutate(
#         iptw_true = ifelse(A == 1, 1 / ps_true, 1 / (1 - ps_true))
#     )
# 
# # Identify 1st and 99th percentiles
# first_percentile <- quantile(data_list$data$iptw_true, probs = 0.01, na.rm = TRUE)
# ninety_ninth_percentile <- quantile(data_list$data$iptw_true, probs = 0.99, na.rm = TRUE)
# 
# # Count outliers below 1st percentile & above 99th percentile
# n_iptw_below_1p <- sum(data_list$data$iptw_true < first_percentile, na.rm = TRUE)
# n_iptw_above_99p <- sum(data_list$data$iptw_true > ninety_ninth_percentile, na.rm = TRUE)
# 
# # Calculate percentages for IPTW outliers
# pct_iptw_below_1p <- 100 * n_iptw_below_1p / nrow(data_list$data)
# pct_iptw_above_99p <- 100 * n_iptw_above_99p / nrow(data_list$data)
# 
# # Combine into a message string
# iptw_msg <- paste0(
#     "Number of cases < 1st percentile of IPTW (", 
#     round(first_percentile, 4), "): ", n_iptw_below_1p, " (",
#     round(pct_iptw_below_1p, 2), "%); ",
#     "Number of cases > 99th percentile of IPTW (", 
#     round(ninety_ninth_percentile, 4), "): ", n_iptw_above_99p, " (",
#     round(pct_iptw_above_99p, 2), "%)"
# )
# # Print to console
# message(iptw_msg)



# Generate Mediator
data_list <- generate_mediator(
    data_list = data_list,
    nj_sizes = data_list$nj_sizes,
    iccm = iccm,
    num_x = num_x,
    m_on_a = m_on_a,
    m_on_az = m_on_az,
    m_on_anj = m_on_anj,
    # m_on_x = m_on_x, 
    # m_on_z = m_on_z,
    quadratic.M = quadratic.M,
    int.XZ = int.XZ, 
    Mfamily = Mfamily
)

# Generate Outcome
data_list <- generate_outcome(
    data_list = data_list,
    iccy = iccy,
    yintercept = yintercept,
    y_on_a = y_on_a,
    y_on_m = y_on_m,
    y_on_am = y_on_am,
    y_on_az = y_on_az,
    y_on_mz = y_on_mz,
    y_on_anj = y_on_anj,
    num_x = num_x,
    # y_on_x = y_on_x,
    # y_on_z = y_on_z,
    quadratic.Y = quadratic.Y,
    int.XZ = int.XZ,
    Yfamily = Yfamily,
    if.null = if.null
)


# ══════════════════════════════
#    test trueVals2.0c 
# ══════════════════════════════

# REPLACE PART OF FUNCTION TO COMPUTE TRUE VALUES WITH UPDATED TRUEVALUES FUNCTION 
# # Compute True Values (if possible)
# if (Mfamily %in% c("binomial","gaussian") & Yfamily %in% c("binomial","gaussian")) {
#     true_vals <- trueVals2.0b(data_list = data_list)
# } else {
#     true_vals <- NULL
# }
true_vals <- trueVals2.0c(data_list = data_list)

# View potential outcomes 
data.frame(
    type = rep(c("individual", "cluster"), each = 4),
    potential_outcome = c("Y(a0=0, gm(a1=0))", "Y(a0=1, gm(a1=0))", 
                  "Y(a0=0, gm(a1=1))", "Y(a0=1, gm(a1=1))"),
    value = c(
        true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`,
        true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`,
        true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`,
        true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`,
        true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`,
        true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`,
        true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`,
        true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
    )
)



# Extract the relevant potential outcomes from true_vals
if (!is.null(true_vals)) {
    # Individual-level true values
    y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
    y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
    y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
    y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
    
    # Compute individual-level mediation effects
    pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
    pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
    tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
    tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
    
    # Cluster-level true values
    y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
    y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
    y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
    y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
    
    # Compute cluster-level mediation effects
    pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
    pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
    tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
    tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
} else {
    pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
    pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
}


effects = list(
    individual = list(pnde = pnde_ind, tnie = tnie_ind, tnde = tnde_ind, pnie = pnie_ind),
    cluster = list(pnde = pnde_cluster, tnie = tnie_cluster, tnde = tnde_cluster, pnie = pnie_cluster)
)

effects







# test new data generation func -------------------------------------------




# ══════════════════════════════
#    load functions (particularly trueVals2.0c & generate_data2.0c)
# ══════════════════════════════

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data2.0b",
    "generate_data2.0", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals2.0b", 
    "trueVals2.0", 
    
    # NEW FUNCTIONS TO TEST
    "trueVals2.0c", 
    "generate_data2.0c"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

set.seed(8675309)
data <- generate_data2.0c(J = 100, 
                  njrange = c(50, 100), 
                  Mfamily = "gaussian", # "binomial", 
                  Yfamily = "binomial", 
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
                  int.XZ = FALSE)

# Effects
data.frame(
    individual = unlist(data$effects$individual),
    cluster = unlist(data$effects$cluster),
    row.names = names(data$effects$individual)
) |> 
    format(scientific = FALSE)

# binomial med & out 
# individual     cluster
# pnde 0.107344744 0.108667581
# tnie 0.009257772 0.009533959
# tnde 0.003682356 0.003771970
# pnie 0.112920161 0.114429569

# gaussian med & binomial out
# individual       cluster
# pnde 0.08904722859 0.08523209978
# tnie 0.27927005272 0.28316283865
# tnde 0.00000186615 0.00000194882
# pnie 0.36831541515 0.36839298962

# PNDE + TNIE = TNDE + PNIE
(data$effects$individual$pnde + data$effects$individual$tnie) == (data$effects$individual$tnde + data$effects$individual$pnie)















# interior of trueVals2.0 func --------------------------------------------

library(dplyr)
library(glue)

# Extract necessary components
data <- data_list$data
nj_sizes <- data_list$nj_sizes
Mfamily <- data_list$Mfamily
Yfamily <- data_list$Yfamily
iccm <- data_list$iccm
iccy <- data_list$iccy

# Extract mediator parameters
gen_m <- list(
    iccm = iccm,
    m_on_a = data_list$m_on_a,
    m_on_x = data_list$m_on_x,
    m_on_z = data_list$m_on_z,
    m_on_az = data_list$m_on_az,
    m_on_anj = data_list$m_on_anj
)

# Extract outcome parameters
gen_y <- list(
    iccy = iccy,
    yintercept = data_list$yintercept,
    y_on_a = data_list$y_on_a,
    y_on_m = data_list$y_on_m,
    y_on_am = data_list$y_on_am,
    y_on_az = data_list$y_on_az,
    y_on_mz = data_list$y_on_mz,
    y_on_anj = data_list$y_on_anj,
    y_on_x = data_list$y_on_x,
    y_on_z = data_list$y_on_z
)

m_given <- data_list$m_given
y_given <- data_list$y_given

data <- data  |> 
    mutate(nj = nj_sizes[school])

# Helper functions:
## Calculate M latent mean
calc_m_latent <- function(a, z, nj, given) {
    # Latent M: includes main effects and interactions handled in data generation:
    # M distribution:
    # For binary M: p(M=1)=pnorm(latent)
    # For gaussian M: M ~ Normal(latent, 1 - iccm)
    latent_m <- gen_m$m_on_a * a +
        gen_m$m_on_az * a * z +
        gen_m$m_on_anj * a * nj +
        given
    return(latent_m)
}

## Calculate Y latent
calc_y_latent <- function(m, a, z, nj, given) {
    latent_y <- gen_y$y_on_m * m +
        gen_y$y_on_a * a +
        gen_y$y_on_am * a * m +
        gen_y$y_on_az * a * z +
        gen_y$y_on_mz * m * z +
        gen_y$y_on_anj * a * nj +
        given
    return(latent_y)
}

# Compute E[Y(a0, gm(a1))] for each observation
compute_expected_y <- function(a0_val, a1_val, data) {
    z <- data$Z
    nj <- data$nj
    given_m <- m_given
    given_y <- y_given
    
    # Compute mediator parameters
    m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)
    
    if (Mfamily == "binomial") {
        # M in {0,1}
        p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - iccm))
        p_m0 <- 1 - p_m1
        
        # Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]
        # For Y:
        # If Y is binary: E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
        # If Y is continuous: E[Y|...] = y_latent
        y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, nj = nj, given = given_y)
        y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, nj = nj, given = given_y)
        
        if (Yfamily == "binomial") {
            E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
            E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
        } else {
            # Continuous Y
            E_y_m0 <- y_latent_m0
            E_y_m1 <- y_latent_m1
        }
        
        E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
        
    } else {
        # M is continuous
        # M ~ Normal(m_latent, var=1 - iccm)
        var_m <- 1 - iccm
        mean_m <- m_latent
        
        if (Yfamily == "gaussian") {
            # Continuous M, continuous Y:
            # E[Y] = E[E[Y|M]] = E[y_latent(M)]
            # If linear, E[Y] = y_latent(E[M]) since it's linear in M.
            # Substitute M = mean_m directly:
            y_latent_mean_m <- calc_y_latent(m = mean_m, a = a0_val, z = z, nj = nj, given = given_y)
            E_y <- y_latent_mean_m
        } else {
            # CHANGING THIS PART
            
            # generate 10,000 
            # a1, m_given, 
            calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m) # combine m_latent with residuals
            # make latent m to cont or bin based on mediator 
            calc_y_latent(m = Ma1, a = a0_val, z = z, nj = nj, given = given_y) # set m to potential 
            # take mean of outcome E[Y] 
            
            # ══════════════════════════════
            #    Regenerate entire data generation pipeline with a large J (e.g., 10,000 clusters) 
            # ══════════════════════════════
            
            large_J <- 10000
            
            big_njrange <- c()
            
            pop_result <- generate_data2.0(
                J = large_J, 
                njrange = big_njrange, 
                Mfamily = "gaussian", 
                Yfamily = "binomial",
                
                if.null = FALSE, # CHANGE 
                seed = seed,            
                num_x = 3, 
                # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
                # a_z = sqrt(0.4 / 1), 
                x_z = 0,   
                m_on_a = 0.2, 
                m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
                m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
                # m_on_x = sqrt(0.15 / num_x), 
                # m_on_z = sqrt(0.4),
                int.XZ = TRUE,       # DELETE 
                yintercept = 1,               
                y_on_a = 0.5,                 
                y_on_m = 1,                   
                y_on_am = 0,                  
                y_on_az = 0.2,                
                y_on_mz = 0.2,                
                y_on_anj = 0.2,               
                # y_on_x = sqrt(0.15 / num_x),
                # y_on_z = sqrt(0.4),
                quadratic.A = FALSE,        
                quadratic.M = FALSE,        
                quadratic.Y = FALSE,        
                iccx = 0.2,                 
                icca = 0.2,                 
                iccm = 0.2,                 
                iccy = 0.2
            )
            
            # # Continuous M, binary Y:
            # # E[Y] = ∫ pnorm(y_latent(M)) dPhi(M; mean_m, var_m)
            # sd_m <- sqrt(var_m)
            # m_points <- seq(mean_m - 3 * sd_m, mean_m + 3 * sd_m, length.out = 31) # range of M values +/- 3 SD 
            # pdf_m <- dnorm(m_points, mean = mean_m, sd = sd_m)
            # y_latent_vals <- sapply(m_points, function(mm) {
            #     calc_y_latent(m = mm, a = a0_val, z = z, nj = nj, given = given_y)
            # })
            # p_y_given_m <- pnorm(y_latent_vals, 0, sqrt(1 - iccy)) # convert latent y to probability 
            # delta_m <- (6 * sd_m) / 30
            # E_y <- sum(p_y_given_m * pdf_m) * delta_m # multiplies the probabilities by the PDF of M at M-grid point & sums/integrates over grid to approximate E[Y]
        }
    }
    
    return(E_y)
}

# We consider (a0,a1) in {0,1}x{0,1}
a_vals <- expand.grid(a0=c(0,1), a1=c(0,1))
truevals_individual <- list()
truevals_cluster <- list()

for (j in 1:nrow(a_vals)) {
    a0_val <- a_vals$a0[j]
    a1_val <- a_vals$a1[j]
    label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
    
    E_y_each <- compute_expected_y(a0_val, a1_val, data)
    
    truevals_individual[[label]] <- mean(E_y_each, na.rm=TRUE)
    
    cluster_means <- data  |> 
        mutate(E_y = E_y_each) |>
        group_by(school) |>
        summarize(cluster_avg = mean(E_y, na.rm=TRUE))
    truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm=TRUE)
}

return(list(
    truevals_individual = truevals_individual,
    truevals_cluster = truevals_cluster
))




# updating compute_expected_y for cont med & binary out ------------------------------------------

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data2.0b",
    "generate_data2.0", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals2.0b", 
    "trueVals2.0"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}


# ═══════════════════
#    generate data 
# ═══════════════════

# Generate Data 
set.seed(8675309)
data_list <- generate_data2.0b(
    J = 100, 
    njrange = c(50, 100), 
    Mfamily = "gaussian", # "binomial", 
    Yfamily = "binomial", # "gaussian", # 
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
    int.XZ = TRUE
)

# data_list


# ═══════════════════
#    run beginning of trueVals2.0 func 
# ═══════════════════

library(dplyr)
library(glue)

# Extract necessary components
data <- data_list$data
nj_sizes <- data_list$nj_sizes
Mfamily <- data_list$Mfamily
Yfamily <- data_list$Yfamily
iccm <- data_list$iccm
iccy <- data_list$iccy

# Extract mediator parameters
gen_m <- list(
    iccm = iccm,
    m_on_a = data_list$m_on_a,
    m_on_x = data_list$m_on_x,
    m_on_z = data_list$m_on_z,
    m_on_az = data_list$m_on_az,
    m_on_anj = data_list$m_on_anj
)

# Extract outcome parameters
gen_y <- list(
    iccy = iccy,
    yintercept = data_list$yintercept,
    y_on_a = data_list$y_on_a,
    y_on_m = data_list$y_on_m,
    y_on_am = data_list$y_on_am,
    y_on_az = data_list$y_on_az,
    y_on_mz = data_list$y_on_mz,
    y_on_anj = data_list$y_on_anj,
    y_on_x = data_list$y_on_x,
    y_on_z = data_list$y_on_z
)

m_given <- data_list$m_given
y_given <- data_list$y_given

data <- data  |> 
    mutate(nj = nj_sizes[school])

# Helper functions:
## Calculate M latent mean
calc_m_latent <- function(a, z, nj, given) {
    # Latent M: includes main effects and interactions handled in data generation:
    # M distribution:
    # For binary M: p(M=1)=pnorm(latent)
    # For gaussian M: M ~ Normal(latent, 1 - iccm)
    latent_m <- gen_m$m_on_a * a +
        gen_m$m_on_az * a * z +
        gen_m$m_on_anj * a * nj +
        given
    return(latent_m)
}

## Calculate Y latent
calc_y_latent <- function(m, a, z, nj, given) {
    latent_y <- gen_y$y_on_m * m +
        gen_y$y_on_a * a +
        gen_y$y_on_am * a * m +
        gen_y$y_on_az * a * z +
        gen_y$y_on_mz * m * z +
        gen_y$y_on_anj * a * nj +
        given
    return(latent_y)
}


# ═══════════════════
#    replace compute_expected_y with code for cont med & binary out (below) <- MAYBE MAKE THIS PART INTO truVals_contM_binY()
# ═══════════════════

## Comment below out b/c i dont think i need it anymore 
# z <- data$Z
# nj <- data$nj
# given_m <- m_given
# given_y <- y_given
# 
# # Compute mediator parameters
# m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)


## values set in original data generation 
J = 100 
njrange = c(50, 100) 
Mfamily = "gaussian" # "binomial", 
Yfamily = "binomial" # "gaussian", # 
seed = 8675309

num_x = 3
m_on_a = 3.5
m_on_anj = 0.2
m_on_az = 0.2
y_on_a = 2
y_on_m = 5
y_on_am = 2
y_on_az = 0.2
y_on_mz = 0.2
y_on_anj = 0.2
int.XZ = TRUE

## Part to add to function 

# generate 10,000 
large_J <- 10000
big_njrange <- c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()

pop_result <- generate_data2.0b( # generate_data2.0(
    J = large_J,
    njrange = big_njrange,
    Mfamily = "gaussian",
    Yfamily = "binomial",

    if.null = FALSE, # CHANGE
    seed = seed,
    num_x = 3,
    # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
    # a_z = sqrt(0.4 / 1),
    x_z = 0,
    m_on_a = 0.2,
    m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
    m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
    # m_on_x = sqrt(0.15 / num_x),
    # m_on_z = sqrt(0.4),
    int.XZ = TRUE,       # DELETE
    yintercept = 1,
    y_on_a = 0.5,
    y_on_m = 1,
    y_on_am = 0,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 0.2,
    # y_on_x = sqrt(0.15 / num_x),
    # y_on_z = sqrt(0.4),
    quadratic.A = FALSE,
    quadratic.M = FALSE,
    quadratic.Y = FALSE,
    iccx = 0.2,
    icca = 0.2,
    iccm = 0.2,
    iccy = 0.2
)


# M is continuous
# M ~ Normal(m_latent, var=1 - iccm)
# var_m <- 1 - iccm
Ma0 <-
    calc_m_latent(
        a = a0_val,
        z = pop_result$data$Z,
        nj = pop_result$parameters$nj_sizes,
        given = pop_result$parameters$m_given
    ) # M is continuous, so m_latent for a0_val = Ma0 
Ma1 <-
    calc_m_latent(
        a = a1_val,
        z = pop_result$data$Z,
        nj = pop_result$parameters$nj_sizes,
        given = pop_result$parameters$m_given
    ) # M is continuous, so m_latent for a1_val = Ma1 

# Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]: Y is binary so E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
y_latent_m0 <-
    calc_y_latent(
        m = Ma0,
        a = a0_val,
        z = pop_result$data$Z,
        nj = pop_result$parameters$nj_sizes,
        given = pop_result$parameters$y_given
    )
y_latent_m1 <-
    calc_y_latent(
        m = Ma1,
        a = a0_val,
        z = pop_result$data$Z,
        nj = pop_result$parameters$nj_sizes,
        given = pop_result$parameters$y_given
    )

E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))




# ══════════════════════════════
#    Testing above in updated compute_expected_y function  
# ══════════════════════════════

# compute_expected_y() will output NULL for E_y (only for cont med & binary out) to initiate the following code 

# run following function to save compute_expected_y() in environment 
compute_expected_y <- function(a0_val, a1_val, data) {
    z <- data$Z
    nj <- data$nj
    given_m <- m_given
    given_y <- y_given
    
    # Compute mediator parameters
    m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)
    
    if (Mfamily == "binomial") {
        # M in {0,1}
        p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - iccm))
        p_m0 <- 1 - p_m1
        
        # Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]
        # For Y:
        # If Y is binary: E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
        # If Y is continuous: E[Y|...] = y_latent
        y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, nj = nj, given = given_y)
        y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, nj = nj, given = given_y)
        
        if (Yfamily == "binomial") {
            E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
            E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
        } else {
            # Continuous Y
            E_y_m0 <- y_latent_m0
            E_y_m1 <- y_latent_m1
        }
        
        E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
        
    } else {
        # M is continuous
        # M ~ Normal(m_latent, var=1 - iccm)
        var_m <- 1 - iccm
        mean_m <- m_latent
        
        if (Yfamily == "gaussian") {
            # Continuous M, continuous Y:
            # E[Y] = E[E[Y|M]] = E[y_latent(M)]
            # If linear, E[Y] = y_latent(E[M]) since it's linear in M.
            # Substitute M = mean_m directly:
            y_latent_mean_m <- calc_y_latent(m = mean_m, a = a0_val, z = z, nj = nj, given = given_y)
            E_y <- y_latent_mean_m
        } else {
            E_y <- NULL
            # # CHANGING THIS PART
            # 
            # # generate 10,000 
            # # a1, m_given, 
            # calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m) # combine m_latent with residuals
            # # make latent m to cont or bin based on mediator 
            # calc_y_latent(m = Ma1, a = a0_val, z = z, nj = nj, given = given_y) # set m to potential 
            # # take mean of outcome E[Y] 
            # 
            # # ══════════════════════════════
            # #    Regenerate entire data generation pipeline with a large J (e.g., 10,000 clusters) 
            # # ══════════════════════════════
            # 
            # large_J <- 10000
            # 
            # big_njrange <- c()
            # 
            # pop_result <- generate_data2.0(
            #     J = large_J, 
            #     njrange = big_njrange, 
            #     Mfamily = "gaussian", 
            #     Yfamily = "binomial",
            #     
            #     if.null = FALSE, # CHANGE 
            #     seed = seed,            
            #     num_x = 3, 
            #     # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
            #     # a_z = sqrt(0.4 / 1), 
            #     x_z = 0,   
            #     m_on_a = 0.2, 
            #     m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
            #     m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
            #     # m_on_x = sqrt(0.15 / num_x), 
            #     # m_on_z = sqrt(0.4),
            #     int.XZ = TRUE,       # DELETE 
            #     yintercept = 1,               
            #     y_on_a = 0.5,                 
            #     y_on_m = 1,                   
            #     y_on_am = 0,                  
            #     y_on_az = 0.2,                
            #     y_on_mz = 0.2,                
            #     y_on_anj = 0.2,               
            #     # y_on_x = sqrt(0.15 / num_x),
            #     # y_on_z = sqrt(0.4),
            #     quadratic.A = FALSE,        
            #     quadratic.M = FALSE,        
            #     quadratic.Y = FALSE,        
            #     iccx = 0.2,                 
            #     icca = 0.2,                 
            #     iccm = 0.2,                 
            #     iccy = 0.2
            # )
            
            # # Continuous M, binary Y:
            # # E[Y] = ∫ pnorm(y_latent(M)) dPhi(M; mean_m, var_m)
            # sd_m <- sqrt(var_m)
            # m_points <- seq(mean_m - 3 * sd_m, mean_m + 3 * sd_m, length.out = 31) # range of M values +/- 3 SD 
            # pdf_m <- dnorm(m_points, mean = mean_m, sd = sd_m)
            # y_latent_vals <- sapply(m_points, function(mm) {
            #     calc_y_latent(m = mm, a = a0_val, z = z, nj = nj, given = given_y)
            # })
            # p_y_given_m <- pnorm(y_latent_vals, 0, sqrt(1 - iccy)) # convert latent y to probability 
            # delta_m <- (6 * sd_m) / 30
            # E_y <- sum(p_y_given_m * pdf_m) * delta_m # multiplies the probabilities by the PDF of M at M-grid point & sums/integrates over grid to approximate E[Y]
        }
    }
    
    return(E_y)
}

# 
# set 
Mfamily = "gaussian"
Yfamily = "binomial"


# We consider (a0,a1) in {0,1}x{0,1}
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
truevals_individual <- list()
truevals_cluster <- list()
# 
#   NEED TO: 
#       + NEXT STEP: work on the line 610 area. Ignore fitting it in a function for now. Just correctly run it in this script, then you can create another section to convert it into the function. 
#       I will also need to test that using generate_data2.0() within trueVals(), which itself is in generate_data2.0(), will work properly on referring to the set parameters. 
#       Maybe make truVals_contM_binY() to specifically re-generate data but large (take the existing data's parameters in a list as an argument & use the calc_m_latent & calc_y_latent functions inside)
#
# Add large data generation for Mfamily == "gaussian" & Yfamily == "binomial" 
## add if statement 
if (Mfamily == "gaussian" & Yfamily == "binomial") {
    # generate 10,000 
    large_J <- 10000
    big_njrange <- c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()
    pop_result <- generate_data2.0b( # generate_data2.0(
        J = large_J,
        njrange = big_njrange,
        Mfamily = "gaussian",
        Yfamily = "binomial",
        
        if.null = FALSE, # CHANGE
        seed = data_list$parameters$seed, 
        num_x = 3,
        # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
        # a_z = sqrt(0.4 / 1),
        x_z = 0,
        m_on_a = 0.2,
        m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
        m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
        # m_on_x = sqrt(0.15 / num_x),
        # m_on_z = sqrt(0.4),
        int.XZ = TRUE,       # DELETE
        yintercept = 1,
        y_on_a = 0.5,
        y_on_m = 1,
        y_on_am = 0,
        y_on_az = 0.2,
        y_on_mz = 0.2,
        y_on_anj = 0.2,
        # y_on_x = sqrt(0.15 / num_x),
        # y_on_z = sqrt(0.4),
        quadratic.A = FALSE,
        quadratic.M = FALSE,
        quadratic.Y = FALSE,
        iccx = 0.2,
        icca = 0.2,
        iccm = 0.2,
        iccy = 0.2
    )
    
    for (j in 1:nrow(a_vals)) {
        a0_val <- a_vals$a0[j]
        a1_val <- a_vals$a1[j]
        label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
        
        # E_y_each <- compute_expected_y(a0_val, a1_val, data)
        
        # 
        # pop M values for a1 {0, 1}
        Ma1 <-
            calc_m_latent(
                a = a0_val,
                z = pop_result$data$Z,
                nj = pop_result$parameters$nj_sizes,
                given = pop_result$parameters$m_given
            ) # M is continuous, so m_latent for a1_val = Ma1 
        
        # latent y under a0_val {0, 1} & potential Ma1
        y_latent_Ma1 <-
            calc_y_latent(
                m = Ma1,
                a = a0_val,
                z = pop_result$data$Z,
                nj = pop_result$parameters$nj_sizes,
                given = pop_result$parameters$y_given
            )
        
        
        # 
        E_y_Ma1 <- pnorm(y_latent_Ma1, 0, sqrt(1 - iccy))
        
        E_y_each <- E_y_Ma1
        
        # # M is continuous
        # # M ~ Normal(m_latent, var=1 - iccm)
        # # var_m <- 1 - iccm
        # Ma0 <-
        #     calc_m_latent(
        #         a = a0_val,
        #         z = pop_result$data$Z,
        #         nj = pop_result$parameters$nj_sizes,
        #         given = pop_result$parameters$m_given
        #     ) # M is continuous, so m_latent for a0_val = Ma0 
        # Ma1 <-
        #     calc_m_latent(
        #         a = a1_val,
        #         z = pop_result$data$Z,
        #         nj = pop_result$parameters$nj_sizes,
        #         given = pop_result$parameters$m_given
        #     ) # M is continuous, so m_latent for a1_val = Ma1 
        # 
        # # Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]: Y is binary so E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
        # y_latent_m0 <-
        #     calc_y_latent(
        #         m = Ma0,
        #         a = a0_val,
        #         z = pop_result$data$Z,
        #         nj = pop_result$parameters$nj_sizes,
        #         given = pop_result$parameters$y_given
        #     )
        # y_latent_m1 <-
        #     calc_y_latent(
        #         m = Ma1,
        #         a = a0_val,
        #         z = pop_result$data$Z,
        #         nj = pop_result$parameters$nj_sizes,
        #         given = pop_result$parameters$y_given
        #     )
        # 
        # E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
        # E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
        
        truevals_individual[[label]] <- mean(E_y_each, na.rm=TRUE)
        
        # cluster_means <- data  |>
        #     mutate(E_y = E_y_each) |>
        #     group_by(school) |>
        #     summarize(cluster_avg = mean(E_y, na.rm=TRUE))
        # truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm=TRUE)
    }
}








#Plexity
if (Yfamily == "binomial" && Mfamily == "gaussian") {
    # Generate a large number of cases
    n_cases <- 10000
    
    # Generate potential mediator values
    m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)
    Ma1 <- rnorm(n_cases, mean = m_latent, sd = sqrt(1 - iccm))
    
    # Calculate potential outcomes
    y_latent <- calc_y_latent(m = Ma1, a = a0_val, z = z, nj = nj, given = given_y)
    
    # Compute probabilities
    probs <- pnorm(y_latent, 0, sqrt(1 - iccy))
    
    # Take the mean to get the expected value
    E_y <- mean(probs)
}

#R chat
# Replace the continuous-M and binary-Y logic
if (Mfamily == "gaussian" && Yfamily == "binomial") {
    # Generate 10,000 samples for M
    set.seed(123) # Ensure reproducibility
    m_samples <- rnorm(10000, mean = mean_m, sd = sqrt(var_m))
    
    # Compute the corresponding latent Y values
    y_latent_vals <- sapply(m_samples, function(m_sample) {
        calc_y_latent(m = m_sample, a = a0_val, z = z, nj = nj, given = given_y)
    })
    
    # Convert latent Y to probabilities using probit link
    p_y_given_m <- pnorm(y_latent_vals, mean = 0, sd = sqrt(1 - iccy))
    
    # Sample binary outcomes based on probabilities
    y_samples <- rbinom(10000, size = 1, prob = p_y_given_m)
    
    # Compute the expected value of Y
    E_y <- mean(y_samples)
}


# chat 01 
