################################################################################
##################### Fixing & Testing trueVals Function  ######################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 12/13/2024
#
#
# Script Description: this is used to fix, test, and properly build the trueVals funciton 
#
#
# Last Updated: 12/13/2024
#
#
# Notes:
#   To-Do: figure out why trueVals (specifically, pm1() & my() are returning integer(0))
#               # So far i have been going through the function (qoeking on loop.. section) & commenting out & adding code for one interation of the loops 
#
#   Done: 
#
################################################################################






# Set Up (Load packages, functions, &/or data) ----------------------------

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    # doParallel, 
    # foreach,
    # parallel, 
    
    dplyr, 
    readr 
    # ggplot2
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
    "trueVals"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}




# generate data & set initial values within function  ---------------------


# Generate Data 
set.seed(8675309)
data_list <- generate_data(
    # J = test_condition[["J"]], 
    # njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
    Mfamily = "binomial",
    Yfamily = "binomial", 
    seed = 8769,
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


# ===========================
# set up of initial part of trueVals 
# ===========================

# Extract necessary components from data_list
data <- data_list$data
nj_sizes <- data_list$parameters$nj_sizes
num_x <- data_list$parameters$num_x
x_z <- data_list$parameters$x_z
a_x <- data_list$parameters$a_x
a_z <- data_list$parameters$a_z
icca <- data_list$parameters$icca
quadratic_A <- data_list$parameters$quadratic.A

iccm <- data_list$parameters$iccm
m_on_a <- data_list$parameters$m_on_a
m_on_x <- data_list$parameters$m_on_x
m_on_z <- data_list$parameters$m_on_z
m_on_az <- data_list$parameters$m_on_az
m_on_anj <- data_list$parameters$m_on_anj
quadratic_M <- data_list$parameters$quadratic.M 
Mfamily = data_list$parameters$Mfamily 
# m_given = data_list$parameters$m_given

iccy = data_list$parameters$iccy 
yintercept = data_list$parameters$yintercept 
y_on_a = data_list$parameters$y_on_a 
y_on_m = data_list$parameters$y_on_m 
y_on_am = data_list$parameters$y_on_am 
y_on_az = data_list$parameters$y_on_az 
y_on_mz = data_list$parameters$y_on_mz 
y_on_anj = data_list$parameters$y_on_anj 
y_on_x = data_list$parameters$y_on_x
y_on_z = data_list$parameters$y_on_z
quadratic.Y = data_list$parameters$quadratic.Y 
Yfamily = data_list$parameters$Yfamily 
# y_given = data_list$parameters$y_given

# Define 'gen_m' if not provided
if (is.null(data_list$gen_m)) {
    gen_m <- list(
        iccm = iccm,
        m_on_a = m_on_a,
        m_on_x = m_on_x,
        m_on_z = m_on_z,
        m_on_az = m_on_az,
        m_on_anj = m_on_anj
    )
} else {
    gen_m <- data_list$gen_m
}

# Define 'gen_y' if not provided
if (is.null(data_list$gen_y)) {
    gen_y <- list(
        iccy = iccy, 
        yintercept = yintercept, 
        y_on_a = y_on_a, 
        y_on_m = y_on_m, 
        y_on_am = y_on_am, 
        y_on_az = y_on_az, 
        y_on_mz = y_on_mz, 
        y_on_anj = y_on_anj, 
        y_on_x = y_on_x,
        y_on_z = y_on_z
    )
} else {
    gen_y <- data_list$gen_y
}

# Prepare the data by adding 'nj' based on 'school'
data <- data |> 
    dplyr::mutate(nj = nj_sizes[school])

# Define treatment combinations
a_vals <- expand.grid(
    a0 = unique(c(0, 1)),  
    a1 = unique(c(0, 1))
)
# a_vals$ajo <- NA  # Not used for single mediator

# Initialize lists to store true values
truevals_individual <- list()
truevals_cluster <- list()





# working on loop section of function (meat of func) ----------------------








# Loop over each combination of treatment values
# for (j in 1:nrow(a_vals)) {

    j = 1

    a0_val <- a_vals$a0[j]
    a1_val <- a_vals$a1[j]
    
    # Define a label for the current treatment combination
    label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
    
    # All possible values of the mediator (binary: 0 and 1)
    mediator_vals <- expand.grid(m = c(0, 1))
    
    # Compute the expected outcome by integrating over mediator distribution
    # intm <- sapply(1:nrow(mediator_vals), function(i) {
        i = 1
        m <- mediator_vals$m[i]
        
        # Compute the probability of mediator being m
        p_m <- pm1(
            m = m,
            a = a1_val,
            z = data$Z,
            nj = data$nj, #nj = data_list$nj_sizes, # 
            given = data_list$m_given,
            gen_m = gen_m
        )
        
        # Compute the expected outcome given mediator and treatment
        expected_y <- my(
            m = m,
            a = a0_val,
            z = data$Z,
            nj = data$nj, #nj = data_list$nj_sizes, # nj = data$nj,
            given = data_list$y_given,
            gen_y = gen_y,
            binary = (Yfamily == "binomial")
        )
        
        # Return the product of mediator probability and expected outcome
        return(p_m * expected_y)
    # })
    
    # Sum over all mediator values to get the expected outcome for each observation
    # Resulting in a vector of expected outcomes per observation
    int_m_sum <- rowSums(intm)
    
    # Compute the average expected outcome across all observations (individual-level)
    truevals_individual[[label]] <- mean(int_m_sum, na.rm = TRUE)
    
    # Compute the cluster-level average expected outcome
    # Aggregating by 'school' and then taking the mean across clusters
    cluster_means <- data  |> 
        mutate(int_m_sum = int_m_sum) |> 
        group_by(school) |> 
        summarize(cluster_avg = mean(int_m_sum, na.rm = TRUE))
    
    truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
}








trueVals <- function(data_list) {
    # Extract necessary components from data_list
    data <- data_list$data
    nj_sizes <- data_list$nj_sizes
    num_x <- data_list$num_x
    x_z <- data_list$x_z
    a_x <- data_list$a_x
    a_z <- data_list$a_z
    icca <- data_list$icca
    quadratic_A <- data_list$quadratic.A
    
    iccm <- data_list$iccm
    m_on_a <- data_list$m_on_a
    m_on_x <- data_list$m_on_x
    m_on_z <- data_list$m_on_z
    m_on_az <- data_list$m_on_az
    m_on_anj <- data_list$m_on_anj
    quadratic_M <- data_list$quadratic.M 
    Mfamily = data_list$Mfamily 
    # m_given = data_list$m_given
    
    iccy = data_list$iccy 
    yintercept = data_list$yintercept 
    y_on_a = data_list$y_on_a 
    y_on_m = data_list$y_on_m 
    y_on_am = data_list$y_on_am 
    y_on_az = data_list$y_on_az 
    y_on_mz = data_list$y_on_mz 
    y_on_anj = data_list$y_on_anj 
    y_on_x = data_list$y_on_x
    y_on_z = data_list$y_on_z
    quadratic.Y = data_list$quadratic.Y 
    Yfamily = data_list$Yfamily 
    # y_given = data_list$y_given
    
    # Define 'gen_m' if not provided
    if (is.null(data_list$gen_m)) {
        gen_m <- list(
            iccm = iccm,
            m_on_a = m_on_a,
            m_on_x = m_on_x,
            m_on_z = m_on_z,
            m_on_az = m_on_az,
            m_on_anj = m_on_anj
        )
    } else {
        gen_m <- data_list$gen_m
    }
    
    # Define 'gen_y' if not provided
    if (is.null(data_list$gen_y)) {
        gen_y <- list(
            iccy = iccy, 
            yintercept = yintercept, 
            y_on_a = y_on_a, 
            y_on_m = y_on_m, 
            y_on_am = y_on_am, 
            y_on_az = y_on_az, 
            y_on_mz = y_on_mz, 
            y_on_anj = y_on_anj, 
            y_on_x = y_on_x,
            y_on_z = y_on_z
        )
    } else {
        gen_y <- data_list$gen_y
    }
    
    # Prepare the data by adding 'nj' based on 'school'
    data <- data %>%
        mutate(nj = nj_sizes[school])
    
    # Define treatment combinations
    a_vals <- expand.grid(
        a0 = unique(c(0, 1)),  
        a1 = unique(c(0, 1))
    )
    # a_vals$ajo <- NA  # Not used for single mediator
    
    # Initialize lists to store true values
    truevals_individual <- list()
    truevals_cluster <- list()
    
    # Loop over each combination of treatment values
    for (j in 1:nrow(a_vals)) {
        a0_val <- a_vals$a0[j]
        a1_val <- a_vals$a1[j]
        
        # Define a label for the current treatment combination
        label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
        
        # All possible values of the mediator (binary: 0 and 1)
        mediator_vals <- expand.grid(m = c(0, 1))
        
        # Compute the expected outcome by integrating over mediator distribution
        intm <- sapply(1:nrow(mediator_vals), function(i) {
            m <- mediator_vals$m[i]
            
            # Compute the probability of mediator being m
            p_m <- pm1(
                m = m,
                a = a1_val,
                z = data$Z,
                nj = data$nj,
                given = data_list$m_given,
                gen_m = gen_m
            )
            
            # Compute the expected outcome given mediator and treatment
            expected_y <- my(
                m = m,
                a = a0_val,
                z = data$Z,
                nj = data$nj,
                given = data_list$y_given,
                gen_y = gen_y,
                binary = (Yfamily == "binomial")
            )
            
            # Return the product of mediator probability and expected outcome
            return(p_m * expected_y)
        })
        
        # Sum over all mediator values to get the expected outcome for each observation
        # Resulting in a vector of expected outcomes per observation
        int_m_sum <- rowSums(intm)
        
        # Compute the average expected outcome across all observations (individual-level)
        truevals_individual[[label]] <- mean(int_m_sum, na.rm = TRUE)
        
        # Compute the cluster-level average expected outcome
        # Aggregating by 'school' and then taking the mean across clusters
        cluster_means <- data  |> 
            mutate(int_m_sum = int_m_sum) |> 
            group_by(school) |> 
            summarize(cluster_avg = mean(int_m_sum, na.rm = TRUE))
        
        truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
    }
    
    # Return the computed true values
    return(
        list(
            truevals_individual = truevals_individual,
            truevals_cluster = truevals_cluster
        )
    )
}
