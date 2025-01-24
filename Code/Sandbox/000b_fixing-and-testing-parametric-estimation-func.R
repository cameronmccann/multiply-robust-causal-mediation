################################################################################
################ Fixing & Testing Parametric Estimation Functions  ############
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 12/14/2024
#
#
# Script Description: this is used to fix, test, and properly build the funcitons relevant to parameteric estimation  
#
#
# Last Updated: 12/17/2024
#
#
# Notes:
#   To-Do: 
#       Update estimation functions to compute different estimates (i.e., TNDE & PNIE in additon to PNDE & TNIE) (Double check whether model should always have TM interaction or to drop it)
#       Need to update functions for mediator and outcome estimation with those in this script 
# 
#       Update analyze_clustered_mediation() to properly handle the update med & out functions and output 
#       Update estimation functions to provide both version of effects cluster-average and individual-average effects 
# 
# 
#   Done: 
#       + Update estimation functions to provide info on model convergence (Need to add to outcome function; already in mediator)
#       + It looks like the estimation funcitons support binary variables 
#       + Apparently the following warnings can be ignored. 
#       Getting a warning (see below) in mediator function for SL & FE models with binomial family 
#           Warning messages:
#               1: In eval(family$initialize) : non-integer #successes in a binomial glm!
#               2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#       
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
    purrr, # for map() & safely() to catch errors
    glue, # for glue()
    dplyr, 
    readr, 
    ggplot2
)


# ══════════════════════════════
#     Load Functions
# ══════════════════════════════

# Define the vector of function names
function_names <- c(
    # data generation 
    "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    "trueVals", 
    
    # estimation 
    "estimate_propensity_score",
    "estimate_mediator_model",
    "estimate_outcome_model",
    "analyze_clustered_mediation"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}




# Updating mediator function to support binary mediator -------------------

# ══════════════════════════════
#    Generate Data 
# ══════════════════════════════

set.seed(8675309)
data_list <- generate_data(
    # J = test_condition[["J"]], 
    # njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
    Mfamily = "binomial",
    Yfamily = "binomial", 
    seed = 8769,
    
    # num_x = 3,
    # m_on_a = 15,
    # m_on_anj = 0.5,
    # m_on_az = 0.2,
    # y_on_a = 2,
    # y_on_m = 15,
    # y_on_am = 5,
    # y_on_az = 0.2,
    # y_on_mz = 0.2,
    # y_on_anj = 5,
    # int.XZ = FALSE
    
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


# Estimate propensity score model
PS_model_type = "FE"
Sname = "school"
L1_baseline_cov = c("X1", "X2", "X3")
treatment = "A"
data = data_list$data


iptw_weights = "iptw"
L2_weight = "L2weight"

ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
data <- ps_result$data

# Ensure L2_weight exists
if (!(L2_weight %in% names(data))) {
    data[[L2_weight]] <- 1
}




# ══════════════════════════════
#    Updating function 
# ══════════════════════════════

# Helper function to estimate mediation model
estimate_mediator_model <- function(Med_model_type = "FE",
                                    Sname,
                                    L1_baseline_cov,
                                    treatment,
                                    mediator,
                                    iptw_weights,
                                    L2_weight,
                                    family = "gaussian", 
                                    data) {
    
    # Construct the formula based on the provided covariates
    covariate_formula <- paste(c(treatment, L1_baseline_cov), collapse = " + ")
    
    # Set the family based on the input
    if (family == "binomial") {
        model_family <- binomial()
    } else if (family == "gaussian") {
        model_family <- gaussian()
    } else {
        stop("Invalid family. Please choose 'binomial' or 'gaussian'.")
    }
    
    
    # if (Med_model_type == "SL") {
    #     formula <- as.formula(paste(mediator, "~", covariate_formula))
    #     med <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    # } else if (Med_model_type == "FE") {
    #     formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
    #     med <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    # } else if (Med_model_type == "RE") {
    #     formula <- as.formula(paste(mediator, "~", covariate_formula, "+ (1 |", Sname, ")"))
    #     med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    # } else if (Med_model_type == "RE-Mean") {
    #     # Calculate cluster mean for treatment
    #     data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
    #     formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0(treatment, "_mean"), "+ (1 |", Sname, ")"))
    #     med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    # } else {
    #     stop("Invalid Med_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    # }
    # 
    # return(list(med = med, data = data))
    if (Med_model_type == "SL") {
        formula <- as.formula(paste(mediator, "~", covariate_formula))
        med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    } else if (Med_model_type == "FE") {
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    } else if (Med_model_type == "RE") {
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+ (1 |", Sname, ")"))
        if (family == "binomial") {
            med <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
        } else {
            med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
        }
    } else if (Med_model_type == "RE-Mean") {
        # Calculate cluster mean for treatment
        data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0(treatment, "_mean"), "+ (1 |", Sname, ")"))
        if (family == "binomial") {
            med <- WeMix::mix(formula = formula, family = family, data = data, weights = c(iptw_weights, L2_weight))
        } else {
            med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
        }
    } else {
        stop("Invalid Med_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    }
    
    return(list(med = med, data = data))
}


# ══════════════════════════════
#    testing function  
# ══════════════════════════════


# med <- WeMix::mix(formula = M ~ X1 + X2 + X3 + A + (1 | school), family = "binomial", data = data, weights = c(iptw_weights, L2_weight))

# data$M <- as.double(data$M)
# data$Y <- as.double(data$Y)
# data <- as.data.frame(data)

t <- estimate_mediator_model(
    Med_model_type = "RE", # "RE-Mean",
    Sname = "school",
    L1_baseline_cov = paste0("X", 1:3),
    treatment = "A",
    mediator = "M",
    iptw_weights = "iptw",
    L2_weight = "L2weight",
    family = "binomial", # "gaussian", # 
    data = data
)

t$convergence
t$warnings
# t$data
t$med

# NOTE: when runnning SL or FE with binary mediator & outcome I get the following warnings. Apparently I can ignore these. 
#   The first (SL) warning occurs because glm() is picky---running ifelse(data$M > 0.5, 1, 0) makes this message dissappear or using family = "quasibinomial"

# warning with binomial SL, FE
# # SL : 
# Warning message:
# In eval(family$initialize) : non-integer #successes in a binomial glm!

# # FE : 
# Warning messages:
# 1: In eval(family$initialize) : non-integer #successes in a binomial glm!
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred


# 
# 
# 
# # data$M <- as.factor(data$M)
# f <- as.formula("M ~ X1 + X2 + X3 + A + as.factor(school)")
# # as.formula(paste("M", "~", paste0("X", 1:3), "+", "as.factor(school)"))
# 
# glm(formula = f, family = "binomial", data = data, weights = data$iptw)
# 
# 



# Set up values for looping
Mfamily_values <- c("binomial", "gaussian")
Yfamily_values <- c("binomial", "gaussian")
Med_model_type_values <- c("SL", "FE", "RE", "RE-Mean")

# Placeholder to store results
results <- data.frame(
    Mfamily = character(),
    Yfamily = character(),
    Med_model_type = character(),
    Warning = logical(),
    Error = logical()
)

# Loop through combinations
set.seed(8675309) # Reproducibility

for (Mfamily in Mfamily_values) {
for (Yfamily in Yfamily_values) {
    # Generate data
    data_list <- generate_data(
        Mfamily = Mfamily,
        Yfamily = Yfamily,
        seed = 8769,
        # num_x = 3,
        # m_on_a = 15,
        # m_on_anj = 0.5,
        # m_on_az = 0.2,
        # y_on_a = 2,
        # y_on_m = 15,
        # y_on_am = 5,
        # y_on_az = 0.2,
        # y_on_mz = 0.2,
        # y_on_anj = 5,
        # int.XZ = FALSE
        
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
    
    # Estimate propensity score model
    PS_model_type <- "FE"
    Sname <- "school"
    L1_baseline_cov <- c("X1", "X2", "X3")
    treatment <- "A"
    data <- data_list$data
    
    ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
    data <- ps_result$data
    
    # Ensure L2_weight exists
    L2_weight <- "L2weight"
    iptw_weights <- "iptw"
    if (!(L2_weight %in% names(data))) {
        data[[L2_weight]] <- 1
    }
    
    for (Med_model_type in Med_model_type_values) {
        # Safely run the outcome model estimation
        result <- safely(function() {
            estimate_mediator_model(
                Med_model_type = Med_model_type, 
                Sname = Sname,
                L1_baseline_cov = paste0("X", 1:3),
                treatment = treatment,
                mediator = "M",
                iptw_weights = iptw_weights,
                L2_weight = L2_weight,
                family = Mfamily, 
                data = data
            )
        })
        
        # Execute the function and store output
        mediator_result <- result()
        
        # Append to results
        results <- rbind(
            results,
            data.frame(
                Mfamily = Mfamily,
                Yfamily = Yfamily,
                Med_model_type = Med_model_type,
                Warning = !is.null(mediator_result$warnings),
                Error = !is.null(mediator_result$error)
            )
        )
    }
}
}

# View results
print(results)
# Mfamily  Yfamily Med_model_type Warning Error
# 1  binomial binomial             SL   FALSE FALSE
# 2  binomial binomial             FE   FALSE FALSE
# 3  binomial binomial             RE   FALSE FALSE
# 4  binomial binomial        RE-Mean   FALSE FALSE
# 5  binomial gaussian             SL   FALSE FALSE
# 6  binomial gaussian             FE   FALSE FALSE
# 7  binomial gaussian             RE   FALSE FALSE
# 8  binomial gaussian        RE-Mean   FALSE FALSE
# 9  gaussian binomial             SL   FALSE FALSE
# 10 gaussian binomial             FE   FALSE FALSE
# 11 gaussian binomial             RE   FALSE FALSE
# 12 gaussian binomial        RE-Mean   FALSE FALSE
# 13 gaussian gaussian             SL   FALSE FALSE
# 14 gaussian gaussian             FE   FALSE FALSE
# 15 gaussian gaussian             RE   FALSE FALSE
# 16 gaussian gaussian        RE-Mean   FALSE FALSE






# Updating outcome function to support binary outcome --------------------


# ══════════════════════════════
#    Updating function 
# ══════════════════════════════

# Helper function to estimate outcome model
estimate_outcome_model <- function(Out_model_type = "FE",
                                   Sname,
                                   L1_baseline_cov,
                                   treatment,
                                   mediator,
                                   outcome,
                                   iptw_weights,
                                   L2_weight,
                                   family = "gaussian",
                                   data) {
    
    # Construct the formula based on the provided covariates
    covariate_formula <- paste(c(mediator, treatment, L1_baseline_cov), collapse = " + ")
    
    # Set the family based on the input
    if (family == "binomial") {
        model_family <- binomial()
    } else if (family == "gaussian") {
        model_family <- gaussian()
    } else {
        stop("Invalid family. Please choose 'binomial' or 'gaussian'.")
    }
    
    # new 
    if (Out_model_type == "SL") {
        formula <- as.formula(paste(outcome, "~", covariate_formula))
        out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    } else if (Out_model_type == "FE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    } else if (Out_model_type == "RE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+ (1 |", Sname, ")"))
        if (family == "binomial") {
            out <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
        } else if (family == "gaussian") {
            out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
        } 
        
    } else if (Out_model_type == "RE-Mean") {
        # Calculate cluster means for treatment and mediator
        data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
        data[[paste0(mediator, "_mean")]] <- ave(data[[mediator]], data[[Sname]], FUN = mean)
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", 
                                    paste0(mediator, "_mean"), "+", paste0(treatment, "_mean"), 
                                    "+ (1 |", Sname, ")"))
        if (family == "binomial") {
            out <- WeMix::mix(formula = formula, family = family, data = data, weights = c(iptw_weights, L2_weight))
        } else if (family == "gaussian") {
            out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
        } 
        
    } else {
        stop("Invalid Out_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    }

    # # old 
    # if (Out_model_type == "SL") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula))
    #     out <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    # } else if (Out_model_type == "FE") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
    #     out <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    # } else if (Out_model_type == "RE") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+ (1 |", Sname, ")"))
    #     out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    # } else if (Out_model_type == "RE-Mean") {
    #     # Calculate cluster means for treatment and mediator
    #     data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
    #     data[[paste0(mediator, "_mean")]] <- ave(data[[mediator]], data[[Sname]], FUN = mean)
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+", 
    #                                 paste0(mediator, "_mean"), "+", paste0(treatment, "_mean"), 
    #                                 "+ (1 |", Sname, ")"))
    #     out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    # } else {
    #     stop("Invalid Out_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    # }
    
    return(out)
}



# ══════════════════════════════
#    testing function  
# ══════════════════════════════

outcome <- estimate_outcome_model(Out_model_type = "RE", 
                       Sname = "school", 
                       L1_baseline_cov = paste0("X", 1:3),
                       treatment = "A",
                       mediator = "M",
                       iptw_weights = "iptw",
                       L2_weight = "L2weight",
                       outcome = "Y", 
                       family = "gaussian", # "binomial", # 
                       data = data)


summary(outcome$out)

outcome$out
outcome$convergence
outcome$warnings



# # Estimate mediation model
# med_results <- estimate_mediator_model(Med_model_type, Sname, L1_baseline_cov, treatment, mediator, iptw_weights, L2_weight, data)
# med <- med_results$med
# data <- med_results$data
# 
# # Estimate outcome model
# out <- estimate_outcome_model(Out_model_type, Sname, L1_baseline_cov, treatment, mediator, outcome, iptw_weights, L2_weight, data)




# TESTING with loop 

# Set up values for looping
Mfamily_values <- c("binomial", "gaussian")
Yfamily_values <- c("binomial", "gaussian")
Out_model_type_values <- c("SL", "FE", "RE", "RE-Mean")

# Placeholder to store results
results <- data.frame(
    Mfamily = character(),
    Yfamily = character(),
    Out_model_type = character(),
    Warning = logical(),
    Error = logical()
)

# Loop through combinations
set.seed(8675309) # Reproducibility

for (Mfamily in Mfamily_values) {
    for (Yfamily in Yfamily_values) {
        # Generate data
        data_list <- generate_data(
            Mfamily = Mfamily,
            Yfamily = Yfamily,
            seed = 8769,
            # num_x = 3,
            # m_on_a = 15,
            # m_on_anj = 0.5,
            # m_on_az = 0.2,
            # y_on_a = 2,
            # y_on_m = 15,
            # y_on_am = 5,
            # y_on_az = 0.2,
            # y_on_mz = 0.2,
            # y_on_anj = 5,
            # int.XZ = FALSE
            
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
        
        # Estimate propensity score model
        PS_model_type <- "FE"
        Sname <- "school"
        L1_baseline_cov <- c("X1", "X2", "X3")
        treatment <- "A"
        data <- data_list$data
        
        ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
        data <- ps_result$data
        
        # Ensure L2_weight exists
        L2_weight <- "L2weight"
        iptw_weights <- "iptw"
        if (!(L2_weight %in% names(data))) {
            data[[L2_weight]] <- 1
        }
        
        for (Out_model_type in Out_model_type_values) {
            # Safely run the outcome model estimation
            result <- safely(function() {
                estimate_outcome_model(
                    Out_model_type = Out_model_type,
                    Sname = Sname,
                    L1_baseline_cov = paste0("X", 1:3),
                    treatment = treatment,
                    mediator = "M",
                    iptw_weights = iptw_weights,
                    L2_weight = L2_weight,
                    outcome = "Y",
                    family = Yfamily, # "gaussian", # Looping over families could be added here
                    data = data
                )
            })
            
            # Execute the function and store output
            outcome_result <- result()
            
            # Append to results
            results <- rbind(
                results,
                data.frame(
                    Mfamily = Mfamily,
                    Yfamily = Yfamily,
                    Out_model_type = Out_model_type,
                    Warning = !is.null(outcome_result$warnings),
                    Error = !is.null(outcome_result$error)
                )
            )
        }
    }
}

# View results
print(results)
# Mfamily  Yfamily Out_model_type Warning Error
# 1  binomial binomial             SL   FALSE FALSE
# 2  binomial binomial             FE   FALSE FALSE
# 3  binomial binomial             RE   FALSE FALSE
# 4  binomial binomial        RE-Mean   FALSE FALSE
# 5  binomial gaussian             SL   FALSE FALSE
# 6  binomial gaussian             FE   FALSE FALSE
# 7  binomial gaussian             RE   FALSE FALSE
# 8  binomial gaussian        RE-Mean   FALSE FALSE
# 9  gaussian binomial             SL   FALSE FALSE
# 10 gaussian binomial             FE   FALSE FALSE
# 11 gaussian binomial             RE   FALSE FALSE
# 12 gaussian binomial        RE-Mean   FALSE FALSE
# 13 gaussian gaussian             SL   FALSE FALSE
# 14 gaussian gaussian             FE   FALSE FALSE
# 15 gaussian gaussian             RE   FALSE FALSE
# 16 gaussian gaussian        RE-Mean   FALSE FALSE






# Adding different effect estimates (TNDE, PNIE, etc) ---------------------

# ══════════════════════════════
#    Modify function output 
# ══════════════════════════════

# Generate Data & set parameters 
set.seed(8675309)
data_list <- generate_data(
    # J = test_condition[["J"]], 
    # njrange = c(test_condition[["Nj_low"]], test_condition[["Nj_high"]]), 
    Mfamily = "binomial",
    Yfamily = "binomial", 
    seed = 8769,
    
    # num_x = 3,
    # m_on_a = 15,
    # m_on_anj = 0.5,
    # m_on_az = 0.2,
    # y_on_a = 2,
    # y_on_m = 15,
    # y_on_am = 5,
    # y_on_az = 0.2,
    # y_on_mz = 0.2,
    # y_on_anj = 5,
    # int.XZ = FALSE
    
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


# Estimate propensity score model
PS_model_type = "FE"
Sname = "school"
L1_baseline_cov = c("X1", "X2", "X3")
treatment = "A"
data = data_list$data


iptw_weights = "iptw"
L2_weight = "L2weight"

ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
data <- ps_result$data

# Ensure L2_weight exists
if (!(L2_weight %in% names(data))) {
    data[[L2_weight]] <- 1
}

# Estimate mediator & outcome to tweak main analysis function output 
med_result <- estimate_mediator_model(Med_model_type = "FE", 
                                      Sname = "school", 
                                      L1_baseline_cov = paste0("X", 1:3),
                                      treatment = "A",
                                      mediator = "M",
                                      iptw_weights = "iptw",
                                      L2_weight = "L2weight",
                                      family = "binomial", # "gaussian", # 
                                      data = data)

out_result <- estimate_outcome_model(Out_model_type = "FE", 
                                     Sname = "school", 
                                     L1_baseline_cov = paste0("X", 1:3),
                                     treatment = "A",
                                     mediator = "M",
                                     iptw_weights = "iptw",
                                     L2_weight = "L2weight",
                                     outcome = "Y", 
                                     family = "binomial", # "gaussian", # 
                                     include_int = TRUE, 
                                     data = data)

# # Individual-average
# ## PNDE
# summary(out_result$out)$coef["A", "Estimate"] # with no interaction in formula
# summary(out_result$out)$coef["A", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"] * summary(med_result$med)$coef["(Intercept)", "Estimate"]
# ## TNIE
# summary(med_result$med)$coef["A", "Estimate"] * summary(out_result$out)$coef["M", "Estimate"] # with no interaction in formula
# summary(med_result$med)$coef["A", "Estimate"] * (summary(out_result$out)$coef["M", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"])
# ## TNDE
# summary(out_result$out)$coef["A", "Estimate"] # with no interaction in formula
# summary(out_result$out)$coef["A", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"] * (summary(med_result$med)$coef["(Intercept)", "Estimate"] + summary(med_result$med)$coef["A", "Estimate"])
# ## PNIE
# summary(med_result$med)$coef["A", "Estimate"] * summary(out_result$out)$coef["M", "Estimate"] # with no interaction in formula
# summary(med_result$med)$coef["A", "Estimate"] * summary(out_result$out)$coef["M", "Estimate"]
# 
# # summary(med_result$med)$coef[1:8, ]
# # summary(out_result$out)$coef[c(1:8, nrow(summary(out_result$out)$coef)), ]




# Compute individual-average effects 
individual_effects <- data.frame(
    PS_model = "FE", # PS_model_type,
    Mediator_model = "FE", # Med_model_type,
    Outcome_model = "FE", # Out_model_type,
    Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
    Estimate = c(
        # PNDE
        summary(out_result$out)$coef["A", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"] * summary(med_result$med)$coef["(Intercept)", "Estimate"], 
        # TNIE
        summary(med_result$med)$coef["A", "Estimate"] * (summary(out_result$out)$coef["M", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"]), 
        # TNDE
        summary(out_result$out)$coef["A", "Estimate"] + summary(out_result$out)$coef["A:M", "Estimate"] * (summary(med_result$med)$coef["(Intercept)", "Estimate"] + summary(med_result$med)$coef["A", "Estimate"]), 
        # PNIE
        summary(med_result$med)$coef["A", "Estimate"] * summary(out_result$out)$coef["M", "Estimate"] 
        # a_effect_est + ab_interaction_est * a_intercept,                        # PNDE
        # a_est * (b_est + ab_interaction_est),                                   # TNIE
        # a_effect_est + ab_interaction_est * (a_intercept + a_est),              # TNDE
        # a_est * b_est                                                             # PNIE
    ),
    Std_Error = c(
        NA,  # PNDE
        NA,  # TNIE
        NA,  # TNDE
        NA   # PNIE
    ),
    stringsAsFactors = FALSE
)

# Placeholder for Cluster-Average (In)Direct Effects
# These can be computed and filled in once the methodology is established
cluster_effects <- data.frame(
    PS_model = "FE", # PS_model_type,
    Mediator_model = "FE", # Med_model_type,
    Outcome_model = "FE", # Out_model_type,
    Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
    Estimate = c(
        NA,  # PNDE Cluster-Average
        NA,  # TNIE Cluster-Average # <-- NEED TO COMPUTE SE FOR INDIRECT EFFECT
        NA,  # TNDE Cluster-Average
        NA   # PNIE Cluster-Average
    ),
    Std_Error = c(
        NA,  # PNDE Cluster-Average SE
        NA,  # TNIE Cluster-Average SE
        NA,  # TNDE Cluster-Average SE
        NA   # PNIE Cluster-Average SE
    ),
    stringsAsFactors = FALSE
)


# Optional: Include path estimates and their standard errors
## Extract path est & SE
a_est <- summary(med_result$me)$coef["A", "Estimate"]
a_se <- summary(med_result$med)$coef["A", "Std. Error"]
b_est <- summary(out_result$out)$coef["M", "Estimate"]
b_se <- summary(out_result$out)$coef["M", "Std. Error"]
ab_interaction_est <- summary(out_result$out)$coef[paste0("A", ":", "M"), "Estimate"]
ab_interaction_se <- summary(out_result$out)$coef[paste0("A", ":", "M"), "Std. Error"]
## Store path est & SE 
path_estimates <- data.frame(
    Path = c("a (Treatment -> Mediator)", "b (Mediator -> Outcome)", "a:b Interaction"),
    Estimate = c(a_est, b_est, ab_interaction_est),
    Std_Error = c(a_se, b_se, ab_interaction_se)
)


# Combine results into a list
results <- list(
    Individual_Average_Effects = individual_effects,
    Cluster_Average_Effects = cluster_effects,
    Path_Estimates = path_estimates,
    Model_Convergence = list(
        Propensity_Score_Model = TRUE, # ps_result$convergence,
        Mediator_Model = med_result$convergence,
        Outcome_Model = out_result$convergence
    ),
    Warnings = list(
        Propensity_Score_Model = FALSE, #ps_result$warnings,
        Mediator_Model = med_result$warnings,
        Outcome_Model = out_result$warnings
    )
)


# output 
results$Individual_Average_Effects
results$Cluster_Average_Effects
results$Path_Estimates
results$Model_Convergence
results$Warnings




# ══════════════════════════════
#    Updating main analysis function  
# ══════════════════════════════

# Main analysis function
analyze_clustered_mediation <- function(PS_model_type = "FE", 
                                        Med_model_type = "FE", 
                                        Out_model_type = "FE", 
                                        data, 
                                        # condition, 
                                        # condition_num,
                                        Sname, # Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
                                        L1_baseline_cov,
                                        treatment,
                                        mediator,
                                        outcome,
                                        Mfamily = "binomial", 
                                        Yfamily = "binomial", 
                                        iptw_weights = "iptw",
                                        L2_weight = "L2weight", 
                                        include_int = TRUE) {
    
    # Estimate propensity score model
    ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
    data <- ps_result$data # add ps info to data 
    
    # Ensure L2_weight exists
    if (!(L2_weight %in% names(data))) {
        data[[L2_weight]] <- 1
    }
    
    # Estimate mediation model
    med_result <- estimate_mediator_model(Med_model_type, Sname, family = Mfamily, L1_baseline_cov, treatment, mediator, iptw_weights, L2_weight, data)
    med <- med_result$med
    # data <- med_results$data
    
    # Estimate outcome model
    out_result <- estimate_outcome_model(Out_model_type, Sname, family = Yfamily, L1_baseline_cov, treatment, mediator, outcome, iptw_weights, L2_weight, include_int, data)
    out <- out_result$out
    
    # # Store results
    # results <- data.frame(
    #     PS_model = paste0(PS_model_type),
    #     Mediator_model = Med_model_type,
    #     Outcome_model = Out_model_type,
    #     Effect = c("Direct Effect", "Indirect Effect"),
    #     Individual.Average_Estimate = c(
    #         summary(out)$coef[treatment, "Estimate"],
    #         summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
    #     ),
    #     Individual.Average_StdError = c(summary(out)$coef[treatment, "Std. Error"], NA) # <-- NEED TO COMPUTE SE FOR INDIRECT EFFECT
    #     
    #     
    # )
    
    
    # Compute individual-average effects without interaction (if include_int = FALSE)
    if (!include_int) {
        # PNDE
        PNDE <- summary(out)$coef[treatment, "Estimate"] 
        # TNIE
        TNIE <- summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
        # TNDE
        TNDE <- summary(out)$coef[treatment, "Estimate"]
        # PNIE
        PNIE <- summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
    } else {
        # Compute effects with interaction 
        PNDE <- summary(out)$coef[treatment, "Estimate"] + 
            summary(out)$coef[paste0(treatment, ":", mediator), "Estimate"] * summary(med)$coef["(Intercept)", "Estimate"]
        
        TNIE <- summary(med)$coef[treatment, "Estimate"] * 
            (summary(out)$coef[mediator, "Estimate"] + summary(out)$coef[paste0(treatment, ":", mediator), "Estimate"])
        
        TNDE <- summary(out)$coef[treatment, "Estimate"] + 
            summary(out)$coef[paste0(treatment, ":", mediator), "Estimate"] * 
            (summary(med)$coef["(Intercept)", "Estimate"] + summary(med)$coef[treatment, "Estimate"])
        
        PNIE <- summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
    }
    
    # Store results in a data frame
    individual_effects <- data.frame(
        PS_model = PS_model_type,
        Mediator_model = Med_model_type,
        Outcome_model = Out_model_type,
        Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
        Estimate = c(PNDE, TNIE, TNDE, PNIE),
        Std_Error = c(
            NA,  # Placeholder for PNDE SE
            NA,  # Placeholder for TNIE SE
            NA,  # Placeholder for TNDE SE
            NA   # Placeholder for PNIE SE
        ),
        stringsAsFactors = FALSE
    )
    
    # # Compute individual-average effects 
    # individual_effects <- data.frame(
    #     PS_model = PS_model_type,
    #     Mediator_model = Med_model_type,
    #     Outcome_model = Out_model_type,
    #     Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
    #     Estimate = c(
    #         # PNDE
    #         summary(out)$coef["A", "Estimate"] + summary(out)$coef["A:M", "Estimate"] * summary(med)$coef["(Intercept)", "Estimate"], 
    #         # TNIE
    #         summary(med)$coef["A", "Estimate"] * (summary(out)$coef["M", "Estimate"] + summary(out)$coef["A:M", "Estimate"]), 
    #         # TNDE
    #         summary(out)$coef["A", "Estimate"] + summary(out)$coef["A:M", "Estimate"] * (summary(med)$coef["(Intercept)", "Estimate"] + summary(med)$coef["A", "Estimate"]), 
    #         # PNIE
    #         summary(med)$coef["A", "Estimate"] * summary(out)$coef["M", "Estimate"] 
    #         # a_effect_est + ab_interaction_est * a_intercept,                        # PNDE
    #         # a_est * (b_est + ab_interaction_est),                                   # TNIE
    #         # a_effect_est + ab_interaction_est * (a_intercept + a_est),              # TNDE
    #         # a_est * b_est                                                             # PNIE
    #     ),
    #     Std_Error = c(
    #         NA,  # PNDE
    #         NA,  # TNIE
    #         NA,  # TNDE
    #         NA   # PNIE
    #     ),
    #     stringsAsFactors = FALSE
    # )
    
    # Placeholder for Cluster-Average (In)Direct Effects
    # These can be computed and filled in once the methodology is established
    cluster_effects <- data.frame(
        PS_model = "FE", # PS_model_type,
        Mediator_model = "FE", # Med_model_type,
        Outcome_model = "FE", # Out_model_type,
        Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
        Estimate = c(
            NA,  # PNDE Cluster-Average
            NA,  # TNIE Cluster-Average # <-- NEED TO COMPUTE SE FOR INDIRECT EFFECT
            NA,  # TNDE Cluster-Average
            NA   # PNIE Cluster-Average
        ),
        Std_Error = c(
            NA,  # PNDE Cluster-Average SE
            NA,  # TNIE Cluster-Average SE
            NA,  # TNDE Cluster-Average SE
            NA   # PNIE Cluster-Average SE
        ),
        stringsAsFactors = FALSE
    )
    
    
    # Optional: Include path estimates and their standard errors
    ## Extract path est & SE
    a_est <- summary(med)$coef[treatment, "Estimate"]
    a_se <- summary(med)$coef[treatment, "Std. Error"]
    b_est <- summary(out)$coef[mediator, "Estimate"]
    b_se <- summary(out)$coef[mediator, "Std. Error"]
    ab_interaction_est <- summary(out)$coef[paste0(treatment, ":", mediator), "Estimate"]
    ab_interaction_se <- summary(out)$coef[paste0(treatment, ":", mediator), "Std. Error"]
    ## Store path est & SE 
    path_estimates <- data.frame(
        Path = c("a (Treatment -> Mediator)", "b (Mediator -> Outcome)", "a:b Interaction"),
        Estimate = c(a_est, b_est, ab_interaction_est),
        Std_Error = c(a_se, b_se, ab_interaction_se)
    )
    
    
    # Combine results into a list
    results <- list(
        Individual_Average_Effects = individual_effects,
        Cluster_Average_Effects = cluster_effects,
        Path_Estimates = path_estimates,
        Model_Convergence = list(
            Propensity_Score_Model = ps_result$convergence,
            Mediator_Model = med_result$convergence,
            Outcome_Model = out_result$convergence
        ),
        Warnings = list(
            Propensity_Score_Model = ps_result$warnings,
            Mediator_Model = med_result$warnings,
            Outcome_Model = out_result$warnings
        )
    )
    
    
    # results <- c(
    #     analysisCond = paste0(PS_model_type, "_", Out_model_type),
    #     PS = PS_model_type,
    #     outModel = Out_model_type,
    #     NDE_est = summary(out)$coef[treatment, "Estimate"],
    #     NIE_est = summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"],
    #     # ICC = condition[condition_num, "icc"],
    #     # clust_size = condition[condition_num, "clust_size"],
    #     # conditionNum = condition_num,
    #     a_path_est = summary(med)$coef[treatment, "Estimate"],
    #     a_path_se = summary(med)$coef[treatment, "Std. Error"],
    #     b_path_est = summary(out)$coef[mediator, "Estimate"],
    #     b_path_se = summary(out)$coef[mediator, "Std. Error"],
    #     direct_est = summary(out)$coef[treatment, "Estimate"],
    #     direct_se = summary(out)$coef[treatment, "Std. Error"]
    # )
    
    # # Calculate standard errors
    # direct_se <- summary(out)$coef[treatment, "Std. Error"]
    # indirect_se <- sqrt((summary(med)$coef[treatment, "Estimate"]^2 * summary(out)$coef[mediator, "Std. Error"]^2) +
    #                         (summary(out)$coef[mediator, "Estimate"]^2 * summary(med)$coef[treatment, "Std. Error"]^2))
    # 
    # # Create results data frame
    # results <- data.frame(
    #     Effect = c("Direct Effect", "Indirect Effect"),
    #     Individual.Average_Estimate = c(
    #         summary(out)$coef[treatment, "Estimate"],
    #         summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
    #     ),
    #     Individual.Average_StdError = c(direct_se, indirect_se)
    # )
    # 
    # # Add additional information as attributes
    # attr(results, "analysisCond") <- paste0(PS_model_type, "_", Out_model_type)
    # attr(results, "PS") <- PS_model_type
    # attr(results, "outModel") <- Out_model_type
    # attr(results, "a_path_est") <- summary(med)$coef[treatment, "Estimate"]
    # attr(results, "a_path_se") <- summary(med)$coef[treatment, "Std. Error"]
    # attr(results, "b_path_est") <- summary(out)$coef[mediator, "Estimate"]
    # attr(results, "b_path_se") <- summary(out)$coef[mediator, "Std. Error"]
    
    
    return(results)
}






# ══════════════════════════════
#    Testing main analysis function  
# ══════════════════════════════

# Estimate mediator & outcome to tweak main analysis function output 
med_result <- estimate_mediator_model(Med_model_type = "FE", 
                                      Sname = "school", 
                                      L1_baseline_cov = paste0("X", 1:3),
                                      treatment = "A",
                                      mediator = "M",
                                      iptw_weights = "iptw",
                                      L2_weight = "L2weight",
                                      family = "binomial", # "gaussian", # 
                                      data = data)

out_result <- estimate_outcome_model(Out_model_type = "FE", 
                                     Sname = "school", 
                                     L1_baseline_cov = paste0("X", 1:3),
                                     treatment = "A",
                                     mediator = "M",
                                     iptw_weights = "iptw",
                                     L2_weight = "L2weight",
                                     outcome = "Y", 
                                     family = "binomial", # "gaussian", # 
                                     include_int = TRUE, 
                                     data = data)

ttt <- analyze_clustered_mediation(PS_model_type = "FE", 
                            Med_model_type = "FE", 
                            Out_model_type = "FE", 
                            data = data, 
                            Sname = "school", 
                            L1_baseline_cov = paste0("X", 1:3), 
                            treatment = "A", 
                            mediator = "M", 
                            outcome = "Y", 
                            Mfamily = "binomial", 
                            Yfamily = "binomial", 
                            iptw_weights = "iptw", 
                            L2_weight = "L2weight", 
                            include_int = TRUE
                            )


ttt$Individual_Average_Effects
ttt$Cluster_Average_Effects
ttt$Model_Convergence
ttt$Warnings

out_result$convergence
ttt$Model_Convergence$Outcome_Model
out_result$warnings



