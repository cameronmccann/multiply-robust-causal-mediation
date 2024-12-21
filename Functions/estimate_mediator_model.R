#' @title estimate_mediator_model 
#' 
#' @description Estimates a mediator model based on specified options and checks for convergence.
#'
#' @param Med_model_type Type of mediator model ("SL", "FE", "RE", "RE-Mean").
#' @param Sname Name of the cluster-level variable (e.g., school name).
#' @param L1_baseline_cov Vector of baseline covariates at level 1.
#' @param treatment Name of the treatment variable.
#' @param mediator Name of the mediator variable.
#' @param iptw_weights Inverse probability of treatment weights column name.
#' @param L2_weight Level 2 weights column name (only for RE models).
#' @param family Family of the outcome variable ("gaussian" or "binomial").
#' @param data Dataset to be used for model estimation.
#'
#' @return A list containing:
#' \describe{
#'   \item{med}{The fitted model object.}
#'   \item{convergence}{Logical indicating whether the model converged.}
#'   \item{warnings}{Warnings (if any) during model fitting.}
#' }
#'
#' @import dplyr
#' @import MASS
#' @importFrom stats glm binomial gaussian as.formula
#' @importFrom utils capture.output
#'
#' @examples
#' # Example usage
#' result <- estimate_mediator_model(Med_model_type = "FE", 
#'                                   Sname = "school", 
#'                                   L1_baseline_cov = c("X1", "X2"),
#'                                   treatment = "A",
#'                                   mediator = "M",
#'                                   iptw_weights = "iptw",
#'                                   L2_weight = "L2weight",
#'                                   family = "binomial",
#'                                   data = example_data)
#' print(result$convergence)
#'
#' @export
#' 
estimate_mediator_model <- function(Med_model_type = "FE",
                                    Sname,
                                    L1_baseline_cov,
                                    treatment,
                                    mediator,
                                    iptw_weights,
                                    L2_weight,
                                    family = "gaussian", 
                                    data) {
    
    # Helper function to capture warnings and results 
    with_warning_capture <- function(expr) {
        warnings <- NULL
        result <- withCallingHandlers(
            expr,
            warning = function(w) {
                warnings <- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        list(result = result, warnings = warnings)
    }
    
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
    
    # Initialize variables for tracking warnings and convergence
    convergence <- TRUE
    warnings_list <- NULL
    
    # Estimate models
    if (Med_model_type == "SL") {
        formula <- as.formula(paste(mediator, "~", covariate_formula))
        fit_result <- with_warning_capture({
            med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
        })
        convergence <- med$converged
        
    } else if (Med_model_type == "FE") {
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        fit_result <- with_warning_capture({
            med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
        })
        convergence <- med$converged
        
    } else if (Med_model_type == "RE") {
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+ (1 |", Sname, ")"))
        fit_result <- with_warning_capture({
            if (family == "binomial") {
                med <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
            } else {
                med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
            }
        })
        convergence <- ifelse(!is.null(med$invHessian) && all(is.finite(med$invHessian)), TRUE, FALSE)
        
    } else if (Med_model_type == "RE-Mean") {
        # Calculate cluster mean for treatment
        data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
        formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0(treatment, "_mean"), "+ (1 |", Sname, ")"))
        fit_result <- with_warning_capture({
            if (family == "binomial") {
                med <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
            } else {
                med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
            }
        })
        convergence <- ifelse(!is.null(med$invHessian) && all(is.finite(med$invHessian)), TRUE, FALSE)
        
    } else {
        stop("Invalid Med_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    }
    
    # Collect warnings and results
    med <- fit_result$result
    warnings_list <- fit_result$warnings
    
    # Return results
    return(list(
        med = med,
        convergence = convergence,
        warnings = warnings_list
    ))
    
}



##################################### END ######################################




# # Estimate models 
# if (Med_model_type == "SL") {
#     formula <- as.formula(paste(mediator, "~", covariate_formula))
#     med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
# } else if (Med_model_type == "FE") {
#     formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
#     med <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
# } else if (Med_model_type == "RE") {
#     formula <- as.formula(paste(mediator, "~", covariate_formula, "+ (1 |", Sname, ")"))
#     if (family == "binomial") {
#         med <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
#     } else {
#         med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
#     }
# } else if (Med_model_type == "RE-Mean") {
#     # Calculate cluster mean for treatment
#     data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
#     formula <- as.formula(paste(mediator, "~", covariate_formula, "+", paste0(treatment, "_mean"), "+ (1 |", Sname, ")"))
#     if (family == "binomial") {
#         med <- WeMix::mix(formula = formula, family = family, data = data, weights = c(iptw_weights, L2_weight))
#     } else {
#         med <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
#     }
# } else {
#     stop("Invalid Med_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
# }
# 
# return(list(med = med, data = data))

