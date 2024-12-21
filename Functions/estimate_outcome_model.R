#' @title estimate_outcome_model
#'
#' @description Estimates an outcome model based on specified options and checks for convergence.
#'
#' @param Out_model_type Type of outcome model ("SL", "FE", "RE", "RE-Mean").
#' @param Sname Name of the cluster-level variable (e.g., school name).
#' @param L1_baseline_cov Vector of baseline covariates at level 1.
#' @param treatment Name of the treatment variable.
#' @param mediator Name of the mediator variable.
#' @param outcome Name of the outcome variable.
#' @param iptw_weights Inverse probability of treatment weights column name.
#' @param L2_weight Level 2 weights column name (only for RE models).
#' @param family Family of the outcome variable ("gaussian" or "binomial").
#' @param include_int description
#' @param data Dataset to be used for model estimation.
#'
#' @return A list containing:
#' \describe{
#'   \item{out}{The fitted model object.}
#'   \item{convergence}{Logical indicating whether the model converged.}
#'   \item{warnings}{List of warnings (if any) during model fitting.}
#' }
#'
#' @import dplyr
#' @importFrom stats glm binomial gaussian as.formula
#' @importFrom utils capture.output
#'
#' @examples
#' # Example usage
#' result <- estimate_outcome_model(Out_model_type = "FE", 
#'                                  Sname = "school", 
#'                                  L1_baseline_cov = c("X1", "X2"),
#'                                  treatment = "A",
#'                                  mediator = "M",
#'                                  outcome = "Y",
#'                                  iptw_weights = "iptw",
#'                                  L2_weight = "L2weight",
#'                                  family = "binomial",
#'                                  data = example_data)
#' print(result$convergence)
#'
#' @export
estimate_outcome_model <- function(Out_model_type = "FE",
                                   Sname,
                                   L1_baseline_cov,
                                   treatment,
                                   mediator,
                                   outcome,
                                   iptw_weights,
                                   L2_weight,
                                   family = "gaussian",
                                   include_int = TRUE, 
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
    if (include_int) {
        covariate_formula <- paste(c(treatment, mediator, paste0(treatment, "*", mediator), L1_baseline_cov), collapse = " + ")
    } else {
        covariate_formula <- paste(c(treatment, mediator, L1_baseline_cov), collapse = " + ")
    }
    
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
    if (Out_model_type == "SL") {
        formula <- as.formula(paste(outcome, "~", covariate_formula))
        fit_result <- with_warning_capture({
            out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
        })
        convergence <- out$converged
        
    } else if (Out_model_type == "FE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        fit_result <- with_warning_capture({
            out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
        })
        convergence <- out$converged
        
    } else if (Out_model_type == "RE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+ (1 |", Sname, ")"))
        fit_result <- with_warning_capture({
            if (family == "binomial") {
                out <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
            } else if (family == "gaussian") {
                out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
            }
        })
        convergence <- ifelse(!is.null(out$invHessian) && all(is.finite(out$invHessian)), TRUE, FALSE)
        
    } else if (Out_model_type == "RE-Mean") {
        # Calculate cluster means for treatment and mediator
        data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
        data[[paste0(mediator, "_mean")]] <- ave(data[[mediator]], data[[Sname]], FUN = mean)
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", 
                                    paste0(mediator, "_mean"), "+", paste0(treatment, "_mean"), 
                                    "+ (1 |", Sname, ")"))
        fit_result <- with_warning_capture({
            if (family == "binomial") {
                out <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
            } else if (family == "gaussian") {
                out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
            }
        })
        convergence <- ifelse(!is.null(out$invHessian) && all(is.finite(out$invHessian)), TRUE, FALSE)
        
    } else {
        stop("Invalid Out_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    }
    
    # Collect warnings and results
    out <- fit_result$result
    warnings_list <- fit_result$warnings
    
    # Return results
    return(list(
        out = out,
        convergence = convergence,
        warnings = warnings_list
    ))
    
    
    # # OLD
    # # Estimate models
    # if (Out_model_type == "SL") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula))
    #     out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    # } else if (Out_model_type == "FE") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
    #     out <- glm(formula = formula, family = model_family, data = data, weights = data[[iptw_weights]])
    # } else if (Out_model_type == "RE") {
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+ (1 |", Sname, ")"))
    #     if (family == "binomial") {
    #         out <- WeMix::mix(formula = formula, family = model_family, data = data, weights = c(iptw_weights, L2_weight))
    #     } else if (family == "gaussian") {
    #         out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    #     } 
    #     
    # } else if (Out_model_type == "RE-Mean") {
    #     # Calculate cluster means for treatment and mediator
    #     data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
    #     data[[paste0(mediator, "_mean")]] <- ave(data[[mediator]], data[[Sname]], FUN = mean)
    #     formula <- as.formula(paste(outcome, "~", covariate_formula, "+", 
    #                                 paste0(mediator, "_mean"), "+", paste0(treatment, "_mean"), 
    #                                 "+ (1 |", Sname, ")"))
    #     if (family == "binomial") {
    #         out <- WeMix::mix(formula = formula, family = family, data = data, weights = c(iptw_weights, L2_weight))
    #     } else if (family == "gaussian") {
    #         out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    #     } 
    #     
    # } else {
    #     stop("Invalid Out_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    # }
    # 
    # return(out)
}



##################################### END ######################################


# # Construct the formula based on the provided covariates
# covariate_formula <- paste(c(mediator, treatment, L1_baseline_cov), collapse = " + ")
# 
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
