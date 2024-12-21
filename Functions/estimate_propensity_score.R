#---------------------------------------------------#
# [UPDATED FUNCTION]
# 
# QP Project 
# Data Generation for Simulation 1 
# 
# ---------------------------------------------------#

#' @title Estimate Propensity Score Model
#'
#' @description 
#' `estimate_propensity_score()` estimates a propensity score model based on specified options and 
#' checks for convergence. The function supports different model types, including simple logistic 
#' regression (SL), fixed effects (FE), and random effects (RE) models, to account for clustered 
#' data structures.
#'
#' @param PS_model_type Type of propensity score model. Options include:
#'   \describe{
#'     \item{"SL"}{Simple logistic regression without cluster terms.}
#'     \item{"FE"}{Fixed effects model including cluster-level dummy variables.}
#'     \item{"RE"}{Random effects model with random intercepts for clusters.}
#'   }
#'   Default is `"FE"`.
#' @param Sname A character string specifying the name of the column in `data` that indicates 
#' cluster membership (e.g., `"school"`).
#' @param L1_baseline_cov A character vector of baseline covariate names at level 1 
#' (e.g., `c("X1", "X2")`).
#' @param treatment A character string specifying the name of the treatment variable 
#' (e.g., `"A"`).
#' @param data A dataframe containing the dataset to be used for propensity score estimation. 
#' The dataframe must include the specified `Sname`, `L1_baseline_cov`, and `treatment` variables.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{The input dataframe augmented with:
#'     \describe{
#'       \item{ps}{Predicted propensity scores from the fitted model.}
#'       \item{ps_logit}{Logit-transformed propensity scores (linear predictors).}
#'       \item{iptw}{Inverse probability of treatment weights calculated as \code{(A / ps) + ((1 - A) / (1 - ps))}.}
#'     }
#'   }
#'   \item{psmod}{The fitted propensity score model object.}
#'   \item{convergence}{Logical value indicating whether the propensity score model converged 
#' successfully (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{warnings}{A character vector of warning messages (if any) that occurred during 
#' model fitting. If no warnings were generated, this will be \code{NULL}.}
#' }
#'
#' @details 
#' The function estimates the propensity score using the specified model type:
#' \itemize{
#'   \item \strong{SL (Simple Logistic):} Fits a standard logistic regression model without 
#' cluster-level terms.
#'   \item \strong{FE (Fixed Effects):} Fits a logistic regression model including cluster-level 
#' dummy variables to account for fixed effects.
#'   \item \strong{RE (Random Effects):} Fits a logistic mixed-effects model with random intercepts 
#' for clusters using the \code{glmer} function from the \pkg{lme4} package.
#' }
#'
#' The function also calculates inverse probability of treatment weights (IPTW) based on the 
#' estimated propensity scores.
#'
#' @import dplyr
#' @importFrom stats glm binomial predict as.formula
#' @importFrom lme4 glmer
#'
#' @examples
#' # Example usage
#' library(dplyr)
#' library(lme4)
#' 
#' # Simulate example data
#' set.seed(123)
#' example_data <- data.frame(
#'     school = rep(1:10, each = 30),
#'     X1 = rnorm(300),
#'     X2 = rbinom(300, 1, 0.5),
#'     A = rbinom(300, 1, 0.4)
#' )
#' 
#' # Estimate propensity score using Fixed Effects model
#' ps_result <- estimate_propensity_score(
#'     PS_model_type = "FE",
#'     Sname = "school",
#'     L1_baseline_cov = c("X1", "X2"),
#'     treatment = "A",
#'     data = example_data
#' )
#' 
#' # View convergence status
#' print(ps_result$convergence)
#' 
#' # View warnings (if any)
#' print(ps_result$warnings)
#' 
#' # View augmented data with propensity scores and IPTW
#' head(ps_result$data)
#'
#' @export

estimate_propensity_score <- function(PS_model_type = "FE", 
                              Sname, # @param Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
                              L1_baseline_cov, #Xnames, # @param Xnames a character vector of the names of the columns in "data" that correspond to individual-level baseline covariates (X).
                              treatment, # @param Aname a character string of the name of the column in "data" that corresponds to a dummy coded treatment variable (A).
                              data) {
    
    # Load necessary packages
    if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Package 'lme4' is required for random effects models. Please install it.")
    }
    
    # Helper function to capture warnings and results 
    with_warning_capture <- function(expr) {
        warnings <- NULL
        result <- withCallingHandlers(
            expr,
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        list(result = result, warnings = warnings)
    }
    
    # Construct the formula based on the provided covariates
    covariate_formula <- paste(L1_baseline_cov, collapse = " + ")
    
    # Initialize variables for tracking warnings and convergence
    convergence <- TRUE
    warnings_list <- NULL
    
    # Estimate propensity score model
    if (PS_model_type == "SL") {
        # Simple Logistic Regression
        formula <- as.formula(paste(treatment, "~", covariate_formula))
        fit_result <- with_warning_capture({
            psmod <- glm(formula = formula, family = binomial(), data = data)
        })
        convergence <- psmod$converged
        
    } else if (PS_model_type == "FE") {
        # Fixed Effects Model (including cluster-level dummies)
        formula <- as.formula(paste(treatment, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        fit_result <- with_warning_capture({
            psmod <- glm(formula = formula, family = binomial(), data = data)
        })
        convergence <- psmod$converged
        
    } else if (PS_model_type == "RE") {
        # Random Effects Model (using glmer from lme4)
        formula <- as.formula(paste(treatment, "~", covariate_formula, "+ (1 |", Sname, ")"))
        fit_result <- with_warning_capture({
            psmod <- lme4::glmer(formula = formula, family = binomial(), data = data, 
                                 control = lme4::glmerControl(optimizer = "bobyqa"))
        })
        # Check convergence for glmer
        convergence <- is.null(psmod@optinfo$conv$lme4$messages)
        
    } else {
        stop("Invalid PS_model_type. Please choose 'SL', 'FE', or 'RE'.")
    }
    
    # Collect warnings and results
    psmod <- fit_result$result
    warnings_list <- fit_result$warnings
    
    # Predict probabilities and calculate IPTW
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    
    # Handle potential division by zero or propensity scores of exactly 0 or 1
    epsilon <- 1e-6
    data$ps <- pmin(pmax(data$ps, epsilon), 1 - epsilon)
    
    data$iptw <- with(data, (get(treatment) / ps) + ((1 - get(treatment)) / (1 - ps)))
    
    # Return results
    return(list(
        data = data,
        psmod = psmod,
        convergence = convergence,
        warnings = warnings_list
    ))
    
}


##################################### END ######################################
