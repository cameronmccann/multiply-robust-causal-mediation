
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# [eventually maybe change function to a_c, if it won't interfere with output being labeled a_c]
# As of 2024-12-19: only modified messaging output 

#' @title a.c
#'
#' @description
#' Estimates propensity scores (predicted probabilities of treatment) using cross-fitting.
#'
#' @details
#' The `a.c()` function takes a dataset along with information about the treatment and covariates, 
#' applies a cross-fitting procedure (via `crossfit()`) to obtain predictions of the probability 
#' of receiving treatment (`a(1|c)`) and not receiving treatment (`a(0|c)`) for each observation. 
#' It supports an option to switch to single-fold (non-crossfit) estimation under certain model 
#' specifications (i.e., if the `cluster_opt` argument contains `"glm"`). Predictions are stored 
#' in a matrix with two columns: `"a(0|c)"` (for no treatment) and `"a(1|c)"` (for treatment).
#'
#' @param data_in A data frame containing the variables of interest.
#' @param varnames A list specifying the names of the treatment variable (`A`), covariates (`X`), and potentially other named elements.
#' @param cluster_opt A character string indicating the clustering or modeling option to use. 
#'   Defaults to `"FE.glm"`, but if it contains `"glm"`, the function will use a single training/validation split.
#' @param folds A list of folds (training and validation sets) created via `origami::make_folds` or a custom fold function.
#' @param learners A set of machine learning algorithms or models passed on to the `crossfit()` function.
#' @param bounded A logical indicating whether to bound predicted probabilities (e.g., ensure they are within [0, 1]). Defaults to `TRUE`.
#'
#' @return
#' A matrix with two columns: 
#' \describe{
#'   \item{\code{a(0|c)}}{Estimated probability of no treatment (i.e., 1 - P(A=1)).}
#'   \item{\code{a(1|c)}}{Estimated probability of receiving treatment (i.e., P(A=1)).}
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' my_data <- data.frame(A = rbinom(100, 1, 0.5), 
#'                      X1 = rnorm(100), 
#'                      X2 = rnorm(100))
#' my_varnames <- list(A = "A", X = c("X1", "X2"))
#'
#' # Suppose we define some folds and a set of learners:
#' library(origami)
#' my_folds <- make_folds(my_data, fold_fun = folds_vfold, V = 2)
#' my_learners <- c("SL.glm", "SL.mean")
#'
#' # We can now call a.c():
#' pred_matrix <- a.c(data_in = my_data,
#'                    varnames = my_varnames,
#'                    cluster_opt = "FE.glm",
#'                    folds = my_folds,
#'                    learners = my_learners,
#'                    bounded = TRUE)
#' head(pred_matrix)
#' }
#'
#' @seealso \code{\link{crossfit}} for details on cross-fitting.
#' @export


a.c <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    # Check for required functions
    if (!exists("crossfit")) {
        stop("Missing required functions: 'crossfit' not found. Ensure they are loaded.")
    }
    
    # Initialize output matrix
    a_c <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_c) <- c("a(0|c)", "a(1|c)")
    
    # Adjust folds if no cross-fitting is required
    if (grepl("glm", cluster_opt)) { 
        folds <- origami::make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    # Check fold structure
    if (!is.list(folds) || length(folds) == 0) {
        stop("Invalid 'folds' input. Ensure 'folds' is a properly generated list of training and validation splits.")
    }
    
    # Process each fold
    for (v in seq_along(folds)) {
        # Extract training and validation sets
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        
        if (is.null(train) || is.null(valid)) {
            stop(sprintf("Error in fold %d: Training or validation set is NULL. Check your folds generation process.", v))
        }
        
        # Cross-fitting with the crossfit function
        alist <- tryCatch({
            crossfit(train, list(valid), varnames$A, c(varnames$X), varnames,
                     ipw = NULL,
                     cluster_opt,
                     type = c("binomial"), learners, bounded)
        }, error = function(e) {
            stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
        })
        
        # Extract predictions
        preds <- alist$preds
        if (is.null(preds) || nrow(preds) != length(folds[[v]]$validation_set)) {
            stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the validation set size.", v))
        }
        
        # Populate the output matrix
        a_c[folds[[v]]$validation_set, "a(0|c)"] <- 1 - preds[, 1]
        a_c[folds[[v]]$validation_set, "a(1|c)"] <- preds[, 1]
    }
    
    return(a_c)
}

# # 
# a.c <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
#     # Check for required packages
#     required_packages <- c("stringr", "origami")
#     missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
#     
#     if (length(missing_packages) > 0) {
#         stop("Missing required packages: ", paste(missing_packages, collapse = ", "), 
#              ". Please install them before running this function.")
#     }
#     
#     # Check if required functions are loaded
#     if (!exists("make_folds") || !exists("crossfit")) {
#         stop("Missing required functions. Ensure that 'make_folds' and 'crossfit' are defined and available in your environment.")
#     }
#     
#     # Initialize output matrix
#     a_c <- matrix(nrow = nrow(data_in), ncol = 2)
#     colnames(a_c) <- c("a(0|c)", "a(1|c)")
#     
#     # Check if cluster_opt contains "glm"
#     # if (stringr::str_detect(cluster_opt, "glm")) { 
#     if (grepl(pattern = "glm", cluster_opt)) {
#         message("No cross-fitting will be performed as 'cluster_opt' contains 'glm'.")
#         folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
#         folds[[1]]$training_set <- folds[[1]]$validation_set
#     }
#     
#     # Check fold structure
#     if (!is.list(folds) || length(folds) == 0) {
#         stop("Invalid 'folds' input. Ensure 'folds' is a properly generated list of training and validation splits.")
#     }
#     
#     # Process each fold
#     for (v in seq_along(folds)) {
#         message(sprintf("Processing fold %d of %d...", v, length(folds)))
#         
#         # Extract training and validation sets
#         train <- origami::training(data_in, folds[[v]])
#         valid <- origami::validation(data_in, folds[[v]])
#         
#         if (is.null(train) || is.null(valid)) {
#             stop(sprintf("Error in fold %d: Training or validation set is NULL. Check your folds generation process.", v))
#         }
#         
#         # Cross-fitting with the crossfit function
#         alist <- tryCatch({
#             crossfit(train, list(valid), varnames$A, c(varnames$X), varnames,
#                      ipw = NULL,
#                      cluster_opt,
#                      type = c("binomial"), learners, bounded)
#         }, error = function(e) {
#             stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
#         })
#         
#         # Extract predictions
#         preds <- alist$preds
#         if (is.null(preds) || nrow(preds) != length(folds[[v]]$validation_set)) {
#             stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the validation set size.", v))
#         }
#         
#         # Populate the output matrix
#         a_c[folds[[v]]$validation_set, "a(0|c)"] <- 1 - preds[, 1]
#         a_c[folds[[v]]$validation_set, "a(1|c)"] <- preds[, 1]
#     }
#     
#     message("a.c() computation completed successfully.")
#     return(a_c)
# }



# 
# a.c <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
#     a_c <- matrix(nrow = nrow(data_in), ncol = 2)
#     colnames(a_c) <- c("a(0|c)", "a(1|c)")
# 
#     if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
#         folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
#         folds[[1]]$training_set <- folds[[1]]$validation_set
#     }
#     v <- 1
#     for (v in seq_along(folds)) {
#         train <- origami::training(data_in, folds[[v]])
#         valid <- origami::validation(data_in, folds[[v]])
# 
#         alist <- crossfit(train, list(valid), varnames$A, c(varnames$X), varnames,
#                           ipw = NULL,
#                           cluster_opt,
#                           type = c("binomial"), learners, bounded)
#         # alist$fit$fitLibrary$SL.glm_All$object$coefficients
#         preds <- alist$preds
#         a_c[folds[[v]]$validation_set, "a(0|c)"] <- 1 - preds[, 1]
#         a_c[folds[[v]]$validation_set, "a(1|c)"] <- preds[, 1]
#     }
#     a_c
# }
