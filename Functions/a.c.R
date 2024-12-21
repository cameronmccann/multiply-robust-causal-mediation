
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# [eventually maybe change function to a_c, if it won't interfere with output being labeled a_c]
# As of 2024-12-19: only modified messaging output 

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
