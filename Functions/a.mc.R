# {UPDATE DOCUMENTATION AT SOMEPOINT}
# [eventually maybe change function to a_c, if it won't interfere with output being labeled a_c]
# As of 2024-12-19: only modified messaging output 



a.mc <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    # Ensure required functions are available
    if (!exists("crossfit")) {
        stop("Missing required functions: 'crossfit'. Ensure these are properly defined or loaded.")
    }
    
    # Initialize output matrix for predictions
    a_mc <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_mc) <- c("a(0|m,c)", "a(1|m,c)")
    
    # Adjust folds if no cross-fitting is required
    if (grepl("glm", cluster_opt)) { 
        folds <- origami::make_folds(data_in, fold_fun = origami::folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    # Validate fold structure
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
        
        # Perform cross-fitting with specified parameters
        alist <- tryCatch({
            crossfit(train, list(valid), varnames$A, c(varnames$M, varnames$X), varnames,
                     ipw = NULL,
                     cluster_opt,
                     type = c("binomial"), learners, bounded)
        }, error = function(e) {
            stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
        })
        
        # Validate predictions
        preds <- alist$preds
        if (is.null(preds) || nrow(preds) != length(folds[[v]]$validation_set)) {
            stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the validation set size.", v))
        }
        
        # Populate the output matrix
        a_mc[folds[[v]]$validation_set, "a(0|m,c)"] <- 1 - preds[, 1]
        a_mc[folds[[v]]$validation_set, "a(1|m,c)"] <- preds[, 1]
    }
    
    # Return the computed matrix
    return(a_mc)
}



# a.mc <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
#     a_mc <- matrix(nrow = nrow(data_in), ncol = 2)
#     colnames(a_mc) <- c("a(0|m,c)", "a(1|m,c)")
#     
#     if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
#         folds <- origami::make_folds(data_in, fold_fun = folds_vfold, V = 1)
#         folds[[1]]$training_set <- folds[[1]]$validation_set
#     }
#     v <- 1
#     for (v in seq_along(folds)) {
#         train <- origami::training(data_in, folds[[v]])
#         valid <- origami::validation(data_in, folds[[v]])
#         
#         alist <- crossfit(train, list(valid), varnames$A, c(varnames$M, varnames$X), varnames,
#                           ipw = NULL,
#                           cluster_opt,
#                           type = c("binomial"), learners, bounded)
#         # alist$fit$fitLibrary$SL.glm_All$object$coefficients
#         preds <- alist$preds
#         a_mc[folds[[v]]$validation_set, "a(0|m,c)"] <- 1 - preds[, 1]
#         a_mc[folds[[v]]$validation_set, "a(1|m,c)"] <- preds[, 1]
#     }
#     a_mc
# }
