# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2024-12-19: only modified messaging output 
# On 2025-07-21: added arguments (like random_slope_vars) to be past to other crossfit function so users can set random slopes for specific variables & source_label argument for warning messages

mu.mac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm", interaction = c("AM"),
                   folds, learners, bounded = FALSE, random_slope_vars, source_label = "mu.mac") {
    # Ensure required functions are available
    if (!exists("crossfit")) {
        stop("Missing required functions: 'crossfit'. Ensure these are properly defined or loaded.")
    }
    
    # Expand grid for all values of 'a'
    a_vals <- expand.grid(a=c(0,1))
    
    # Initialize output matrix for predicted values
    mu <- matrix(nrow = nrow(data_in), ncol = nrow(a_vals))
    colnames(mu) <- glue::glue("mu(m,a{a_vals$a},c)")
    
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
        
        # Generate a list of modified validation sets for each value of 'a'
        valid_list <- lapply(1:nrow(a_vals),
                             function(jj=1) {
                                 valid_v <- valid
                                 # Set the treatment variable to the current 'a' value
                                 valid_v[, c(varnames$A)] <- a_vals$a[jj]
                                 
                                 # Update interaction terms
                                 valid_v[, varnames$AM] <- valid_v[[varnames$A]] * valid_v[, varnames$M]
                                 
                                 valid_v
                             })
        
        # Perform cross-fitting with the specified parameters
        alist <- tryCatch({
            crossfit(train, valid_list,
                     varnames$Y,
                     c(varnames$M, varnames$A, varnames[[interaction]], varnames$X),
                     varnames,
                     ipw,
                     cluster_opt,
                     type = Yfamily,
                     learners, bounded, 
                     random_slope_vars = random_slope_vars,
                     source_label = source_label)
        }, error = function(e) {
            stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
        })
        
        # Validate predictions
        preds <- alist$preds
        if (is.null(preds) || ncol(preds) != nrow(a_vals)) {
            stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the expected number of 'a' levels.", v))
        }
        
        # Populate the output matrix for each 'a' value
        for (jj in 1:nrow(a_vals)) {
            mu[folds[[v]]$validation_set, glue::glue("mu(m,a{a_vals$a[jj]},c)")] <- preds[, jj]
        }
    }
    mu
}

