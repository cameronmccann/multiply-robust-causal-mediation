# {UPDATE DOCUMENTATION AT SOMEPOINT}
# On 2025-07-21: added arguments (like random_slope_vars) to be past to other crossfit function so users can set random slopes for specific variables & source_label argument for warning messages

# for one, binary mediator p(M=1|a,C)

m.ac <- function(data_in, varnames, ipw = NULL, cluster_opt = "FE.glm", folds, learners, bounded = FALSE, Mfamily = "binomial", 
                 random_slope_vars = random_slope_vars, source_label = "m.ac") {
    
    # Check for required functions
    if (!exists("crossfit")) {
        stop("Missing required functions: 'crossfit' not found. Ensure they are loaded.")
    }
    
    # Initialize output matrix
    m_ac <- matrix(nrow = nrow(data_in), ncol = 4)
    colnames(m_ac) <- c(glue("m(0|0,c)"), glue("m(1|0,c)"), 
                        glue("m(0|1,c)"), glue("m(1|1,c)"))
    
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
        valid_1 <- valid_0 <- origami::validation(data_in, folds[[v]])
        valid_1[[varnames$A]] <- 1
        valid_0[[varnames$A]] <- 0
        
        if (is.null(train) || is.null(valid_1) || is.null(valid_0)) {
            stop(sprintf("Error in fold %d: Training or validation set is NULL. Check your folds generation process.", v))
        }
        
        # Perform cross-fitting with specified parameters
        alist <- tryCatch({
            # if (length(ipw) == 0) {
            crossfit(train, list(valid_0, valid_1), 
                     varnames$M, 
                     c(varnames$A, varnames$X), 
                     varnames, 
                     ipw = ipw, # NULL,
                     cluster_opt, 
                     type = Mfamily, # c("binomial"), 
                     learners, 
                     bounded = FALSE, 
                     random_slope_vars = random_slope_vars, 
                     source_label = source_label)
            preds <- alist$preds
            # }
        }, error = function(e) {
            stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
        })
        
        # Validate predictions
        preds <- alist$preds
        if (is.null(preds) || nrow(preds) != length(folds[[v]]$validation_set)) {
            stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the validation set size.", v))
        }
        
        # Populate the output matrix 
        ## valid_0
        m_ac[folds[[v]]$validation_set, glue("m(0|0,c)")] <- 1 - preds[, 1]
        m_ac[folds[[v]]$validation_set, glue("m(1|0,c)")] <- preds[, 1]
        ## valid_1
        m_ac[folds[[v]]$validation_set, glue("m(0|1,c)")] <- 1 - preds[, 2]
        m_ac[folds[[v]]$validation_set, glue("m(1|1,c)")] <- preds[, 2]
    }
    
    # Return the computed matrix
    return(m_ac)

}


# 
# # Mediators --------------
# # for one mediator p(M1=1|a,C)
# m1.ac <- function(data_in, whichM = 1, varnames, ipw = NULL, cluster_opt = "FE.glm", folds, learners, bounded = FALSE) {
#     m1_ac <- matrix(nrow = nrow(data_in), ncol = 2*2)
#     colnames(m1_ac) <- c(glue("m{whichM}(0|0,c)"), glue("m{whichM}(1|0,c)"),
#                          glue("m{whichM}(0|1,c)"), glue("m{whichM}(1|1,c)"))
#     
#     if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
#         folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
#         folds[[1]]$training_set <- folds[[1]]$validation_set 
#     }
#     
#     v <- 1
#     for (v in seq_along(folds)) {
#         train <- origami::training(data_in, folds[[v]])
#         valid_1 <- valid_0 <- origami::validation(data_in, folds[[v]])
#         valid_1[[varnames$A]] <- 1
#         valid_0[[varnames$A]] <- 0
#         
#         # if (length(ipw) > 0) {
#         #   preds <- sapply(c(0, 1), function(a_val = 0) {
#         #     valid_a <- origami::validation(data_in, folds[[v]])
#         #     valid_a[[varnames$A]] <- a_val
#         #     ind_a <- train[[varnames$A]] == a_val
#         #     train_a <- train[ind_a, ]
#         #     ipw_a <- ipw[ind_a, grep(a_val, colnames(ipw))]
#         #     alist_a <- crossfit(train_a, list(valid_a), 
#         #                         varnames$M[whichM], 
#         #                         c(varnames$X), 
#         #                         varnames, 
#         #                         # weighted logistic regression in the group
#         #                         # with treatment a with weights:
#         #                         ipw_a,
#         #                         cluster_opt, 
#         #                         type = c("binomial"), learners, bounded)
#         #     alist_a$preds
#         #     
#         #   }, simplify = TRUE)
#         #    
#         # }
#         
#         if (length(ipw) == 0) {
#             alist <- crossfit(train, list(valid_0, valid_1), 
#                               varnames$M[whichM], 
#                               c(varnames$A, varnames$X), 
#                               varnames, 
#                               NULL,
#                               cluster_opt, 
#                               type = c("binomial"), learners, bounded = FALSE)
#             preds <- alist$preds
#         }
#         
#         # alist$fit$fitLibrary$SL.glm_All$object$coefficients
#         
#         # valid_0
#         m1_ac[folds[[v]]$validation_set, glue("m{whichM}(0|0,c)")] <- 1 - preds[, 1]
#         m1_ac[folds[[v]]$validation_set, glue("m{whichM}(1|0,c)")] <- preds[, 1]
#         # valid_1
#         m1_ac[folds[[v]]$validation_set, glue("m{whichM}(0|1,c)")] <- 1 - preds[, 2]
#         m1_ac[folds[[v]]$validation_set, glue("m{whichM}(1|1,c)")] <- preds[, 2]
#     }
#     m1_ac
# }
