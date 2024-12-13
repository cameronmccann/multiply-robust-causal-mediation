
a.mc <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    a_mc <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_mc) <- c("a(0|m,c)", "a(1|m,c)")

    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    v <- 1
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])

        alist <- crossfit(train, list(valid), varnames$A, c(varnames$M, varnames$X), varnames,
                          ipw = NULL,
                          cluster_opt,
                          type = c("binomial"), learners, bounded)
        # alist$fit$fitLibrary$SL.glm_All$object$coefficients
        preds <- alist$preds
        a_mc[folds[[v]]$validation_set, "a(0|m,c)"] <- 1 - preds[, 1]
        a_mc[folds[[v]]$validation_set, "a(1|m,c)"] <- preds[, 1]
    }
    a_mc
}
