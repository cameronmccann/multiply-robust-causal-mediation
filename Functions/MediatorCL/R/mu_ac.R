mu.ac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                   folds, learners, bounded = FALSE) {

    a_vals <- expand.grid(a=c(0,1))

    mu <- matrix(nrow = nrow(data_in), ncol = nrow(a_vals))
    colnames(mu) <- (glue("mu(a{a_vals$a},c)"))

    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }


    v <- 1
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        valid_list <- lapply(1:nrow(a_vals),
                             function(jj=1) {
                                 valid_v <- valid
                                 valid_v[, c(varnames$A)] <- a_vals$a[jj]
                                 # update the interactions
                                 valid_v[, varnames$AM] <- valid_v[[varnames$A]] * valid_v[, varnames$M]

                                 valid_v
                             })

        alist <- crossfit(train, valid_list,
                          varnames$Y,
                          c(varnames$A, varnames$X),
                          varnames,
                          ipw,
                          cluster_opt,
                          type = Yfamily,
                          learners, bounded)
        # alist$fit$fitLibrary$SL.glm_All$object$coefficients
        preds <- alist$preds
        for (jj in 1:nrow(a_vals)) {
            mu[folds[[v]]$validation_set, glue("mu(a{a_vals$a[jj]},c)")] <-  preds[, jj]
        }
    }
    mu
}
