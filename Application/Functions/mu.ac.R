# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2025-01-07: nothing modified yet (only added package names; e.g., origami::)
# On 2025-07-21: added arguments (like random_slope_vars) to be past to other crossfit function so users can set random slopes for specific variables & source_label argument for warning messages

mu.ac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                   folds, learners, bounded = FALSE, 
                  random_slope_vars = random_slope_vars, source_label = "mu.ac") {

    a_vals <- expand.grid(a=c(0,1))

    mu <- matrix(nrow = nrow(data_in), ncol = nrow(a_vals))
    colnames(mu) <- (glue::glue("mu(a{a_vals$a},c)"))

    if (stringr::str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- origami::make_folds(data_in, fold_fun = origami::folds_vfold, V = 1)
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
                          learners, bounded, 
                          random_slope_vars = random_slope_vars, 
                          source_label = source_label)
        # alist$fit$fitLibrary$SL.glm_All$object$coefficients
        preds <- alist$preds
        for (jj in 1:nrow(a_vals)) {
            mu[folds[[v]]$validation_set, glue::glue("mu(a{a_vals$a[jj]},c)")] <-  preds[, jj]
        }
    }
    mu
}
