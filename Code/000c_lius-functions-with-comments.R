
# ══════════════════════════════
#    Dr. Liu's functions for oneM.cl() with comments  
# ══════════════════════════════



#' Compute Intervention Effects under a Clustered Causal Inference Setting
#'
#' This function `oneM.cl()` estimates certain intervention effects in a causal inference setup with clustered data. 
#' Specifically, it computes effects such as Y(a,M(a)) and Y(a,M(a*)), 
#' representing counterfactual outcomes under interventions on treatment and mediators.
#'
#' @param data A data frame containing the variables of interest.
#' @param Sname Name of the cluster-level variable (a factor or integer).
#' @param Wnames Names of baseline covariates (optional).
#' @param Xnames Names of time-varying or other covariates (required).
#' @param Aname Name of the treatment variable.
#' @param Mnames Name(s) of mediator variable(s).
#' @param Yname Name of the outcome variable.
#' @param Yfamily Family of the outcome, e.g. "gaussian" or "binomial".
#' @param cluster_opt_a Estimation option for models involving treatment (A) given cluster.
#' @param cluster_opt_m Estimation option for models involving mediators (M) given cluster.
#' @param cluster_opt_y Estimation option for models involving outcome (Y) given cluster.
#' @param cluster_opt_v Estimation option for models involving v(c) regression.
#' @param interaction_fity Vector indicating which interactions to include in the outcome model (default is c("AM") for A*M).
#' @param num_folds Number of folds for cross-fitting (default 1 means no cross-fitting).
#' @param learners_a Super Learner library for models of A.
#' @param learners_m Super Learner library for models of M.
#' @param learners_y Super Learner library for models of Y.
#' @param contrast_a Contrast to consider (default a=1 and a*=0).
#'
#' @return A list with three data frames:
#'   \item{mr_results}{Multiply-robust results (point estimates, standard errors, and confidence intervals)}
#'   \item{reg_results}{Results from regression-only estimators}
#'   \item{rmpw_results}{Results from weighting-only estimators}
#'
#' @details
#' This function uses a cluster-level causal inference setup to estimate direct and indirect effects via mediation. 
#' It applies cross-fitting and various estimation approaches (regression, weighting, multiply-robust) to handle 
#' confounding and clustering.
#'
#' The output includes effects such as Y(1,M(1)), Y(1,M(0)), Y(0,M(0)), their decompositions into direct (DE) and 
#' indirect (IE) effects, and associated inference.
#'
#' @section Converting for Natural Effects:
#' Currently, the code computes controlled direct and indirect effects, i.e., setting the mediator to a fixed 
#' (possibly counterfactual) distribution. For *natural direct* and *indirect* effects, you would:
#' - Redefine the mediator interventions to reflect the *natural* (counterfactual) distribution of M under a 
#'   specified exposure (e.g., M(a*)). 
#' - That is, instead of explicitly fixing M at a certain level or distribution, you need to estimate what M 
#'   would have been under exposure a* (the "natural" level of the mediator under the reference exposure), 
#'   and then plug this back into the outcome model.
#' - Concretely, you would model M under both exposures (a and a*) and then predict Y under a but with M fixed 
#'   to what it would naturally be if a* had been assigned. 
#' - This typically requires two steps: estimate M(a*) from the data, and then estimate Y(a, M(a*)) using the 
#'   predicted M(a*). Adaptation involves modifying the code that simulates mediator values and then feeding 
#'   them into the outcome model.
#'
#' @importFrom dplyr group_by mutate across bind_cols filter
#' @importFrom stats qnorm var sd glm binomial gaussian
#' @importFrom lme4 lmer glmer
#' @importFrom SuperLearner SuperLearner
#' @importFrom data.table setDT setDF
#' @importFrom origami folds_vfold make_folds training validation
#' @importFrom magrittr %>%
#' @importFrom purrr map map_dbl map_df
#' @importFrom glue glue
#' @importFrom fastDummies dummy_cols
#'
#' @examples
#' # This is a simplified example (user must provide a proper dataset)
#' # out <- oneM.cl(data = mydata, Sname = "cluster_id", Xnames = c("age","sex"), Aname = "treatment",
#' #                Mnames = "mediator", Yname = "outcome")
#'
#' @export
oneM.cl <- function(data,
                    Sname,
                    Wnames = NULL, Xnames,
                    Aname,
                    Mnames,
                    Yname, Yfamily = "gaussian",
                    cluster_opt_a = "cwc.FE",
                    cluster_opt_m = "cwc.FE",
                    cluster_opt_y = "cwc.FE",
                    cluster_opt_v = "cwc.FE",
                    interaction_fity = c("AM"),
                    num_folds = 1,
                    learners_a = c("SL.glm"),
                    learners_m = c("SL.glm"),
                    learners_y = c("SL.glm"),
                    contrast_a = c(a=1, astar=0)
) {
    # Set a random seed for reproducibility
    set.seed(12345)
    
    # Create an ID variable for indexing
    data$id <- 1:nrow(data)
    
    # Save a copy of the input data
    data_in <- data
    
    # Create interaction terms between A and M (if specified in interaction_fity)
    # Here we assume interaction_fity includes "AM"
    AM <- data_in[[Aname]] * data_in[, Mnames, drop=FALSE]
    colnames(AM) <- glue("AM.{Mnames}")
    
    # Create dummy indicators for cluster variable Sname (removing the first dummy for reference)
    Sdumm <- dummy_cols(data[[Sname]], remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    colnames(Sdumm) <- paste0("S", 1:ncol(Sdumm))
    Sname_dummies <- colnames(Sdumm)
    
    # Compute cluster-level means and cluster-within-cluster (cwc) transformations
    # The idea is to partial out cluster-level means from predictors to handle clustering
    data_in <- data.frame(data, AM) %>%
        group_by(!!as.name(Sname)) %>%
        mutate(across(c(!id), list(clmean = ~mean(.), cwc = ~.-mean(.)))) %>%
        bind_cols(Sdumm)
    
    # Convert Sname to a factor with unique cluster IDs
    data_in[[Sname]] <- match(data[[Sname]], unique(data[[Sname]]))
    data_in[[Sname]] <- as.factor(data_in[[Sname]])
    
    # Keep track of variable names in a structured list
    varnames <- list("A" = Aname, "M" = Mnames, "Y" = Yname,
                     "AM" = paste0("AM.", Mnames),
                     "S" = Sname, "Sdumm" = Sname_dummies,
                     "X" = Xnames, "W" = Wnames)
    
    Cnames <- list("X" = Xnames, "W" = Wnames, "Sdumm" = Sname_dummies)
    
    # Create folds for cross-fitting
    if (num_folds > 1) {
        folds <- make.fold_K(data_in, Sname, cv_folds = num_folds)
    }
    if (num_folds <= 1) {
        # No cross-fitting, just use the entire dataset as training and validation
        folds <- make_folds(cluster_ids = data[[Sname]], fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    # Fit models for P(A|C), P(A|M,C), Y(a,m,c), v_ac(c), and mu(a,c)
    a_c <- a.c(data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE)
    a_mc <- a.mc(data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE)
    mu_mac <- mu.mac(data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                     cluster_opt = cluster_opt_y,
                     interaction = interaction_fity,
                     folds, learners_y, bounded = FALSE)
    v_ac <- v.ac(a=contrast_a["a"], astar=contrast_a["astar"], mu_mac,
                 data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                 cluster_opt = cluster_opt_v,
                 folds, learners_y, bounded = FALSE,
                 full.sample = FALSE)
    mu_ac <- mu.ac(data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                   cluster_opt = cluster_opt_y,
                   folds, learners_y, bounded = FALSE)
    
    # Compute the efficient influence functions (EIF) cluster-wise
    cluster_sizes <- table(data_in[[Sname]])
    trtprop <- aggregate(data_in[[varnames$A]], by=list(data_in[[Sname]]), mean)[,2]
    J <- length(unique(data_in[[Sname]]))
    eligible_clusters <- c(1:J)[trtprop > 0 & trtprop < 1]
    
    # Define a helper to compute cluster-specific estimates
    cluster.k <- function(k=1) {
        ind_k <- which(data_in[[Sname]] == unique(data_in[[Sname]])[k])
        nk <- length(ind_k)
        
        if (trtprop[k]>0 & trtprop[k]<1) {
            eif_k <- eif(data_in[ind_k, ], varnames,
                         a_c[ind_k, ],
                         a_mc[ind_k, ],
                         mu_mac[ind_k, ],
                         v_ac[ind_k, , drop=FALSE],
                         mu_ac[ind_k, ])
            meank <- map_dbl(effect(eif_k$eifs), ~mean(., na.rm=TRUE))
        } else {
            meank <- rep(NA, 5)
        }
        meank
    }
    
    eif_clmean <- map(eligible_clusters, cluster.k)
    eif_clmean <- do.call(rbind, eif_clmean)
    sizes <- cluster_sizes[eligible_clusters]
    
    # Obtain inference metrics at the cluster-level and individual-level scale
    mr_individual <- map_df(1:ncol(eif_clmean), ~get.inference(estimand=.x, eif_clmean, sizes, average = "individual"))
    mr_cluster <- map_df(1:ncol(eif_clmean), ~get.inference(estimand=.x, eif_clmean, sizes, average = "cluster"))
    
    mr_results <- data.frame(estimand=colnames(eif_clmean),
                             mr_individual=mr_individual, mr_cluster=mr_cluster)
    mr_results <- mr_results %>%
        mutate(
            J = length(unique(data[[Sname]])),
            df_eff = case_when(str_detect(estimand, "Y") ~ J-1,
                               .default = J-2),
            adjse = sqrt(J/df_eff),
            mr_waldci1_individual = mr_individual.est - qt(0.975, df=df_eff)*mr_individual.se*adjse,
            mr_waldci2_individual = mr_individual.est + qt(0.975, df=df_eff)*mr_individual.se*adjse,
            mr_waldci1_cluster = mr_cluster.est - qt(0.975, df=df_eff)*mr_cluster.se*adjse,
            mr_waldci2_cluster = mr_cluster.est + qt(0.975, df=df_eff)*mr_cluster.se*adjse
        )
    
    # Compute EIF for the full sample (for regression-only and weighting-only)
    eif_full <- eif(data_in, varnames,
                    a_c,
                    a_mc,
                    mu_mac,
                    v_ac[, , drop=FALSE],
                    mu_ac)
    eifs <- eif_full$eifs
    regs <- eif_full$regs
    rmpw <- eif_full$rmpw
    
    # Extract effects from regs and rmpw
    regs_effs <- effect(regs)
    reg_results <- data.frame(
        reg_est = sapply(regs_effs, mean),
        reg_se = sapply(regs_effs, function(s) { sqrt(var(s) / nrow(data)) }),
        reg_ci = t(sapply(regs_effs, function(s) {
            mean(s) + c(-1, 1) * qnorm(0.975) * sqrt(var(s) / nrow(data))
        }))
    ) %>%
        rownames_to_column(var = "estimand")
    
    rmpw_effs <- effect(rmpw)
    rmpw_results <- data.frame(
        rmpw_est = sapply(rmpw_effs, mean),
        rmpw_se = sapply(rmpw_effs, function(s) { sqrt(var(s) / nrow(data)) }),
        rmpw_ci = t(sapply(rmpw_effs, function(s) {
            mean(s) + c(-1, 1) * qnorm(0.975) * sqrt(var(s) / nrow(data))
        }))
    ) %>%
        rownames_to_column(var = "estimand")
    
    # Return results
    out <- list(mr_results=mr_results, reg_results=reg_results, rmpw_results=rmpw_results)
    return(out)
}


# Below are helper functions used by `oneM.cl`. They have been included here for convenience.
# In a real package, each would have its own @export and documentation.

###############################################################################
# Helper functions (from part (b)) - included for completeness
###############################################################################

#' Compute a(c): Probability of treatment A given cluster c
#'
#' @noRd
a.c <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    a_c <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_c) <- c("a(0|c)", "a(1|c)")
    
    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        
        alist <- crossfit(train, list(valid), varnames$A, c(varnames$X), varnames,
                          ipw = NULL,
                          cluster_opt,
                          type = c("binomial"), learners, bounded)
        preds <- alist$preds
        a_c[folds[[v]]$validation_set, "a(0|c)"] <- 1 - preds[, 1]
        a_c[folds[[v]]$validation_set, "a(1|c)"] <- preds[, 1]
    }
    a_c
}

#' Compute a(m,c): Probability of treatment A given M and c
#'
#' @noRd
a.mc <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    a_mc <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_mc) <- c("a(0|m,c)", "a(1|m,c)")
    
    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        
        alist <- crossfit(train, list(valid), varnames$A, c(varnames$M, varnames$X), varnames,
                          ipw = NULL,
                          cluster_opt,
                          type = c("binomial"), learners, bounded)
        preds <- alist$preds
        a_mc[folds[[v]]$validation_set, "a(0|m,c)"] <- 1 - preds[, 1]
        a_mc[folds[[v]]$validation_set, "a(1|m,c)"] <- preds[, 1]
    }
    a_mc
}

#' Compute mu(m,a,c): Expected outcome Y under given (m,a,c)
#'
#' @noRd
mu.mac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm", interaction = c("AM"),
                   folds, learners, bounded = FALSE) {
    
    a_vals <- expand.grid(a=c(0,1))
    mu <- matrix(nrow = nrow(data_in), ncol = nrow(a_vals))
    colnames(mu) <- (glue("mu(m,a{a_vals$a},c)"))
    
    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        valid_list <- lapply(1:nrow(a_vals),
                             function(jj=1) {
                                 valid_v <- valid
                                 valid_v[, c(varnames$A)] <- a_vals$a[jj]
                                 # update interaction terms A*M if needed
                                 valid_v[, varnames$AM] <- valid_v[[varnames$A]] * valid_v[, varnames$M]
                                 valid_v
                             })
        
        alist <- crossfit(train, valid_list,
                          varnames$Y,
                          c(varnames$M, varnames$A, varnames[[interaction]], varnames$X),
                          varnames,
                          ipw,
                          cluster_opt,
                          type = Yfamily,
                          learners, bounded)
        preds <- alist$preds
        for (jj in 1:nrow(a_vals)) {
            mu[folds[[v]]$validation_set, glue("mu(m,a{a_vals$a[jj]},c)")] <-  preds[, jj]
        }
    }
    mu
}

#' Compute v_ac(c): Intermediate regression for indirect effect components
#'
#' @noRd
v.ac <- function(a=1, astar=0, mu_mac, data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                 folds, learners, bounded = FALSE,
                 full.sample = FALSE) {
    
    Sname <- varnames$S
    data_in[["mu"]] <- mu_mac[, glue("mu(m,a{a},c)")]
    data_in <- data_in %>%
        group_by(!!as.name(Sname)) %>%
        mutate(across(c("mu"), list(clmean = ~mean(.), cwc = ~.-mean(.))))
    
    v_ac <- matrix(nrow = nrow(data_in), ncol = 1)
    colnames(v_ac) <- glue("v_ac(c)")
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        
        valid_list <- lapply(1:1, function(jj=1) {
            valid_v <- valid
            valid_v[, c(varnames$A)] <- astar
            valid_v
        })
        if (full.sample == TRUE) {
            alist <- crossfit(train, valid_list,
                              "mu",
                              c(varnames$A, varnames$X),
                              varnames,
                              ipw,
                              cluster_opt,
                              type = "gaussian",
                              learners, bounded)
        }
        
        if (full.sample == FALSE) {
            predict_subset <- (train[[varnames$A]] == unique(valid_list[[1]][[varnames$A]]))
            alist <- crossfit(train[predict_subset, ], valid_list,
                              "mu",
                              c(varnames$X),
                              varnames,
                              ipw,
                              cluster_opt,
                              type = "gaussian",
                              learners, bounded)
        }
        
        preds <- alist$preds
        v_ac[folds[[v]]$validation_set, glue("v_ac(c)")] <- preds[, 1]
    }
    
    v_ac
}

#' Compute mu(a,c): Expected outcome Y under given (a,c)
#'
#' @noRd
mu.ac <- function(data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                  folds, learners, bounded = FALSE) {
    
    a_vals <- expand.grid(a=c(0,1))
    mu <- matrix(nrow = nrow(data_in), ncol = nrow(a_vals))
    colnames(mu) <- (glue("mu(a{a_vals$a},c)"))
    
    if (str_detect(cluster_opt, "glm")) { # no cross-fitting if glm
        folds <- make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    for (v in seq_along(folds)) {
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        valid_list <- lapply(1:nrow(a_vals),
                             function(jj=1) {
                                 valid_v <- valid
                                 valid_v[, c(varnames$A)] <- a_vals$a[jj]
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
        preds <- alist$preds
        for (jj in 1:nrow(a_vals)) {
            mu[folds[[v]]$validation_set, glue("mu(a{a_vals$a[jj]},c)")] <-  preds[, jj]
        }
    }
    mu
}

#' Compute efficient influence functions
#'
#' @noRd
eif <- function(data_in, varnames, a_c, a_mc, mu_mac, v_ac, mu_ac) {
    
    A <- data_in[[varnames$A]]
    Y <- data_in[[varnames$Y]]
    
    # a, a* combinations:
    a_vals <- data.frame(a = c(1, 1, 0),
                         astar = c(0, 1, 0))
    
    eifs <- list()
    regs <- list()
    rmpw <- list()
    
    for (i in 1:nrow(a_vals)) {
        a <- a_vals$a[i]
        astar <- a_vals$astar[i]
        
        # Compute Y(a,M(astar))
        if (a != astar) {
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            ipw_astar <- 1*(A==astar) / bound(a_c[, glue("a({astar}|c)")])
            
            mu <- mu_mac[, glue("mu(m,a{a},c)")]
            vbar <- v_ac[, glue("v_ac(c)")]
            
            # h_m is for mediating the probability shift
            h_m <- ( a_c[, glue("a({a}|c)")]*a_mc[, glue("a({astar}|m,c)")] ) /
                (a_c[, glue("a({astar}|c)")]*a_mc[, glue("a({a}|m,c)")])
            
            eify <- 1*(A == a)*ipw_a*h_m*(Y - mu) / mean(1*(A == a)*ipw_a*h_m)
            eifm <- 1*(A == astar)*ipw_astar*(mu - vbar) / mean(1*(A == astar)*ipw_astar)
            eif <- eify + eifm + vbar
            
            eifs[[glue("Y({a},M({astar}))")]] <- eif
            rmpw[[glue("Y({a},M({astar}))")]] <- 1*(A == a)*ipw_a*h_m*Y / mean(1*(A == a)*ipw_a*h_m)
            regs[[glue("Y({a},M({astar}))")]] <- vbar
        }
        
        # Compute Y(a,M(a))
        if (a == astar) {
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            mu_c <- mu_ac[, glue("mu(a{a},c)")]
            
            eifya <- 1*(A == a)*ipw_a*(Y - mu_c) / mean(1*(A == a)*ipw_a)
            eif_a <- eifya + mu_c
            
            eifs[[glue("Y({a},M({a}))")]] <- eif_a
            rmpw[[glue("Y({a},M({a}))")]] <- 1*(A == a)*ipw_a*(Y) / mean(1*(A == a)*ipw_a)
            regs[[glue("Y({a},M({a}))")]] <- mu_c
        }
    }
    
    list(eifs=eifs, rmpw=rmpw, regs=regs)
}

#' Compute direct and indirect effects
#'
#' @noRd
effect <- function(thetas) {
    thetas[["DE(,M(0))"]] <- thetas[["Y(1,M(0))"]] - thetas[["Y(0,M(0))"]]
    thetas[["IE(1,)"]] <- thetas[["Y(1,M(1))"]] - thetas[["Y(1,M(0))"]]
    thetas
}

#' Compute inference quantities from cluster-level EIF means
#'
#' @noRd
get.inference <- function(estimand=1, eif_clmean, sizes, average = "individual") {
    K <- length(sizes)
    mean_size <- sum(sizes)/K
    
    if (average=="individual") {
        estimate <- sum(eif_clmean[1:K, estimand] * sizes/mean_size) / K
        variance <- var(eif_clmean[1:K, estimand] * sizes/mean_size)/K
    }
    
    if (average=="cluster") {
        estimate <- sum(eif_clmean[1:K, estimand])/K
        variance <- var(eif_clmean[1:K, estimand])/K
    }
    
    c(est=estimate, se=sqrt(variance))
}

#' Cross-fitting helper function
#'
#' @noRd
crossfit <- function(train, valid.list, yname, xnames, varnames,
                     ipw = NULL,
                     cluster_opt = "FE.glm",
                     type, learners, bounded = FALSE) {
    # This function fits models on the training set and predicts on each validation set in valid.list.
    # It supports various cluster adjustment methods (fixed effects, random effects, cwc) and uses SuperLearner when needed.
    
    Sname <- varnames$S
    Sname_dummies <- varnames$Sdumm
    
    family <- ifelse(type == "binomial", binomial(), gaussian())
    
    df_lm <- data.frame(Y = train[[yname]],
                        train[, c(xnames, varnames$W), drop = FALSE],
                        S = train[[Sname]] )
    if (length(ipw) > 0) {
        df_lm$wreg <- ipw
    } else {
        df_lm$wreg <- rep(1, nrow(df_lm))
    }
    
    # Depending on cluster_opt, fit different models
    # ... (see provided code in original script) ...
    # The logic below is essentially unchanged, just with added comments.
    
    # Random Effects using lmer/glmer
    if (cluster_opt == "RE.glm") {
        # Fit a random intercept model with cluster as random effect
        REformula <- paste("Y ~", paste(c(xnames, varnames$W), collapse = " + "), "+ (1 | S)")
        if (family[[1]] == "gaussian") {
            fit <- lmer(formula = REformula,
                        weights = wreg,
                        data = df_lm)
        } else {
            fit <- glmer(formula = REformula,
                         weights = wreg,
                         data = df_lm, family = family[[1]])
        }
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(xnames, varnames$W), drop = FALSE],
                               S = validX[[Sname]] )
            preds <- predict(fit, newX, type = "response")
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # Fixed effects model (GLM) including cluster indicators if cluster_opt == "FE.glm"
    if (cluster_opt %in% c("FE.glm", "noncluster.glm")) {
        if (cluster_opt == "FE.glm") {
            fit <- glm(formula = paste("Y ~ S +", paste(xnames, collapse = " + ")),
                       weights = wreg,
                       data = df_lm, family = family[[1]])
        }
        if (cluster_opt == "noncluster.glm") {
            fit <- glm(formula = paste("Y ~", paste(xnames, collapse = " + ")),
                       weights = wreg,
                       data = df_lm, family = family[[1]])
        }
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, xnames, drop = FALSE],
                               S = validX[[Sname]] )
            preds <- predict(fit, newX, type = "response")
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # Fixed effects with ML (SuperLearner) - using cluster dummies
    if (cluster_opt %in% c("fix.mlr", "noncluster.mlr")) {
        df_FE <- data.frame(df_lm, train[, Sname_dummies])
        set.seed(12345)
        if (cluster_opt == "fix.mlr") {
            fit <- SuperLearner(
                df_FE$Y,
                df_FE[, c(xnames, varnames$W, Sname_dummies), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        if (cluster_opt == "noncluster.mlr") {
            fit <- SuperLearner(
                df_FE$Y,
                df_FE[, c(xnames), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(xnames, varnames$W, Sname_dummies), drop = FALSE])
            preds <- predict(fit, newX[, fit$varNames])$pred
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # Centered within cluster (cwc) option using SuperLearner
    # Similar logic for cwc, sufficient_stats, cwc.FE as in original code
    # ...
    # For brevity, we trust that the above code block is correct and unchanged.
    # (See user's provided code for details.)
    
    # If cluster_opt == "cwc", "sufficient_stats", or "cwc.FE", that logic follows similarly.
    # The user's original code handles these cases. We keep them intact:
    
    # The following blocks are unchanged from the user's code, just moved inside crossfit().
    
    if (cluster_opt == "cwc") { 
        df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                             train[, c(glue("{xnames}_cwc"), glue("{yname}_clmean"), varnames$W), drop = FALSE])
        
        fit <- SuperLearner(
            df_cwc$Y,
            df_cwc[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner)
        )
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), varnames$W), drop = FALSE]) 
            validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue("{xnames}_cwc")
            newX <- newX %>% bind_cols(validX_cwc)
            preds <- predict(fit, newX[, fit$varNames])$pred
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    if (cluster_opt == "sufficient_stats")  {
        if (family[[1]] == "binomial") {
            df_ss <- data.frame(Y = train[, glue("{yname}"), drop=TRUE],
                                train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), glue("{yname}_clmean"), varnames$W), drop = FALSE])
        }
        if (family[[1]] == "gaussian") {
            df_ss <- data.frame(Y = train[, glue("{yname}_cwc"), drop=TRUE],
                                train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), varnames$W), drop = FALSE])
        } 
        
        fit <- SuperLearner(
            df_ss$Y,
            df_ss[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner)
        )
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(glue("{yname}_clmean"), xnames, glue("{xnames}_clmean"), varnames$W), drop = FALSE]) 
            validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue("{xnames}_cwc")
            newX <- newX %>% bind_cols(validX_cwc)
            preds <- predict(fit, newX[, fit$varNames])$pred
            
            if (family[[1]] == "gaussian") {
                preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
            }
            
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    if (cluster_opt == "cwc.FE") {
        if (family[[1]]=="gaussian") {
            df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                                 train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), varnames$W), drop = FALSE])
        }
        if (family[[1]]=="binomial") {
            df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
                                 train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), varnames$W), drop = FALSE])
        }
        
        fit <- SuperLearner(
            df_cwc$Y,
            df_cwc[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner)
        )
        
        preds <- sapply(valid.list, function(validX) {
            newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), Sname_dummies, varnames$W), drop = FALSE]) 
            validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue("{xnames}_cwc")
            newX <- newX %>% bind_cols(validX_cwc)
            preds <- predict(fit, newX[, fit$varNames])$pred
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    out <- list(fit = fit, preds = preds)
    return(out)
}

#' Bound values to avoid numerical issues in probabilities
#'
#' @noRd
bound <- function(vals, tol = 0.025) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}

#' Another bounding function for probabilities
#'
#' @noRd
bound.precison <- function(vals, tol = 1e-4) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}

#' Modified SL learners
#'
#' @noRd
SL.xgboost.modified <- function(...) {
    SL.xgboost(..., ntrees = 100)
}

#' @noRd
SL.ranger.modified <- function(...) {
    SL.ranger(...,num.trees = 200)
}

#' @noRd
SL.gam.modified <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, ...) {
    Xoffset <- NULL
    if (length(grep("_clmean", colnames(X), value = T))>0) {
        Xoffset <- X[,grep("_clmean", colnames(X), value = T)]
        if (family=="binomial") {
            Xoffset <- bound.precison(qlogis(Xoffset))
        }
    }
    X <- X[, !grepl("_clmean", colnames(X))]
    
    cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
    if (sum(!cts.x) > 0) {
        gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                        colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                        ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
                                                                                                               !cts.x, drop = FALSE]), collapse = "+")))
    }
    else {
        gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                        colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                        ")", sep = ""), collapse = "+")))
    }
    if (sum(!cts.x) == length(cts.x)) {
        gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                                  collapse = "+"), sep = ""))
    }
    fit.gam <- gam::gam(gam.model, data = data.frame(X, Y=Y), family = family, offset=Xoffset,
                        control = gam::gam.control(maxit = 50, bf.maxit = 50), 
                        weights = obsWeights)
    if (packageVersion("gam") >= "1.15") {
        pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
    }
    else {
        stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package.")
    }
    fit <- list(object = fit.gam)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.gam")
    return(out)
}

#' Create folds by cluster
#'
#' @noRd
make.fold_K <- function(data_in, Sname, cv_folds=4) {
    fold_K <- lapply(unique(data_in[[Sname]]), FUN = function(k=1) {
        data_in$K <- match(data_in[[Sname]], unique(data_in[[Sname]]))
        if (nrow(data_in[data_in$K==k, ]) >= 1) {
            fk <- origami::make_folds(data_in[data_in$K==k, ],
                                      fold_fun = origami::folds_vfold,
                                      V = cv_folds)
            fold_k <- fk
            for(v in 1:cv_folds) {
                fold_k[[v]]$validation_set <- data_in$id[data_in$K==k][fk[[v]]$validation_set]
                fold_k[[v]]$training_set <- data_in$id[data_in$K==k][fk[[v]]$training_set]
            }
        }
        return(fold_k)
    } )
    
    folds <- origami::make_folds(data_in,
                                 fold_fun = origami::folds_vfold,
                                 V = cv_folds)
    
    for(v in 1:cv_folds) {
        folds[[v]]$validation_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
            fold_K[[k]][[v]]$validation_set
        }))
        folds[[v]]$training_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
            fold_K[[k]][[v]]$training_set
        }))
    }
    
    folds
}
