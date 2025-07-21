# {UPDATE DOCUMENTATION AT SOMEPOINT} 
# As of 2025-01-07 (updated 2025-04-19): only modified comments in code; did not modify code yet; come back and add error messages 
# internal_estimate_mediation <= oneMcl.R
# On 2025-07-21: added arguments (like random_slope_vars_a) to be past to othere functions so users can set random slopes for specific variables


#' @title Compute Mediation Effects (Internal Function)
#' 
#' @description
#' **Non-user-facing** function that performs cluster-based estimations for mediation analysis, 
#' including computing efficient influence functions (EIFs) and deriving multiply-robust 
#' point estimates and confidence intervals. This function is designed to be called internally 
#' by a user-facing function such as `estimate_mediation()`.
#'
#' @details
#' - Computes intermediate nuisance estimates \eqn{\hat{a}_c}, \eqn{\hat{a}_{mc}}, \eqn{\mu_{mac}}, 
#'   \eqn{\mu_{ac}}, \eqn{v_{ac}}, and then calculates efficient influence functions for each cluster.  
#' - Derives multiply-robust (MR) and regression-based (REG) estimates, along with weighting-based 
#'   (RMPW) estimates.  
#' - Facilitates cluster-level inference by averaging the cluster-specific EIF contributions.
#'
#' Because this function is called inside `estimate_mediation()`, it expects certain 
#' arguments (e.g., `data`, variable names, clustering options, folds) to be pre-validated 
#' or constructed by the caller.
#'
#' @param data A data frame containing all variables required for mediation analysis.
#' @param Sname Character; name of the cluster identifier variable.
#' @param Wnames Optional character vector of baseline covariates for treatment assignment or outcome modeling.
#' @param Xnames Character vector of covariate names used in the estimation process.
#' @param Aname Character; name of the treatment variable.
#' @param Mnames Character vector of mediator variable names.
#' @param Yname Character; name of the outcome variable.
#' @param Yfamily Character; specifying the family of the outcome (`"gaussian"` or `"binomial"`). Defaults to `"gaussian"`.
#' @param cluster_opt_a Character; indicates how to handle clustering in \eqn{\hat{a}_c}. Defaults to `"cwc.FE"`.
#' @param cluster_opt_m Character; indicates how to handle clustering in mediator modeling. Defaults to `"cwc.FE"`.
#' @param cluster_opt_y Character; indicates how to handle clustering in outcome modeling. Defaults to `"cwc.FE"`.
#' @param cluster_opt_v Character; indicates how to handle clustering in variance estimation. Defaults to `"cwc"`.
#' @param interaction_fity A character vector (e.g. `c("AM")`) specifying which interactions are included in the outcome model.
#' @param num_folds Integer; number of cross-validation folds. Defaults to `1`.
#' @param learners_a Character vector specifying the learner library for \eqn{\hat{a}_c}. Defaults to `c("SL.glm")`.
#' @param learners_m Character vector specifying the learner library for mediator models. Defaults to `c("SL.glm")`.
#' @param learners_y Character vector specifying the learner library for outcome models. Defaults to `c("SL.glm")`.
#' @param contrast_a Named numeric vector providing treatment contrasts, typically `c(a=1, astar=0)`.
#'
#' @return
#' A named list containing:
#' \describe{
#'   \item{\code{mr_results}}{Data frame of multiply-robust estimates, standard errors, and confidence intervals.}
#'   \item{\code{reg_results}}{Data frame of regression-based estimates, standard errors, and CIs.}
#'   \item{\code{rmpw_results}}{Data frame of weighting-based estimates, standard errors, and CIs.}
#' }
#'
#' @seealso 
#' \code{\link{a.c}} for propensity score estimation, 
#' \code{\link{crossfit}} for cross-fitting, 
#' and \code{\link{effect}} for decomposing total effects into direct and indirect components.
#'
#' @keywords internal
#' @noRd
internal_estimate_mediation <- function( # replaced: compute_eifs_for_mediation 
        data,
        Sname,
        Wnames = NULL, 
        Xnames,
        Aname,
        Mnames,
        Yname,
        Yfamily = "gaussian",
        cluster_opt_a = "cwc.FE",
        cluster_opt_m = "cwc.FE",
        cluster_opt_y = "cwc.FE",
        cluster_opt_v = "cwc",
        interaction_fity = c("AM"),
        num_folds = 1,
        learners_a = c("SL.glm"),
        learners_m = c("SL.glm"),
        learners_y = c("SL.glm"),
        contrast_a = c(a = 1, astar = 0), 
        random_slope_vars_a = NULL,
        random_slope_vars_m = NULL,
        random_slope_vars_y = NULL
) {
    # --------------------------------------------------------------------------
    # 0. INITIAL SETUP & CHECKS
    # --------------------------------------------------------------------------
    # Set a seed for reproducibility
    set.seed(12345)
    
    # Basic check: maybe include possible error message here 
    # if (nrow(data) == 0) {
    #     stop("Error in `internal_estimate_mediation()`: 'data' has zero rows. Cannot proceed.")
    # }
    
    # --------------------------------------------------------------------------
    # 1. CREATE INTERACTION TERMS AND PREPARE DATA
    # --------------------------------------------------------------------------
    # Add a unique ID for each row
    data$id <- 1:nrow(data) #data$id <- seq_len(nrow(data))
    data_in <- data
    
    # Create an interaction between A and each mediator M
    # e.g. AM.M1 = A * M1, AM.M2 = A * M2, etc.
    AM <- data_in[[Aname]] * data_in[, Mnames, drop = FALSE]
    colnames(AM) <- glue::glue("AM.{Mnames}")
    
    # Create dummy indicators for cluster variable S (excluding the first dummy for reference)
    #  e.g. if S is group with levels {A, B, C}, we get S1, S2 to indicate B=1 or C=1
    Sdumm <- fastDummies::dummy_cols(data[[Sname]], 
                                     remove_first_dummy = TRUE, 
                                     remove_selected_columns = TRUE)
    colnames(Sdumm) <- paste0("S", 1:ncol(Sdumm)) #paste0("S", seq_len(ncol(Sdumm)))
    Sname_dummies <- colnames(Sdumm)
    
    # --------------------------------------------------------------------------
    # 2. CALCULATE CLUSTER-LEVEL MEANS & WITHIN-CLUSTER CENTERING
    # --------------------------------------------------------------------------
    # cluster means
    data_in <- data.frame(data, AM) %>%
        dplyr::group_by(!!as.name(Sname)) %>% #dplyr::group_by(!!rlang::sym(Sname)) %>%
        # For each variable (except 'id'), calculate:
        #   - 'clmean' = cluster mean
        #   - 'cwc' = raw value minus the cluster mean
        # dplyr::mutate(dplyr::across(c(!id), list(clmean = ~mean(.), cwc = ~.-mean(.)))) %>%
        dplyr::mutate(dplyr::across(
            .cols = -id,
            .fns = list(clmean = ~mean(.), cwc = ~. -mean(.))
        )) %>%
        dplyr::bind_cols(Sdumm)
    
    # Convert the cluster variable to a factor 
    data_in[[Sname]] <- match(data[[Sname]], unique(data[[Sname]]))
    data_in[[Sname]] <- as.factor(data_in[[Sname]])
    
    # Create lists of variable names
    varnames <- list(
        "A" = Aname,
        "M" = Mnames,
        "Y" = Yname,
        "AM" = paste0("AM.", Mnames),
        "S" = Sname,
        "Sdumm" = Sname_dummies,
        "X" = Xnames,
        "W" = Wnames
    )
    
    Cnames <- list("X" = Xnames, "W" = Wnames, "Sdumm" = Sname_dummies)
    
    # ADD DEBUG
    # print(paste0("varnames in `internal_estimate_mediation()`", varnames))
    
    # --------------------------------------------------------------------------
    # 3. CREATE FOLDS FOR CROSS-FITTING
    # --------------------------------------------------------------------------
    if (num_folds > 1) {
        # # For multi-fold cross-fitting
        # folds <- make_fold_K(data_in, Sname, cv_folds = num_folds)
        # 2025-04-19: modifications to meet Philips et al. recommendation on number of folds
        folds <- origami::make_folds(
            cluster_ids = data_in[[Sname]],
            fold_fun = origami::folds_vfold,
            V = num_folds
        )
    } else {
        # For single-fold (non-cross-fitting) -- typically a "train=valid" scenario
        folds <- origami::make_folds( #make_folds(
            cluster_ids = data[[Sname]], 
            fold_fun = origami::folds_vfold, 
            V = 1
        )
        # Force training set = validation set
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    # --------------------------------------------------------------------------
    # 4. FIT NUISANCE FUNCTIONS
    # --------------------------------------------------------------------------
    # (a) Propensity of A
    # a.c() -> predicted P(A=1|X,...)
    a_c <- tryCatch({
        a.c(
            data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE, random_slope_vars = random_slope_vars_a
        )
    }, error = function(e) {
        stop(sprintf("Error in internal function `a.c()`: %s", e$message))
    })
    
    # (b) Combined mediator & A
    # a.mc() -> predicted probabilities that incorporate mediators?
    a_mc <- tryCatch({
        a.mc(
            data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE, random_slope_vars = random_slope_vars_a
        )
    }, error = function(e) {
        stop(sprintf("Error in internal function `a.mc()`: %s", e$message))
    })
    
    # (c) Outcome regression mu.mac()
    mu_mac <- tryCatch({
        mu.mac(
            data_in, varnames, 
            Yfamily = Yfamily, 
            ipw = NULL,
            cluster_opt = cluster_opt_y,
            interaction = interaction_fity,
            folds = folds, 
            learners = learners_y, 
            bounded = FALSE, 
            random_slope_vars = random_slope_vars_y
        )
    }, error = function(e) {
        stop(sprintf("Error in internal function `mu.mac()`: %s", e$message))
    })
    
    # (d) v_ac 
    v_ac <- tryCatch({
        v.ac(
            a = contrast_a["a"], 
            astar = contrast_a["astar"], 
            mu_mac,
            data_in, 
            varnames, 
            Yfamily = Yfamily, 
            ipw = NULL,
            cluster_opt = cluster_opt_v,
            folds = folds, 
            learners = learners_y, 
            bounded = FALSE,
            full.sample = FALSE, 
            random_slope_vars = random_slope_vars_y
        )
    }, error = function(e) {
        stop(sprintf("Error in internal function `v.ac()`: %s", e$message))
    })
    
    # (e) mu_ac 
    mu_ac <- tryCatch({
        mu.ac(
            data_in, varnames, 
            Yfamily = Yfamily, 
            ipw = NULL,
            cluster_opt = cluster_opt_y,
            folds = folds, 
            learners = learners_y, 
            bounded = FALSE, 
            random_slope_vars = random_slope_vars_y
        )
    }, error = function(e) {
        stop(sprintf("Error in internal function `mu.ac()`: %s", e$message))
    })
    
    # --------------------------------------------------------------------------
    # 5. CLUSTER-LEVEL EIF CALCULATIONS
    # --------------------------------------------------------------------------
    # eif for each cluster -------------
    # and then pull cluster-specific eifs for inference (Balzer et al. 2019)
    cluster_sizes <- table(data_in[[Sname]])
    
    # Proportion of A=1 in each cluster
    trtprop <- stats::aggregate(data_in[[varnames$A]],
                                by = list(data_in[[Sname]]),
                                mean)[, 2]
    
    # Number of unique clusters
    J <- length(unique(data_in[[Sname]]))
    
    # Eligible clusters (those with non-extreme, i.e., 0 < prop < 1)
    eligible_clusters <- c(1:J)[trtprop > 0 & trtprop < 1] #seq_len(J)[trtprop > 0 & trtprop < 1]
    
    # Define helper function for extracting cluster-specific EIF
    cluster.k <- function(k = 1) {
        ind_k <- which(data_in[[Sname]] == unique(data_in[[Sname]])[k])
        nk <- length(ind_k) # 1/nk is the weight for pooling the cluster-specific eifs
        
        # If cluster k has both A=0 and A=1, we can compute eifs
        if (trtprop[k] > 0 & trtprop[k] < 1) {
            eif_k <- eif(
                data_in[ind_k, ], varnames,
                a_c[ind_k, ],
                a_mc[ind_k, ],
                mu_mac[ind_k, ],
                v_ac[ind_k, , drop = FALSE],
                mu_ac[ind_k, ]
            )
            # Compute mean over individuals in cluster k
            # map(eif_k$eifs, ~sum(.)/nk) # equivalently:
            meank <- purrr::map_dbl(effect(eif_k$eifs), ~ mean(.x, na.rm = TRUE))
            
        } else {
            # Otherwise, it's undefined for that cluster
            meank <- rep(NA, 5)            #meank <- rep(NA_real_, 5)
        }
        # vark <- map_dbl(effect(eif_k$eifs), ~var(., na.rm=T))
        # meank
        return(meank)
    }
    
    # Compute cluster-level means of the EIF
    eif_clmean <- purrr::map(eligible_clusters, cluster.k)
    eif_clmean <- do.call(rbind, eif_clmean)
    sizes <- cluster_sizes[eligible_clusters]
    
    # --------------------------------------------------------------------------
    # 6. SUMMARIZE MR AND CLUSTER-BASED ESTIMATES
    # --------------------------------------------------------------------------
    mr_individual <- purrr::map_df(
        1:ncol(eif_clmean), #seq_len(ncol(eif_clmean)),
        ~get_inference(estimand = .x, eif_clmean, sizes, average = "individual")
    )
    
    mr_cluster <- purrr::map_df(
        1:ncol(eif_clmean), #seq_len(ncol(eif_clmean)), 
        ~get_inference(estimand = .x, eif_clmean, sizes, average = "cluster")
    )
    
    mr_results <- data.frame(
        estimand = colnames(eif_clmean),
        mr_individual = mr_individual,
        mr_cluster = mr_cluster
    )
    
    # Additional fields
    mr_results <- mr_results %>%
        dplyr::mutate(
            J = length(unique(data[[Sname]])),
            df_eff = dplyr::case_when(stringr::str_detect(estimand, "Y") ~J - 1,
                               .default = J - 2),
            adjse = sqrt(J / df_eff),
            mr_waldci1_individual = mr_individual.est - stats::qt(0.975, df = df_eff) * mr_individual.se * adjse,
            mr_waldci2_individual = mr_individual.est + stats::qt(0.975, df = df_eff) * mr_individual.se * adjse,
            mr_waldci1_cluster = mr_cluster.est - stats::qt(0.975, df = df_eff) * mr_cluster.se * adjse,
            mr_waldci2_cluster = mr_cluster.est + stats::qt(0.975, df = df_eff) * mr_cluster.se * adjse
        )

    # --------------------------------------------------------------------------
    # 7. INDIVIDUAL-LEVEL EIF FOR THE FULL SAMPLE (NOT CLUSTER-AVERAGED)
    # --------------------------------------------------------------------------
    # eif full -----
    eif_full <- eif(data_in, varnames,
                    a_c, a_mc,
                    mu_mac, v_ac[, , drop = FALSE],
                    mu_ac)
    
    # eif_full is a list with $eifs, $regs, and $rmpw
    eifs <- eif_full$eifs
    regs <- eif_full$regs
    rmpw <- eif_full$rmpw
    
    # # multiply-robust
    # eifs_effs <- effect(eifs)
    # est_effs <- sapply(eifs_effs, mean)
    # se_effs <- sapply(eifs_effs, function(s) {
    #     sqrt(var(s) / nrow(data))
    # })
    # ci_effs <- sapply(eifs_effs, function(s) {
    #     mean(s) + c(-1, 1) * qnorm(0.975) * sqrt(var(s) / nrow(data))
    # })
    #
    # mr_results <- data.frame(mr_est=est_effs, mr_se=se_effs, mr_ci=t(ci_effs)) %>%
    #   rownames_to_column(var = "estimand")
    
    # --------------------------------------------------------------------------
    # 8. DERIVE REGRESSION-BASED AND WEIGHTING ESTIMATES
    # --------------------------------------------------------------------------
    regs_effs <- effect(regs)
    reg_results <- data.frame(
        reg_est = sapply(regs_effs, mean),
        reg_se = sapply(regs_effs, function(s) { sqrt(stats::var(s) / nrow(data)) }),
        reg_ci = t(sapply(regs_effs, function(s) {
            mean(s) + c(-1, 1) * stats::qnorm(0.975) * sqrt(stats::var(s) / nrow(data))
            # mean_s <- mean(s)
            # se_s <- sqrt(stats::var(s) / nrow(data))
            # ci_half <- stats::qnorm(0.975) * se_s
            # c(mean_s - ci_half, mean_s + ci_half)
        }))
    ) %>%
        tibble::rownames_to_column(var = "estimand")
    
    # Weighting
    rmpw_effs <- (effect(rmpw))   #rmpw_effs <- effect(rmpw)
    rmpw_results <- data.frame(
        rmpw_est = sapply(rmpw_effs, mean),
        rmpw_se = sapply(rmpw_effs, function(s) { sqrt(stats::var(s) / nrow(data)) }),
        rmpw_ci = t(sapply(rmpw_effs, function(s) {
            mean(s) + c(-1, 1) * stats::qnorm(0.975) * sqrt(stats::var(s) / nrow(data))
            # mean_s <- mean(s)
            # se_s <- sqrt(stats::var(s) / nrow(data))
            # ci_half <- stats::qnorm(0.975) * se_s
            # c(mean_s - ci_half, mean_s + ci_half)
        }))
    ) %>%
        tibble::rownames_to_column(var = "estimand")
    
    # --------------------------------------------------------------------------
    # 9. RETURN RESULTS
    # --------------------------------------------------------------------------
    # out <- mget(ls(envir = environment()))
    out <- list(
        mr_results = mr_results,
        reg_results = reg_results,
        rmpw_results = rmpw_results
    )
    
    return(out)
}

