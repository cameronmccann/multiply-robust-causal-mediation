
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2025-01-09: ony modified `type` argument in crossfit() to take on `Yfamily` instead of "gaussian"; did not modify anything else yet  
# On 2025-07-21: added arguments (like random_slope_vars) to be past to other crossfit function so users can set random slopes for specific variables & source_label argument for warning messages


v.ac <- function(a=1, astar=0, mu_mac, data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                 folds, learners, bounded = FALSE,
                 full.sample = FALSE, random_slope_vars = random_slope_vars, source_label = "v.ac") {
    
    # data_in[["mu"]] <- mu_mac[, glue("mu(m,a{a},c)")]
    col_name <- glue::glue("mu(m,a{a},c)")
    if (!col_name %in% colnames(mu_mac)) {
        stop(sprintf("Column %s not found in mu_mac", col_name))
    }
    data_in[["mu"]] <- mu_mac[, col_name]
    
    data_in <- data_in %>%
        # dplyr::group_by(!!as.name(Sname)) %>% # Note this was an error in Dr. Liu's package sent
        group_by(!!as.name(varnames$S)) %>% # Changed Sname => varnames$Sname #dplyr::group_by(!!rlang::sym(varnames$Sname)) |>
        dplyr::mutate(dplyr::across(c("mu"), list(clmean = ~mean(.), cwc = ~.-mean(.))))
    
    # tvals <- expand.grid(tt=c(0,1))
    v_ac <- matrix(nrow = nrow(data_in), ncol = 1)
    colnames(v_ac) <- glue::glue("v_ac(c)")
    v <- 1
    # ═══════════════════
    #    Debugging version of code 
    # ═══════════════════
    # for (v in seq_along(folds)) {
    #     train <- origami::training(data_in, folds[[v]])
    #     valid <- origami::validation(data_in, folds[[v]])
    #     
    #     valid_list <- lapply(1:1, function(jj=1) {
    #         valid_v <- valid
    #         valid_v[, c(varnames$A)] <- astar
    #         valid_v
    #     })
    #     
    #     if (is.null(unique(valid_list[[1]][[varnames$A]]))) {
    #         stop("Invalid value for A in validation set.")
    #     }
    #     
    #     predict_subset <- (train[[varnames$A]] == unique(valid_list[[1]][[varnames$A]]))
    #     
    #     if (sum(predict_subset) == 0) {
    #         stop(glue("No matching rows in train where A equals astar for fold {v}."))
    #     }
    #     
    #     alist <- crossfit(
    #         train[predict_subset, ], valid_list,
    #         "mu",
    #         c(varnames$X),
    #         varnames,
    #         ipw,
    #         cluster_opt,
    #         type = Yfamily,
    #         learners, bounded
    #     )
    #     
    #     if (is.null(alist$preds) || nrow(alist$preds) == 0) {
    #         stop(glue("Error: `crossfit()` returned no predictions for fold {v}."))
    #     }
    #     
    #     v_ac[folds[[v]]$validation_set, glue("v_ac(c)")] <- alist$preds[, 1]
    # }
    # ═══════════════════
    #    original code 
    # ═══════════════════

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
                              c(varnames$A,
                                varnames$X),
                              varnames,
                              ipw,
                              cluster_opt,
                              # type = "gaussian",
                              type = Yfamily,
                              learners, bounded, 
                              random_slope_vars = random_slope_vars, 
                              source_label = source_label)
        }


        if (full.sample == FALSE) {
            # predict_subset <- ( (train[[varnames$tt]] == tt) & (train[[varnames$R]] == rstar) )
            # predict_subset <- (train[[varnames$A]] == unique(valid_list[[1]][[varnames$A]]))
            if (is.null(unique(valid_list[[1]][[varnames$A]]))) {
                stop("Invalid value for A in validation set.")
            }
            predict_subset <- (train[[varnames$A]] == unique(valid_list[[1]][[varnames$A]]))

            ##
            train_sub <- train[predict_subset, ]
            message("In v.ac() fold ", v, ": nrow(train_sub) = ", nrow(train_sub))
            if (nrow(train_sub) == 0) {
                stop("Predict subset is empty—cannot fit a model.")
            }
            
            ##
            if (sum(predict_subset) == 0) {
                stop(glue("No matching rows in train where A equals astar for fold {v}."))
            }

            alist <- crossfit(train[predict_subset, ], valid_list,
                              "mu",
                              c(varnames$X),
                              varnames,
                              ipw,
                              cluster_opt,
                              type = "gaussian",
                              learners, bounded, 
                              random_slope_vars = random_slope_vars)
        }

        # ADD DEBUGGING
        if (is.null(alist$preds) || nrow(alist$preds) == 0) {
            stop("Error: `crossfit()` returned no predictions.")
        }

        preds <- alist$preds
        v_ac[folds[[v]]$validation_set, glue("v_ac(c)")] <- preds[, 1]

    }
    
    v_ac
}
