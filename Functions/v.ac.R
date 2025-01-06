
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2025-01-03: did not modify anything yet  


v.ac <- function(a=1, astar=0, mu_mac, data_in, varnames, Yfamily = "gaussian", ipw = NULL, cluster_opt = "FE.glm",
                 folds, learners, bounded = FALSE,
                 full.sample = FALSE) {
    
    data_in[["mu"]] <- mu_mac[, glue("mu(m,a{a},c)")]
    data_in <- data_in %>%
        group_by(!!as.name(Sname)) %>%
        mutate(across(c("mu"), list(clmean = ~mean(.), cwc = ~.-mean(.))))
    
    # tvals <- expand.grid(tt=c(0,1))
    v_ac <- matrix(nrow = nrow(data_in), ncol = 1)
    colnames(v_ac) <- glue("v_ac(c)")
    v <- 1
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
                              type = "gaussian",
                              learners, bounded)
        }
        
        
        if (full.sample == FALSE) {
            # predict_subset <- ( (train[[varnames$tt]] == tt) & (train[[varnames$R]] == rstar) )
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
