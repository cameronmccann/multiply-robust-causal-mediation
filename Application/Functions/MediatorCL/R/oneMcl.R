
    oneM.cl <- function(data,
                          Sname,
                          Wnames = NULL, Xnames,
                          Aname,
                          Mnames,
                          Yname, Yfamily = "gaussian",
                          cluster_opt_a = "cwc.FE",
                          cluster_opt_m = "cwc.FE",
                          cluster_opt_y = "cwc.FE",
                          cluster_opt_v = "cwc",
                          interaction_fity = c("AM"),
                          num_folds = 1,
                          # bounds ,
                          learners_a = c("SL.glm"),
                          learners_m = c("SL.glm"),
                          learners_y = c("SL.glm"),
                          contrast_a = c(a=1, astar=0)
) {
  set.seed(12345)

  # add interactions
  data$id <- 1:nrow(data)
  data_in <- data
  AM <- data_in[[Aname]] * data_in[, Mnames, drop=FALSE]
  colnames(AM) <- glue("AM.{Mnames}")

  # dummy indicators
  Sdumm <- dummy_cols(data[[Sname]], remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  colnames(Sdumm) <- paste0("S", 1:ncol(Sdumm))
  Sname_dummies <- colnames(Sdumm)

  # cluster means
  data_in <- data.frame(data, AM) %>%
    group_by(!!as.name(Sname)) %>%
    mutate(across(c(!id), list(clmean = ~mean(.), cwc = ~.-mean(.)))) %>%
    bind_cols(Sdumm)

  data_in[[Sname]] <- match(data[[Sname]], unique(data[[Sname]]))
  data_in[[Sname]] <- as.factor(data_in[[Sname]])

  varnames <- list("A" = Aname, "M" = Mnames, "Y" = Yname,
                   "AM" = paste0("AM.", Mnames),
                   "S" = Sname, "Sdumm" = Sname_dummies,
                   "X" = Xnames, "W" = Wnames)

  Cnames <- list("X" = Xnames, "W" = Wnames, "Sdumm" = Sname_dummies)

  #
  if (num_folds > 1) {
    folds <- make.fold_K(data_in, Sname, cv_folds = num_folds)
  }


  if (num_folds<=1) {
    folds <- make_folds(cluster_ids = data[[Sname]], fold_fun = folds_vfold, V = 1)
    folds[[1]]$training_set <- folds[[1]]$validation_set
  }
  # try
  # train <- valid <- data_in

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


  # eif for each cluster -------------
  # and then pull cluster-specific eifs for inference (Balzer et al. 2019)
  cluster_sizes <- table(data_in[[Sname]])
  trtprop <- aggregate(data_in[[varnames$A]], by=list(data_in[[Sname]]), mean)[,2]
  J <- length(unique(data_in[[Sname]]))
  eligible_clusters <- c(1:J)[trtprop>0&trtprop<1]

  cluster.k <- function(k=1) {
    ind_k <- which(data_in[[Sname]] == unique(data_in[[Sname]])[k])
    nk <- length(ind_k) # 1/nk is the weight for pooling the cluster-specific eifs

    if (trtprop[k]>0 & trtprop[k]<1) {
      eif_k <- eif(data_in[ind_k, ], varnames,
                   a_c[ind_k, ],
                   a_mc[ind_k, ],
                   mu_mac[ind_k, ],
                   v_ac[ind_k, , drop=FALSE],
                   mu_ac[ind_k, ]
                   )
      # map(eif_k$eifs, ~sum(.)/nk) # equivalently:
      meank <- map_dbl(effect(eif_k$eifs), ~mean(., na.rm=T))
    } else {
      meank <- rep(NA, 5)
    }

    # vark <- map_dbl(effect(eif_k$eifs), ~var(., na.rm=T))
    meank
  }

  eif_clmean <- map(eligible_clusters, cluster.k)
  eif_clmean <- do.call(rbind, eif_clmean)
  sizes <- cluster_sizes[eligible_clusters]

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
      mr_waldci1_individual = mr_individual.est-qt(0.975, df=df_eff)*mr_individual.se*adjse,
      mr_waldci2_individual = mr_individual.est+qt(0.975, df=df_eff)*mr_individual.se*adjse,
      mr_waldci1_cluster = mr_cluster.est-qt(0.975, df=df_eff)*mr_cluster.se*adjse,
      mr_waldci2_cluster = mr_cluster.est+qt(0.975, df=df_eff)*mr_cluster.se*adjse
    )

  # eif full -----
  eif_full <- eif(data_in, varnames,
                  a_c,
                  a_mc,
                  mu_mac,
                  v_ac[, , drop=FALSE],
                  mu_ac)
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

  # regression
  regs_effs <- effect(regs)

  reg_results <- data.frame(
    reg_est = sapply(regs_effs, mean),
    reg_se = sapply(regs_effs, function(s) { sqrt(var(s) / nrow(data)) }),
    reg_ci = t(sapply(regs_effs, function(s) {
      mean(s) + c(-1, 1) * qnorm(0.975) * sqrt(var(s) / nrow(data))
    }))
  ) %>%
    rownames_to_column(var = "estimand")

  # weighting
  rmpw_effs <- (effect(rmpw))

  rmpw_results <- data.frame(
    rmpw_est = sapply(rmpw_effs, mean),
    rmpw_se = sapply(rmpw_effs, function(s) { sqrt(var(s) / nrow(data)) }),
    rmpw_ci = t(sapply(rmpw_effs, function(s) {
      mean(s) + c(-1, 1) * qnorm(0.975) * sqrt(var(s) / nrow(data))
    }))
  ) %>%
    rownames_to_column(var = "estimand")


  # out <- mget(ls(envir = environment()))
  out <- list(mr_results=mr_results, reg_results=reg_results, rmpw_results=rmpw_results)

  return(out)
}



effect <- function(thetas) {
  thetas[["DE(,M(0))"]] <- thetas[["Y(1,M(0))"]] - thetas[["Y(0,M(0))"]]
  thetas[["IE(1,)"]] <- thetas[["Y(1,M(1))"]] - thetas[["Y(1,M(0))"]]

  thetas
}
