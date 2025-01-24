################################################################################
################### Compare current func to mediator_ClusterData ###############
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 2025-01-13
#
#
# Script Description: 
#   In this script I will be comparing the current data generation, true value 
# calculation, & effect estimation functions to Dr Liu's (in mediator_ClusterData R project) 
# to make sure the current functions are not differing in important ways. 
#
#
# Last Updated: 2025-01-13
#
#
# Notes/Summary:
# ## Data generation:
# The mediator & outcome values differ between functions. 
# It may partly be due to the fact that the int.XZ argument is not correctly able to set "m_on_anj" to 0 in generate_data2.0c()
# However, mediator values still differ when int.XZ matches btw functions
# 
#
#
################################################################################


# Set up packages & functions ---------------------------------------------

# ══════════════════════════════
#    Load packages 
# ══════════════════════════════
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    doParallel, 
    foreach,
    parallel, 
    purrr, # for map()
    glue, # for glue()
    dplyr, 
    readr, 
    ggplot2, 
    fastDummies, # for Dr Liu's package/function (specifically, for dummy_cols() in fastDummies package)
    stringr, # for str_detect() in Dr Liu's package/function
    tibble # for rownames_to_column() in Dr Liu's package/function
)

# ══════════════════════════════
#     Source Updated Functions
# ══════════════════════════════
function_names <- c(
    # "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    # "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c", 
    
    # Estimation functions 
    "crossfit", 
    "make_fold_K", 
    "eif", 
    "a.c",
    "a.mc",
    "mu.mac",
    "v.ac", 
    "mu.ac",
    "get_inference", 
    "internal_estimate_mediation", 
    "bound", 
    "effect", 
    "estimate_mediation"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# Script for dr lius functions 
source("Functions/fun_gen.R")


# ══════════════════════════════
#     Simulation Conditions & Methods
# ══════════════════════════════
# conditions_all <- data.frame(rbind(
#     expand.grid(
#         J = c(10, 20, 40),
#         Nj_low = c(50),
#         Nj_high = c(100), 
#         quadratic = c(F), #c(T, F), 
#         Mfamily = c("binomial", "gaussian"),
#         Yfamily = c("binomial", "gaussian")
#     ),
#     expand.grid(
#         J = c(40, 70, 100),
#         Nj_low = c(5),
#         Nj_high = c(20), 
#         quadratic = c(F), #c(T, F), 
#         Mfamily = c("binomial", "gaussian"),
#         Yfamily = c("binomial", "gaussian")
#     )
# ))
# 
# # limit conditions for testing 
# # conditions <- conditions[1:10, ]
# ## binomial M & Y 
# # conditions <- conditions[1:3, ]
# ## focus on binomial M
# # conditions <- conditions[conditions$Mfamily == "binomial", ]
# # conditions <- conditions_all[1:2, ]
# ## focus on binomial M & gaussian Y (like Dr Liu sim)
# conditions <- conditions_all[conditions_all$Mfamily == "binomial" & conditions_all$Yfamily == "gaussian", ]

# This code is from dr lius (sim.R)
condition_all <- data.frame(expand.grid(
    J = c(100, 70,40,  20, 10),
    nj = c(5),
    # generating data
    quadratic.A = c(F, T),
    quadratic.M = c(F, T),
    quadratic.Y = c(F, T),
    icc = c(0.2), #c(0.2, 0.4),
    x_z = 0, #c(0, 0.3),
    Yfamily = c("gaussian"),
    indep_M = c(F),
    int.XZ = c(T),
    if.null = c(F,T)
))

condition <- condition_all %>%
    filter((quadratic.A + quadratic.M + quadratic.Y == 0) | (quadratic.A + quadratic.M + quadratic.Y == 0)) %>%
    filter( if.null == F) %>% 
    filter(J %in% c(70)
           # ((J == 10) &nj %in% c(20)) #| ((J == 50)&nj %in% c(10)) 
    ) %>% 
    mutate(nj = 5) # nj=5, 50



# Methods 
# methds_all <- data.frame(expand.grid(
#     cluster_a = "FE", #c("FE", "RE", "noncluster"), # "RE", # "noncluster", #
#     cluster_m = "FE", # c("FE", "RE", "noncluster"), # "RE", #  "noncluster.mlr", #
#     cluster_y =  "FE", #c("FE", "RE", "noncluster"), # "noncluster.mlr", ## "FE.mlr", #
#     # interact_fitm2 =  c(T), # NULL, #
#     # interact_fity = c(T), # NULL, #
#     # Morder = c("21", "12"),
#     Fit = c("mlr","glm"), # 
#     # cluster_opt_a = c("sufficient_stats",  "cwc.FE"), # "FE.glm", #  
#     # cluster_opt_m = c("sufficient_stats",  "cwc.FE"),  #"FE.glm", # 
#     # cluster_opt_y = c("sufficient_stats",  "cwc.FE") # "cwc.FE"#c("sufficient_stats") #, 
#     cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
# )) %>% 
#     mutate(
#         cluster_opt_a = cluster_opt, 
#         cluster_opt_m= cluster_opt, 
#         cluster_opt_y=cluster_opt
#     )
# 
# # (methds <- methds_all %>%
# #     filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") ))
# # methds <- methds_all
# # limiting to mlr & cwc (like Dr Liu's)
# methds <- methds_all[methds_all$Fit =="mlr" & methds_all$cluster_opt =="cwc", ] 

# This code is from dr lius (sim.R)
methds_all <- data.frame(expand.grid(
    cluster_a = "FE", #c("FE", "RE", "noncluster"), # "RE", # "noncluster", #
    cluster_m = "FE", # c("FE", "RE", "noncluster"), # "RE", #  "noncluster.mlr", #
    cluster_y =  "FE", #c("FE", "RE", "noncluster"), # "noncluster.mlr", ## "FE.mlr", #
    interact_fitm2 =  c(T), # NULL, #
    interact_fity = c(T), # NULL, #
    # Morder = c("21", "12"),
    Fit = c("mlr","glm"), # 
    # cluster_opt_a = c("sufficient_stats",  "cwc.FE"), # "FE.glm", #  
    # cluster_opt_m = c("sufficient_stats",  "cwc.FE"),  #"FE.glm", # 
    # cluster_opt_y = c("sufficient_stats",  "cwc.FE") # "cwc.FE"#c("sufficient_stats") #, 
    cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
)) %>% 
    mutate(
        cluster_opt_a = cluster_opt, cluster_opt_m= cluster_opt, cluster_opt_y=cluster_opt
    )

methds <- methds_all %>% 
    filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") )



# set.seed(12)
# datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))
# 
# iseed <-9
# cond <- 1




# Data generation ---------------------------------------------------------



GenData <- function(
        seedone = 123,
        J = 100,
        nj = 30,
        quadratic.A = FALSE,
        quadratic.M = FALSE,
        quadratic.Y = FALSE,
        icca = 0.2,
        iccm = 0.2, 
        iccy = 0.2,
        x_z = 0,
        num_m = 2,
        num_x = 3,
        Yfamily = "gaussian",
        indep_M = FALSE,
        int.XZ = FALSE,
        if.null = FALSE
) {
    
    set.seed(seed = seedone)
    
    if (nj==5) { 
        njrange <- c(5, 20)
    }
    
    if (nj == 20) {
        njrange <- c(20, 40)
    }
    
    if (nj == 50) {
        njrange <- c(50, 100)
    }
    
    nj_sizes <- runif(J, njrange[1], njrange[2]) %>% round()
    
    N <- sum(nj_sizes)
    
    data <- data.frame(
        id = 1:N,
        school = unlist(map(1:J, ~rep(.x, each = nj_sizes[.x])))
    ) %>% 
        group_by(school) %>% 
        mutate(W_nj = (n()-njrange[1])/(njrange[2]-njrange[1]) )
    
    
    
    # Z (unobserved) cluster-level confounders -----------------
    # z <- rep(rnorm(J, sd = 1), each = nj)
    data$Z <- unlist(map(1:J, ~rep(rnorm(1), each = nj_sizes[.x])))
    
    # X individual-level confounders ------
    iccx <- 0.2
    # x_z <- 0.5
    gen_x <- list(iccx = iccx, x_z = x_z)
    
    xb <- mvtnorm::rmvnorm(n = J,
                           mean = rep(0, num_x),
                           sigma = diag((1 - gen_x[["x_z"]]^2)*gen_x[["iccx"]], nrow = num_x))[data$school, ] #[rep(1:J, each = nj), ]
    xe <- mvtnorm::rmvnorm(n = N,
                           mean = rep(0, num_x),
                           sigma = diag(1 - gen_x[["iccx"]], nrow = num_x))
    x <- gen_x[["x_z"]] * data$Z + xb + xe
    data$X <- x
    
    
    # Treatment ---------
    # icca <- 0.2
    gen_a <- list(icca = icca, a_x = sqrt(0.15 * 1 / num_x), a_z = sqrt(0.4 / 1))
    # ab <- rep(rnorm(J, sd = sqrt(gen_a[["icca"]])), each = nj)
    ab <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])), each = nj_sizes[.x])))
    if (quadratic.A == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        a_given <- ab +  gen_a[["a_x"]] * rowSums(Xlinear) + gen_a[["a_z"]] * data$Z
    }
    if (quadratic.A == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4) # mean(rnorm(.)^2) = 1; var(rnorm(.)^2) = 2
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_a[["a_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        a_given <- ab +  gen_a[["a_x"]] * rowSums(Xquad) + gen_a[["a_z"]] * data$Z
    }
    
    data$A <- rbinom(N, 1, pa(1, a_given, gen_a))
    
    # Mediators ------------------
    # iccm <- 0.2
    iccm1 <- iccm2 <- iccm
    gen_m <- list(iccm1 = iccm1, iccm2 = iccm2,
                  m1_on_a = 0.2, m1_on_x = sqrt(0.15 / num_x), m1_on_z = sqrt(0.4),
                  m1_on_az = 0.2, m1_on_anj = 0.2,
                  # M2
                  m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
                  # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
                  m2_on_x = sqrt(0.15 / num_x), m2_on_z = sqrt(0.4),
                  m2_on_az = 0.2, m2_on_anj = 0.2
    )
    # conditional independent mediators
    if (indep_M) {
        gen_m[c("m2_on_m1","m2_on_am1")] <- 0
    }
    
    if (int.XZ==FALSE) {
        # gen_m[c("m1_on_az","m2_on_az")] <- 0
        gen_m[c("m1_on_anj","m2_on_anj")] <- 0
    }
    # m1b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm1"]])), each = nj)
    # m2b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm2"]])), each = nj)
    m1b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm1"]])), each = nj_sizes[.x])))
    m2b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm2"]])), each = nj_sizes[.x])))
    
    if (quadratic.M == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4)
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_m[["m1_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        # gen_m[["m2_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        
        m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xquad) +
            gen_m[["m1_on_z"]] * data$Z
        data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
        
        m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xquad) +
            gen_m[["m2_on_z"]] * data$Z
        data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
        
    }
    if (quadratic.M == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
            gen_m[["m1_on_z"]] * data$Z
        # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
        data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
        
        m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
            gen_m[["m2_on_z"]] * data$Z
        data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
    }
    
    # Outcome -------------------
    
    y_on_amint <- 0
    # iccy <- 0.2
    gen_y <- list(iccy = iccy, yintercept = 1,
                  # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
                  y_on_a = 0.2, y_on_m1 = 1, y_on_m2 = 1,
                  y_on_am1 = 0, y_on_am2 = 0, 
                  y_on_m1m2 = 0.2, y_on_am1m2 = 0.2,
                  # y_on_m1m2 = 0, y_on_am1m2 = 0,
                  y_on_az = 0.2, y_on_m1z = 0.2, y_on_m2z = 0.2,
                  y_on_anj = 0.2,  
                  y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
    )
    yb <- rnorm(J, sd = sqrt(gen_y[["iccy"]]))[data$school] #[rep(1:J, each = nj)]
    
    if (if.null==TRUE) {
        gen_y <- list(iccy = iccy, yintercept = 1,
                      # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
                      y_on_a = 0, y_on_m1 = 0, y_on_m2 = 0,
                      y_on_am1 = 0, y_on_am2 = 0, 
                      y_on_m1m2 = 0, y_on_am1m2 = 0,
                      # y_on_m1m2 = 0, y_on_am1m2 = 0,
                      y_on_az = 0, y_on_m1z = 0, y_on_m2z = 0,
                      y_on_anj = 0,  
                      y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
        )
    }
    
    if (int.XZ==FALSE) {
        # gen_y[c("y_on_az","y_on_m1z","y_on_m2z")] <- 0
        gen_y[c("y_on_anj")] <- 0
    }
    
    if (quadratic.Y == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4)
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_y[["y_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        
        y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xquad) +
            gen_y[["y_on_z"]] * data$Z
    }
    if (quadratic.Y == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        
        y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xlinear) +
            gen_y[["y_on_z"]] * data$Z
    }
    
    if (Yfamily == "gaussian") {
        condmy <- my(m2 = data$M2, m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = FALSE)
        data$Y <- condmy + rnorm(N, sd = sqrt(1 - gen_y[["iccy"]]))
    }
    if (Yfamily == "binomial") {
        condmy <- my(m2 = data$M2, m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = TRUE)
        data$Y <- rbinom(N, 1, condmy)
    }
    
    datobs <- do.call(data.frame, data)
    
    
    # nj_sizes <- as.numeric(table(data$school))
    
    trueVals <- function() {
        
        a_vals <- rbind(data.frame(a0 = c(1, 1, 0),
                                   
                                   ajo = c(0, 1, 0)))
        truevals <- truevals_individual <- truevals_cluster <- list()
        j <- 1
        for (j in 1:nrow(a_vals)) {
            a0 <- a_vals$a0[j]
            
            ajo <- a_vals$ajo[j]
            
            vals <- expand.grid(m1 = c(0, 1), m2 = c(0, 1))
            
            if (!is.na(ajo)) {
                # integrate over the joint mediator distribution, (M1, M2) under ajo
                intm1m2 <- sapply(1:nrow(vals), function(i=1) {
                    m1 <- vals$m1[i]
                    m2 <- vals$m2[i]
                    p_joint <- pm1(m1, ajo, data$Z, data$W_nj, m1_given, gen_m) *
                        pm2(m2, m1, ajo, data$Z, data$W_nj, m2_given, gen_m)
                    
                    p_joint * my(m2 = vals$m2[i], m1 = vals$m1[i], a = a0, data$Z, data$W_nj,
                                 y_given, gen_y, binary = (Yfamily=="binomial"))
                    
                }, simplify = TRUE)
                int_m1m2 <- rowSums(intm1m2)
                truevals_individual[[glue("Y({a0},M({ajo}))")]] <- mean(int_m1m2)
                truevals_cluster[[glue("Y({a0},M({ajo}))")]] <- mean(aggregate(data.frame(int_m1m2), by=list(data$school), mean)[,2])
            }
        }
        
        
        list(truevals_individual=truevals_individual, truevals_cluster=truevals_cluster)
    }
    
    truevals <- trueVals()
    
    out <- mget(ls(envir = environment()))
    
    return(out)
    
}

## Lius func ---------------------------------------------------------------

# GenData <- function(
#         seedone = 123,
#         J = 100,
#         nj = 30,
#         quadratic.A = FALSE,
#         quadratic.M = FALSE,
#         quadratic.Y = FALSE,
#         icca = 0.2,
#         iccm = 0.2, 
#         iccy = 0.2,
#         x_z = 0,
#         num_m = 2,
#         num_x = 3,
#         Yfamily = "gaussian",
#         indep_M = FALSE,
#         int.XZ = FALSE,
#         if.null = FALSE
# ) {
#     
#     set.seed(seed = seedone)
#     
#     if (nj==5) { 
#         njrange <- c(5, 20)
#     }
#     
#     if (nj == 20) {
#         njrange <- c(20, 40)
#     }
#     
#     if (nj == 50) {
#         njrange <- c(50, 100)
#     }
#     
#     nj_sizes <- runif(J, njrange[1], njrange[2]) %>% round()
#     
#     N <- sum(nj_sizes)
#     
#     data <- data.frame(
#         id = 1:N,
#         school = unlist(map(1:J, ~rep(.x, each = nj_sizes[.x])))
#     ) %>% 
#         group_by(school) %>% 
#         mutate(W_nj = (n()-njrange[1])/(njrange[2]-njrange[1]) )
#     
#     
#     
#     # Z (unobserved) cluster-level confounders -----------------
#     # z <- rep(rnorm(J, sd = 1), each = nj)
#     data$Z <- unlist(map(1:J, ~rep(rnorm(1), each = nj_sizes[.x])))
#     
#     # X individual-level confounders ------
#     iccx <- 0.2
#     # x_z <- 0.5
#     gen_x <- list(iccx = iccx, x_z = x_z)
#     
#     xb <- mvtnorm::rmvnorm(n = J,
#                            mean = rep(0, num_x),
#                            sigma = diag((1 - gen_x[["x_z"]]^2)*gen_x[["iccx"]], nrow = num_x))[data$school, ] #[rep(1:J, each = nj), ]
#     xe <- mvtnorm::rmvnorm(n = N,
#                            mean = rep(0, num_x),
#                            sigma = diag(1 - gen_x[["iccx"]], nrow = num_x))
#     x <- gen_x[["x_z"]] * data$Z + xb + xe
#     data$X <- x
#     
#     
#     # Treatment ---------
#     # icca <- 0.2
#     gen_a <- list(icca = icca, a_x = sqrt(0.15 * 1 / num_x), a_z = sqrt(0.4 / 1))
#     # ab <- rep(rnorm(J, sd = sqrt(gen_a[["icca"]])), each = nj)
#     ab <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])), each = nj_sizes[.x])))
#     if (quadratic.A == FALSE) {
#         Xlinear <- data$X
#         # if (int.XZ) {
#         #   Xlinear <- data$X * data$Z
#         # }
#         a_given <- ab +  gen_a[["a_x"]] * rowSums(Xlinear) + gen_a[["a_z"]] * data$Z
#     }
#     if (quadratic.A == TRUE) {
#         Xquad <- (data$X ^ 2 - 1) / sqrt(4) # mean(rnorm(.)^2) = 1; var(rnorm(.)^2) = 2
#         # if (int.XZ) {
#         #   Xquad <- Xquad * data$Z
#         # }
#         # previous
#         # Xquad <- Xquad * data$Z
#         # gen_a[["a_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
#         a_given <- ab +  gen_a[["a_x"]] * rowSums(Xquad) + gen_a[["a_z"]] * data$Z
#     }
#     
#     data$A <- rbinom(N, 1, pa(1, a_given, gen_a))
#     
#     # Mediators ------------------
#     # iccm <- 0.2
#     iccm1 <- iccm2 <- iccm
#     gen_m <- list(iccm1 = iccm1, iccm2 = iccm2,
#                   m1_on_a = 0.2, m1_on_x = sqrt(0.15 / num_x), m1_on_z = sqrt(0.4),
#                   m1_on_az = 0.2, m1_on_anj = 0.2,
#                   # M2
#                   m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
#                   # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
#                   m2_on_x = sqrt(0.15 / num_x), m2_on_z = sqrt(0.4),
#                   m2_on_az = 0.2, m2_on_anj = 0.2
#     )
#     # conditional independent mediators
#     if (indep_M) {
#         gen_m[c("m2_on_m1","m2_on_am1")] <- 0
#     }
#     
#     if (int.XZ==FALSE) {
#         # gen_m[c("m1_on_az","m2_on_az")] <- 0
#         gen_m[c("m1_on_anj","m2_on_anj")] <- 0
#     }
#     # m1b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm1"]])), each = nj)
#     # m2b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm2"]])), each = nj)
#     m1b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm1"]])), each = nj_sizes[.x])))
#     m2b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm2"]])), each = nj_sizes[.x])))
#     
#     if (quadratic.M == TRUE) {
#         Xquad <- (data$X ^ 2 - 1) / sqrt(4)
#         # if (int.XZ) {
#         #   Xquad <- Xquad * data$Z
#         # }
#         # previous
#         # Xquad <- Xquad * data$Z
#         # gen_m[["m1_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
#         # gen_m[["m2_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
#         
#         m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xquad) +
#             gen_m[["m1_on_z"]] * data$Z
#         data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
#         
#         m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xquad) +
#             gen_m[["m2_on_z"]] * data$Z
#         data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
#         
#     }
#     if (quadratic.M == FALSE) {
#         Xlinear <- data$X
#         # if (int.XZ) {
#         #   Xlinear <- data$X * data$Z
#         # }
#         m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
#             gen_m[["m1_on_z"]] * data$Z
#         # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
#         data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
#         
#         m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
#             gen_m[["m2_on_z"]] * data$Z
#         data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
#     }
#     
#     # Outcome -------------------
#     
#     y_on_amint <- 0
#     # iccy <- 0.2
#     gen_y <- list(iccy = iccy, yintercept = 1,
#                   # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
#                   y_on_a = 0.2, y_on_m1 = 1, y_on_m2 = 1,
#                   y_on_am1 = 0, y_on_am2 = 0, 
#                   y_on_m1m2 = 0.2, y_on_am1m2 = 0.2,
#                   # y_on_m1m2 = 0, y_on_am1m2 = 0,
#                   y_on_az = 0.2, y_on_m1z = 0.2, y_on_m2z = 0.2,
#                   y_on_anj = 0.2,  
#                   y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
#     )
#     yb <- rnorm(J, sd = sqrt(gen_y[["iccy"]]))[data$school] #[rep(1:J, each = nj)]
#     
#     if (if.null==TRUE) {
#         gen_y <- list(iccy = iccy, yintercept = 1,
#                       # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
#                       y_on_a = 0, y_on_m1 = 0, y_on_m2 = 0,
#                       y_on_am1 = 0, y_on_am2 = 0, 
#                       y_on_m1m2 = 0, y_on_am1m2 = 0,
#                       # y_on_m1m2 = 0, y_on_am1m2 = 0,
#                       y_on_az = 0, y_on_m1z = 0, y_on_m2z = 0,
#                       y_on_anj = 0,  
#                       y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
#         )
#     }
#     
#     if (int.XZ==FALSE) {
#         # gen_y[c("y_on_az","y_on_m1z","y_on_m2z")] <- 0
#         gen_y[c("y_on_anj")] <- 0
#     }
#     
#     if (quadratic.Y == TRUE) {
#         Xquad <- (data$X ^ 2 - 1) / sqrt(4)
#         # if (int.XZ) {
#         #   Xquad <- Xquad * data$Z
#         # }
#         # previous
#         # Xquad <- Xquad * data$Z
#         # gen_y[["y_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
#         
#         y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xquad) +
#             gen_y[["y_on_z"]] * data$Z
#     }
#     if (quadratic.Y == FALSE) {
#         Xlinear <- data$X
#         # if (int.XZ) {
#         #   Xlinear <- data$X * data$Z
#         # }
#         
#         y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xlinear) +
#             gen_y[["y_on_z"]] * data$Z
#     }
#     
#     if (Yfamily == "gaussian") {
#         condmy <- my(#m2 = data$M2, 
#             m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = FALSE)
#         data$Y <- condmy + rnorm(N, sd = sqrt(1 - gen_y[["iccy"]]))
#     }
#     if (Yfamily == "binomial") {
#         condmy <- my(m2 = data$M2, m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = TRUE)
#         data$Y <- rbinom(N, 1, condmy)
#     }
#     
#     datobs <- do.call(data.frame, data)
#     
#     
#     # nj_sizes <- as.numeric(table(data$school))
#     
#     # trueVals <- function() {
#     #     
#     #     a_vals <- rbind(data.frame(a0 = c(1, 1, 0),
#     #                                
#     #                                ajo = c(0, 1, 0)))
#     #     truevals <- truevals_individual <- truevals_cluster <- list()
#     #     j <- 1
#     #     for (j in 1:nrow(a_vals)) {
#     #         a0 <- a_vals$a0[j]
#     #         
#     #         ajo <- a_vals$ajo[j]
#     #         
#     #         vals <- expand.grid(m1 = c(0, 1), m2 = c(0, 1))
#     #         
#     #         if (!is.na(ajo)) {
#     #             # integrate over the joint mediator distribution, (M1, M2) under ajo
#     #             intm1m2 <- sapply(1:nrow(vals), function(i=1) {
#     #                 m1 <- vals$m1[i]
#     #                 m2 <- vals$m2[i]
#     #                 p_joint <- pm1(m1, ajo, data$Z, data$W_nj, m1_given, gen_m) *
#     #                     pm2(m2, m1, ajo, data$Z, data$W_nj, m2_given, gen_m)
#     #                 
#     #                 p_joint * my(m2 = vals$m2[i], m1 = vals$m1[i], a = a0, data$Z, data$W_nj,
#     #                              y_given, gen_y, binary = (Yfamily=="binomial"))
#     #                 
#     #             }, simplify = TRUE)
#     #             int_m1m2 <- rowSums(intm1m2)
#     #             truevals_individual[[glue("Y({a0},M({ajo}))")]] <- mean(int_m1m2)
#     #             truevals_cluster[[glue("Y({a0},M({ajo}))")]] <- mean(aggregate(data.frame(int_m1m2), by=list(data$school), mean)[,2])
#     #         }
#     #     }
#     #     
#     #     
#     #     list(truevals_individual=truevals_individual, truevals_cluster=truevals_cluster)
#     # }
#     # 
#     # truevals <- trueVals()
#     
#     out <- mget(ls(envir = environment()))
#     
#     return(out)
#     
# }



## Test/comparison ---------------------------------------------------------

set.seed(12)
datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

iseed <- 9
cond <- 1

# ══════════════════════════════
#    Dr Lius generate data  
# ══════════════════════════════
# mediator_ClusterData GenData()
seedone <- datseeds[iseed]
gen_data <- GenData(
    seedone = seedone, # datseeds[iseed],
    J = condition$J[cond],
    nj = condition$nj[cond],
    quadratic.A = condition$quadratic.A[cond],
    quadratic.M = condition$quadratic.M[cond],
    quadratic.Y = condition$quadratic.Y[cond],
    icca = condition$icc[cond],
    iccm = condition$icc[cond],
    iccy = condition$icc[cond],
    x_z = condition$x_z[cond],
    Yfamily = condition$Yfamily[cond], #Yfamily,
    num_m = 2,
    num_x = 3,
    indep_M = TRUE, #condition$indep_M[cond],
    int.XZ = condition$int.XZ[cond], # Keeping it true because both should use be 0.2 #FALSE, #<- testing to match current func #
    if.null = condition$if.null[cond]
)


# ══════════════════════════════
#    current generate data 
# ══════════════════════════════
# Clearing only functions to avoid interference between the current & Dr Lius functions 
rm(list = ls()[sapply(ls(), function(x) is.function(get(x)))])
# reload functions 
function_names <- c(
    # "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    # "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c", 
    
    # Estimation functions 
    "crossfit", 
    "make_fold_K", 
    "eif", 
    "a.c",
    "a.mc",
    "mu.mac",
    "v.ac", 
    "mu.ac",
    "get_inference", 
    "internal_estimate_mediation", 
    "bound", 
    "effect", 
    "estimate_mediation"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# current data generation [note: have to not run Dr Liu's func & sourcing so this does not interfer with the other functions]
seedone <- datseeds[iseed]
sim_data <- generate_data2.0c(
    J = condition$J[cond], #Jval, 
    njrange = c(5, 20), #c(Nj_low, Nj_high), 
    Mfamily = "binomial", #Mfamily,
    Yfamily = condition$Yfamily[cond], #Yfamily,
    seed = seedone, #seeds[rep_idx],
    quadratic.A = condition$quadratic.A[cond], #isQuad,
    quadratic.M = condition$quadratic.M[cond], #isQuad,
    quadratic.Y = condition$quadratic.Y[cond], #isQuad,
    num_x = 3,
    include_overlapMsg = FALSE,
    
    m_on_a = 0.2, # 2, # Dr Lius func has m1_on_a = 0.2 & m2_on_a = 0.2
    m_on_anj = 0.2, # 0.5, # Dr Lius func has m1_on_anj = 0.2, & m2_on_anj = 0.2
    m_on_az = 0.2, # Dr Lius func has m1_on_az = 0.2 & m2_on_az = 0.2
    y_on_a = 0.2, #1, # Dr Lius func has y_on_a = 0.2, 
    y_on_m = 1, #2, # Dr Lius func has y_on_m1 = 1, y_on_m2 = 1,
    y_on_am = 0, #2, # Dr Lius func has y_on_am1 = 0, y_on_am2 = 0, 
    y_on_az = 0.2, # Dr Lius func has y_on_az = 0.2
    y_on_mz = 0.2, # Dr Lius func has y_on_m1z = 0.2, y_on_m2z = 0.2,
    y_on_anj = 0.2 #1, # Dr Lius func has y_on_anj = 0.2,  
    
    # m_on_a = 15,
    # m_on_anj = 0.5,
    # m_on_az = 0.2,
    # y_on_a = 2,
    # y_on_m = 15,
    # y_on_am = 5,
    # y_on_az = 0.2,
    # y_on_mz = 0.2,
    # y_on_anj = 5,
    # int.XZ = TRUE, #FALSE # try TRUE later b/c condition$int.XZ[cond] had true 
)
# Error in `colnames<-`(`*tmp*`, value = gsub(pattern = "^X\\.", replacement = "X",  : 
#                                                 attempt to set 'colnames' on an object with less than two dimensions



# head(sim_data$data)
# # id school  W_nj      Z     A ps_true iptw_true     M     Y     X1      X2      X3
# # <int>  <int> <dbl>  <dbl> <int>   <dbl>     <dbl> <int> <dbl>  <dbl>   <dbl>   <dbl>
# #     1     1      1 0.333 0.0730     0   0.610      2.57     1 2.04   0.889 -0.0615  2.88  
# # 2     2      1 0.333 0.0730     0   0.278      1.38     1 1.83   1.35  -0.542  -0.0572
# # 3     3      1 0.333 0.0730     1   0.384      2.60     1 0.373  1.92  -0.686   0.594 
# # 4     4      1 0.333 0.0730     1   0.237      4.21     1 1.96  -0.641 -0.665  -0.532 
# # 5     5      1 0.333 0.0730     0   0.239      1.31     0 0.131  0.771 -0.0940  0.763 
# # 6     6      1 0.333 0.0730     0   0.324      1.48     1 1.85  -0.626 -1.03   -1.32  
# str(sim_data)
# 
# str(gen_data)
# 
# head(gen_data$data)
# # id school  W_nj      Z  X[,1]    [,2]    [,3]     A    M1    M2      Y
# # <int>  <int> <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <int> <int> <int>  <dbl>
# #     1     1      1 0.333 0.0730  0.889 -0.0615  2.88       0     1     1  3.63 
# # 2     2      1 0.333 0.0730  1.35  -0.542  -0.0572     0     0     0  1.21 
# # 3     3      1 0.333 0.0730  1.92  -0.686   0.594      1     1     1  3.91 
# # 4     4      1 0.333 0.0730 -0.641 -0.665  -0.532      1     0     1  1.39 
# # 5     5      1 0.333 0.0730  0.771 -0.0940  0.763      0     0     0 -0.354
# # 6     6      1 0.333 0.0730 -0.626 -1.03   -1.32       0     1     0  0.137


# ══════════════════════════════
#    comparison of generated data 
# ══════════════════════════════

# 
sum(gen_data$data$school != sim_data$data$school)
sum(gen_data$data$Z != sim_data$data$Z)
sum(gen_data$data$A != sim_data$data$A)
sum(gen_data$data$X[, 1] != sim_data$data$X1)
sum(gen_data$data$X[, 2] != sim_data$data$X2)
sum(gen_data$data$X[, 3] != sim_data$data$X3)
sum(gen_data$data$W_nj != sim_data$data$W_nj)

# 
## Regardless of int.XZ argument matching up Mediator & Outcome differ 
# Mediator differs
sum(gen_data$data$M1 != sim_data$data$M)
sum(gen_data$data$M2 != sim_data$data$M)

# head(sim_data$data$M)
# head(gen_data$data$M1)
# head(gen_data$data$M2)

# Outcomes differ
sum(gen_data$data$Y != sim_data$data$Y)

dim(gen_data$data); dim(sim_data$data)

### Investigate Ms differing  -----------------------------------------------

sum(gen_data$m1_given != sim_data$parameters$m_given) # m_given is the same for M1 & M 
head(gen_data$m1_given); head(sim_data$parameters$m_given)


# When creating these two variables ensure their respective pm1() is loaded when creating it 
rm(list = ls()[sapply(ls(), function(x) is.function(get(x)))]) # clear functions 
source("Functions/fun_gen.R")
liu_pm1 <- pm1(m = 1, a = gen_data$data$A, z = gen_data$data$Z, nj = gen_data$data$W_nj, gen_data$m1_given, gen_data$gen_m)
## replace funcitons 
rm(list = ls()[sapply(ls(), function(x) is.function(get(x)))])
# reload functions 
function_names <- c(
    # "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    # "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c", 
    
    # Estimation functions 
    "crossfit", 
    "make_fold_K", 
    "eif", 
    "a.c",
    "a.mc",
    "mu.mac",
    "v.ac", 
    "mu.ac",
    "get_inference", 
    "internal_estimate_mediation", 
    "bound", 
    "effect", 
    "estimate_mediation"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

cam_pm1 <- pm1(m = 1, a = sim_data$data$A, z = sim_data$data$Z, nj = sim_data$data$W_nj, sim_data$parameters$m_given, list(iccm = sim_data$parameters$iccm, 
                                                                                                                m_on_a = sim_data$parameters$m_on_a, 
                                                                                                                m_on_x = sqrt(0.15 / sim_data$parameters$num_x), 
                                                                                                                m_on_z = sqrt(0.4), 
                                                                                                                m_on_az = sim_data$parameters$m_on_az, 
                                                                                                                m_on_anj = sim_data$parameters$m_on_anj))
# The pm1() functions result in slightly different values (374 diff out of 880)
sum(liu_pm1 != cam_pm1)
head(liu_pm1, 8); head(cam_pm1, 8)
# length(liu_pm1); length(cam_pm1)

# the functions look the same though (maybe some input is different)
## liu func
# pm1 <- function(m1, a, z, nj, given, gen_m) {
#     latent <- gen_m[["m1_on_a"]] * a + gen_m[["m1_on_az"]] * a*z + gen_m[["m1_on_anj"]] * a*nj + given
#     prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm1"]]))
#     m1 * prob1 + (1 - m1) * (1 - prob1)
# }

## cam fun 
# pm1 <- function(m, a, z, nj, given, gen_m) {
#     
#     latent <- gen_m[["m_on_a"]] * a + 
#         gen_m[["m_on_az"]] * a * z + 
#         gen_m[["m_on_anj"]] * a * nj + 
#         given
#     prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
#     # m * prob1 + (1 - m) * (1 - prob1)
#     return(m * prob1 + (1 - m) * (1 - prob1))
# }


gen_data$gen_m$iccm1 == sim_data$parameters$iccm

gen_data$gen_m$m1_on_a == sim_data$parameters$m_on_a

gen_data$gen_m$m1_on_az == sim_data$parameters$m_on_az

# m_on_anj differs 
gen_data$gen_m$m1_on_anj == sim_data$parameters$m_on_anj # when GenData() has int.XZ = TRUE & generate_data() has int.XZ = FALSE both = 0.2
## so i think the int.XZ argument in generate_data2.0c() does not work 

# However, while matching int.XZ leads to the pm1() output to match, the mediators still differ
sum(liu_pm1 != cam_pm1)
sum(gen_data$data$M1 != sim_data$data$M)
# sum(gen_data$data$M2 != sim_data$data$M)



sum(gen_data$m1_given != sim_data$parameters$m_given) # m_given is still good

sum(gen_data$gen_m$m1_on_x != sim_data$parameters$m_on_x) 
sum(gen_data$gen_m$m1_on_z != sim_data$parameters$m_on_z)

sum(gen_data$data$school != sim_data$data$school)
sum(gen_data$data$id != sim_data$data$id)
# head(gen_data$data$school)


# checking whether rbinom() produces different values 

source("Functions/fun_gen.R")

# This is Dr Liu's code modified to be tested 
gen_m <- list(iccm1 = gen_data$gen_m[["iccm1"]], # load existing values #iccm1, 
              iccm2 = gen_data$gen_m[["iccm2"]], #iccm2,
              m1_on_a = 0.2, m1_on_x = sqrt(0.15 / gen_data$num_x), m1_on_z = sqrt(0.4),
              m1_on_az = 0.2, m1_on_anj = 0.2,
              # M2
              m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
              # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
              m2_on_x = sqrt(0.15 / gen_data$num_x), m2_on_z = sqrt(0.4),
              m2_on_az = 0.2, m2_on_anj = 0.2
)

if (gen_data$int.XZ==FALSE) {
    # gen_m[c("m1_on_az","m2_on_az")] <- 0
    gen_m[c("m1_on_anj","m2_on_anj")] <- 0
}

m1b <- unlist(map(1:gen_data$J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_data$gen_m[["iccm1"]])), each = gen_data$nj_sizes[.x])))
m2b <- unlist(map(1:gen_data$J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_data$gen_m[["iccm2"]])), each = gen_data$nj_sizes[.x])))

# Lius code
if (gen_data$quadratic.M == FALSE) {
    Xlinear <- gen_data$data$X
    # if (int.XZ) {
    #   Xlinear <- data$X * data$Z
    # }
    m1_given <- m1b + gen_data$gen_m[["m1_on_x"]] * rowSums(Xlinear) +
        gen_data$gen_m[["m1_on_z"]] * gen_data$data$Z
    # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
    # data$M1
    set.seed(seedone)
    prob1 <- pm1(1, gen_data$data$A, gen_data$data$Z, gen_data$data$W_nj, m1_given, gen_data$gen_m)
    liu_M1 <- rbinom(gen_data$N, 1, prob = prob1) #pm1(1, gen_data$data$A, gen_data$data$Z, gen_data$data$W_nj, m1_given, gen_data$gen_m) )

    # m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
    #     gen_m[["m2_on_z"]] * data$Z
    # data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
}

sum(liu_M1 != gen_data$data$M1) # So far this 

# replace functions and test cam 
## replace funcitons 
rm(list = ls()[sapply(ls(), function(x) is.function(get(x)))])
# reload functions 
function_names <- c(
    # "generate_data", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1", 
    "my", 
    # "trueVals", 
    
    # As of 01/01/2025 the two funcs below are the updated versions 
    "generate_data2.0c", 
    "trueVals2.0c", 
    
    # Estimation functions 
    "crossfit", 
    "make_fold_K", 
    "eif", 
    "a.c",
    "a.mc",
    "mu.mac",
    "v.ac", 
    "mu.ac",
    "get_inference", 
    "internal_estimate_mediation", 
    "bound", 
    "effect", 
    "estimate_mediation"
)
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}


# Cam
# Generate mediator 'M'

gen_m <- list(
    iccm = sim_data$parameters$iccm, #iccm,         # Intra-class correlation for 'M'
    m_on_a = sim_data$parameters$m_on_a, #m_on_a,        # Effect of 'A' on 'M'
    m_on_x = sqrt(0.15 / sim_data$parameters$num_x), # m_on_x, # Effect of 'X' on 'M'
    m_on_z = sqrt(0.4), # m_on_z,  # Effect of 'Z' on 'M'
    m_on_az = sim_data$parameters$m_on_az, #m_on_az,       # Interaction effect of 'A' and 'Z' on 'M'
    m_on_anj = sim_data$parameters$m_on_anj #m_on_anj      # Interaction effect of 'A' and cluster size on 'M'
)

# J <- length(unique(sim_data$data$school))
N <- nrow(sim_data$data) # N <- sum(nj_sizes)

# skipping this part
# # If interaction between 'A' and cluster size is not included
if (sim_data$parameters$int.XZ == FALSE) {
    gen_m[c("m_on_anj")] <- 0 # NOTE: delete 
}

# Generate cluster-level random effects for mediators
mb <- unlist(map(1:sim_data$parameters$J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm"]])), each = sim_data$parameters$nj_sizes[.x])))

# Generate mediator 'M'
if (sim_data$parameters$quadratic.M == TRUE) {
    # Include quadratic terms if specified
    # Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
    # 
    # # Compute the linear predictor for 'M1'
    # m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
    #     gen_m[["m_on_z"]] * data_list$data$Z
} else {
    # Linear terms only
    Xlinear <- sim_data$data[, c("X1", "X2", "X3")]
    # Xlinear <- data_list$data$X
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
        gen_m[["m_on_z"]] * sim_data$data$Z
}

# Generate mediator values
if (sim_data$parameters$Mfamily == "binomial") {
    prob <- pm1(1, sim_data$data$A, sim_data$data$Z, sim_data$data$W_nj, m_given, gen_m)
    # data_list$data$M <- 
    set.seed(seedone)
    
    cam_M <- rbinom(N, size = 1, prob = prob)
    # latent <- gen_m[["m_on_a"]] * data_list$data$A + gen_m[["m_on_az"]] * data_list$data$A*data_list$data$Z + gen_m[["m_on_anj"]] * data_list$data$A*data_list$data$W_nj + m_given
    # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
    # # m * prob1 + (1 - m) * (1 - prob1) # m = 1 so can drop
    # data_list$data$M <- rbinom(N, 1, prob = prob1)
} else if (sim_data$parameters$Mfamily == "gaussian") {
    # mean_m <- m_given
    # data_list$data$M <- rnorm(N, mean = mean_m, sd = sqrt(1 - gen_m$iccm))
}


sum(liu_M1 != gen_data$data$M1)
sum(liu_M1 != cam_M)

sum(prob1 != prob)
prob

gen_data$m1_given
sim_data$parameters


if (sim_data$parameters$quadratic.M == TRUE) {
    # Include quadratic terms if specified
    Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
        gen_m[["m_on_z"]] * data_list$data$Z
} else {
    # Linear terms only
    Xlinear <- sim_data$data[, c("X1", "X2", "X3")]
    # Xlinear <- data_list$data$X
    
    # Compute the linear predictor for 'M1'
    sim_data$parameter
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
        gen_m[["m_on_z"]] * data_list$data$Z
}

# Generate mediator values
if (Mfamily == "binomial") {
    prob <- pm1(1, data_list$data$A, data_list$data$Z, data_list$data$W_nj, m_given, gen_m)
    data_list$data$M <- rbinom(N, size = 1, prob = prob)
    # latent <- gen_m[["m_on_a"]] * data_list$data$A + gen_m[["m_on_az"]] * data_list$data$A*data_list$data$Z + gen_m[["m_on_anj"]] * data_list$data$A*data_list$data$W_nj + m_given
    # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
    # # m * prob1 + (1 - m) * (1 - prob1) # m = 1 so can drop
    # data_list$data$M <- rbinom(N, 1, prob = prob1)
} else if (Mfamily == "gaussian") {
    mean_m <- m_given
    data_list$data$M <- rnorm(N, mean = mean_m, sd = sqrt(1 - gen_m$iccm))
}







# Here is current code modified to be test/compared to m1b 


# STOPPED HERE 




m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
    gen_m[["m1_on_z"]] * data$Z
# pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )


prob <- pm1(1, data_list$data$A, data_list$data$Z, data_list$data$W_nj, m_given, gen_m)
data_list$data$M <- rbinom(N, size = 1, prob = prob)



# Lius code
if (quadratic.M == FALSE) {
    Xlinear <- data$X
    # if (int.XZ) {
    #   Xlinear <- data$X * data$Z
    # }
    m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
        gen_m[["m1_on_z"]] * data$Z
    # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
    data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
    
    m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
        gen_m[["m2_on_z"]] * data$Z
    data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
}

# Cam
# Generate mediator 'M'
if (quadratic.M == TRUE) {
    # Include quadratic terms if specified
    Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
        gen_m[["m_on_z"]] * data_list$data$Z
} else {
    # Linear terms only
    Xlinear <- data_list$data$X
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
        gen_m[["m_on_z"]] * data_list$data$Z
}

# Generate mediator values
if (Mfamily == "binomial") {
    prob <- pm1(1, data_list$data$A, data_list$data$Z, data_list$data$W_nj, m_given, gen_m)
    data_list$data$M <- rbinom(N, size = 1, prob = prob)
    # latent <- gen_m[["m_on_a"]] * data_list$data$A + gen_m[["m_on_az"]] * data_list$data$A*data_list$data$Z + gen_m[["m_on_anj"]] * data_list$data$A*data_list$data$W_nj + m_given
    # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
    # # m * prob1 + (1 - m) * (1 - prob1) # m = 1 so can drop
    # data_list$data$M <- rbinom(N, 1, prob = prob1)
} else if (Mfamily == "gaussian") {
    mean_m <- m_given
    data_list$data$M <- rnorm(N, mean = mean_m, sd = sqrt(1 - gen_m$iccm))
}






gen_m <- list(iccm1 = iccm1, iccm2 = iccm2,
              m1_on_a = 0.2, m1_on_x = sqrt(0.15 / num_x), m1_on_z = sqrt(0.4),
              m1_on_az = 0.2, m1_on_anj = 0.2,
              # M2
              m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
              # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
              m2_on_x = sqrt(0.15 / num_x), m2_on_z = sqrt(0.4),
              m2_on_az = 0.2, m2_on_anj = 0.2
)

if (int.XZ==FALSE) {
    # gen_m[c("m1_on_az","m2_on_az")] <- 0
    gen_m[c("m1_on_anj","m2_on_anj")] <- 0
}

if (quadratic.M == FALSE) {
    Xlinear <- data$X
    # if (int.XZ) {
    #   Xlinear <- data$X * data$Z
    # }
    m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
        gen_m[["m1_on_z"]] * data$Z
    # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
    data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
    
    m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
        gen_m[["m2_on_z"]] * data$Z
    data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
}



# Generate mediator 'M'
if (quadratic.M == TRUE) {
    # Include quadratic terms if specified
    Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
        gen_m[["m_on_z"]] * data_list$data$Z
} else {
    # Linear terms only
    Xlinear <- data_list$data$X
    
    # Compute the linear predictor for 'M1'
    m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
        gen_m[["m_on_z"]] * data_list$data$Z
}



gen_m <- list(
    iccm = iccm,         # Intra-class correlation for 'M'
    m_on_a = m_on_a,        # Effect of 'A' on 'M'
    m_on_x = sqrt(0.15 / num_x), # m_on_x, # Effect of 'X' on 'M'
    m_on_z = sqrt(0.4), # m_on_z,  # Effect of 'Z' on 'M'
    m_on_az = m_on_az,       # Interaction effect of 'A' and 'Z' on 'M'
    m_on_anj = m_on_anj      # Interaction effect of 'A' and cluster size on 'M'
)
pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) 

gen_data$gen_m
if (quadratic.M == FALSE) {
    Xlinear <- data$X
    # if (int.XZ) {
    #   Xlinear <- data$X * data$Z
    # }
    m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
        gen_m[["m1_on_z"]] * data$Z
    # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
    data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
    
    m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
        gen_m[["m2_on_z"]] * data$Z
    data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
}





gen_m <- list(iccm1 = iccm1, iccm2 = iccm2,
              m1_on_a = 0.2, m1_on_x = sqrt(0.15 / num_x), m1_on_z = sqrt(0.4),
              m1_on_az = 0.2, m1_on_anj = 0.2,
              # M2
              m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
              # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
              m2_on_x = sqrt(0.15 / num_x), m2_on_z = sqrt(0.4),
              m2_on_az = 0.2, m2_on_anj = 0.2
)
# conditional independent mediators
if (indep_M) {
    gen_m[c("m2_on_m1","m2_on_am1")] <- 0
}

if (int.XZ==FALSE) {
    # gen_m[c("m1_on_az","m2_on_az")] <- 0
    gen_m[c("m1_on_anj","m2_on_anj")] <- 0
}
# m1b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm1"]])), each = nj)
# m2b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm2"]])), each = nj)
m1b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm1"]])), each = nj_sizes[.x])))
m2b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm2"]])), each = nj_sizes[.x])))

if (quadratic.M == TRUE) {
    Xquad <- (data$X ^ 2 - 1) / sqrt(4)
    # if (int.XZ) {
    #   Xquad <- Xquad * data$Z
    # }
    # previous
    # Xquad <- Xquad * data$Z
    # gen_m[["m1_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
    # gen_m[["m2_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
    
    m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xquad) +
        gen_m[["m1_on_z"]] * data$Z
    data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
    
    m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xquad) +
        gen_m[["m2_on_z"]] * data$Z
    data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
    
}
if (quadratic.M == FALSE) {
    Xlinear <- data$X
    # if (int.XZ) {
    #   Xlinear <- data$X * data$Z
    # }
    m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
        gen_m[["m1_on_z"]] * data$Z
    # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
    data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
    
    m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
        gen_m[["m2_on_z"]] * data$Z
    data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
}

y_on_amint <- 0
# iccy <- 0.2
gen_y <- list(iccy = iccy, yintercept = 1,
              # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
              y_on_a = 0.2, y_on_m1 = 1, y_on_m2 = 1,
              y_on_am1 = 0, y_on_am2 = 0, 
              y_on_m1m2 = 0.2, y_on_am1m2 = 0.2,
              # y_on_m1m2 = 0, y_on_am1m2 = 0,
              y_on_az = 0.2, y_on_m1z = 0.2, y_on_m2z = 0.2,
              y_on_anj = 0.2,  
              y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
)


