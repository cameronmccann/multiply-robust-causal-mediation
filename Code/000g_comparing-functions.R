################################################################################
########### Compare mediator_ClusterData func to modified version of it ###############
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 2025-01-13
#
#
# Script Description: 
#   In this script I will be comparing functions (for estimation, data generation, 
# & true value calculations) from mediator_ClusterData (for 2 mediators) to modified 
# versions of these i am creating (& sometimes to the funcitons I already had made; e.g., trueVals2.0c()). 
# I will try to clearly document so that we can refer to this doc in meetings about potential discrepancies. 
#
#
# Last Updated: 2025-01-17
#
#
# Notes/Summary:
# ## Data generation:
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
    tidyr, 
    readr,
    ggplot2,
    fastDummies, # for Dr Liu's package/function (specifically, for dummy_cols() in fastDummies package)
    stringr, # for str_detect() in Dr Liu's package/function
    tibble # for rownames_to_column() in Dr Liu's package/function
)

# ══════════════════════════════
#     Source Updated Functions
# ══════════════════════════════
# function_names <- c(
#     # "generate_data", 
#     "generate_clusters", 
#     "generate_confounders", 
#     "generate_treatment", 
#     "generate_mediator", 
#     "generate_outcome", 
#     "pm1", 
#     "my", 
#     # "trueVals", 
#     
#     # As of 01/01/2025 the two funcs below are the updated versions 
#     "generate_data2.0c", 
#     "trueVals2.0c", 
#     
#     # Estimation functions 
#     "crossfit", 
#     "make_fold_K", 
#     "eif", 
#     "a.c",
#     "a.mc",
#     "mu.mac",
#     "v.ac", 
#     "mu.ac",
#     "get_inference", 
#     "internal_estimate_mediation", 
#     "bound", 
#     "effect", 
#     "estimate_mediation"
# )
# for (func in function_names) {
#     source(file.path("Functions", paste0(func, ".R")))
# }

# Script for dr lius functions 
# source("Functions/fun_gen.R")


# 1. view output from prior simulation runs ----------------------------------

# check prior run (200 reps) of your code/functions 
ind_summary <- readRDS(file = "Output/S1_Results/Tables/S1_performance-measures.rds")

# narrow to one condition (J = 70 & nj = [5, 20] & binomail M & gaussian Y)
ind_summary <- ind_summary |> 
    filter(J == 70 & Nj_low == 5) |> 
    select(cluster_a, Fit, cluster_opt, quadratic:t1Error_individual_TNIE)
# update format 
ind_summary <- ind_summary |> 
    pivot_longer(
        cols = c(cover_PNDE, cover_TNIE, 
                 bias_individual_PNDE, bias_individual_TNIE,
                 MSE_individual_PNDE, MSE_individual_TNIE,
                 t1Error_individual_PNDE, t1Error_individual_TNIE),
        names_to = c(".value", "type"),
        names_pattern = "(.+)_(.+)"
    ) |> 
    select(type, bias_individual, cover, t1Error_individual, MSE_individual, everything())

# Output result
view(ind_summary)

# check prior run (200 reps) of existing (mediator_ClusterData)
load("Output/Output_mediator_ClusterData/Results_2025-01-14/res_summ.RData")

res_summ |> 
    filter(Fit == "mlr" & cluster_opt_y == "cwc.FE" & quadratic.A == FALSE) |> 
    ungroup() |> 
    select(estimand,  contains("_individual"), ends_with("_cluster")) |> 
    view()

ind_summary



# Set up conditions -------------------------------------------------------

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


# Set seed & condition 
set.seed(12)
datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

iseed <- 9
cond <- 1



# GenData og func ---------------------------------------------------------

# load in helper func
source("Functions/fun_gen.R")

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



# 2. generated data ----------------------------------------------------------

## OG data generation func (GenData) ---------------------------------------

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


## modified data generation func (GenData_2.0) --------------------------------------------

# clear everything but the output (gen_data), conditions & methods, and values 
rm(list = setdiff(ls(),
    c("gen_data",
    "condition", "condition_all", "methds", "methds_all", "cond", "datseeds", "iseed", "seedone")))
# rm(list = setdiff(ls(), "gen_data_2"))

# load in modified functions for 1 mediator 
source("Functions/new-data-gen-functions.R")

seedone <- datseeds[iseed]
gen_data_2 <- GenData_2.0(
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
    # num_m = 2,
    num_x = 3,
    indep_M = TRUE, #condition$indep_M[cond],
    int.XZ = condition$int.XZ[cond], # Keeping it true because both should use be 0.2 #FALSE, #<- testing to match current func #
    if.null = condition$if.null[cond]
)


# 3. Compare data generation -----------------------------------------------------------------

setdiff(names(gen_data), names(gen_data_2)) 
# [1] "m2_given" "truevals" "trueVals"

head(gen_data$m1b); head(gen_data_2$m1b)
# head(gen_data$m2b); head(gen_data_2$m2b)

sum(gen_data$data$school != gen_data_2$data$school)
sum(gen_data$data$Z != gen_data_2$data$Z)
sum(gen_data$data$X != gen_data_2$data$X)
sum(gen_data$data$W_nj != gen_data_2$data$W_nj)
sum(gen_data$data$A != gen_data_2$data$A)
sum(gen_data$data$M1 != gen_data_2$data$M1) 
# ══════════════════════════════
# oddly only if you comment out the line that creates m2b (121 in new-data-gen-functions.R; see below) 
# ~259 of M1s do not match. This was about the same amount that 
# did not match my original data generation function (generate_data2.0c()). 
# Would this suggest that the M2 in the GenData() is affecting Y, 
# thus GenData() & GenData_2.0() differ in outcomes? 
# m2b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm2"]])), each = nj_sizes[.x])))
# ══════════════════════════════

# covariates (X & Z) & trt (A) & mediator (M1) are all the same 



# Outcomes differ
sum(gen_data$data$Y != gen_data_2$data$Y) # outcome still differs 
# head(gen_data$data$Y)
# head(gen_data_2$data$Y)

# strangely to me the yb differs (I am assuming M2 is somehow affecting this but I cannot see how it is exactly) 
sum(gen_data$yb != gen_data_2$yb) # yb differs 
head(gen_data$yb, 30); head(gen_data_2$yb, 30)
summary(gen_data$yb); summary(gen_data_2$yb) 
summary(gen_data$yb - gen_data_2$yb)

sum(gen_data$y_given != gen_data_2$y_given) # y_given differs 
# but none of the creating y_given differ except for yb 
sum(gen_data$gen_y$yintercept != gen_data_2$gen_y$yintercept)
sum(gen_data$gen_y$y_on_x != gen_data_2$gen_y$y_on_x)
sum(rowSums(gen_data$data$X) != rowSums(gen_data_2$data$X))
sum(gen_data$gen_y$y_on_z != gen_data_2$gen_y$y_on_z)
sum(gen_data$data$Z != gen_data_2$data$Z)

sum(gen_data$condmy != gen_data_2$condmy) # condmy differs too (which is expected since the my() functions differ)

# ══════════════════════════════
# the data generation functions (GenData() & GenData_2.0()) output do not match up; 
# specifically, the outcome (Y) differs & some of the mediators (M1).  
# I do not understand why yet. 
# 
# Possible questions/misunderstanding:
# - why do we use yb <- rnorm(J, sd = sqrt(gen_y[["iccy"]]))[data$school] #[rep(1:J, each = nj)] 
#       for yb but different methods (see below) for Z, ab, & m1b? 
#       m1b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm1"]])), each = nj_sizes[.x])))
# 
# For now I will skip this part to evaluate the estimation & true value calculation processes
# ══════════════════════════════

# Also, tested dropping M2s in gen_m inside of GenData_2.0() & it does not change impact data generation 


### quick comparison to generate_data() [SKIP] -------------------------------------

# # ══════════════════════════════
# #     Source Updated Functions
# # ══════════════════════════════
# function_names <- c(
#     # "generate_data",
#     "generate_clusters",
#     "generate_confounders",
#     "generate_treatment",
#     "generate_mediator",
#     "generate_outcome",
#     # "pm1",
#     # "my",
#     
# 
#     # As of 01/01/2025 the two funcs below are the updated versions
#     "generate_data2.0c",
#     "trueVals2.0c"
# 
#     # # Estimation functions
#     # "crossfit",
#     # "make_fold_K",
#     # "eif",
#     # "a.c",
#     # "a.mc",
#     # "mu.mac",
#     # "v.ac",
#     # "mu.ac",
#     # "get_inference",
#     # "internal_estimate_mediation",
#     # "bound",
#     # "effect",
#     # "estimate_mediation"
# )
# for (func in function_names) {
#     source(file.path("Functions", paste0(func, ".R")))
# }
# 
# 
# gen_data_3 <- generate_data2.0c(
#     seed = seedone, 
#     J = condition$J[cond],
#     njrange = c(5, 20), 
#     Mfamily = "binomial",
#     Yfamily = condition$Yfamily[cond], 
#     icca = condition$icc[cond],
#     iccx = condition$icc[cond],
#     iccm = condition$icc[cond],
#     iccy = condition$icc[cond],
#     # quadratic.A = FALSE, # condition$quadratic.A[cond],
#     # quadratic.Y = condition$quadratic.M[cond],
#     # quadratic.A = condition$quadratic.Y[cond],
#     num_x = 3, 
#     # x_z = 0,                         # Correlation between 'X' and 'Z'
#     m_on_a = 0.2,                    # Effect of 'A' on 'M'
#     m_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'M'
#     m_on_anj = 0.2,                  # Interaction: 'A' x cluster size on 'M'
#     m_on_x = sqrt(0.15 / num_x),     # Effect of 'X' on 'M'
#     m_on_z = sqrt(0.4),              # Effect of 'Z' on 'M'
#     # int.XZ = TRUE,                   # Include X:Z interaction in mediator/outcome model
#     yintercept = 1,                  # Intercept for outcome model
#     y_on_a = 0.2,                    # Effect of 'A' on 'Y'
#     y_on_m = 1,                      # Effect of 'M' on 'Y'
#     y_on_am = 0,                     # Interaction: 'A' x 'M' on 'Y'
#     y_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'Y'
#     y_on_mz = 0.2,                   # Interaction: 'M' x 'Z' on 'Y'
#     y_on_anj = 0.2,
#     
#     include_truevals = FALSE, 
#     include_overlapMsg = FALSE
# )





# 4. Compare estimation funcs (`MediatorCL()` & `estimate_mediation()` --------

# Note: ensure data is generated with GenData_2.0() (i.e., sec 2.) prior to this section

# ══════════════════════════════
#    skipping true vals quickly to see if estimation works with estimate_mediation  
# ══════════════════════════════

## `MediatorCL()` ----------------------------------------------------------

# Running dr lius func on data generated from GenData_2.0()
devtools::load_all("Functions/MediatorCL")
Sname <- "school"
estMediatorCL <- MediatorCL(data = as.data.frame(gen_data_2$datobs), 
                            Sname = "school", 
                            Wnames = "W_nj", 
                            Xnames = c("X1", "X2", "X3"), 
                            # Xnames = c("X.1", "X.2", "X.3"), 
                            Aname = "A", 
                            Mnames = "M1", 
                            Yname = "Y", 
                            learners_a = c("SL.nnet", "SL.gam"), 
                            learners_m = c("SL.nnet", "SL.gam"), 
                            learners_y = c("SL.nnet", "SL.gam"), 
                            cluster_opt = "cwc.FE", 
                            num_folds = 5)
# Warning messages:
#     1: In fold_fun(n, ...) :
#     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n
# 2: In fold_fun(n, ...) :
#     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n

estMediatorCL


## `estimate_mediation()` --------------------------------------------------

# First ensure GenData_2.0() output matches generate_data2.0c() output before putting it into estimate_mediation()

# change X.1 to X1
names(gen_data_2$datobs) <- gsub(pattern = "X\\.", replacement = "X", names(gen_data_2$datobs))
# 
source("Functions/trueVals2.0c.R")
temp <- trueVals2.0c(data_list = gen_data_2, from_GenData_2.0 = TRUE)
# Putting GenData_2.0() output into format for estimate_mediaiton()
gen_data_2_result_data <- list(data = gen_data_2$datobs, 
                               truevals = temp, 
                               effects = c(NA), 
                               overlap = c(NA), 
                               parameters = list(
                                   J = gen_data_2$J, 
                                   N = gen_data_2$N, 
                                   njrange = gen_data_2$njrange, 
                                   nj_sizes = gen_data_2$nj_sizes, 
                                   y_given = gen_data_2$y_given,
                                   m_given = gen_data_2$m_given,
                                   seed = gen_data_2$seedone, 
                                   num_x = gen_data_2$num_x,
                                   iccx = gen_data_2$iccx,
                                   x_z = gen_data_2$x_z,
                                   icca = gen_data_2$icca,
                                   quadratic.A = gen_data_2$quadratic.A,
                                   iccm = gen_data_2$iccm,
                                   m_on_a = gen_data_2$gen_m$m1_on_a,
                                   m_on_az = gen_data_2$gen_m$m1_on_az,
                                   m_on_anj = gen_data_2$gen_m$m1_on_anj,
                                   m_on_x = gen_data_2$gen_m$m1_on_x, #
                                   m_on_z = gen_data_2$gen_m$m1_on_z, #
                                   quadratic.M = gen_data_2$quadratic.M,
                                   # int.XZ = int.XZ,
                                   iccy = gen_data_2$iccy,
                                   yintercept = gen_data_2$gen_y$yintercept,
                                   y_on_a = gen_data_2$gen_y$y_on_a,
                                   y_on_m = gen_data_2$gen_y$y_on_m1,
                                   y_on_am = gen_data_2$gen_y$y_on_am1,
                                   y_on_az = gen_data_2$gen_y$y_on_az,
                                   y_on_mz = gen_data_2$gen_y$y_on_m1z,
                                   y_on_anj = gen_data_2$gen_y$y_on_anj,
                                   y_on_x = gen_data_2$gen_y$y_on_x, #
                                   y_on_z = gen_data_2$gen_y$y_on_z, #
                                   quadratic.Y = gen_data_2$quadratic.Y,
                                   Yfamily = gen_data_2$Yfamily,
                                   Mfamily = "binomial",
                                   if.null = gen_data_2$if.null
                               ))


# estimate with estimate_mediation() 
# import estimation functions 
function_names <- c(
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


estEstimate_mediation <- estimate_mediation(data = gen_data_2_result_data$data, 
                                            Sname = "school", 
                                            Wnames = "W_nj",  
                                            Xnames = c("X1", "X2", "X3"), 
                                            Aname = "A", 
                                            Mnames = "M1", 
                                            Yname = "Y", 
                                            learners_a = c("SL.nnet", "SL.gam"), 
                                            learners_m = c("SL.nnet", "SL.gam"), 
                                            learners_y = c("SL.nnet", "SL.gam"),
                                            cluster_opt = "cwc.FE", 
                                            num_folds = 5)
# Warning messages:
#     1: In fold_fun(n, ...) :
#     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n
# 2: In fold_fun(n, ...) :
#     n (the number of units, clusters, or clusters in a specific strata) is 5 and V is 5, so using leave-one-out CV, i.e. setting V = n

estEstimate_mediation


## compare estimates -------------------------------------------------------

# Does this match the estMediatorCL output = YES 
estEstimate_mediation
#                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.45457774 0.12376721  0.20399848 0.7051570
# 2 Indirect Effect (IE) Individual-Avg 0.07363529 0.05366729 -0.03501958 0.1822902
# 3   Direct Effect (DE)    Cluster-Avg 0.38537738 0.12453151  0.13325070 0.6375041
# 4 Indirect Effect (IE)    Cluster-Avg 0.06880246 0.05806682 -0.04875970 0.1863646
estMediatorCL
# Effect Individual.Average_Estimate Individual.Average_StdError CI.lower_Individual.Average CI.upper_Individual.Average Cluster.Average_Estimate
# 1     DE                  0.45457774                  0.12376721                  0.20399848                   0.7051570               0.38537738
# 2     IE                  0.07363529                  0.05366729                 -0.03501958                   0.1822902               0.06880246
# Cluster.Average_StdError CI.lower_Cluster.Average CI.upper_Cluster.Average
# 1               0.12453151                0.1332507                0.6375041
# 2               0.05806682               -0.0487597                0.1863646
# the two estimation functions (MediatorCL & estimate_mediation) produce the same estimates when the same dataset is used



# estimates with m2b dropped 
# Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.45457774 0.12376721  0.20399848 0.7051570
# 2 Indirect Effect (IE) Individual-Avg 0.07363529 0.05366729 -0.03501958 0.1822902
# 3   Direct Effect (DE)    Cluster-Avg 0.38537738 0.12453151  0.13325070 0.6375041
# 4 Indirect Effect (IE)    Cluster-Avg 0.06880246 0.05806682 -0.04875970 0.1863646
# estimates with m2b included
# Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.30743210 0.11281383  0.07902907 0.5358351
# 2 Indirect Effect (IE) Individual-Avg 0.05963872 0.05141014 -0.04444632 0.1637238
# 3   Direct Effect (DE)    Cluster-Avg 0.19611778 0.11166076 -0.02995075 0.4221863
# 4 Indirect Effect (IE)    Cluster-Avg 0.05951152 0.05005092 -0.04182163 0.1608447

# how does this compare to true values from truevals2.0c()

# # true value calculations below are a lot larger than estimates 
# temp <- trueVals2.0c(data_list = gen_data_2, from_GenData_2.0 = TRUE)
# temp$truevals_individual$`Y(a0=0, gm(a1=0))` - temp$truevals_individual$`Y(a0=1, gm(a1=0))`
# temp$truevals_individual$`Y(a0=1, gm(a1=1))` - temp$truevals_individual$`Y(a0=1, gm(a1=0))`

####





# 5a. Compare true value calculations --------------------------------------

## existing true value code (from mediator_ClusterData) ----------------------

# ═══════════════════
#    function from within GenData() 
# ═══════════════════
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

# ═══════════════════
#    function from true.R (in mediator_ClusterData) 
# ═══════════════════
OneTrue <- function(iseed = 1, cond = 1){
    
    if (condition$if.null[cond]==TRUE) {
        largeJ <- 10
    } 
    if (condition$if.null[cond]==FALSE) {
        largeJ <- 5e4
    }
    gen_largeJ <- GenData( # go back to section 2. to load in GenData 
        seedone = datseeds[iseed],
        J = largeJ,
        nj = condition$nj[cond],
        quadratic.A = condition$quadratic.A[cond],
        quadratic.M = condition$quadratic.M[cond],
        quadratic.Y = condition$quadratic.Y[cond],
        icca = condition$icc[cond],
        iccm = condition$icc[cond],
        iccy = condition$icc[cond],
        x_z = condition$x_z[cond],
        Yfamily = as.character(condition$Yfamily[cond]),
        # num_m = 2,
        num_x = 3,
        indep_M = TRUE, #condition$indep_M[cond],
        int.XZ = condition$int.XZ[cond],
        if.null = condition$if.null[cond]
    )
    
    trueEffs_individual <- effect(gen_largeJ$truevals$truevals_individual) %>% unlist()
    trueEffs_cluster <- effect(gen_largeJ$truevals$truevals_cluster) %>% unlist()
    
    if (condition$if.null[cond]==TRUE) {
        trueEffs_individual <- trueEffs_individual*0
        trueEffs_cluster <- trueEffs_cluster*0
    }
    res <- data.frame(condition[cond, ], estimand = names(trueEffs_cluster),
                      # trueEffs_individual,
                      # trueEffs_cluster,
                      cbind(trueEffs_individual,trueEffs_cluster), row.names = NULL)
    
    res
}


## modified true.R ---------------------------------------------------------

source("Functions/trueVals2.0c.R") # was modified to handle GenData_2.0() output; this func will be inserted below 
source("Functions/effect.R") # this is identical to the one in mediator_Clusterdata under oneMcl.R
source("Functions/effect_2.0.R") # this is identical to the one in mediator_Clusterdata under oneMcl.R except labels are changed to work with trueVals2.0c output 

OneTrue_2.0 <- function(iseed = 1, cond = 1){
    
    if (condition$if.null[cond]==TRUE) {
        largeJ <- 10
    } 
    if (condition$if.null[cond]==FALSE) {
        largeJ <- 5e4
    }
    # gen_largeJ <- GenData(
    #     seedone = datseeds[iseed],
    #     J = largeJ,
    #     nj = condition$nj[cond],
    #     quadratic.A = condition$quadratic.A[cond],
    #     quadratic.M = condition$quadratic.M[cond],
    #     quadratic.Y = condition$quadratic.Y[cond],
    #     icca = condition$icc[cond],
    #     iccm = condition$icc[cond],
    #     iccy = condition$icc[cond],
    #     x_z = condition$x_z[cond],
    #     Yfamily = as.character(condition$Yfamily[cond]),
    #     num_m = 2,
    #     num_x = 3,
    #     indep_M = condition$indep_M[cond],
    #     int.XZ = condition$int.XZ[cond],
    #     if.null = condition$if.null[cond]
    # )
    
    gen_largeJ <- GenData_2.0( #GenData( 
        seedone = datseeds[iseed], # seedone, 
        J = largeJ, #condition$J[cond],
        nj = condition$nj[cond],
        quadratic.A = condition$quadratic.A[cond],
        quadratic.M = condition$quadratic.M[cond],
        quadratic.Y = condition$quadratic.Y[cond],
        icca = condition$icc[cond],
        iccm = condition$icc[cond],
        iccy = condition$icc[cond],
        x_z = condition$x_z[cond],
        Yfamily = as.character(condition$Yfamily[cond]), #condition$Yfamily[cond], #Yfamily,
        # num_m = 2,
        num_x = 3,
        indep_M = TRUE, #condition$indep_M[cond],
        int.XZ = condition$int.XZ[cond], # Keeping it true because both should use be 0.2 #FALSE, #<- testing to match current func #
        if.null = condition$if.null[cond]
    )
    
    # added function to add truevals to gen_largeJ (since it is not within GenData_2.0(), unlike GenData())
    temp <- trueVals2.0c(data_list = gen_largeJ, from_GenData_2.0 = TRUE)
    ## [ADD CODE TO MIMIC trueVals HERE & COMPARE RESULTS WITH trueVals2.0c; there is no reason (currently) why these should differ]
    gen_largeJ$truevals$truevals_cluster <- temp$truevals_cluster
    gen_largeJ$truevals$truevals_individual <- temp$truevals_individual
    
    trueEffs_individual <- effect_2.0(gen_largeJ$truevals$truevals_individual) %>% unlist()
    trueEffs_cluster <- effect_2.0(gen_largeJ$truevals$truevals_cluster) %>% unlist()
    
    # trueEffs_individual <- gen_largeJ$truevals_individual$`Y(a0=1, gm(a1=0))` - gen_largeJ$truevals_individual$`Y(a0=0, gm(a1=0))`
    
    # temp$truevals_individual$`Y(a0=1, gm(a1=0))` - temp$truevals_individual$`Y(a0=0, gm(a1=0))`
    # thetas[["DE(,M(0))"]] <- thetas[["Y(1,M(0))"]] - thetas[["Y(0,M(0))"]]
    # thetas[["IE(1,)"]] <- thetas[["Y(1,M(1))"]] - thetas[["Y(1,M(0))"]]
    
    
    if (condition$if.null[cond]==TRUE) {
        trueEffs_individual <- trueEffs_individual*0
        trueEffs_cluster <- trueEffs_cluster*0
    }
    res <- data.frame(condition[cond, ], estimand = names(trueEffs_cluster),
                      # trueEffs_individual,
                      # trueEffs_cluster,
                      cbind(trueEffs_individual, trueEffs_cluster), row.names = NULL)
    
    res
}


## Compare true values & effects computations ------------------------------
source("Functions/new-data-gen-functions.R") 
# ═══════════════════
#    Note: these outputs should be different  
# ═══════════════════

(t <- OneTrue_2.0())
# J nj quadratic.A quadratic.M quadratic.Y icc x_z  Yfamily indep_M int.XZ if.null          estimand trueEffs_individual trueEffs_cluster
# 1 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(a0=0, gm(a1=0))            1.549345         1.549397
# 2 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(a0=1, gm(a1=0))            4.554345         4.250549
# 3 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(a0=0, gm(a1=1))            1.984172         1.971044
# 4 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(a0=1, gm(a1=1))            4.989171         4.672196
t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"] - t[t$estimand == "Y(a0=0, gm(a1=0))", "trueEffs_individual"] # DE 3.005
t[t$estimand == "Y(a0=1, gm(a1=1))", "trueEffs_individual"] - t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"] # IE 0.4348266

source("Functions/fun_gen.R") # NOTE: ensure to re-import/run GenData (in sec 1)
OneTrue()
# J nj quadratic.A quadratic.M quadratic.Y icc x_z  Yfamily indep_M int.XZ if.null  estimand trueEffs_individual trueEffs_cluster
# 1 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(1,M(0))           2.5327099        2.5113506
# 2 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(1,M(1))           2.7798305        2.7445607
# 3 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE Y(0,M(0))           2.1501024        2.1490894
# 4 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE DE(,M(0))           0.3826075        0.3622612
# 5 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE    IE(1,)           0.2471206        0.2332100


# Quick Digression: comparing estimation to true values 
estEstimate_mediation
estMediatorCL # we can see that the estimates are off relative to both true value calculations 
# (we would expect OneTrue() to be off---b/c it uses trueVals() inside GenData() 
# which computes joint dist between 2 mediators---but it might be closer to the estimate than OneTrue_2.0())


# ══════════════════════════════
# # OneTrue_2.0() & OneTrue() are using there respective data generation functions (GenData_2.0() & GenData()) so it is 
# not strange for the true values to be different if the data gen functions are already generating different numbers 
# 
# It would be strange however if I replace trueVals2.0c() within OneTrue_2.0() to 
# mimic something close to trueVals() wihtin OneTrue() and get something 
# different---b/c mimicking trueVals() should be the correct way of computing 
# true values and thus differences in the new computation and trueVals2.0c() may 
# suggest issues in trueVals2.0c(). So that is what I will do next. 
# 
# Note: have Dr Liu double check this computation 
# ══════════════════════════════


# 5b. Mimic trueVals() & test true values  --------------------------------

# Maybe: if it matches trueVals (dr lius), then problematic; if it matches trueVals2.0c (your func), then possible validation
# ══════════════════════════════
# Here I am generating a large dataset then using code (closely aligning to 
# trueVals()) to compute the true values  
# ══════════════════════════════

# Steps:
## generate large data set
largeJ <- 5e4
iseed = 1; cond = 1
### load in modified functions for 1 mediator 
source("Functions/new-data-gen-functions.R")
gen_largeJ <- GenData_2.0( #GenData( 
    seedone = datseeds[iseed], # seedone, 
    J = largeJ, #condition$J[cond],
    nj = condition$nj[cond],
    quadratic.A = condition$quadratic.A[cond],
    quadratic.M = condition$quadratic.M[cond],
    quadratic.Y = condition$quadratic.Y[cond],
    icca = condition$icc[cond],
    iccm = condition$icc[cond],
    iccy = condition$icc[cond],
    x_z = condition$x_z[cond],
    Yfamily = as.character(condition$Yfamily[cond]), #condition$Yfamily[cond], #Yfamily,
    # num_m = 2,
    num_x = 3,
    indep_M = TRUE, #condition$indep_M[cond],
    int.XZ = condition$int.XZ[cond], # Keeping it true because both should use be 0.2 #FALSE, #<- testing to match current func #
    if.null = condition$if.null[cond]
)
## obtain truevals from large data set
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1)) #truevals <- 
truevals_individual <- truevals_cluster <- list()
# j <- 1
for (j in 1:nrow(a_vals)) {
    a0_val <- a_vals$a0[j]
    a1_val <- a_vals$a1[j]
    label <- glue::glue("Y(a0={a0_val}, gm(a1={a1_val}))")
    
    
    # E_y_each <- compute_expected_y(a0_val, a1_val, data)
    
    # Below is from within compute_expected_y()
    # Compute mediator parameters
    # m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m) #NOTE: does m (m1 in dr lius) need to go at beginning of next line? 
    m_latent <- gen_largeJ$gen_m$m1_on_a * a1_val + gen_largeJ$gen_m$m1_on_az * a1_val * gen_largeJ$datobs$Z + gen_largeJ$gen_m$m1_on_anj * a1_val * gen_largeJ$nj + gen_largeJ$m1_given
    p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - gen_largeJ$iccm))
    # m1 * p_m1 + (1 - m1) * (1 - p_m1)
    pm1 <- a1_val * p_m1 + (1 - a1_val) * (1 - p_m1) # this is equivalent to dr liu's p_joint which gets multiplied by my()
    
    # pm1 <- function(m1, a, z, nj, given, gen_m) { # pm1 from new-data-gen-functions.R
    #     latent <- gen_m[["m1_on_a"]] * a + gen_m[["m1_on_az"]] * a*z + gen_m[["m1_on_anj"]] * a*nj + given
    #     prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm1"]]))
    #     m1 * prob1 + (1 - m1) * (1 - prob1)
    # }
    
    latent_y <- gen_largeJ$gen_y$y_on_m1 * a1_val + 
        gen_largeJ$gen_y$y_on_a * a0_val + 
        gen_largeJ$gen_y$y_on_am1 * a0_val * a1_val + 
        gen_largeJ$gen_y$y_on_az * a0_val * gen_largeJ$datobs$Z +
        gen_largeJ$gen_y$y_on_m1z * a1_val * gen_largeJ$datobs$Z +
        gen_largeJ$gen_y$y_on_anj * a0_val * gen_largeJ$nj + 
        gen_largeJ$y_given
    binary <- FALSE # Y is not binary #TRUE
    if (binary) {
        cond_mean <- pnorm(latent_y, mean = 0, sd = sqrt(1 - gen_largeJ$gen_y$iccy))
    }
    if (!binary) {
        cond_mean <- latent_y
    }
    
    # my <- function(#m2,
    #     m1, a, z, nj, given, gen_y, binary = TRUE) {
    #     latent <- #gen_y[["y_on_m2"]] * m2 +
    #         gen_y[["y_on_m1"]] * m1 + gen_y[["y_on_a"]] * a +
    #         # gen_y[["y_on_am2"]] * a * m2 +
    #         gen_y[["y_on_am1"]] * a * m1 +
    #         # gen_y[["y_on_m1m2"]] * m1 * m2 +
    #         # gen_y[["y_on_am1m2"]] * a * m1 * m2 +
    #         gen_y[["y_on_az"]] * a * z + gen_y[["y_on_m1z"]] * m1 * z +
    #         # gen_y[["y_on_m2z"]] * m2 * z +
    #         gen_y[["y_on_anj"]] * a * nj +
    #         given
    # 
    #     if (binary) {
    #         cond_mean <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_y[["iccy"]]))
    #     }
    #     if (!binary) {
    #         cond_mean <- latent
    #     }
    # 
    #     cond_mean
    # }
    
    E_y_each <- pm1 * cond_mean
    
    # if (Mfamily == "binomial") {
        # ══════════════════════════════
        # Binary mediator:
        # p(M=1) = pnorm(m_latent)
        # p(M=0) = 1 - p(M=1)
        # E[Y] = p(M=0)*E[Y|M=0] + p(M=1)*E[Y|M=1]
        # ══════════════════════════════
        # p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - gen_largeJ$iccm))
        # p_m0 <- 1 - p_m1
        # 
        # # For each M = 0/1, compute latent Y
        # y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, nj = nj, given = given_y)
        # y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, nj = nj, given = given_y)
        # 
        # # Then get E[Y|M=0], E[Y|M=1] depending on outcome family
        # if (Yfamily == "binomial") {
        #     E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
        #     E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
        # } else {
        #     # If Y is gaussian
        #     E_y_m0 <- y_latent_m0
        #     E_y_m1 <- y_latent_m1
        # }
        # 
        # E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
        
    # }
    
    # E_y_each <- compute_expected_y(a0_val, a1_val, data)
    
    # Individual-level average
    truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
    
    # Cluster-level average
    cluster_means <- gen_largeJ$datobs |>
        dplyr::mutate(E_y = E_y_each) |>
        dplyr::group_by(school) |>
        dplyr::summarize(cluster_avg = mean(E_y, na.rm = TRUE))
    truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
}


# ══════════════════════════════
# these estimates do not match those from OneTrue_2.0() (which includes trueVals2.0c())
## DE
t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"]; truevals_individual$`Y(a0=1, gm(a1=0))`
# [1] 4.554345
# [1] 0.8821843
t[t$estimand == "Y(a0=0, gm(a1=0))", "trueEffs_individual"]; truevals_individual$`Y(a0=0, gm(a1=0))`
# [1] 1.549345
# [1] 0.3246575
t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"] - t[t$estimand == "Y(a0=0, gm(a1=0))", "trueEffs_individual"]
truevals_individual$`Y(a0=1, gm(a1=0))` - truevals_individual$`Y(a0=0, gm(a1=0))`
# [1] 3.005
# [1] 0.5575268
## IE 
t[t$estimand == "Y(a0=1, gm(a1=1))", "trueEffs_individual"]; truevals_individual$`Y(a0=1, gm(a1=1))`
# [1] 4.989171
# [1] 2.807353
t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"]; truevals_individual$`Y(a0=1, gm(a1=0))`
# [1] 4.554345
# [1] 0.8821843
t[t$estimand == "Y(a0=1, gm(a1=1))", "trueEffs_individual"] - t[t$estimand == "Y(a0=1, gm(a1=0))", "trueEffs_individual"] 
truevals_individual$`Y(a0=1, gm(a1=1))` - truevals_individual$`Y(a0=1, gm(a1=0))`
# [1] 0.4348266
# [1] 1.925169

# Comparing the DE & IE to the estimations: 
estEstimate_mediation[estEstimate_mediation$EffectVersion == "Individual-Avg", ]
#                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.45457774 0.12376721  0.20399848 0.7051570
# 2 Indirect Effect (IE) Individual-Avg 0.07363529 0.05366729 -0.03501958 0.1822902
truevals_individual$`Y(a0=1, gm(a1=0))` - truevals_individual$`Y(a0=0, gm(a1=0))` # [1] 0.5575268
# We see the DE is somewhat close to the estimate and within the CI
truevals_individual$`Y(a0=1, gm(a1=1))` - truevals_individual$`Y(a0=1, gm(a1=0))` # [1] 1.925169
# We see the IE is way off and not within the CI
# ══════════════════════════════







###################################################################################################### 



# # added function to add truevals to gen_largeJ (since it is not within GenData_2.0(), unlike GenData())
# temp <- trueVals2.0c(data_list = gen_largeJ, from_GenData_2.0 = TRUE)




# Putting GenData() output (which differs from GenData_2.0() output) into trueVals2.0c() 
# gives us estimands close to those with GenData_2.0() data (see below)

# change X.1 to X1 for gen_data$datobs; 
# put gen_data$datobs in trueVals2.0c() & see what you get 
temp_gen_data_test <- gen_data
temp_gen_data_test$datobs
# temp_gen_data_test <- gen_data$datobs
names(temp_gen_data_test$datobs) <- gsub(pattern = "X\\.", replacement = "X", names(temp_gen_data_test$datobs))

trueVals2.0c(data_list = temp_gen_data_test, from_GenData_2.0 = TRUE) |> 
    lapply(as.data.frame) |> 
    bind_rows(.id = "truevals_type") |> 
    pivot_longer(
        cols = -truevals_type,
        names_to = "estimand",
        values_to = "value"
    ) |> 
    pivot_wider(
        names_from = truevals_type,
        values_from = value
    )












################################## END #########################################




# Compare mediator  -------------------------------------------------------

set.seed(12)
datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

iseed <-9
cond <- 1

seedone <- datseeds[iseed]
gen_data_2 <- GenData_2.0(
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
    # num_m = 2,
    num_x = 3,
    indep_M = TRUE, #condition$indep_M[cond],
    int.XZ = condition$int.XZ[cond], # Keeping it true because both should use be 0.2 #FALSE, #<- testing to match current func #
    if.null = condition$if.null[cond]
)


# 
head(
    # gen_data_2$data$A
    gen_data_2$data$Z
    # gen_data_2$data$W_nj
    # gen_data_2$m1_given
)




# ══════════════════════════════
#    comparison of generated data 
# ══════════════════════════════

# 
sum(gen_data$data$school != gen_data_2$data$school)
sum(gen_data$data$Z != gen_data_2$data$Z)
sum(gen_data$data$A != gen_data_2$data$A)
# sum(gen_data$data$X[, 1] != gen_data_2$data$X1)
# sum(gen_data$data$X[, 2] != gen_data_2$data$X2)
# sum(gen_data$data$X[, 3] != gen_data_2$data$X3)
sum(gen_data$data$W_nj != gen_data_2$data$W_nj)

# 
## Regardless of int.XZ argument matching up Mediator & Outcome differ 
# Mediator differs
sum(gen_data$data$M1 != gen_data_2$data$M1)
# sum(gen_data$data$M2 != gen_data_2$data$M)

# head(gen_data_2$data$M)
# head(gen_data$data$M1)
# head(gen_data$data$M2)

# Outcomes differ
sum(gen_data$data$Y != gen_data_2$data$Y) # outcome still differs 
head(gen_data$data$Y)
head(gen_data_2$data$Y)

# dim(gen_data$data); dim(gen_data_2$data)

sum(gen_data$y_given != gen_data_2$y_given) # y_given differs 
sum(gen_data$condmy != gen_data_2$condmy) # so does condmy

# so what is everything going into y set to 
gen_data$gen_y 
gen_data_2$gen_y # looking at both list, all relevant parameter (ie, not m2 ones) are the same 

sum(gen_data$yb != gen_data_2$yb) # yb differs 
head(gen_data$yb, 30)
head(gen_data_2$yb, 30)

summary(gen_data$yb); summary(gen_data_2$yb)

sum(gen_data$J != gen_data_2$J)

sum(gen_data$nj_sizes != gen_data_2$nj_sizes)

gen_data$iccy
gen_data_2$iccy

# sum(gen_data$datobs$X.1 != gen_data_2$datobs$X.1)
# sum(gen_data$datobs$X.2 != gen_data_2$datobs$X.2)
# sum(gen_data$datobs$X.3 != gen_data_2$datobs$X.3)

gen_data$Yfamily
sum(gen_data$ab != gen_data_2$ab)

gen_data$gen_y$y_on_anj
gen_data_2$gen_y$y_on_anj

gen_data$truevals


rm(list = setdiff(ls(), "gen_data"))
rm(list = setdiff(ls(), "gen_data_2"))

