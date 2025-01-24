################################################################################
##################### Testing Data Generation Functions ####################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 2025-01-17
#
#
# Script Description: 
#   I am using this script to test, modify, and improve the data generation & true value functions
# 
# 
# Last Updated: 2025-01-23  
#
#
# Notes:
#   To-Do:
#
#   Done: 
#
################################################################################



# Load packages and functions ---------------------------------------------

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
    "trueVals2.0d",
    "trueVals2.0e", # under testing 
    "trueVals2.0f", # under testing 
    
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




# Set sim conditions & methods --------------------------------------------

# ══════════════════════════════
#     Simulation Conditions 
# ══════════════════════════════
conditions_all <- data.frame(rbind(
    expand.grid(
        J = c(10, 20, 40),
        Nj_low = c(50),
        Nj_high = c(100), 
        quadratic = c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian"), 
        if.null = c(F,T)
    ),
    expand.grid(
        J = c(40, 70, 100),
        Nj_low = c(5),
        Nj_high = c(20), 
        quadratic = c(T, F), 
        Mfamily = c("binomial", "gaussian"),
        Yfamily = c("binomial", "gaussian"), 
        if.null = c(F,T)
    )
), 
icc = c(0.2))

# limit conditions for testing 
condition <- conditions_all |> 
    filter(quadratic == F) |> 
    filter(if.null == F) |> 
    filter(J %in% c(20)) |> 
    filter(Nj_low == 50) |> 
    filter(Mfamily %in% c("gaussian"), Yfamily %in% c("binomial"))
# conditions <- conditions[1:10, ]
## binomial M & Y 
# conditions <- conditions[1:3, ]
## focus on binomial M
# conditions <- conditions[conditions$Mfamily == "binomial", ]


# ══════════════════════════════
#    Methods  
# ══════════════════════════════
methds_all <- data.frame(expand.grid(
    cluster_a = "FE", #c("FE", "RE", "noncluster"), # "RE", # "noncluster", #
    cluster_m = "FE", # c("FE", "RE", "noncluster"), # "RE", #  "noncluster.mlr", #
    cluster_y =  "FE", #c("FE", "RE", "noncluster"), # "noncluster.mlr", ## "FE.mlr", #
    # interact_fitm2 =  c(T), # NULL, #
    # interact_fity = c(T), # NULL, #
    # Morder = c("21", "12"),
    Fit = c("mlr","glm"), # 
    # cluster_opt_a = c("sufficient_stats",  "cwc.FE"), # "FE.glm", #  
    # cluster_opt_m = c("sufficient_stats",  "cwc.FE"),  #"FE.glm", # 
    # cluster_opt_y = c("sufficient_stats",  "cwc.FE") # "cwc.FE"#c("sufficient_stats") #, 
    cluster_opt = c("cwc.FE", "cwc") #,  "noncluster.glm"
)) %>% 
    mutate(
        cluster_opt_a = cluster_opt, 
        cluster_opt_m = cluster_opt, 
        cluster_opt_y = cluster_opt
    )

# (methds <- methds_all %>%
#     filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr") ))
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr"))



# Set seeds & cond --------------------------------------------------------

# Set seed & condition 
set.seed(12)
datseeds <- c(sample(1:1e6, 3000), sample(1:1e6+1e6, 200))

iseed <- 10 #9
cond <- 1






# Test Generate data  -----------------------------------------------------

data_list <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = condition$Mfamily[cond], 
    Yfamily = condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = TRUE
)


# DE 
data_list$truevals$truevals_individual$`Y(a0=1, gm(a1=0))` - data_list$truevals$truevals_individual$`Y(a0=0, gm(a1=0))`
data_list$effects$individual$pnde

# J nj quadratic.A quadratic.M quadratic.Y icc x_z  Yfamily indep_M int.XZ if.null
# 1 70  5       FALSE       FALSE       FALSE 0.2   0 gaussian   FALSE   TRUE   FALSE





# Estimate ----------------------------------------------------------------

test <- estimate_mediation(
    data = data_list$data,
    Sname = "school",
    Wnames = names(data_list$data)[grep("^W", names(data_list$data))],
    Xnames = names(data_list$data)[grep("^X", names(data_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)


# Why is the difference so huge? 
test
#               Effect  EffectVersion   Estimate   StdError     CILower   CIUpper   # J=20; nj=[50,100]; linear
# 1   Direct Effect (DE) Individual-Avg 0.33594877 0.09619228  0.12292458 0.5489730
# 2 Indirect Effect (IE) Individual-Avg 0.08475035 0.04375195 -0.01214125 0.1816420
# 3   Direct Effect (DE)    Cluster-Avg 0.29501397 0.08465024  0.10755041 0.4824775
# 4 Indirect Effect (IE)    Cluster-Avg 0.07403727 0.03868515 -0.01163356 0.1597081
data_list$effects$individual$pnde
# [1] 0.3319704
data_list$effects$individual$tnie
# [1] 0.09314005
data_list$effects$individual$tnde
data_list$effects$individual$pnie

# Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.47256542 0.10476082  0.26046651 0.6846643
# 2 Indirect Effect (IE) Individual-Avg 0.08061567 0.04659768 -0.01372605 0.1749574
# 3   Direct Effect (DE)    Cluster-Avg 0.42020278 0.10270657  0.21226292 0.6281426
# 4 Indirect Effect (IE)    Cluster-Avg 0.07484828 0.04877704 -0.02390578 0.1736023
data_list$effects$individual$pnde
# 2.927903







# will need this later on 
if (Fit == "glm") {
    learners_a <- learners_m <- learners_y <- c("SL.glm")
    num_folds <- 1
}
if (Fit == "mlr") {
    learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
    # try
    #learners_a <- learners_m <- learners_y <- c("SL.gam.modified")
    #learners_a <- learners_m <- learners_y <- c("SL.lightgbm", "SL.gam") #, "SL.glm.interaction"
    
    # learners_a <- c("SL.ranger.modified")
    num_folds <- 5 # ceiling(condition$J[cond]/1)
}







###
ind_summary <- readRDS(file = "Output/S1_Results/Tables/S1_performance-measures.rds")


load(file = "Output/S1_Results/Tables/S1_performance-measures.rds")

saveRDS(ind_summary, file = "Output/S1_Results/Tables/S1_performance-measures.rds")


# check trueVals2.0e ------------------------------------------------------

# load function 
# source("Functions/trueVals2.0e")

# using J=20, nj=[50,100]

### test binomial M & gaussian Y  -------------------------------------------

# generate data 
list_bin_gau <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "binomial", #condition$Mfamily[cond], 
    Yfamily = "gaussian", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = TRUE
)

# estimate 
est_bin_gau <- estimate_mediation(
    data = list_bin_gau$data,
    Sname = "school",
    Wnames = names(list_bin_gau$data)[grep("^W", names(list_bin_gau$data))],
    Xnames = names(list_bin_gau$data)[grep("^X", names(list_bin_gau$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)

# estimation 
est_bin_gau
#               Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.33594877 0.09619228  0.12292458 0.5489730
# 2 Indirect Effect (IE) Individual-Avg 0.08475035 0.04375195 -0.01214125 0.1816420
# 3   Direct Effect (DE)    Cluster-Avg 0.29501397 0.08465024  0.10755041 0.4824775
# 4 Indirect Effect (IE)    Cluster-Avg 0.07403727 0.03868515 -0.01163356 0.1597081

# existing true effects
## DE
list_bin_gau$effects$individual$pnde
# 0.3319704
## IE
list_bin_gau$effects$individual$tnie
# 0.09314005

# true values 
trueval_bin_gau <- trueVals2.0e(data_list = list_bin_gau, 
                             # J_big = 10000, 
                             from_GenData_2.0 = FALSE)
# DE
trueval_bin_gau$truevals_individual$`Y(a0=1, gm(a1=0))` - trueval_bin_gau$truevals_individual$`Y(a0=0, gm(a1=0))`
# 0.3381905
# IE 
trueval_bin_gau$truevals_individual$`Y(a0=1, gm(a1=1))` - trueval_bin_gau$truevals_individual$`Y(a0=1, gm(a1=0))`
# 0.08925424



### test binomial M & binomial Y  -------------------------------------------

# generate data 
list_bin_bin <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "binomial", #condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = TRUE
)

# estimate 
est_bin_bin <- estimate_mediation(
    data = list_bin_bin$data,
    Sname = "school",
    Wnames = names(list_bin_bin$data)[grep("^W", names(list_bin_bin$data))],
    Xnames = names(list_bin_bin$data)[grep("^X", names(list_bin_bin$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)

# estimation 
est_bin_bin
#             Effect  EffectVersion     Estimate    StdError     CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg -0.017087511 0.022876734 -0.06774956 0.03357453
# 2 Indirect Effect (IE) Individual-Avg  0.004769265 0.004037055 -0.00417106 0.01370959
# 3   Direct Effect (DE)    Cluster-Avg -0.019269906 0.023281675 -0.07082872 0.03228891
# 4 Indirect Effect (IE)    Cluster-Avg  0.004884698 0.004000308 -0.00397425 0.01374365

# existing true effects
## DE
list_bin_bin$effects$individual$pnde
# 0.02697363
## IE
list_bin_bin$effects$individual$tnie
# 0.007471182

# true values 
trueval_bin_bin <- trueVals2.0e(data_list = list_bin_bin, 
                                # J_big = 10000, 
                                from_GenData_2.0 = FALSE)

# DE
trueval_bin_bin$truevals_individual$`Y(a0=1, gm(a1=0))` - trueval_bin_bin$truevals_individual$`Y(a0=0, gm(a1=0))`
# 0.02831127
# IE 
trueval_bin_bin$truevals_individual$`Y(a0=1, gm(a1=1))` - trueval_bin_bin$truevals_individual$`Y(a0=1, gm(a1=0))`
# 0.006037628



### test gaussian M & binomial Y  -------------------------------------------

# generate data 
list_gau_bin <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", #condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = TRUE
)

# estimate 
est_gau_bin <- estimate_mediation(
    data = list_gau_bin$data,
    Sname = "school",
    Wnames = names(list_gau_bin$data)[grep("^W", names(list_gau_bin$data))],
    Xnames = names(list_gau_bin$data)[grep("^X", names(list_gau_bin$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)

# estimation 
est_gau_bin
#             Effect  EffectVersion      Estimate    StdError      CILower     CIUpper
# 1   Direct Effect (DE) Individual-Avg -0.0030268048 0.033168437 -0.076480506 0.070426897
# 2 Indirect Effect (IE) Individual-Avg  0.0005092192 0.003246046 -0.006679366 0.007697804
# 3   Direct Effect (DE)    Cluster-Avg -0.0187365717 0.035525429 -0.097409989 0.059936846
# 4 Indirect Effect (IE)    Cluster-Avg -0.0003091689 0.003741091 -0.008594062 0.007975725

# existing true effects
## DE
list_gau_bin$effects$individual$pnde
# 0.03954568
## IE
list_gau_bin$effects$individual$tnie
# 0.03000016

# true values 
trueval_gau_bin <- trueVals2.0e(data_list = list_gau_bin, 
                                # J_big = 10000, 
                                from_GenData_2.0 = FALSE)

# DE
trueval_gau_bin$truevals_individual$`Y(a0=1, gm(a1=0))` - trueval_gau_bin$truevals_individual$`Y(a0=0, gm(a1=0))`
# 0.04546613
# IE 
trueval_gau_bin$truevals_individual$`Y(a0=1, gm(a1=1))` - trueval_gau_bin$truevals_individual$`Y(a0=1, gm(a1=0))`
# 0.02978871



### test gaussian M & gaussian Y  -------------------------------------------

# generate data 
list_gau_gau <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", #condition$Mfamily[cond], 
    Yfamily = "gaussian", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = TRUE
)

# estimate 
est_gau_gau <- estimate_mediation(
    data = list_gau_gau$data,
    Sname = "school",
    Wnames = names(list_gau_gau$data)[grep("^W", names(list_gau_gau$data))],
    Xnames = names(list_gau_gau$data)[grep("^X", names(list_gau_gau$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)

# estimation 
est_gau_gau
#                 Effect  EffectVersion    Estimate   StdError     CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg  0.21444581 0.07667511  0.04464371 0.3842479
# 2 Indirect Effect (IE) Individual-Avg -0.00456208 0.07315302 -0.16656429 0.1574401
# 3   Direct Effect (DE)    Cluster-Avg  0.19865322 0.07213064  0.03891514 0.3583913
# 4 Indirect Effect (IE)    Cluster-Avg  0.01833312 0.07612265 -0.15024552 0.1869118

# existing true effects
## DE
list_gau_gau$effects$individual$pnde
# 0.332555
## IE
list_gau_gau$effects$individual$tnie
# 0.3806687

# true values 
trueval_gau_gau <- trueVals2.0e(data_list = list_gau_gau, 
                                # J_big = 10000, 
                                from_GenData_2.0 = FALSE)

# DE
trueval_gau_gau$truevals_individual$`Y(a0=1, gm(a1=0))` - trueval_gau_gau$truevals_individual$`Y(a0=0, gm(a1=0))`
# 0.320392
# IE 
trueval_gau_gau$truevals_individual$`Y(a0=1, gm(a1=1))` - trueval_gau_gau$truevals_individual$`Y(a0=1, gm(a1=0))`
# 0.3643199



# ══════════════════════════════
# NOTE:
# in the above testing the trueVals2.0e() did not work in obtaining an accurate tnie estimate when mediator is gaussian and outcome is either binomial or gaussian
# 
# Running trueVals2.0e() inside the generate data func on 2025-01-20 and the true tnie was larger than the upper CI for cases with gaussian mediator & true tnie was smaller than the lower CI in binomial mediator cases. 
# also in the case with gaussian mediator and gaussian outcome the true pnde was also much larger than upper CI
# 
# trueVals2.0d() in generate data func resulted in 
# - more accurate true pnde effects than trueVals2.0e()
# - for binomail M, true tnie was within CI
# 
# - but for gaussian M, true tnie was consistently larger than upper CI

# ══════════════════════════════












trueVals2.0e(data_list = data_list, 
             J_big = 10000)

data_list$parameters$Mfamily
data_list$parameters$Yfamily

trueval_test <- trueVals2.0e(data_list = data_list, 
             J_big = 10000, 
             from_GenData_2.0 = FALSE)


test

trueval_test$truevals_individual$`Y(a0=1, gm(a1=0))` - trueval_test$truevals_individual$`Y(a0=0, gm(a1=0))`



# Individual-level potential outcomes
y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`

# Compute individual-level mediation effects
pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect



# test gaussian M & Y true value calculations -----------------------------

## 
# num_reps <- 5
# for (i in seq_len(num_reps)) {
#     
#     
# }
# generate data
test_list <- generate_data2.0c(
    J = condition$J[cond], 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", # condition$Mfamily[cond], 
    Yfamily = "gaussian", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 

test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)


# Why is the difference so huge? 
# test_est


## Test possible solutions on one dataset prior to implementing (de --------

# helper functions
# Calculate latent mean for M
calc_m_latent <- function(a, z, W_nj, given) {
    latent_m <- gen_m$m_on_a * a +
        gen_m$m_on_az * a * z +
        gen_m$m_on_anj * a * W_nj +
        given
    return(latent_m)
}

# Calculate latent outcome Y
calc_y_latent <- function(m, a, z, W_nj, given) {
    latent_y <- gen_y$y_on_m * m +
        gen_y$y_on_a * a +
        gen_y$y_on_am * a * m +
        gen_y$y_on_az * a * z +
        gen_y$y_on_mz * m * z +
        gen_y$y_on_anj * a * W_nj +
        given
    return(latent_y)
}


# Extract mediator parameters
gen_m <- list(
    iccm = test_list$parameter$iccm,
    m_on_a = test_list$parameter$m_on_a,
    m_on_x = test_list$parameter$m_on_x,
    m_on_z = test_list$parameter$m_on_z,
    m_on_az = test_list$parameter$m_on_az,
    m_on_anj = test_list$parameter$m_on_anj
)

# Extract outcome parameters
gen_y <- list(
    iccy = test_list$parameter$iccy,
    yintercept = test_list$parameter$yintercept,
    y_on_a = test_list$parameter$y_on_a,
    y_on_m = test_list$parameter$y_on_m,
    y_on_am = test_list$parameter$y_on_am,
    y_on_az = test_list$parameter$y_on_az,
    y_on_mz = test_list$parameter$y_on_mz,
    y_on_anj = test_list$parameter$y_on_anj,
    y_on_x = test_list$parameter$y_on_x,
    y_on_z = test_list$parameter$y_on_z
)


# put in loop 
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
results_list <- list()
for (i in 1:nrow(a_vals)) {
    a0_val <- a_vals$a0[i]
    a1_val <- a_vals$a1[i]
    label <- sprintf("Y(a0=%d, gm(a1=%d))", a0_val, a1_val)
# 
m_latent <- calc_m_latent(a = a1_val, #1, 
                          z = test_list$data$Z, 
                          W_nj = test_list$data$W_nj, 
                          given = test_list$parameters$m_given)
# number of draws done multiple times from the distribution of M 
num_draws <- 5000
N <- nrow(test_list$data)
# Create matrix of repeated draws from M distribution for each subject i
set.seed(datseeds[iseed])
M_draws <- sapply(1:num_draws, function(k) {
    rnorm(N, mean = m_latent, sd = sqrt(1 - test_list$parameters$iccm))
})
# For each subject i & draw k, we compute the latent Y under A=0 (stored in y_latent_mat)
y_latent_mat <- matrix(NA, nrow = N, ncol = num_draws)
for (k in seq_len(num_draws)) {
    y_latent_mat[, k] <- calc_y_latent(
        m = M_draws[, k], 
        a = a0_val, #0, 
        z = test_list$data$Z, 
        W_nj = test_list$data$W_nj, 
        given = test_list$parameters$y_given
    )
}
# 4) Because Y is continuous normal with intraclass correlation iccy,
#    the final outcome is Y_latent + N(0, 1 - iccy).
#    But the *expected value* of Y is just Y_latent, so we do not
#    add a random draw for Y if we want E[Y].
#    The expected outcome for each subject i is the average across draws k.
E_Y_each_subj <- rowMeans(y_latent_mat)
# overall E[Y(a0=0, gm(a1=1))]
mean(E_Y_each_subj)

# E[]
results_list[[label]] <- list(
    true = mean(E_Y_each_subj), 
    sd_true = sd(E_Y_each_subj)
)
}

# Compare to existing true vals & compare effect to estimated effect 
# test_list$truevals$truevals_individual$`Y(a0=0, gm(a1=1))`

# results_list

comparison_df <- data.frame(
    name = names(results_list), 
    true = sapply(results_list, function(x) x$true),
    sd_true = sapply(results_list, function(x) x$sd_true),
    existing_true = unlist(test_list$truevals$truevals_individual), 
    p_outcome = names(test_list$truevals$truevals_individual)
)

test_est
#                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.2184985 0.07119759 0.06082671 0.3761703
# 2 Indirect Effect (IE) Individual-Avg 0.3050502 0.10671986 0.06871199 0.5413884
# 3   Direct Effect (DE)    Cluster-Avg 0.2069983 0.06947993 0.05313040 0.3608662
# 4 Indirect Effect (IE)    Cluster-Avg 0.2969619 0.09964902 0.07628257 0.5176413

## DE (PNDE)
# comparison_df$true[2] - comparison_df$true[1]
# comparison_df$existing_true[2] - comparison_df$existing_true[1]
comparison_df["Y(a0=1, gm(a1=0))", "true"] - comparison_df["Y(a0=0, gm(a1=0))", "true"]
comparison_df["Y(a0=1, gm(a1=0))", "existing_true"] - comparison_df["Y(a0=0, gm(a1=0))", "existing_true"]
# [1] 0.3311595
# [1] 0.332555
## IE (TNIE)
# comparison_df$true[4] - comparison_df$true[2]
# comparison_df$existing_true[4] - comparison_df$existing_true[2]
comparison_df["Y(a0=1, gm(a1=1))", "true"] - comparison_df["Y(a0=1, gm(a1=0))", "true"]
comparison_df["Y(a0=1, gm(a1=1))", "existing_true"] - comparison_df["Y(a0=1, gm(a1=0))", "existing_true"]
# [1] 0.3870896
# [1] 0.3806687




# test gaussian M & binomial Y true value calculations -----------------------------

# generate data
test_list <- generate_data2.0c(
    J = 40, # condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", # condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 

test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)



# Gaussian M & Binomial Y 
test_est
#                 Effect  EffectVersion   Estimate   StdError     CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.03277956 0.01191602 0.008030162 0.05752895
# 2 Indirect Effect (IE) Individual-Avg 0.03960298 0.01159223 0.015526091 0.06367987
# 3   Direct Effect (DE)    Cluster-Avg 0.02924917 0.01211974 0.004076664 0.05442168
# 4 Indirect Effect (IE)    Cluster-Avg 0.03666673 0.01052841 0.014799388 0.05853408

## DE (PNDE)
test_list$effects$individual$pnde
# 0.04128271
## IE (TNIE)
test_list$effects$individual$tnie
# 0.03406409





# test binomial M & Y true value calculations -----------------------------------------

# generate data
test_list <- generate_data2.0c(
    J = 40, # condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "binomial", # condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 

test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)



# Binomial M & Y 
test_est
#                 Effect  EffectVersion    Estimate    StdError      CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.020364937 0.017272399 -0.015509570 0.05623944
# 2 Indirect Effect (IE) Individual-Avg 0.007403764 0.004962891 -0.002904084 0.01771161
# 3   Direct Effect (DE)    Cluster-Avg 0.016413784 0.018662088 -0.022347085 0.05517465
# 4 Indirect Effect (IE)    Cluster-Avg 0.007764030 0.005047396 -0.002719334 0.01824739

## DE (PNDE)
test_list$effects$individual$pnde
# 0.03102391
## IE (TNIE)
test_list$effects$individual$tnie
# 0.007865101



# Find good true effect sizes (for binomial Y cases) -----------------------------------------

# generate data
test_list <- generate_data2.0c(
    J = 40, # condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "binomial", # condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 3, #0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.5, #0.2, 
    y_on_m = 3, #1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 
test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = c("SL.nnet", "SL.gam"),
    learners_m = c("SL.nnet", "SL.gam"),
    learners_y = c("SL.nnet", "SL.gam"),
    cluster_opt = methds$cluster_opt,
    num_folds = 5 
    # Yfamily = "gaussian"
)



# Binomial M & Y 
## OG
#                 Effect  EffectVersion    Estimate    StdError      CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.020364937 0.017272399 -0.015509570 0.05623944
# 2 Indirect Effect (IE) Individual-Avg 0.007403764 0.004962891 -0.002904084 0.01771161
# 3   Direct Effect (DE)    Cluster-Avg 0.016413784 0.018662088 -0.022347085 0.05517465
# 4 Indirect Effect (IE)    Cluster-Avg 0.007764030 0.005047396 -0.002719334 0.01824739

## m_on_a = 1; y_on_a = 0.2, y_on_m = 2
test_est
#               Effect  EffectVersion   Estimate    StdError    CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.04839861 0.015397638 0.01641795 0.08037926
# 2 Indirect Effect (IE) Individual-Avg 0.05257806 0.009655345 0.03252406 0.07263206
# 3   Direct Effect (DE)    Cluster-Avg 0.05191630 0.017740009 0.01507058 0.08876203
# 4 Indirect Effect (IE)    Cluster-Avg 0.05562672 0.012204233 0.03027872 0.08097473
## DE (PNDE)
test_list$effects$individual$pnde
# 0.02662902
## IE (TNIE)
test_list$effects$individual$tnie
# 0.0581877

## m_on_a = 3; y_on_a = 1, y_on_m = 3
test_est
#                 Effect  EffectVersion   Estimate   StdError    CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.12223303 0.01956252 0.08160197 0.16286409
# 2 Indirect Effect (IE) Individual-Avg 0.03560799 0.01069439 0.01339591 0.05782006
# 3   Direct Effect (DE)    Cluster-Avg 0.12628295 0.02038432 0.08394503 0.16862086
# 4 Indirect Effect (IE)    Cluster-Avg 0.03971875 0.01220011 0.01437931 0.06505820
## DE (PNDE)
test_list$effects$individual$pnde
# 0.110257
## IE (TNIE)
test_list$effects$individual$tnie
# 0.0414448

## m_on_a = 2; y_on_a = 1, y_on_m = 3
test_est
#                 Effect  EffectVersion   Estimate   StdError      CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.11494153 0.02371839  0.065678804 0.16420425
# 2 Indirect Effect (IE) Individual-Avg 0.03979671 0.01932571 -0.000342479 0.07993591
# 3   Direct Effect (DE)    Cluster-Avg 0.11547642 0.02406013  0.065503905 0.16544893
# 4 Indirect Effect (IE)    Cluster-Avg 0.04445375 0.02226262 -0.001785378 0.09069287
## DE (PNDE)
test_list$effects$individual$pnde
# 0.1078391
## IE (TNIE)
test_list$effects$individual$tnie
# 0.04280788

## m_on_a = 3; y_on_a = 0.5, y_on_m = 3
test_est
#                 Effect  EffectVersion    Estimate   StdError    CILower     CIUpper
# 1   Direct Effect (DE) Individual-Avg -0.09845611 0.01931358 -0.1385701 -0.05834210
# 2 Indirect Effect (IE) Individual-Avg  0.26128510 0.03558564  0.1873743  0.33519592
# 3   Direct Effect (DE)    Cluster-Avg -0.10119117 0.01803229 -0.1386440 -0.06373837
# 4 Indirect Effect (IE)    Cluster-Avg  0.26927977 0.03568529  0.1951620  0.34339756
## DE (PNDE)
test_list$effects$individual$pnde
# 0.06229872
## IE (TNIE)
test_list$effects$individual$tnie
# 0.09586237





# test cwc.FE with mlr ----------------------------------------------------

Fit <- "mlr"

# will need this later on 
if (Fit == "glm") {
    learners_a <- learners_m <- learners_y <- c("SL.glm")
    num_folds <- 1
}
if (Fit == "mlr") {
    learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
    # try
    #learners_a <- learners_m <- learners_y <- c("SL.gam.modified")
    #learners_a <- learners_m <- learners_y <- c("SL.lightgbm", "SL.gam") #, "SL.glm.interaction"
    
    # learners_a <- c("SL.ranger.modified")
    num_folds <- 5 # ceiling(condition$J[cond]/1)
}



# generate data
test_list <- generate_data2.0c(
    J = condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", # condition$Mfamily[cond], 
    Yfamily = "gaussian", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 
test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = learners_a, 
    learners_m = learners_m, 
    learners_y = learners_y, 
    cluster_opt = "cwc.FE", # "cwc", # methds$cluster_opt,
    num_folds = num_folds
    # Yfamily = "gaussian"
)



## Testing with each type of mediator-outcome type comibination 

# Gaussian M & Y 
test_est
## cwc.FE
#                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.2126999 0.07213054 0.05296201 0.3724378
# 2 Indirect Effect (IE) Individual-Avg 0.3045689 0.10493213 0.07218976 0.5369481
# 3   Direct Effect (DE)    Cluster-Avg 0.2020708 0.07140341 0.04394317 0.3601983
# 4 Indirect Effect (IE)    Cluster-Avg 0.2949364 0.09782502 0.07829639 0.5115764
## cwc
#                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.2184985 0.07119759 0.06082671 0.3761703
# 2 Indirect Effect (IE) Individual-Avg 0.3050502 0.10671986 0.06871199 0.5413884
# 3   Direct Effect (DE)    Cluster-Avg 0.2069983 0.06947993 0.05313040 0.3608662
# 4 Indirect Effect (IE)    Cluster-Avg 0.2969619 0.09964902 0.07628257 0.5176413
## DE (PNDE)
test_list$effects$individual$pnde
# 0.3321417
## IE (TNIE)
test_list$effects$individual$tnie
# 0.3853798

# # Binomial M & Y 
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion    Estimate    StdError       CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.009316415 0.016702499 -0.0276723759 0.04630521
# # 2 Indirect Effect (IE) Individual-Avg 0.015103382 0.007229113 -0.0009059689 0.03111273
# # 3   Direct Effect (DE)    Cluster-Avg 0.006945141 0.018109352 -0.0331592200 0.04704950
# # 4 Indirect Effect (IE)    Cluster-Avg 0.016868808 0.007777808 -0.0003556623 0.03409328
# ## cwc
# #                 Effect  EffectVersion    Estimate    StdError      CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.009043935 0.019213924 -0.033506573 0.05159444
# # 2 Indirect Effect (IE) Individual-Avg 0.014084890 0.008110120 -0.003875507 0.03204529
# # 3   Direct Effect (DE)    Cluster-Avg 0.005857833 0.020986279 -0.040617673 0.05233334
# # 4 Indirect Effect (IE)    Cluster-Avg 0.016327615 0.008253289 -0.001949841 0.03460507
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.02697363
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.007471182

# # Binomial M & Gaussian Y 
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion   Estimate   StdError    CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.30527152 0.09898565 0.08606124 0.5244818
# # 2 Indirect Effect (IE) Individual-Avg 0.10647170 0.04301901 0.01120324 0.2017402
# # 3   Direct Effect (DE)    Cluster-Avg 0.25996599 0.08834509 0.06431993 0.4556120
# # 4 Indirect Effect (IE)    Cluster-Avg 0.09781993 0.03737965 0.01504022 0.1805996
# ## cwc
# #                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.33594877 0.09619228  0.12292458 0.5489730
# # 2 Indirect Effect (IE) Individual-Avg 0.08475035 0.04375195 -0.01214125 0.1816420
# # 3   Direct Effect (DE)    Cluster-Avg 0.29501397 0.08465024  0.10755041 0.4824775
# # 4 Indirect Effect (IE)    Cluster-Avg 0.07403727 0.03868515 -0.01163356 0.1597081
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.3319704
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.09314005

# # Gaussian M & Binomial Y 
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion   Estimate   StdError      CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.06442433 0.02333915  0.012738237 0.11611043
# # 2 Indirect Effect (IE) Individual-Avg 0.02630546 0.01282966 -0.002106664 0.05471758
# # 3   Direct Effect (DE)    Cluster-Avg 0.06388714 0.02328610  0.012318525 0.11545575
# # 4 Indirect Effect (IE)    Cluster-Avg 0.02617538 0.01386252 -0.004524082 0.05687484
# ## cwc
# #                 Effect  EffectVersion   Estimate   StdError      CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.06845471 0.02526958  0.012493545 0.12441588
# # 2 Indirect Effect (IE) Individual-Avg 0.02264404 0.01181641 -0.003524185 0.04881227
# # 3   Direct Effect (DE)    Cluster-Avg 0.07085395 0.02571406  0.013908457 0.12779944
# # 4 Indirect Effect (IE)    Cluster-Avg 0.02108829 0.01279068 -0.007237529 0.04941410
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.03539488
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.02753713


# test cwc & cwc.FE with glm ----------------------------------------------------

Fit <- "glm"

# will need this later on 
if (Fit == "glm") {
    learners_a <- learners_m <- learners_y <- c("SL.glm")
    num_folds <- 1
}
if (Fit == "mlr") {
    learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
    # try
    #learners_a <- learners_m <- learners_y <- c("SL.gam.modified")
    #learners_a <- learners_m <- learners_y <- c("SL.lightgbm", "SL.gam") #, "SL.glm.interaction"
    
    # learners_a <- c("SL.ranger.modified")
    num_folds <- 5 # ceiling(condition$J[cond]/1)
}



# generate data
test_list <- generate_data2.0c(
    J = condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", # condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 
test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = learners_a, 
    learners_m = learners_m, 
    learners_y = learners_y, 
    cluster_opt = "cwc.FE", # "cwc", # methds$cluster_opt,
    num_folds = num_folds
    # Yfamily = "gaussian"
)



## Testing with each type of mediator-outcome type comibination 

# # Gaussian M & Y 
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.2353662 0.07068312 0.07883373 0.3918987
# # 2 Indirect Effect (IE) Individual-Avg 0.2834967 0.10587998 0.04901851 0.5179750
# # 3   Direct Effect (DE)    Cluster-Avg 0.2232786 0.06767193 0.07341457 0.3731425
# # 4 Indirect Effect (IE)    Cluster-Avg 0.2755489 0.10089599 0.05210807 0.4989898
# ## cwc
# #                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.2357069 0.07094543 0.07859352 0.3928203
# # 2 Indirect Effect (IE) Individual-Avg 0.2826616 0.10546240 0.04910811 0.5162151
# # 3   Direct Effect (DE)    Cluster-Avg 0.2236371 0.06802213 0.07299756 0.3742766
# # 4 Indirect Effect (IE)    Cluster-Avg 0.2746814 0.10042659 0.05228010 0.4970828
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.3321417
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.3853798

# # Binomial M & Y
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion     Estimate     StdError      CILower      CIUpper
# # 1   Direct Effect (DE) Individual-Avg  0.087954009 0.0304799849  0.009370786 0.1665372316
# # 2 Indirect Effect (IE) Individual-Avg -0.001903641 0.0010592532 -0.004634598 0.0008273161
# # 3   Direct Effect (DE)    Cluster-Avg  0.089580722 0.0268258517  0.020418553 0.1587428921
# # 4 Indirect Effect (IE)    Cluster-Avg -0.001585056 0.0009355386 -0.003997053 0.0008269412
# ## cwc
# #                 Effect  EffectVersion    Estimate    StdError      CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.078791643 0.031054410 -0.001272557 0.15885584
# # 2 Indirect Effect (IE) Individual-Avg 0.003500604 0.004539958 -0.008204276 0.01520548
# # 3   Direct Effect (DE)    Cluster-Avg 0.081018320 0.027553605  0.009979863 0.15205678
# # 4 Indirect Effect (IE)    Cluster-Avg 0.003696824 0.004385155 -0.007608944 0.01500259
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.02415721
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.004881029

# # Binomial M & Gaussian Y
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.35946560 0.09015455  0.12702995 0.5919013
# # 2 Indirect Effect (IE) Individual-Avg 0.03805111 0.04618066 -0.08101146 0.1571137
# # 3   Direct Effect (DE)    Cluster-Avg 0.36976505 0.09086902  0.13548736 0.6040427
# # 4 Indirect Effect (IE)    Cluster-Avg 0.02830833 0.04163681 -0.07903932 0.1356560
# ## cwc
# #                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.36062680 0.08920203  0.13064694 0.5906067
# # 2 Indirect Effect (IE) Individual-Avg 0.03815549 0.04592503 -0.08024801 0.1565590
# # 3   Direct Effect (DE)    Cluster-Avg 0.37071052 0.08930733  0.14045915 0.6009619
# # 4 Indirect Effect (IE)    Cluster-Avg 0.02856138 0.04135852 -0.07806878 0.1351915
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.2752673
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.08320804

# # Gaussian M & Binomial Y
# test_est
# ## cwc.FE
# #                 Effect  EffectVersion   Estimate   StdError     CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.09636428 0.02812713  0.02384717 0.16888139
# # 2 Indirect Effect (IE) Individual-Avg 0.03007630 0.02228588 -0.02738095 0.08753355
# # 3   Direct Effect (DE)    Cluster-Avg 0.09420135 0.02596578  0.02725660 0.16114609
# # 4 Indirect Effect (IE)    Cluster-Avg 0.03680406 0.02739791 -0.03383298 0.10744110
# ## cwc
# #                 Effect  EffectVersion   Estimate   StdError     CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.09641424 0.02959874  0.02010303 0.17272545
# # 2 Indirect Effect (IE) Individual-Avg 0.03181379 0.02040519 -0.02079469 0.08442228
# # 3   Direct Effect (DE)    Cluster-Avg 0.09554950 0.02924321  0.02015493 0.17094407
# # 4 Indirect Effect (IE)    Cluster-Avg 0.03846497 0.02617431 -0.02901741 0.10594734
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.04465983
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.03733755




# loop through cwc & cwc.FE with mlr --------------------------------------


# change conditions 
condition <- conditions_all |> 
    filter(quadratic == F) |> 
    filter(if.null == F) |> 
    # filter(J %in% c(20)) |> 
    # filter(Nj_low == 50) |> 
    filter(Mfamily %in% c("binomial"), Yfamily %in% c("gaussian"))
# J Nj_low Nj_high quadratic  Mfamily  Yfamily if.null icc
# 1  10     50     100     FALSE binomial gaussian   FALSE 0.2
# 2  20     50     100     FALSE binomial gaussian   FALSE 0.2
# 3  40     50     100     FALSE binomial gaussian   FALSE 0.2
# 4  40      5      20     FALSE binomial gaussian   FALSE 0.2
# 5  70      5      20     FALSE binomial gaussian   FALSE 0.2
# 6 100      5      20     FALSE binomial gaussian   FALSE 0.2

# change method to glm with cwc
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc", "cwc.FE"), Fit %in% c("mlr"))
# cluster_a cluster_m cluster_y Fit cluster_opt cluster_opt_a cluster_opt_m cluster_opt_y
# 1        FE        FE        FE mlr      cwc.FE        cwc.FE        cwc.FE        cwc.FE
# 2        FE        FE        FE mlr         cwc           cwc           cwc           cwc


# 
## generate data 
## estimate 
## create dataframe with true values and estimates 
## save to ongoing results 

# Load necessary libraries
library(dplyr)

# Initialize results dataframe
results <- data.frame(
    condition_num = integer(),
    method_num = integer(),
    estimate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    true_value = numeric(),
    stringsAsFactors = FALSE
)

# Iterate over each method
for (method_num in seq_len(nrow(methds))) {
    method <- methds[method_num, ]
    
    # Set learners based on method
    if (method$Fit == "glm") {
        learners_a <- learners_m <- learners_y <- c("SL.glm")
        num_folds <- 1
    } else if (method$Fit == "mlr") {
        learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
        num_folds <- 5
    }
    
    # Generate data
    generated_data <- generate_data2.0c(
        J = cond$J,
        njrange = c(cond$Nj_low, cond$Nj_high),
        Mfamily = cond$Mfamily,
        Yfamily = cond$Yfamily,
        seed = sample(1:1e6, 1),
        num_x = 3,
        quadratic.A = cond$quadratic,
        quadratic.M = cond$quadratic,
        quadratic.Y = cond$quadratic,
        iccx = cond$icc,
        icca = cond$icc,
        iccm = cond$icc,
        iccy = cond$icc,
        m_on_a = 0.2,
        m_on_az = 0.2,
        m_on_anj = 0.2,
        m_on_x = sqrt(0.15 / 3),
        m_on_z = sqrt(0.4),
        y_on_a = 0.2,
        y_on_m = 1,
        y_on_am = 0,
        y_on_az = 0.2,
        y_on_mz = 0.2,
        y_on_anj = 0.2,
        y_on_x = sqrt(0.15 / 3),
        y_on_z = sqrt(0.4),
        yintercept = 1,
        x_z = 0,
        include_truevals = TRUE,
        include_overlapMsg = FALSE
    )
    
    # Estimate effects
    estimates <- estimate_mediation(
        data = generated_data$data,
        Sname = "school",
        Wnames = names(generated_data$data)[grep("^W", names(generated_data$data))],
        Xnames = names(generated_data$data)[grep("^X", names(generated_data$data))],
        Aname = "A",
        Mnames = "M",
        Yname = "Y",
        learners_a = learners_a,
        learners_m = learners_m,
        learners_y = learners_y,
        cluster_opt = method$cluster_opt,
        num_folds = num_folds
    )
    
    # Extract true values and estimates
    true_pnde <- generated_data$effects$individual$pnde
    true_tnie <- generated_data$effects$individual$tnie
    
    print(estimates)
    print(true_pnde)
    print(true_tnie)
    # # Store results
    # for (effect_row in seq_len(nrow(estimates))) {
    #     effect <- estimates[effect_row, ]
    #     results <- rbind(
    #         results,
    #         data.frame(
    #             condition_num = cond_num,
    #             method_num = method_num,
    #             estimate = effect$Estimate,
    #             ci_lower = effect$CILower,
    #             ci_upper = effect$CIUpper,
    #             true_value = ifelse(effect$Effect == "Direct Effect (DE)", true_pnde, true_tnie),
    #             stringsAsFactors = FALSE
    #         )
    #     )
    # }
    
    message(glue::glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] method {method_num} of condition {cond_num} done"
    ))
}





# test glm with cwc -------------------------------------------------------

Fit <- "glm"

# will need this later on 
if (Fit == "glm") {
    learners_a <- learners_m <- learners_y <- c("SL.glm")
    num_folds <- 1
}
if (Fit == "mlr") {
    learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
    # try
    #learners_a <- learners_m <- learners_y <- c("SL.gam.modified")
    #learners_a <- learners_m <- learners_y <- c("SL.lightgbm", "SL.gam") #, "SL.glm.interaction"
    
    # learners_a <- c("SL.ranger.modified")
    num_folds <- 5 # ceiling(condition$J[cond]/1)
}



# generate data
test_list <- generate_data2.0c(
    J = condition$J[cond], #1000, 
    njrange = c(condition$Nj_low[cond], condition$Nj_high[cond]), 
    Mfamily = "gaussian", # condition$Mfamily[cond], 
    Yfamily = "binomial", #condition$Yfamily[cond], 
    seed = datseeds[iseed], 
    num_x = 3, 
    quadratic.A = condition$quadratic[cond], 
    quadratic.M = condition$quadratic[cond], 
    quadratic.Y = condition$quadratic[cond], 
    iccx = condition$icc[cond], 
    icca = condition$icc[cond], 
    iccm = condition$icc[cond], 
    iccy = condition$icc[cond], 
    m_on_a = 0.2, 
    m_on_az = 0.2, 
    m_on_anj = 0.2, 
    m_on_x = sqrt(0.15 / 3), #num_x
    m_on_z = sqrt(0.4), 
    y_on_a = 0.2, 
    y_on_m = 1, 
    y_on_am = 0, 
    y_on_az = 0.2, 
    y_on_mz = 0.2, 
    y_on_anj = 0.2, 
    y_on_x = sqrt(0.15 / 3), #num_x
    y_on_z = sqrt(0.4), 
    yintercept = 1, 
    x_z = 0, 
    include_truevals = TRUE, # FALSE, 
    include_overlapMsg = FALSE #TRUE
)

# estimate true effects 
test_est <- estimate_mediation(
    data = test_list$data,
    Sname = "school",
    Wnames = names(test_list$data)[grep("^W", names(test_list$data))],
    Xnames = names(test_list$data)[grep("^X", names(test_list$data))],
    Aname = "A",
    Mnames = "M",
    Yname = "Y",
    learners_a = learners_a, 
    learners_m = learners_m, 
    learners_y = learners_y, 
    cluster_opt = "cwc", # methds$cluster_opt,
    num_folds = num_folds
    # Yfamily = "gaussian"
)

# Gaussian M & Binomial Y 
test_est
#                 Effect  EffectVersion   Estimate   StdError     CILower    CIUpper
# 1   Direct Effect (DE) Individual-Avg 0.06337671 0.02240007 0.013770272 0.11298315
# 2 Indirect Effect (IE) Individual-Avg 0.02867685 0.01019553 0.006098181 0.05125552
# 3   Direct Effect (DE)    Cluster-Avg 0.06491736 0.02269477 0.014658287 0.11517643
# 4 Indirect Effect (IE)    Cluster-Avg 0.02837333 0.01038366 0.005378039 0.05136863

## DE (PNDE)
test_list$effects$individual$pnde
# 0.03539488
## IE (TNIE)
test_list$effects$individual$tnie
# 0.02753713


# # Gaussian M & Y 
# test_est
# #                 Effect  EffectVersion  Estimate   StdError    CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.2357069 0.07094543 0.07859352 0.3928203
# # 2 Indirect Effect (IE) Individual-Avg 0.2826616 0.10546240 0.04910811 0.5162151
# # 3   Direct Effect (DE)    Cluster-Avg 0.2236371 0.06802213 0.07299756 0.3742766
# # 4 Indirect Effect (IE)    Cluster-Avg 0.2746814 0.10042659 0.05228010 0.4970828
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.3321417
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.3853798


# # Binomial M & Gaussian Y 
# test_est
# #                 Effect  EffectVersion   Estimate   StdError     CILower   CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.32857277 0.09675672 0.114298598 0.5428469
# # 2 Indirect Effect (IE) Individual-Avg 0.08905137 0.03875545 0.003224857 0.1748779
# # 3   Direct Effect (DE)    Cluster-Avg 0.28332102 0.08453386 0.096115189 0.4705268
# # 4 Indirect Effect (IE)    Cluster-Avg 0.08269610 0.03373981 0.007977066 0.1574151
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.3319704
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.09314005


# # Binomial M & Y 
# test_est
# #             Effect  EffectVersion    Estimate    StdError       CILower    CIUpper
# # 1   Direct Effect (DE) Individual-Avg 0.009839151 0.018789364 -0.0317711416 0.05144944
# # 2 Indirect Effect (IE) Individual-Avg 0.012770920 0.006537865 -0.0017076162 0.02724946
# # 3   Direct Effect (DE)    Cluster-Avg 0.006492452 0.020510351 -0.0389290801 0.05191398
# # 4 Indirect Effect (IE)    Cluster-Avg 0.014522040 0.006743383 -0.0004116288 0.02945571
# 
# ## DE (PNDE)
# test_list$effects$individual$pnde
# # 0.02697363
# ## IE (TNIE)
# test_list$effects$individual$tnie
# # 0.007471182








# loop through models  ----------------------------------------------------


# change conditions 
condition <- conditions_all |> 
    filter(quadratic == F) |> 
    filter(if.null == F) |> 
    # filter(J %in% c(20)) |> 
    # filter(Nj_low == 50) |> 
    filter(Mfamily %in% c("binomial"), Yfamily %in% c("gaussian"))
# J Nj_low Nj_high quadratic  Mfamily  Yfamily if.null icc
# 1  10     50     100     FALSE binomial gaussian   FALSE 0.2
# 2  20     50     100     FALSE binomial gaussian   FALSE 0.2
# 3  40     50     100     FALSE binomial gaussian   FALSE 0.2
# 4  40      5      20     FALSE binomial gaussian   FALSE 0.2
# 5  70      5      20     FALSE binomial gaussian   FALSE 0.2
# 6 100      5      20     FALSE binomial gaussian   FALSE 0.2

# change method to glm with cwc
methds <- methds_all |> 
    filter(cluster_opt %in% c("cwc"), Fit %in% c("mlr", "glm"))
# cluster_a cluster_m cluster_y Fit cluster_opt cluster_opt_a cluster_opt_m cluster_opt_y
# 1        FE        FE        FE mlr         cwc           cwc           cwc           cwc
# 2        FE        FE        FE glm         cwc           cwc           cwc           cwc


# 
## generate data 
## estimate 
## create dataframe with true values and estimates 
## save to ongoing results 

# Load necessary libraries
library(dplyr)

# Initialize results dataframe
results <- data.frame(
    condition_num = integer(),
    method_num = integer(),
    estimate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    true_value = numeric(),
    stringsAsFactors = FALSE
)

# Iterate over each condition
for (cond_num in seq_len(nrow(condition))) {
    cond <- condition[cond_num, ]
    
    # Iterate over each method
    for (method_num in seq_len(nrow(methds))) {
        method <- methds[method_num, ]
        
        # Set learners based on method
        if (method$Fit == "glm") {
            learners_a <- learners_m <- learners_y <- c("SL.glm")
            num_folds <- 1
        } else if (method$Fit == "mlr") {
            learners_a <- learners_m <- learners_y <- c("SL.nnet", "SL.gam")
            num_folds <- 5
        }
        
        # Generate data
        generated_data <- generate_data2.0c(
            J = cond$J,
            njrange = c(cond$Nj_low, cond$Nj_high),
            Mfamily = cond$Mfamily,
            Yfamily = cond$Yfamily,
            seed = sample(1:1e6, 1),
            num_x = 3,
            quadratic.A = cond$quadratic,
            quadratic.M = cond$quadratic,
            quadratic.Y = cond$quadratic,
            iccx = cond$icc,
            icca = cond$icc,
            iccm = cond$icc,
            iccy = cond$icc,
            m_on_a = 0.2,
            m_on_az = 0.2,
            m_on_anj = 0.2,
            m_on_x = sqrt(0.15 / 3),
            m_on_z = sqrt(0.4),
            y_on_a = 0.2,
            y_on_m = 1,
            y_on_am = 0,
            y_on_az = 0.2,
            y_on_mz = 0.2,
            y_on_anj = 0.2,
            y_on_x = sqrt(0.15 / 3),
            y_on_z = sqrt(0.4),
            yintercept = 1,
            x_z = 0,
            include_truevals = TRUE,
            include_overlapMsg = FALSE
        )
        
        # Estimate effects
        estimates <- estimate_mediation(
            data = generated_data$data,
            Sname = "school",
            Wnames = names(generated_data$data)[grep("^W", names(generated_data$data))],
            Xnames = names(generated_data$data)[grep("^X", names(generated_data$data))],
            Aname = "A",
            Mnames = "M",
            Yname = "Y",
            learners_a = learners_a,
            learners_m = learners_m,
            learners_y = learners_y,
            cluster_opt = method$cluster_opt,
            num_folds = num_folds
        )
        
        # Extract true values and estimates
        true_pnde <- generated_data$effects$individual$pnde
        true_tnie <- generated_data$effects$individual$tnie
        
        # Store results
        for (effect_row in seq_len(nrow(estimates))) {
            effect <- estimates[effect_row, ]
            results <- rbind(
                results,
                data.frame(
                    condition_num = cond_num,
                    method_num = method_num,
                    estimate = effect$Estimate,
                    ci_lower = effect$CILower,
                    ci_upper = effect$CIUpper,
                    true_value = ifelse(effect$Effect == "Direct Effect (DE)", true_pnde, true_tnie),
                    stringsAsFactors = FALSE
                )
            )
        }
        
        message(glue::glue(
            "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] method {method_num} of condition {cond_num} done"
        ))
    }
    
    message(glue::glue(
        "[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] Condition {cond_num} done ~~~~~~~~~"
    ))
}








results














# test true value 

test_list
trueVals2.0f(data_list = test_list)


# ══════════════════════════════
#    one dataset test 
# ══════════════════════════════

# helper functions
# Calculate latent mean for M
calc_m_latent <- function(a, z, W_nj, given) {
    latent_m <- gen_m$m_on_a * a +
        gen_m$m_on_az * a * z +
        gen_m$m_on_anj * a * W_nj +
        given
    return(latent_m)
}

# Calculate latent outcome Y
calc_y_latent <- function(m, a, z, W_nj, given) {
    latent_y <- gen_y$y_on_m * m +
        gen_y$y_on_a * a +
        gen_y$y_on_am * a * m +
        gen_y$y_on_az * a * z +
        gen_y$y_on_mz * m * z +
        gen_y$y_on_anj * a * W_nj +
        given
    return(latent_y)
}

# Extract mediator parameters
gen_m <- list(
    iccm = test_list$parameter$iccm,
    m_on_a = test_list$parameter$m_on_a,
    m_on_x = test_list$parameter$m_on_x,
    m_on_z = test_list$parameter$m_on_z,
    m_on_az = test_list$parameter$m_on_az,
    m_on_anj = test_list$parameter$m_on_anj
)

# Extract outcome parameters
gen_y <- list(
    iccy = test_list$parameter$iccy,
    yintercept = test_list$parameter$yintercept,
    y_on_a = test_list$parameter$y_on_a,
    y_on_m = test_list$parameter$y_on_m,
    y_on_am = test_list$parameter$y_on_am,
    y_on_az = test_list$parameter$y_on_az,
    y_on_mz = test_list$parameter$y_on_mz,
    y_on_anj = test_list$parameter$y_on_anj,
    y_on_x = test_list$parameter$y_on_x,
    y_on_z = test_list$parameter$y_on_z
)



# 
a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
results_list <- list()
for (i in 1:nrow(a_vals)) {
    a0_val <- a_vals$a0[i]
    a1_val <- a_vals$a1[i]
    label <- sprintf("Y(a0=%d, gm(a1=%d))", a0_val, a1_val)
    
    m_mean <- calc_m_latent(a = a1_val, z = test_list$data$Z, W_nj = test_list$data$W_nj, given = test_list$parameters$m_given)
    a0 = a0_val
    integrate_over_m <- function(m) {
        y_mean <- calc_y_latent(m, a0, z = test_list$data$Z, W_nj = test_list$data$W_nj, given = test_list$parameters$y_given) # z, W_nj, y_given) #, gen_y)
        dnorm(m, mean = m_mean, sd = sqrt(1 - gen_m$iccm)) * y_mean
    }
    
    results_list[[label]] <- list(
        t = integrate(integrate_over_m, -Inf, Inf), 
        mean = mean(integrate_over_m(m_mean)), #, a0 = a0_val)), 
        sd = sd(integrate_over_m(m_mean)), #, a0 = a0_val))
    )
    
}

comparison <- data.frame(
    Scenario = names(results_list),
    Simulated_Mean = sapply(results_list, function(x) x$mean),
    Simulated_SE = sapply(results_list, function(x) x$sd),
    existing_mean = unlist(test_list$truevals$truevals_individual),
    names = names(unlist(test_list$truevals$truevals_individual)),
    # Analytical_Mean = unlist(analytical_results$truevals_individual),
    stringsAsFactors = FALSE
) 

comparison

unlist(test_list$truevals$truevals_individual)

results_list


a1 <- 1
a0 <- 0
m_mean <- calc_m_latent(a = a1, z = test_list$data$Z, W_nj = test_list$data$W_nj, given = test_list$parameters$m_given)

# integrate_over_m <- calc_y_latent()
integrate_over_m <- function(m, a0) {
    y_mean <- calc_y_latent(m, a0, z = test_list$data$Z, W_nj = test_list$data$W_nj, given = test_list$parameters$y_given) # z, W_nj, y_given) #, gen_y)
    dnorm(m, mean = m_mean, sd = sqrt(1 - gen_m$iccm)) * y_mean
}


mean(integrate_over_m(m_mean))
test_list$truevals$truevals_individual$`Y(a0=0, gm(a1=1))`
integrate(integrate_over_m, -Inf, Inf)



a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
simulation_results <- list()

for (i in 1:nrow(a_vals)) {
    a0_val <- a_vals$a0[i]
    a1_val <- a_vals$a1[i]
    label <- sprintf("Y(a0=%d, gm(a1=%d))", a0_val, a1_val)
    
    sim_outcomes <- replicate(num_simulations, mean(simulate_outcome(a0_val, a1_val, data_list$data)))
    simulation_results[[label]] <- list(
        mean = mean(sim_outcomes),
        se = sd(sim_outcomes)
    )
}




# Compute mediator parameters
# m_latent <- calc_m_latent(a = test_list$data$)
# m_latent <- calc_m_latent(a = a1_val, z = z, W_nj = W_nj, given = given_m)
compute_potential_outcome <- function(a0, a1, z, W_nj, m_given, y_given, gen_m, gen_y, iccm, iccy) {
    # Compute mean of M
    m_mean <- calc_m_latent(a1, z, W_nj, m_given)
    
    # Integrate over M
    integrate_over_m <- function(m) {
        y_mean <- calc_y_latent(m, a0, z, W_nj, y_given)
        dnorm(m, mean = m_mean, sd = sqrt(1 - iccm)) * y_mean
    }
    
    # Numerical integration
    result <- integrate(integrate_over_m, -Inf, Inf)
    
    return(result$value)
}

compute_potential_outcome(a0 = 0, a1 = 1, z = test_list$data$Z, 
                          W_nj = test_list$data$W_nj, 
                          m_given = test_list$parameters$m_given, 
                          y_given = test_list$parameters$y_given, 
                          gen_m = gen_m, 
                          gen_y = gen_y, 
                          iccm = test_list$parameters$iccm, 
                          iccy = test_list$parameters$iccy)



# -------------------------------------------------------------------------




validate_trueVals <- function(data_list, num_simulations = 10000) {
    # Extract necessary parameters
    Mfamily <- data_list$parameters$Mfamily
    Yfamily <- data_list$parameters$Yfamily
    gen_m <- list(
        iccm = data_list$parameters$iccm,
        m_on_a = data_list$parameters$m_on_a,
        m_on_x = data_list$parameters$m_on_x,
        m_on_z = data_list$parameters$m_on_z,
        m_on_az = data_list$parameters$m_on_az,
        m_on_anj = data_list$parameters$m_on_anj
    )
    gen_y <- list(
        iccy = data_list$parameters$iccy,
        yintercept = data_list$parameters$yintercept,
        y_on_a = data_list$parameters$y_on_a,
        y_on_m = data_list$parameters$y_on_m,
        y_on_am = data_list$parameters$y_on_am,
        y_on_az = data_list$parameters$y_on_az,
        y_on_mz = data_list$parameters$y_on_mz,
        y_on_anj = data_list$parameters$y_on_anj,
        y_on_x = data_list$parameters$y_on_x,
        y_on_z = data_list$parameters$y_on_z
    )
    
    # Helper functions (same as in trueVals2.0f)
    calc_m_latent <- function(a, z, W_nj, given) {
        gen_m$m_on_a * a + gen_m$m_on_az * a * z + gen_m$m_on_anj * a * W_nj + given
    }
    
    calc_y_latent <- function(m, a, z, W_nj, given) {
        gen_y$y_on_m * m + gen_y$y_on_a * a + gen_y$y_on_am * a * m +
            gen_y$y_on_az * a * z + gen_y$y_on_mz * m * z + gen_y$y_on_anj * a * W_nj + given
    }
    
    # Simulation function
    simulate_outcome <- function(a0, a1, data) {
        z <- data$Z
        W_nj <- data$W_nj
        given_m <- data_list$parameters$m_given
        given_y <- data_list$parameters$y_given
        
        m_latent <- calc_m_latent(a = a1, z = z, W_nj = W_nj, given = given_m)
        
        # print(dim(m_latent))
        
        if (Mfamily == "binomial") {
            M <- rbinom(nrow(data), 1, pnorm(m_latent, mean = 0, sd = sqrt(1 - gen_m$iccm)))
        } else {
            M <- rnorm(nrow(data), mean = m_latent, sd = sqrt(1 - gen_m$iccm))
        }
        
        y_latent <- calc_y_latent(m = M, a = a0, z = z, W_nj = W_nj, given = given_y)
        
        if (Yfamily == "binomial") {
            Y <- rbinom(nrow(data), 1, pnorm(y_latent, mean = 0, sd = sqrt(1 - gen_y$iccy)))
        } else {
            Y <- rnorm(nrow(data), mean = y_latent, sd = sqrt(1 - gen_y$iccy))
        }
        
        return(Y)
    }
    
    # Run simulations
    a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
    simulation_results <- list()
    
    for (i in 1:nrow(a_vals)) {
        a0_val <- a_vals$a0[i]
        a1_val <- a_vals$a1[i]
        label <- sprintf("Y(a0=%d, gm(a1=%d))", a0_val, a1_val)
        
        sim_outcomes <- replicate(num_simulations, mean(simulate_outcome(a0_val, a1_val, data_list$data)))
        simulation_results[[label]] <- list(
            mean = mean(sim_outcomes),
            se = sd(sim_outcomes)
        )
    }
    
    # Compare with analytical results
    # analytical_results <- trueVals2.0f(data_list)
    
    comparison <- data.frame(
        Scenario = names(simulation_results),
        Simulated_Mean = sapply(simulation_results, function(x) x$mean),
        Simulated_SE = sapply(simulation_results, function(x) x$se),
        # Analytical_Mean = unlist(analytical_results$truevals_individual),
        stringsAsFactors = FALSE
    )
    
    # comparison$Difference <- comparison$Analytical_Mean - comparison$Simulated_Mean
    # comparison$Within_2SE <- abs(comparison$Difference) <= 2 * comparison$Simulated_SE
    
    return(list(
        comparison = comparison,
        simulation_results = simulation_results#,
        # analytical_results = analytical_results
    ))
}

# Usage example:
data_list <- generate_data2.0c(J = 50, Mfamily = "gaussian", Yfamily = "gaussian")
validation_results <- validate_trueVals(data_list)
print(validation_results$comparison)

cbind(validation_results$comparison, 
      # data_list$truevals$truevals_individual)
data.frame(Scenario = names(data_list$truevals$truevals_individual), 
           trueVal = sapply(data_list$truevals$truevals_individual, function(x) x)
))
data_list$truevals$truevals_individual$`Y(a0=0, gm(a1=0))`
