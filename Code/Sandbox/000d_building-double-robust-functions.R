################################################################################
##################### Building Doubly Robust Est. Functions ####################
################################################################################

############################ Script Description ################################
#
# Author: Your Name
# 
# Date Created: 12/19/2024
#
#
# Script Description: This script is used to build the functions for doubly robust estimation. 
#   Generally, the goal is to create a function equivalent to oneMcl.R (from Dr Liu's package). 
#   We will focus on binary mediator & outcome currently then work towards handling continuous mediator and outcomes. 
#
#   There is a markdown file corresponding to this script for greater detail: [000d_building-double-robust-functions.md]
# 
# 
# Last Updated: 2025-01-03
#
#
# Notes:
#   To-Do:
#       + 
#       + 
#
#   Done: 
#
################################################################################


# Notes: 
## Likely need to modify mu.mac, v.ac, & eif (maybe even mu.ac, a.c, & a.mc)
## Will modify for binary med & out, then work towards continuous variables 




# building fun equivalent to oneMcl.R -------------------------------------


## Generate data -----------------------------------------------------------

# ══════════════════════════════
#    generate data 
# ══════════════════════════════

# Load Packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    # Packages 
    # doParallel, 
    # foreach,
    # parallel, 
    purrr, # for map()
    glue, # for glue()
    dplyr, 
    readr 
    # ggplot2
)

# Load Data gen functions 
# Define the vector of function names
function_names <- c(
    "generate_data2.0", 
    "generate_clusters", 
    "generate_confounders", 
    "generate_treatment", 
    "generate_mediator", 
    "generate_outcome", 
    "pm1",
    "my",
    "trueVals2.0", 
    
    # As of 2025-01-02, these are the most up-to-date functions 
    "generate_data2.0c", 
    "trueVals2.0c"
)

# Loop through the function names and source each file
for (func in function_names) {
    source(file.path("Functions", paste0(func, ".R")))
}

# Generate data 
data_list <- generate_data2.0c(
    J = 100,
    njrange = c(50, 100),
    Mfamily = "binomial",
    Yfamily = "binomial",
    seed = 8675309,
    num_x = 3,
    m_on_a = 3.5,
    m_on_anj = 0.2,
    m_on_az = 0.2,
    y_on_a = 2,
    y_on_m = 5,
    y_on_am = 2,
    y_on_az = 0.2,
    y_on_mz = 0.2,
    y_on_anj = 0.2,
    int.XZ = FALSE
)



## Slowly adding components from oneMcl() ----------------------------------

# ══════════════════════════════
#    From oneMcl.R func 
# ══════════════════════════════

# Here I am slowly added pieces of the oneMcl() and commenting to understand 

# ══════════════════════════════
#    arguments 
# ══════════════════════════════
# arguments for function: 
data = data_list$data
Sname = "school"
Wnames = NULL 
Xnames = names(data)[grep("^X", names(data))]
Aname = "A"
Mnames = "M"
Yname = "Y"
Yfamily = "binomial" #"gaussian" # change to binomial (or work on continuous outcome?)
cluster_opt_a = "cwc.FE"
cluster_opt_m = "cwc.FE"
cluster_opt_y = "cwc.FE"
cluster_opt_v = "cwc"
interaction_fity = c("AM")
num_folds = 1
# bounds 
learners_a = c("SL.glm")
learners_m = c("SL.glm")
learners_y = c("SL.glm")
contrast_a = c(a = 1, astar = 0)


# ══════════════════════════════
#    initial set up (beginning) of function 
# ══════════════════════════════
# 
set.seed(12345)

# add interactions
data$id <- 1:nrow(data)
data_in <- data
# Create interaction terms between A and M (if specified in interaction_fity)
# Here we assume interaction_fity includes "AM"
AM <- data_in[[Aname]] * data_in[, Mnames, drop=FALSE]
colnames(AM) <- glue("AM.{Mnames}")

# Create dummy indicators for cluster variable Sname (removing the first dummy for reference)
Sdumm <- fastDummies::dummy_cols(data[[Sname]], remove_first_dummy = TRUE, remove_selected_columns = TRUE)
colnames(Sdumm) <- paste0("S", 1:ncol(Sdumm))
Sname_dummies <- colnames(Sdumm)

# Compute cluster-level means and centering within cluster (cwc) transformations
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
## load function 
source("Functions/make_fold_K.R")
if (num_folds > 1) {
    folds <- make_fold_K(data_in, Sname, cv_folds = num_folds)
}
if (num_folds <= 1) {
    # No cross-fitting, just use the entire dataset as training and validation
    folds <- origami::make_folds(cluster_ids = data[[Sname]], fold_fun = origami::folds_vfold, V = 1)
    folds[[1]]$training_set <- folds[[1]]$validation_set
}





### Fit models for P(A|C), P(A|M,C), Y(a,m,c), v_ac(c), and mu(a,c) ---------

# ══════════════════════════════
#    These are the next general steps/overview of the code that follows 
# ══════════════════════════════

## Likely need to modify mu.mac, v.ac, & eif (maybe even mu.ac, a.c, & a.mc)
# Fit models for P(A|C), P(A|M,C), Y(a,m,c), v_ac(c), and mu(a,c)
if (FALSE) { # Note: skipping this chunk of code (to avoid commenting it out)
    source("Functions/crossfit.R")
    source("Functions/a.c.R")
    a_c <- a.c(data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE) # I believe a.c does not change for natural effects (so it is same as prior version; dr liu version)
    
    source("Functions/a.mc.R")
    a_mc <- a.mc(data_in, varnames, cluster_opt_a, folds, learners_a, bounded = FALSE) # why is this here? why is m included?
    
    # source("Functions/m.ac.R")
    # m_ac <- m.ac(data_in, varnames, ipw = NULL, cluster_opt = cluster_opt_m,
    #              folds, learners = learners_m, bounded = FALSE, Mfamily = "binomial") # <-- NOT USED (need it for multiple mediators)
    
    source("Functions/mu.mac.R")
    mu_mac <- mu.mac(data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                     cluster_opt = cluster_opt_y,
                     interaction = interaction_fity,
                     folds, learners_y, bounded = FALSE)
    
    v_ac <- v.ac(a=contrast_a["a"], astar=contrast_a["astar"], mu_mac,
                 data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                 cluster_opt = cluster_opt_v,
                 folds, learners_y, bounded = FALSE,
                 full.sample = FALSE) # this handles mediator & stuff
    mu_ac <- mu.ac(data_in, varnames, Yfamily = Yfamily, ipw = NULL,
                   cluster_opt = cluster_opt_y,
                                  folds, learners_y, bounded = FALSE)
}




# ══════════════════════════════
#    crossfit() 
# ══════════════════════════════




# ══════════════════════════════
#    a.c() --- PSs 
# ══════════════════════════════

a.c <- function(data_in, varnames, cluster_opt = "FE.glm", folds, learners, bounded = TRUE) {
    # Check for required functions
    if (!exists("crossfit")) {
        stop("Missing required functions: 'crossfit' not found. Ensure they are loaded.")
    }
    
    # Initialize output matrix
    a_c <- matrix(nrow = nrow(data_in), ncol = 2)
    colnames(a_c) <- c("a(0|c)", "a(1|c)")
    
    # Adjust folds if no cross-fitting is required
    if (grepl("glm", cluster_opt)) { 
        folds <- origami::make_folds(data_in, fold_fun = folds_vfold, V = 1)
        folds[[1]]$training_set <- folds[[1]]$validation_set
    }
    
    # Check fold structure
    if (!is.list(folds) || length(folds) == 0) {
        stop("Invalid 'folds' input. Ensure 'folds' is a properly generated list of training and validation splits.")
    }
    
    # Process each fold
    for (v in seq_along(folds)) {
        # Extract training and validation sets
        train <- origami::training(data_in, folds[[v]])
        valid <- origami::validation(data_in, folds[[v]])
        
        if (is.null(train) || is.null(valid)) {
            stop(sprintf("Error in fold %d: Training or validation set is NULL. Check your folds generation process.", v))
        }
        
        # Cross-fitting with the crossfit function
        alist <- tryCatch({
            crossfit(train, list(valid), varnames$A, c(varnames$X), varnames,
                     ipw = NULL,
                     cluster_opt,
                     type = c("binomial"), learners, bounded)
        }, error = function(e) {
            stop(sprintf("Error in 'crossfit' for fold %d: %s", v, e$message))
        })
        
        # Extract predictions
        preds <- alist$preds
        if (is.null(preds) || nrow(preds) != length(folds[[v]]$validation_set)) {
            stop(sprintf("Prediction error in fold %d: Predictions are NULL or do not match the validation set size.", v))
        }
        
        # Populate the output matrix
        a_c[folds[[v]]$validation_set, "a(0|c)"] <- 1 - preds[, 1]
        a_c[folds[[v]]$validation_set, "a(1|c)"] <- preds[, 1]
    }
    
    return(a_c)
}






# Efficient Influence Functions (EIFs)



# EIF for natural effects -------------------------------------------------

# arguments in eif(): data_in, varnames, a_c, a_mc, mu_mac, v_ac, mu_ac

a_vals <- data.frame(a = c(1, 1, 0, 0), 
                     astar = c(0, 1, 0, 1))

A <- data_in[[varnames$A]]
Y <- data_in[[varnames$Y]]

# multiply-robust
eifs <- list()
# regression (G-computation)
regs <- list()
# weighting
rmpw <- list()

i <-1






# EIF ---------------------------------------------------------------------

# eif function from Dr Liu code 


# eif
eif <- function(data_in, varnames, a_c, a_mc, mu_mac, v_ac, mu_ac) {
    
    a_vals <- data.frame(a = c(1, 1, 0),
                         astar = c(0, 1, 0))
    
    A <- data_in[[varnames$A]]
    Y <- data_in[[varnames$Y]]
    
    # multiply-robust
    eifs <- list()
    # regression (G-computation)
    regs <- list()
    # weighting
    rmpw <- list()
    
    i <-1
    for (i in 1:nrow(a_vals)) {
        a <- a_vals$a[i]
        astar <- a_vals$astar[i]
        
        # Y(a,M(astar)) --------------
        if (a != astar) {
            # observed treatment
            # ipwA <- ipwA_c[, glue("ipwA({a0}|c)")]
            # p_a <- a_c[, glue("a({a}|c)")]
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            ipw_astar <- 1*(A==astar) / bound(a_c[, glue("a({astar}|c)")])
            # observed outcome
            mu <- mu_mac[, glue("mu(m,a{a},c)")]
            vbar <- v_ac[, glue("v_ac(c)")]
            # observed A,M distribution
            h_m <- ( a_c[, glue("a({a}|c)")]*a_mc[, glue("a({astar}|m,c)")] ) /
                (a_c[, glue("a({astar}|c)")]*a_mc[, glue("a({a}|m,c)")])
            
            
            eify <- 1*(A == a)*ipw_a*h_m*(Y - mu) / mean(1*(A == a)*ipw_a*h_m)
            eifm <- 1*(A == astar)*ipw_astar*(mu - vbar) / mean(1*(A == astar)*ipw_astar)
            eif <- eify + eifm + vbar
            
            eifs[[glue("Y({a},M({astar}))")]] <- eif
            
            rmpw[[glue("Y({a},M({astar}))")]] <- 1*(A == a)*ipw_a*h_m*(Y) / mean(1*(A == a)*ipw_a*h_m)
            regs[[glue("Y({a},M({astar}))")]] <- vbar
        }
        
        # Y(a) -------------
        if (a == astar) {
            # observed treatment
            # ipwA <- ipwA_c[, glue("ipwA({a0}|c)")]
            # p_a <- a_c[, glue("a({a}|c)")]
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            # observed outcome
            mu_c <- mu_ac[, glue("mu(a{a},c)")]
            
            eifya <- 1*(A == a)*ipw_a*(Y - mu_c) / mean(1*(A == a)*ipw_a)
            
            eif_a <- eifya + mu_c
            
            eifs[[glue("Y({a},M({a}))")]] <- eif_a
            
            rmpw[[glue("Y({a},M({a}))")]] <- 1*(A == a)*ipw_a*(Y) / mean(1*(A == a)*ipw_a)
            regs[[glue("Y({a},M({a}))")]] <- mu_c
        }
    }
    
    
    list(eifs=eifs, #eify_ajo=eify_ajo, eifm_ajo=eifm_ajo, mu_a0c_jo=mu_a0c_jo,
         rmpw=rmpw, regs=regs)
}




