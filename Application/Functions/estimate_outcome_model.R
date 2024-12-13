#---------------------------------------------------#
# [UPDATE INFO!]
# 
# QP Project 
# Data Generation for Simulation 1 
#' 
#' `estimate_outcome_model()` generates clustered data consisting of a level-1 treatment, 
#' 3 level-1 confounders, a level-1 mediator, and a level-1 outcome as well as 
#' a level-2 confounder, with control over the number of clusters, cluster size, 
#' and ICC. Specifically, the dataframe returned from this function consists of 
#' the following variables: observation ID (id); cluster ID (school); 
#' 3 level-1 confounders of T, M, Y relations (x1-x3); a level-2 confounder of 
#' T, M, Y relations (z); (t_ast); the true propensity score of an observation (ps_true); 
#' a level-1 treatment assignment (t); level-1 mediator value (m); and the level-1 outcome measure (y). 
#' 
#' @param num_clust Number of clusters
#' @param clust_size Cluster size for each cluster (i.e., number of observations per cluster)
#' @param num_x Number of x (level-1) confounders
#' @param iccx,icct,iccm,iccy The intraclass correlation (ICC) for covariate x, treatment, mediator, and outcome
#' @returns Returns a dataframe of generated data
#' @examples
#' genOneData_Sim1(num_clust = 30, clust_size = 30)
#' 
#' 
#' 


# Helper function to estimate outcome model
estimate_outcome_model <- function(Out_model_type = "FE",
                                   Sname,
                                   L1_baseline_cov,
                                   treatment,
                                   mediator,
                                   outcome,
                                   iptw_weights,
                                   L2_weight,
                                   data) {
    
    # Construct the formula based on the provided covariates
    covariate_formula <- paste(c(mediator, treatment, L1_baseline_cov), collapse = " + ")
    
    if (Out_model_type == "SL") {
        formula <- as.formula(paste(outcome, "~", covariate_formula))
        out <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    } else if (Out_model_type == "FE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        out <- glm(formula = formula, data = data, weights = data[[iptw_weights]])
    } else if (Out_model_type == "RE") {
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+ (1 |", Sname, ")"))
        out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    } else if (Out_model_type == "RE-Mean") {
        # Calculate cluster means for treatment and mediator
        data[[paste0(treatment, "_mean")]] <- ave(data[[treatment]], data[[Sname]], FUN = mean)
        data[[paste0(mediator, "_mean")]] <- ave(data[[mediator]], data[[Sname]], FUN = mean)
        formula <- as.formula(paste(outcome, "~", covariate_formula, "+", 
                                    paste0(mediator, "_mean"), "+", paste0(treatment, "_mean"), 
                                    "+ (1 |", Sname, ")"))
        out <- WeMix::mix(formula = formula, data = data, weights = c(iptw_weights, L2_weight))
    } else {
        stop("Invalid Out_model_type. Please choose 'SL', 'FE', 'RE', or 'RE-Mean'.")
    }
    
    return(out)
}



##################################### END ######################################
