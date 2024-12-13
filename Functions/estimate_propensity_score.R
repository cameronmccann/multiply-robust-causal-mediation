#---------------------------------------------------#
# [UPDATE INFO!]
# 
# QP Project 
# Data Generation for Simulation 1 
#' 
#' `estimate_propensity_score()` generates clustered data consisting of a level-1 treatment, 
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




estimate_propensity_score <- function(PS_model_type = "FE", 
                              Sname, # @param Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
                              L1_baseline_cov, #Xnames, # @param Xnames a character vector of the names of the columns in "data" that correspond to individual-level baseline covariates (X).
                              treatment, # @param Aname a character string of the name of the column in "data" that corresponds to a dummy coded treatment variable (A).
                              data) {
    
    # Construct the formula based on the provided covariates
    covariate_formula <- paste(L1_baseline_cov, collapse = " + ")
    
    if (PS_model_type == "SL") {
        formula <- as.formula(paste(treatment, "~", covariate_formula))
        psmod <- glm(formula = formula, family = "binomial", data = data)
    } else if (PS_model_type == "FE") {
        formula <- as.formula(paste(treatment, "~", covariate_formula, "+", paste0("as.factor(", Sname, ")")))
        psmod <- glm(formula = formula, family = "binomial", data = data)
    } else if (PS_model_type == "RE") {
        formula <- as.formula(paste(treatment, "~", covariate_formula, "+ (1 |", Sname, ")"))
        psmod <- lme4::glmer(formula = formula, family = "binomial", data = data)
    } else {
        stop("Invalid PS_model_type. Please choose 'SL', 'FE', or 'RE'.")
    }
    
    # Predict probabilities and calculate IPTW
    data$ps <- predict(psmod, type = "response")
    data$ps_logit <- predict(psmod, type = "link")
    data$iptw <- with(data, (get(treatment) / ps) + (1 - get(treatment)) / (1 - ps))
    
    return(list(data = data, psmod = psmod))
}



##################################### END ######################################
