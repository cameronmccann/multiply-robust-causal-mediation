#---------------------------------------------------#
# [UPDATE INFO!]
# 
# QP Project 
# Data Generation for Simulation 1 
#' 
#' `analyze_clustered_mediation()` generates clustered data consisting of a level-1 treatment, 
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

# Main analysis function
analyze_clustered_mediation <- function(PS_model_type = "FE", 
                              Med_model_type = "FE", 
                              Out_model_type = "FE", 
                              data, 
                              # condition, 
                              # condition_num,
                              Sname, # Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
                              L1_baseline_cov,
                              treatment,
                              mediator,
                              outcome,
                              iptw_weights = "iptw",
                              L2_weight = "L2weight") {
    
    # Estimate propensity score model
    ps_result <- estimate_propensity_score(PS_model_type, Sname, L1_baseline_cov, treatment, data)
    data <- ps_result$data
    
    # Ensure L2_weight exists
    if (!(L2_weight %in% names(data))) {
        data[[L2_weight]] <- 1
    }
    
    # Estimate mediation model
    med_results <- estimate_mediator_model(Med_model_type, Sname, L1_baseline_cov, treatment, mediator, iptw_weights, L2_weight, data)
    med <- med_results$med
    data <- med_results$data
    
    # Estimate outcome model
    out <- estimate_outcome_model(Out_model_type, Sname, L1_baseline_cov, treatment, mediator, outcome, iptw_weights, L2_weight, data)
    
    # Store results
    results <- data.frame(
        PS_model = paste0(PS_model_type),
        Mediator_model = Med_model_type,
        Outcome_model = Out_model_type,
        Effect = c("Direct Effect", "Indirect Effect"),
        Individual.Average_Estimate = c(
            summary(out)$coef[treatment, "Estimate"],
            summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
        ),
        Individual.Average_StdError = c(summary(out)$coef[treatment, "Std. Error"], NA) # <-- NEED TO COMPUTE SE FOR INDIRECT EFFECT
        

    )
    # results <- c(
    #     analysisCond = paste0(PS_model_type, "_", Out_model_type),
    #     PS = PS_model_type,
    #     outModel = Out_model_type,
    #     NDE_est = summary(out)$coef[treatment, "Estimate"],
    #     NIE_est = summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"],
    #     # ICC = condition[condition_num, "icc"],
    #     # clust_size = condition[condition_num, "clust_size"],
    #     # conditionNum = condition_num,
    #     a_path_est = summary(med)$coef[treatment, "Estimate"],
    #     a_path_se = summary(med)$coef[treatment, "Std. Error"],
    #     b_path_est = summary(out)$coef[mediator, "Estimate"],
    #     b_path_se = summary(out)$coef[mediator, "Std. Error"],
    #     direct_est = summary(out)$coef[treatment, "Estimate"],
    #     direct_se = summary(out)$coef[treatment, "Std. Error"]
    # )
    
    # # Calculate standard errors
    # direct_se <- summary(out)$coef[treatment, "Std. Error"]
    # indirect_se <- sqrt((summary(med)$coef[treatment, "Estimate"]^2 * summary(out)$coef[mediator, "Std. Error"]^2) +
    #                         (summary(out)$coef[mediator, "Estimate"]^2 * summary(med)$coef[treatment, "Std. Error"]^2))
    # 
    # # Create results data frame
    # results <- data.frame(
    #     Effect = c("Direct Effect", "Indirect Effect"),
    #     Individual.Average_Estimate = c(
    #         summary(out)$coef[treatment, "Estimate"],
    #         summary(med)$coef[treatment, "Estimate"] * summary(out)$coef[mediator, "Estimate"]
    #     ),
    #     Individual.Average_StdError = c(direct_se, indirect_se)
    # )
    # 
    # # Add additional information as attributes
    # attr(results, "analysisCond") <- paste0(PS_model_type, "_", Out_model_type)
    # attr(results, "PS") <- PS_model_type
    # attr(results, "outModel") <- Out_model_type
    # attr(results, "a_path_est") <- summary(med)$coef[treatment, "Estimate"]
    # attr(results, "a_path_se") <- summary(med)$coef[treatment, "Std. Error"]
    # attr(results, "b_path_est") <- summary(out)$coef[mediator, "Estimate"]
    # attr(results, "b_path_se") <- summary(out)$coef[mediator, "Std. Error"]
    
    
    return(results)
}



##################################### END ######################################
