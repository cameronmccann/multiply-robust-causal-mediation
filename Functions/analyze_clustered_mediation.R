#---------------------------------------------------#
# [UPDATED FUNCTION]
# 
# QP Project 
# Data Generation for Simulation 1 
# 
# ---------------------------------------------------#

#' @title Analyze Clustered Mediation
#'
#' @description 
#' `analyze_clustered_mediation()` performs mediation analysis on clustered data by estimating 
#' propensity score, mediator, and outcome models. It computes four individual-average (in)direct 
#' effect estimates: Principal Natural Direct Effect (PNDE), Total Natural Indirect Effect (TNIE), 
#' Total Natural Direct Effect (TNDE), and Principal Natural Indirect Effect (PNIE). 
#' Additionally, it provides placeholders for cluster-average effects for future implementation.
#'
#' @param PS_model_type Type of propensity score model ("FE", "RE", "RE-Mean", etc.).
#' @param Med_model_type Type of mediator model ("FE", "RE", "RE-Mean", etc.).
#' @param Out_model_type Type of outcome model ("FE", "RE", "RE-Mean", etc.).
#' @param data Dataset to be used for analysis.
#' @param Sname A character string specifying the name of the column in `data` that indicates 
#' cluster membership (e.g., "school").
#' @param L1_baseline_cov A vector of baseline covariate names at level 1.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param mediator A character string specifying the name of the mediator variable.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param iptw_weights A character string specifying the name of the inverse probability 
#' of treatment weights column in `data`. Default is "iptw".
#' @param L2_weight A character string specifying the name of the level 2 weights column 
#' in `data`. Default is "L2weight".
#'
#' @return A list containing:
#' \describe{
#'   \item{Individual_Average_Effects}{A dataframe with four individual-average (in)direct effects and their estimates. Standard errors are set to `NA` and can be computed using the Delta Method or bootstrapping in future updates.}
#'   \item{Cluster_Average_Effects}{A dataframe with four cluster-average (in)direct effects placeholders (`Estimate` and `Std_Error` set to `NA`). These can be populated once the computation method is implemented.}
#'   \item{Path_Estimates}{A dataframe detailing the individual path estimates (`a`, `b`, and `a:b` interaction) along with their standard errors.}
#'   \item{Model_Convergence}{A list indicating whether each of the three models (propensity score, mediator, outcome) converged.}
#'   \item{Warnings}{A list of any warnings that occurred during model fitting for each model.}
#' }
#'
#' @examples
#' # Example usage
#' result <- analyze_clustered_mediation(
#'     PS_model_type = "FE",
#'     Med_model_type = "FE",
#'     Out_model_type = "FE",
#'     data = example_data,
#'     Sname = "school",
#'     L1_baseline_cov = c("X1", "X2"),
#'     treatment = "A",
#'     mediator = "M",
#'     outcome = "Y",
#'     iptw_weights = "iptw",
#'     L2_weight = "L2weight"
#' )
#' print(result$Individual_Average_Effects)
#' print(result$Cluster_Average_Effects)
#' print(result$Path_Estimates)
#' print(result$Model_Convergence)
#' print(result$Warnings)
#'
#' @export
#' 
#' 
#' 
#' 
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




# possible new function follows: 

#---------------------------------------------------#
# [UPDATED FUNCTION]
# 
# QP Project 
# Data Generation for Simulation 1 
# 
# ---------------------------------------------------#

#' @title Analyze Clustered Mediation
#'
#' @description 
#' `analyze_clustered_mediation()` performs mediation analysis on clustered data by estimating 
#' propensity score, mediator, and outcome models. It computes four individual-average (in)direct 
#' effect estimates: Principal Natural Direct Effect (PNDE), Total Natural Indirect Effect (TNIE), 
#' Total Natural Direct Effect (TNDE), and Principal Natural Indirect Effect (PNIE). 
#' Additionally, it provides placeholders for cluster-average effects for future implementation.
#'
#' @param PS_model_type Type of propensity score model ("FE", "RE", "RE-Mean", etc.).
#' @param Med_model_type Type of mediator model ("FE", "RE", "RE-Mean", etc.).
#' @param Out_model_type Type of outcome model ("FE", "RE", "RE-Mean", etc.).
#' @param data Dataset to be used for analysis.
#' @param Sname A character string specifying the name of the column in `data` that indicates 
#' cluster membership (e.g., "school").
#' @param L1_baseline_cov A vector of baseline covariate names at level 1.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param mediator A character string specifying the name of the mediator variable.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param iptw_weights A character string specifying the name of the inverse probability 
#' of treatment weights column in `data`. Default is "iptw".
#' @param L2_weight A character string specifying the name of the level 2 weights column 
#' in `data`. Default is "L2weight".
#'
#' @return A list containing:
#' \describe{
#'   \item{Individual_Average_Effects}{A dataframe with four individual-average (in)direct effects and their estimates. Standard errors are set to `NA` and can be computed using the Delta Method or bootstrapping in future updates.}
#'   \item{Cluster_Average_Effects}{A dataframe with four cluster-average (in)direct effects placeholders (`Estimate` and `Std_Error` set to `NA`). These can be populated once the computation method is implemented.}
#'   \item{Path_Estimates}{A dataframe detailing the individual path estimates (`a`, `b`, and `a:b` interaction) along with their standard errors.}
#'   \item{Model_Convergence}{A list indicating whether each of the three models (propensity score, mediator, outcome) converged.}
#'   \item{Warnings}{A list of any warnings that occurred during model fitting for each model.}
#' }
#'
#' @examples
#' # Example usage
#' result <- analyze_clustered_mediation(
#'     PS_model_type = "FE",
#'     Med_model_type = "FE",
#'     Out_model_type = "FE",
#'     data = example_data,
#'     Sname = "school",
#'     L1_baseline_cov = c("X1", "X2"),
#'     treatment = "A",
#'     mediator = "M",
#'     outcome = "Y",
#'     iptw_weights = "iptw",
#'     L2_weight = "L2weight"
#' )
#' print(result$Individual_Average_Effects)
#' print(result$Cluster_Average_Effects)
#' print(result$Path_Estimates)
#' print(result$Model_Convergence)
#' print(result$Warnings)
#'
#' @export
analyze_clustered_mediation <- function(PS_model_type = "FE", 
                                        Med_model_type = "FE", 
                                        Out_model_type = "FE", 
                                        data, 
                                        Sname, 
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
    
    # Estimate mediator model
    med_results <- estimate_mediator_model(Med_model_type, Sname, L1_baseline_cov, treatment, mediator, iptw_weights, L2_weight, data)
    med <- med_results$med
    data <- med_results$data
    
    # Estimate outcome model
    out_result <- estimate_outcome_model(Out_model_type, Sname, L1_baseline_cov, treatment, mediator, outcome, iptw_weights, L2_weight, data)
    out <- out_result$out
    
    # Check if interaction term exists in outcome model
    interaction_term <- paste0(treatment, ":", mediator)
    if (!(interaction_term %in% rownames(summary(out)$coef))) {
        stop(paste("Interaction term", interaction_term, "is not present in the outcome model. Ensure that the interaction is included in the model."))
    }
    
    # Extract coefficients from mediator model
    med_summary <- summary(med)
    a_est <- med_summary$coef[treatment, "Estimate"]
    a_se <- med_summary$coef[treatment, "Std. Error"]
    a_intercept <- med_summary$coef["(Intercept)", "Estimate"]
    
    # Extract coefficients from outcome model
    out_summary <- summary(out)
    b_est <- out_summary$coef[mediator, "Estimate"]
    b_se <- out_summary$coef[mediator, "Std. Error"]
    ab_interaction_est <- out_summary$coef[interaction_term, "Estimate"]
    ab_interaction_se <- out_summary$coef[interaction_term, "Std. Error"]
    a_effect_est <- out_summary$coef[treatment, "Estimate"]
    a_effect_se <- out_summary$coef[treatment, "Std. Error"]
    
    # Compute Individual-Average (In)Direct Effects
    # Note: Standard errors for effects involving multiple coefficients are set to NA.
    # Implementing the Delta Method or bootstrapping is recommended for accurate SEs.
    individual_effects <- data.frame(
        PS_model = PS_model_type,
        Mediator_model = Med_model_type,
        Outcome_model = Out_model_type,
        Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
        Estimate = c(
            a_effect_est + ab_interaction_est * a_intercept,                        # PNDE
            a_est * (b_est + ab_interaction_est),                                   # TNIE
            a_effect_est + ab_interaction_est * (a_intercept + a_est),              # TNDE
            a_est * b_est                                                             # PNIE
        ),
        Std_Error = c(
            NA,  # PNDE: Complex to compute; set to NA
            NA,  # TNIE: Complex to compute; set to NA
            NA,  # TNDE: Complex to compute; set to NA
            NA   # PNIE: Complex to compute; set to NA
        ),
        stringsAsFactors = FALSE
    )
    
    # Placeholder for Cluster-Average (In)Direct Effects
    # These can be computed and filled in once the methodology is established
    cluster_effects <- data.frame(
        PS_model = PS_model_type,
        Mediator_model = Med_model_type,
        Outcome_model = Out_model_type,
        Effect = c("PNDE", "TNIE", "TNDE", "PNIE"),
        Estimate = c(
            NA,  # PNDE Cluster-Average
            NA,  # TNIE Cluster-Average
            NA,  # TNDE Cluster-Average
            NA   # PNIE Cluster-Average
        ),
        Std_Error = c(
            NA,  # PNDE Cluster-Average SE
            NA,  # TNIE Cluster-Average SE
            NA,  # TNDE Cluster-Average SE
            NA   # PNIE Cluster-Average SE
        ),
        stringsAsFactors = FALSE
    )
    
    # Optional: Include path estimates and their standard errors
    path_estimates <- data.frame(
        Path = c("a (Treatment -> Mediator)", "b (Mediator -> Outcome)", "a:b Interaction"),
        Estimate = c(a_est, b_est, ab_interaction_est),
        Std_Error = c(a_se, b_se, ab_interaction_se),
        stringsAsFactors = FALSE
    )
    
    # Combine results into a list
    results <- list(
        Individual_Average_Effects = individual_effects,
        Cluster_Average_Effects = cluster_effects,
        Path_Estimates = path_estimates,
        Model_Convergence = list(
            Propensity_Score_Model = ps_result$convergence,
            Mediator_Model = med_results$convergence,
            Outcome_Model = out_result$convergence
        ),
        Warnings = list(
            Propensity_Score_Model = ps_result$warnings,
            Mediator_Model = med_results$warnings,
            Outcome_Model = out_result$warnings
        )
    )
    
    # Suggestions for computing standard errors:
    # To accurately compute the standard errors for the (in)direct effects, consider using the Delta Method
    # which involves the variance-covariance matrix of the estimated coefficients. Alternatively, bootstrapping
    # can be employed to empirically estimate the standard errors.
    # For simplicity and due to complexity, standard errors are set to NA in this implementation.
    
    return(results)
}
