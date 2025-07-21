# {UPDATE DOCUMENTATION AT SOMEPOINT} 
# As of 2025-01-08: only updated documentation (old documentation is at bottom of script) & comments within function 
# On 2025-07-21: added arguments (like random_slope_vars_a) to be past to other functions so users can set random slopes for specific variables

# NEED TO DOUBLE CHECK DOCUMENTATION (start with older documentation at bottom of script)
# estimate_mediation <= MediatorCL.R




#' @title Estimate Causal Mediation Effects in Clustered Data
#' 
#' @description
#' This function estimates direct and indirect effects of a treatment on an outcome
#' in the presence of multiple mediators and clustered data. It leverages machine learning
#' to model nuisance functions such as the propensity scores, mediator models, and outcome regressions.
#'
#' @details
#' This function performs the following steps:
#' - Splits data into folds for cross-fitting if `num_folds > 1`.
#' - Fits models for the treatment, mediators, and outcome using user-specified learners.
#' - Computes efficient influence functions (EIFs) for causal mediation analysis.
#' - Aggregates EIFs at the cluster level and derives multiply-robust (MR) point estimates
#'   and confidence intervals for direct and indirect effects.
#'
#' Supports flexible clustering options (`"cwc.FE"` or `"cwc"`), different outcome types
#' (`"gaussian"` for continuous and `"binomial"` for binary outcomes), and customizable
#' machine learning algorithms via the "SuperLearner" package.
#'
#' @param data A \code{data.frame} containing the analysis variables.
#' @param Sname A character string indicating the cluster membership variable.
#' @param Wnames Optional; character vector of cluster-level baseline covariates.
#' @param Xnames Character vector of individual-level baseline covariates.
#' @param Aname A character string for the treatment variable (binary).
#' @param Mnames A character vector for the mediators.
#' @param Yname A character string for the outcome variable.
#' @param learners_a A character vector specifying learners for the treatment model. Defaults to `"SL.glm"`.
#' @param learners_m A character vector specifying learners for the mediator models. Defaults to `"SL.glm"`.
#' @param learners_y A character vector specifying learners for the outcome model. Defaults to `"SL.glm"`.
#' @param cluster_opt A character string specifying the clustering adjustment method (`"cwc.FE"` or `"cwc"`).
#' @param num_folds Integer specifying the number of folds for cross-fitting. Defaults to `1` (no cross-fitting).
#' @param Yfamily A character string specifying the family for the outcome (`"gaussian"` or `"binomial"`). Automatically set if not specified.
#'
#' @return
#' A data frame containing causal mediation estimates (direct and indirect effects)
#' with their standard errors and confidence intervals at both individual and cluster levels.
#'
#' @examples
#' \dontrun{
#' results <- estimate_mediation(
#'   data = my_data,
#'   Sname = "cluster_id",
#'   Wnames = c("cluster_size"),
#'   Xnames = c("age", "gender"),
#'   Aname = "treatment",
#'   Mnames = c("mediator1", "mediator2"),
#'   Yname = "outcome",
#'   num_folds = 5
#' )
#' }
#' 
#' @seealso \code{\link{internal_estimate_mediation}} for the internal computation.
#' @export

estimate_mediation <- function(data,
                               Sname,
                               Wnames, 
                               Xnames,
                               Aname,
                               Mnames,
                               Yname,
                               learners_a = c("SL.glm"),
                               learners_m = c("SL.glm"),
                               learners_y = c("SL.glm"),
                               cluster_opt = "cwc.FE",
                               num_folds = 1, 
                               random_slope_vars_a = NULL,
                               random_slope_vars_m = NULL,
                               random_slope_vars_y = NULL) {
    set.seed(12345)
    
    # Set the family of the outcome variable (binary or continuous)
    Yfamily <- ifelse(length(unique(data[[Yname]])) > 2, "gaussian", "binomial")
    
    # Call the internal estimation function
    out <- internal_estimate_mediation(
        data = data,
        Sname = Sname,
        Wnames = Wnames,
        Xnames = Xnames,
        Aname = Aname,
        Mnames = Mnames,
        Yname = Yname,
        Yfamily = Yfamily,
        cluster_opt_a = cluster_opt,
        cluster_opt_m = cluster_opt,
        cluster_opt_y = cluster_opt,
        cluster_opt_v = "cwc",                # SHOULD USERS BE ABLE TO SPECIFY THIS THROUGH estimate_mediation()?
        interaction_fity = c("AM"),
        num_folds = num_folds,
        learners_a = learners_a,
        learners_m = learners_m,
        learners_y = learners_y,
        contrast_a = c(a = 1, astar = 0), 
        random_slope_vars_a = random_slope_vars_a,
        random_slope_vars_m = random_slope_vars_m,
        random_slope_vars_y = random_slope_vars_y
    )
    
    # Extract results for individual- and cluster-average effects
    results <- out$mr_results
    
    # Clean up and format the results
    results1 <- results %>%
        dplyr::filter(stringr::str_detect(estimand, "E")) %>%  # Keep direct and indirect effect rows
        dplyr::mutate(Effect = dplyr::case_when(
            stringr::str_detect(estimand, "DE") ~ "Direct Effect (DE)", # ~ "DE",
            stringr::str_detect(estimand, "IE") ~ "Indirect Effect (IE)" # ~ "IE"
        )) %>%
        dplyr::select(Effect, dplyr::starts_with("mr_")) %>%  # Select multiply-robust estimates
        dplyr::rename_with(.fn = ~gsub("mr_", "", .)) %>%  # Remove the "mr_" prefix from column names
        dplyr::rename_with(.fn = ~gsub("individual", "Individual.Average", .)) %>%
        dplyr::rename_with(.fn = ~gsub("cluster", "Cluster.Average", .)) %>%
        dplyr::rename_with(.fn = ~gsub(".est", "_Estimate", .)) %>%
        dplyr::rename_with(.fn = ~gsub(".se", "_StdError", .)) %>%
        dplyr::rename_with(.fn = ~gsub("waldci1_", "CI.lower_", .)) %>%
        dplyr::rename_with(.fn = ~gsub("waldci2_", "CI.upper_", .)) %>%
        dplyr::select(Effect, dplyr::contains("Individual.Average"), dplyr::contains("Cluster.Average"))
    
    # return(results1)
    
    # Changing output to different format and titles (maybe change back or modify headers to be like glm outputs)
    results2 <- results1
    names(results2) <-
        c(
            "Effect",
            "Individual.Average_Estimate",
            "Individual.Average_StdError",
            "Individual.Average_CILower",
            "Individual.Average_CIUpper",
            "Cluster.Average_Estimate",
            "Cluster.Average_StdError",
            "Cluster.Average_CILower",
            "Cluster.Average_CIUpper"
        )
    results2 <- results2 |>
        tidyr::pivot_longer(
            cols = -Effect,
            names_to = c("EffectVersion", "Measure"),
            names_pattern = "(.*)_(.*)"
        ) |>
        tidyr::pivot_wider(names_from = Measure,
                           values_from = value) |>
        mutate(EffectVersion = gsub(".Average", "-Avg", EffectVersion)) |>
        arrange(factor(EffectVersion, levels = c("Individual-Avg", "Cluster-Avg")), Effect) |> 
        as.data.frame()
    
    return(results2)
    
}



############################### OLD/ORIGIANL DOCUMENTATION ###############################


#' Estimating Causal Mediation Effects in Multiple-Mediator Analyses with Clustered Data
#' @param data a \code{data.frame} containing the analysis variables.
#' @param Sname a character string of the name of the column in "data" that indicates the cluster membership of each individual (S).
#' @param Wnames a character vector of the names of the columns in "data" that correspond to cluster-level baseline (i.e., pre-treatment) covariates (W). "Wnames" may include the cluster size (i.e., number of individuals in each cluster).
#' @param Xnames a character vector of the names of the columns in "data" that correspond to individual-level baseline covariates (X).
#' @param Aname a character string of the name of the column in "data" that corresponds to a dummy coded treatment variable (A).
#' @param Mnames a character vector of the names of the columns in "data" that correspond to mediators (M).
#' @param Yname a character string of the name of the column in "data" that corresponds to an outcome variable (Y).
#' @param cluster_opt a character string of the option of incorporating clusters in the nuisance model estimation. Currently supported options are "cwc.FE" (the default option) and "cwc". "cwc.FE": Control for cluster means  and perform cluster-mean centering for individual-level variables, and also control for dummy indicators of an individual's  cluster membership. "cwc": Control for cluster means  and perform cluster-mean centering for individual-level variables (while omitting the cluster dummies).
#' @param learners_a a character vector specifying estimation methods for the model of the treatment For "learners_a", "learners_m", and "learners_y", currently supported options include methods in the "SuperLearner" package (<https://cran.r-project.org/package=SuperLearner>), available via the "SuperLearner::listWrappers()".
#' @param learners_m a character vector specifying estimation methods for the models of the mediators.
#' @param learners_y a character vector specifying estimation methods for the model of the outcome.
#' @param num_folds the number of folds to use for the cross-fitting procedure.
#'
#' @return A data frame containing the results (estimates and 0.95 confidence intervals) for each of the cluster-average and individual-average causal mediation effects (i.e., DE, IE, including the cluster-average and individual-average versions).
#' DE, IE are, respectively, the pure direct effect, total indirect effect.

#'
#'
#' @export
#'

# estimate_mediation <- function(data,
#                        Sname,
#                        Wnames, Xnames,
#                        Aname,
#                        Mnames,
#                        Yname,
#                        learners_a = c("SL.glm"),
#                        learners_m = c("SL.glm"),
#                        learners_y = c("SL.glm"),
#                        cluster_opt = "cwc.FE",
#                        num_folds = 1, 
#                        # Yfamily = "gaussian" # Cam added this (DO YOU NEED TO ADD Mfamily too?)
# ) {
#     set.seed(12345)
#     
#     Yfamily <- ifelse(length(unique(data[[Yname]]))>2, "gaussian", "binomial")
#     
#     out <- internal_estimate_mediation(
#         data = data,
#         Sname = Sname,
#         Wnames = Wnames,
#         Xnames = Xnames,
#         Aname = Aname,
#         Mnames = Mnames,
#         Yname = Yname,
#         Yfamily = Yfamily,
#         cluster_opt_a = cluster_opt,
#         cluster_opt_m = cluster_opt,
#         cluster_opt_y = cluster_opt,
#         cluster_opt_v = "cwc",         # SHOULD USERS BE ABLE TO SPECIFY THIS THROUGH estimate_mediation()?
#         interaction_fity = c("AM"),
#         num_folds = num_folds,
#         learners_a = learners_a,
#         learners_m = learners_m,
#         learners_y = learners_y,
#         contrast_a = c(a = 1, astar = 0)
#     )
#     
#     results <- out$mr_results
#     results1 <- results %>%
#         filter(str_detect(estimand, "E")) %>%
#         mutate(Effect = case_when(str_detect(estimand, "DE") ~ "DE",
#                                   str_detect(estimand, "IE") ~ "IE")
#         ) %>%
#         select(Effect, starts_with("mr_")) %>%
#         rename_with(.fn = ~gsub("mr_", "", .) ) %>%
#         rename_with(.fn = ~gsub("individual", "Individual.Average", .) ) %>%
#         rename_with(.fn = ~gsub("cluster", "Cluster.Average", .) ) %>%
#         rename_with(.fn = ~gsub(".est", "_Estimate", .) ) %>%
#         rename_with(.fn = ~gsub(".se", "_StdError", .) ) %>%
#         rename_with(.fn = ~gsub("waldci1_", "CI.lower_", .) ) %>%
#         rename_with(.fn = ~gsub("waldci2_", "CI.upper_", .) ) %>%
#         select(Effect, contains("Individual.Average"), contains("Cluster.Average"))
#     
#     results1
# }
# 
