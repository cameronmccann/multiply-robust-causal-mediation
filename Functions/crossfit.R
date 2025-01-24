# {UPDATE DOCUMENTATION AT SOMEPOINT} 
# As of 2025-01-02: only modified comments in code; did not modify code yet


#' @title crossfit
#'
#' @description
#' Fits a model (either random effects, fixed effects, or a SuperLearner approach) on 
#' a training dataset and uses it to predict outcomes on a set of validation datasets.
#'
#' @details
#' The `crossfit()` function is primarily used for cross-fitting in causal inference or 
#' machine learning workflows. Depending on the argument \code{cluster_opt}, it can 
#' perform:
#' \itemize{
#'   \item \code{"RE.glm"}: Random effects regression via \code{lme4::lmer} or \code{lme4::glmer}.
#'   \item \code{"FE.glm"} or \code{"noncluster.glm"}: Ordinary \code{glm} with optional fixed cluster indicators.
#'   \item \code{"fix.mlr"} or \code{"noncluster.mlr"}: SuperLearner-based prediction using cluster dummies or not.
#'   \item \code{"cwc"} or \code{"sufficient_stats"}: Centered within clusters or sufficient statistics approach.
#'   \item \code{"cwc.FE"}: Combination of cluster-mean centering with fixed effects indicators.
#' }
#'
#' @param train A data frame containing the training set (used to fit the model).
#' @param valid.list A list of one or more data frames. Each data frame is a validation set 
#'   on which predictions will be generated.
#' @param yname A character string specifying the outcome variable name (e.g., \code{"Y"}).
#' @param xnames A character vector of predictor variable names (e.g., \code{c("X1", "X2")}).
#' @param varnames A list containing named elements that specify additional variables:
#'   \itemize{
#'     \item \code{S}: The cluster identifier variable name.
#'     \item \code{Sdumm}: Vector of dummy variable names if fixed cluster indicators are used.
#'     \item \code{W}: (Optional) Additional covariate names to include.
#'   }
#' @param ipw (Optional) A numeric vector of inverse probability weights to use during model fitting. 
#'   If \code{NULL}, equal weights are assumed.
#' @param cluster_opt A character string specifying the clustering or modeling approach. 
#'   Possible values include \code{"RE.glm"}, \code{"FE.glm"}, \code{"noncluster.glm"}, \code{"fix.mlr"}, 
#'   \code{"noncluster.mlr"}, \code{"cwc"}, \code{"sufficient_stats"}, \code{"cwc.FE"}. 
#'   See Details for how each is handled.
#' @param type A character string specifying the outcome family, such as \code{"binomial"} or \code{"gaussian"}.
#' @param learners A set of machine learning algorithms passed to \code{SuperLearner::SuperLearner} 
#'   if \code{cluster_opt} requires it (e.g., \code{"fix.mlr"}). Ignored if a simple GLM or random-effects GLM is used.
#' @param bounded A logical indicating whether to bound the predictions (e.g., to [0,1] if \code{type="binomial"}). 
#'   Defaults to \code{FALSE}.
#'
#' @return
#' A list with two elements:
#' \describe{
#'   \item{\code{fit}}{The fitted model object (e.g., a \code{glm}, \code{lmer}, or \code{SuperLearner} object).}
#'   \item{\code{preds}}{A matrix or vector of predictions, with one column per validation set if there are multiple 
#'                       entries in \code{valid.list}.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Using a simple binomial GLM with fixed cluster effects:
#' data_train <- data.frame(Y = rbinom(100, 1, 0.3),
#'                          X1 = rnorm(100),
#'                          X2 = rnorm(100),
#'                          S  = sample(1:5, 100, replace = TRUE))
#' data_valid <- data.frame(Y = rbinom(50, 1, 0.3),
#'                          X1 = rnorm(50),
#'                          X2 = rnorm(50),
#'                          S  = sample(1:5, 50, replace = TRUE))
#'
#' varnames_list <- list(S = "S", Sdumm = NULL, W = NULL)
#'
#' # Fit and predict
#' result <- crossfit(
#'   train = data_train,
#'   valid.list = list(data_valid),
#'   yname = "Y",
#'   xnames = c("X1","X2"),
#'   varnames = varnames_list,
#'   cluster_opt = "FE.glm",
#'   type = "binomial",
#'   learners = NULL,     # Not needed for GLM
#'   bounded = TRUE       # Bound predictions to [0,1]
#' )
#'
#' head(result$preds)
#'
#' # Example 2: Using SuperLearner with cluster dummy indicators:
#' data_train$Sdumm1 <- ifelse(data_train$S == 1, 1, 0)
#' data_train$Sdumm2 <- ifelse(data_train$S == 2, 1, 0)
#' # (Create as many dummy variables as clusters minus 1, or use a method for that.)
#'
#' varnames_list$Sdumm <- c("Sdumm1", "Sdumm2") # Suppose we have 3 clusters total
#' library(SuperLearner)
#'
#' result_mlr <- crossfit(
#'   train = data_train,
#'   valid.list = list(data_valid),
#'   yname = "Y",
#'   xnames = c("X1","X2"),
#'   varnames = varnames_list,
#'   cluster_opt = "fix.mlr",
#'   type = "binomial",
#'   learners = c("SL.glm","SL.mean"), # Example learner library
#'   bounded = TRUE
#' )
#' head(result_mlr$preds)
#' }
#'
#' @seealso 
#'  \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}} for random effects models, 
#'  \code{\link[SuperLearner]{SuperLearner}} for ensemble learning, 
#'  and \code{\link{a.c}} for an example of how `crossfit()` is used in cross-fitting propensity.
#'
#' @export


crossfit <- function(train, valid.list, yname, xnames, varnames,
                     ipw = NULL,
                     cluster_opt = "FE.glm",
                     type, learners, bounded = FALSE) {
    
    # --------------------------------------------------------------------------
    # 1. SET UP VARIABLE NAMES AND FAMILIES
    # --------------------------------------------------------------------------
    # Extract the name of the cluster variable (S) and any dummy variables (Sdumm)
    Sname <- varnames$S
    Sname_dummies <- varnames$Sdumm
    
    # Determine if the outcome is binomial or gaussian
    family <- ifelse(type == "binomial", binomial(), gaussian())
    
    # Create a data frame---with outcome (Y), covariates (Xs & W), & cluster IDs---used for model fitting
    df_lm <- data.frame(
        Y = train[[yname]],
        train[, c(xnames, varnames$W), drop = FALSE],
        S = train[[Sname]]
    )

    # --------------------------------------------------------------------------
    # 2. HANDLE OPTIONAL IPW (INVERSE PROBABILITY WEIGHTS)
    # --------------------------------------------------------------------------
    # If ipw is provided, store these weights in the 'wreg' column
    if (length(ipw) > 0) {
        df_lm$wreg <- ipw
    }
    # Otherwise, if ipw is NULL or empty, use weights = 1
    if (length(ipw) == 0) {
        df_lm$wreg <- rep(1, nrow(df_lm))
    }
    
    # --------------------------------------------------------------------------
    # 3. RANDOM EFFECTS GLM (RE.glm)
    # --------------------------------------------------------------------------
    # random effects ----
    # Using lmer/glmer from the lme4 package for random effects
    if (cluster_opt == "RE.glm") {
        
        # Build a formula that includes all covariates and a random intercept for S
        REformula <- paste("Y ~", paste(c(xnames, varnames$W), collapse = " + "), "+ (1 | S)")
        
        # Fit either a linear mixed model (gaussian) or a generalized linear mixed model (binomial)
        if (family[[1]] == "gaussian") {
            fit <- lme4::lmer(
                formula = REformula,
                weights = wreg,
                data = df_lm
            )
        }
        if (family[[1]] != "gaussian") {
            fit <- lme4::glmer(
                formula = REformula,
                weights = wreg,
                data = df_lm, 
                family = family[[1]]
            )
        }
        
        # Generate predictions for each validation set in valid.list
        preds <- sapply(valid.list, function(validX) {
            # Construct a new data frame with the same columns used in the model
            newX <- data.frame(
                validX[, c(xnames, varnames$W), drop = FALSE],
                S = validX[[Sname]]
            )
            # Predict on this new data
            preds <- stats::predict(fit, newX, type = "response")
            
            # If bounded = TRUE, constrain predictions to [0,1]
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
        
    }
    
    # fixed effects ----
    
    # --------------------------------------------------------------------------
    # 4. FIXED EFFECTS GLM (FE.glm) AND NON-CLUSTER GLM
    # --------------------------------------------------------------------------
    # S is either included as a fixed effect (FE.glm) or exclude it (noncluster.glm)
    
    ## glm ----
    if (cluster_opt %in% c("FE.glm", "noncluster.glm")) {
        
        # Build different formulas depending on whether or not we include S
        if (cluster_opt == "FE.glm") {
            fit <- stats::glm(
                formula = paste("Y ~ S +", paste(xnames, collapse = " + ")),
                weights = wreg,
                data = df_lm, 
                family = family[[1]]
            )
        }
        if (cluster_opt == "noncluster.glm") {
            fit <- stats::glm(
                formula = paste("Y ~", paste(xnames, collapse = " + ")),
                weights = wreg,
                data = df_lm, 
                family = family[[1]]
            )
        }
        
        # Generate predictions for each validation set in valid.list
        preds <- sapply(valid.list, function(validX) {
            # Prepare the new data with the same columns as the model
            newX <- data.frame(
                validX[, xnames, drop = FALSE],
                S = validX[[Sname]]
            )
            # Make predictions using glm
            preds <- stats::predict(fit, newX, type = "response")
            
            # Bound predictions if requested
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # --------------------------------------------------------------------------
    # 5. SUPERLEARNER WITH CLUSTER DUMMY INDICATORS (fix.mlr or noncluster.mlr)
    # --------------------------------------------------------------------------
    # Use SuperLearner with cluster dummies ('fix.mlr') or without ('noncluster.mlr')
    ## superlearner (SL) dummy cluster indicator with SL ----
    if (cluster_opt %in% c("fix.mlr", "noncluster.mlr")) {                      ## NOTE: IF fix is fixed-effect, use "FE.mlr" instead of "fix.mlr" to be consistent with "FE.glm"
        
        # Combine our df_lm with the dummy variables for each cluster
        df_FE <- data.frame(df_lm, train[, Sname_dummies])
        
        # Build different models depending on whether cluster dummies are used
        set.seed(12345)
        if (cluster_opt == "fix.mlr") {
            fit <- SuperLearner::SuperLearner(
                Y = df_FE$Y,
                X = df_FE[, c(xnames, varnames$W, Sname_dummies), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        if (cluster_opt == "noncluster.mlr") {
            fit <- SuperLearner::SuperLearner(
                Y = df_FE$Y,
                X = df_FE[, c(xnames), drop = FALSE],
                obsWeights = df_FE$wreg,
                family = family[[1]],
                SL.library = learners
            )
        }
        
        # Use the fitted SuperLearner model to predict on the validation sets
        preds <- sapply(valid.list, function(validX) {
            # Build the new data frame with the same variables used in training
            newX <- data.frame(
                validX[, c(xnames, varnames$W, Sname_dummies), drop = FALSE]
            )
            # Predict returns a list; we extract the 'pred' element
            preds <- stats::predict(fit, newX[, fit$varNames])$pred
            
            # Bound if necessary
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }

    # --------------------------------------------------------------------------
    # 6. CENTERED WITHIN-CLUSTER (CWC) APPROACH WITH SUPERLEARNER
    # --------------------------------------------------------------------------
    # cwc with SL ------------------------
    if (cluster_opt == "cwc") { 
        # Create data frame for training that includes: outcome (Y), cluster-mean-centered covariates (xnames_cwc), cluster mean of Y (yname_clmean), & additional covariates (varnames$W)
        df_cwc <- data.frame(
            Y = train[, glue::glue("{yname}"), drop=TRUE],
            train[, c(glue::glue("{xnames}_cwc"), glue::glue("{yname}_clmean"), varnames$W), drop = FALSE]
        )
        
        # Fit a SuperLearner model on these centered variables
        fit <- SuperLearner::SuperLearner(
            Y = df_cwc$Y,
            X = df_cwc[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner::SuperLearner)
        )
        
        # Predict on validation data
        preds <- sapply(valid.list, function(validX) {
            
            # Rebuild the new data with cluster means and centered covariates
            newX <- data.frame(
                validX[, c(xnames, glue::glue("{xnames}_clmean"), glue::glue("{yname}_clmean"), varnames$W), drop = FALSE]
            )
            # Create the within-cluster centered version of the X variables
            validX_cwc <- validX[, xnames] - validX[, glue::glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue::glue("{xnames}_cwc")
            # Combine centered covariates with newX
            newX <- newX %>% dplyr::bind_cols(validX_cwc)
            # colnames(newX)
            # Get predictions using the SuperLearner model
            preds <- stats::predict(fit, newX[, fit$varNames])$pred
            # preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
            
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # --------------------------------------------------------------------------
    # 7. SUFFICIENT STATISTICS APPROACH
    # --------------------------------------------------------------------------
    # sufficient stats ----
    if (cluster_opt == "sufficient_stats")  { # continuous outcome
        if (family[[1]] == "binomial") {
            # For binomial, we use Y and cluster-level variables
            df_ss <- data.frame(
                Y = train[, glue::glue("{yname}"), drop=TRUE],
                train[, c(glue::glue("{xnames}_cwc"), glue::glue("{xnames}_clmean"), glue::glue("{yname}_clmean"), varnames$W), drop = FALSE]
            )
        }
        if (family[[1]] == "gaussian") {
            # For gaussian, we use the cluster-mean-centered Y (yname_cwc)
            df_ss <- data.frame(
                Y = train[, glue::glue("{yname}_cwc"), drop=TRUE],
                train[, c(glue::glue("{xnames}_cwc"), glue::glue("{xnames}_clmean"), #glue("{yname}_clmean"), 
                          varnames$W), drop = FALSE]
            )
        }
        
        # Fit a SuperLearner model on the data
        fit <- SuperLearner::SuperLearner(
            Y = df_ss$Y,
            X = df_ss[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner::SuperLearner)
        )
        
        # Predict on each validation set
        preds <- sapply(valid.list, function(validX) {
            
            # Build the new data frame with cluster means, centered covariates, etc.
            newX <- data.frame(
                validX[, c(glue::glue("{yname}_clmean"), xnames, glue::glue("{xnames}_clmean"), varnames$W), drop = FALSE]
            )
            # Again, compute the within-cluster centered covariates
            validX_cwc <- validX[, xnames] - validX[, glue::glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue::glue("{xnames}_cwc")
            # Bind them
            newX <- newX %>% dplyr::bind_cols(validX_cwc)
            # colnames(newX)
            # Get predictions from the model
            preds <- stats::predict(fit, newX[, fit$varNames])$pred
            
            # For Gaussian outcomes, we add back the cluster mean of Y
            if (family[[1]] == "gaussian") {
                preds <- preds + validX[, glue::glue("{yname}_clmean"), drop=TRUE]
            }
            
            # Bound if needed
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # --------------------------------------------------------------------------
    # 8. CWC WITH FIXED EFFECTS (cwc.FE)
    # --------------------------------------------------------------------------
    # Similar to 'cwc' but also including cluster dummy variables for FE.
    # ═══════════════════
    #    Debugging version of code 
    # ═══════════════════
    # if (cluster_opt == "cwc.FE") {
    #     print("Fitting CWC.FE...")
    #     print(glue::glue("Columns in training data: {paste(colnames(df_cwc), collapse=', ')}"))
    #     
    #     fit <- SuperLearner::SuperLearner(
    #         Y = df_cwc$Y,
    #         X = df_cwc[, -1, drop = FALSE],
    #         family = family[[1]],
    #         SL.library = learners,
    #         env = environment(SuperLearner::SuperLearner)
    #     )
    #     
    #     if (is.null(fit)) stop("Error: `fit` returned NULL in `cwc.FE`")
    #     
    #     preds <- sapply(valid.list, function(validX) {
    #         print(glue::glue("Validation fold size: {nrow(validX)}"))
    #         
    #         newX <- data.frame(
    #             validX[, c(xnames, glue::glue("{xnames}_clmean"), glue::glue("{yname}_clmean"), Sname_dummies, varnames$W), drop = FALSE]
    #         )
    #         
    #         # Center X within clusters
    #         validX_cwc <- validX[, xnames] - validX[, glue::glue("{xnames}_clmean")]
    #         colnames(validX_cwc) <- glue::glue("{xnames}_cwc")
    #         newX <- newX %>% dplyr::bind_cols(validX_cwc)
    #         
    #         if (is.null(newX) || ncol(newX) == 0) stop("Error: Validation data is NULL or empty.")
    #         if (!all(fit$varNames %in% colnames(newX))) stop("Error: Missing predictors in newX")
    #         
    #         preds <- stats::predict(fit, newX[, fit$varNames])$pred
    #         
    #         if (is.null(preds) || nrow(preds) == 0) {
    #             stop(glue::glue("Error: Predictions are NULL or empty for fold {v}."))
    #         }
    #         
    #         preds
    #     }, simplify = TRUE)
    # }
    
    # ═══════════════════
    #    origianl version of code 
    # ═══════════════════
    # cwc.FE ----
    if (cluster_opt == "cwc.FE") { # continuous outcome
        # Build the training data with cluster-centered covariates, cluster means, and dummy variables
        # colnames(train) # note: train[, glue("{yname}"), drop=TRUE] doesn't work well
        if (family[[1]]=="gaussian") {
            df_cwc <- data.frame(Y = train[, glue::glue("{yname}"), drop=TRUE],
                                 train[, c(glue::glue("{xnames}_cwc"), Sname_dummies, glue::glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
                                           #glue("{xnames}_clmean"),
                                           varnames$W
                                 ), drop = FALSE])
        }
        if (family[[1]]=="binomial") {
            df_cwc <- data.frame(Y = train[, glue::glue("{yname}"), drop=TRUE],
                                 train[, c(glue::glue("{xnames}_cwc"), Sname_dummies, glue::glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
                                           #glue("{xnames}_clmean"),
                                           varnames$W
                                 ), drop = FALSE])
        }

        ##
        # print("Fitting CWC.FE...")
        # print(glue::glue("Columns in training data: {paste(colnames(df_cwc), collapse=', ')}"))
        
        # Fit a SuperLearner model, now with cluster dummies + cwc columns
        fit <- SuperLearner::SuperLearner(
            Y = df_cwc$Y,
            X = df_cwc[, -1, drop = FALSE],
            family = family[[1]],
            SL.library = learners,
            env = environment(SuperLearner::SuperLearner)
        )
        
        ##
        if (is.null(fit)) stop("Error: `fit` returned NULL in `cwc.FE`")

        # Predict on validation sets
        preds <- sapply(valid.list, function(validX) {

            ##
            # print(glue::glue("Validation fold size: {nrow(validX)}"))
            
            # Construct new data that includes xnames, cluster means, cluster dummies, etc.
            newX <- data.frame(
                validX[, c(xnames, glue::glue("{xnames}_clmean"), glue::glue("{yname}_clmean"), Sname_dummies, varnames$W), drop = FALSE]
            )
            # Compute within-cluster centered X
            validX_cwc <- validX[, xnames] - validX[, glue::glue("{xnames}_clmean")]
            colnames(validX_cwc) <- glue::glue("{xnames}_cwc")
            newX <- newX %>% dplyr::bind_cols(validX_cwc)
            # colnames(newX)
            
            ##
            if (is.null(newX) || ncol(newX) == 0) stop("Error: Validation data is NULL or empty.")
            if (!all(fit$varNames %in% colnames(newX))) stop("Error: Missing predictors in newX")
            
            # Predict using SL
            preds <- stats::predict(fit, newX[, fit$varNames])$pred

            # For gaussian, we might add back the cluster mean
            # e.g. preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
            if (family[[1]]=="gaussian") {
                preds <- preds #+ validX[, glue("{yname}_clmean"), drop=TRUE]
            }

            ##
            if (is.null(preds) || nrow(preds) == 0) {
                stop(glue::glue("Error: Predictions are NULL or empty for fold {v}."))
            }
            
            if (!bounded) {
                return(preds)
            }
            bound(preds)
        }, simplify = TRUE)
    }
    
    # --------------------------------------------------------------------------
    # 9. RETURN OUTPUT
    # --------------------------------------------------------------------------
    # The final output is a list with:
    # - 'fit': the fitted model object (glm, lmer, SuperLearner, etc.)
    # - 'preds': a matrix or vector of predictions for each validation set
    out <- list(
        fit   = fit,
        preds = preds
    )
    return(out)
    
}






################################## END #########################################

# ══════════════════════════════
#    ORIGINAL FUNCTION BELOW 
# ══════════════════════════════
# crossfit <- function(train, valid.list, yname, xnames, varnames,
#                      ipw = NULL,
#                      cluster_opt = "FE.glm",
#                      type, learners, bounded = FALSE) {
#     
#     Sname <- varnames$S
#     Sname_dummies <- varnames$Sdumm
#     
#     family <- ifelse(type == "binomial", binomial(), gaussian())
#     
#     df_lm <- data.frame(Y = train[[yname]],
#                         train[, c(xnames, varnames$W), drop = FALSE],
#                         S = train[[Sname]] )
#     if (length(ipw) > 0) {
#         df_lm$wreg <- ipw
#     }
#     if (length(ipw) == 0) {
#         df_lm$wreg <- rep(1, nrow(df_lm))
#     }
#     
#     # random effects ----
#     if (cluster_opt == "RE.glm") {
#         REformula <- paste("Y ~", paste(c(xnames, varnames$W), collapse = " + "), "+ (1 | S)")
#         if (family[[1]] == "gaussian") {
#             fit <- lmer(formula = REformula,
#                         weights = wreg,
#                         data = df_lm)
#         }
#         if (family[[1]] != "gaussian") {
#             fit <- glmer(formula = REformula,
#                          weights = wreg,
#                          data = df_lm, family = family[[1]])
#         }
#         preds <- sapply(valid.list, function(validX) {
#             newX <- data.frame(validX[, c(xnames, varnames$W), drop = FALSE],
#                                S = validX[[Sname]] )
#             preds <- predict(fit, newX, type = "response")
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#         
#     }
#     
#     # fixed effects ----
#     
#     ## glm ----
#     if (cluster_opt %in% c("FE.glm", "noncluster.glm")) {
#         if (cluster_opt == "FE.glm") {
#             fit <- glm(formula = paste("Y ~ S +", paste(xnames, collapse = " + ")),
#                        weights = wreg,
#                        data = df_lm, family = family[[1]])
#         }
#         if (cluster_opt == "noncluster.glm") {
#             fit <- glm(formula = paste("Y ~", paste(xnames, collapse = " + ")),
#                        weights = wreg,
#                        data = df_lm, family = family[[1]])
#         }
#         preds <- sapply(valid.list, function(validX) {
#             newX <- data.frame(validX[, xnames, drop = FALSE],
#                                S = validX[[Sname]] )
#             preds <- predict(fit, newX, type = "response")
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#     }
#     
#     ## dummy cluster indicator with SL ----
#     if (cluster_opt %in% c("fix.mlr", "noncluster.mlr")) {
#         df_FE <- data.frame(df_lm, train[, Sname_dummies])
#         set.seed(12345)
#         if (cluster_opt == "fix.mlr") {
#             
#             fit <- SuperLearner::SuperLearner(
#                 df_FE$Y,
#                 df_FE[, c(xnames, varnames$W, Sname_dummies), drop = FALSE],
#                 obsWeights = df_FE$wreg,
#                 family = family[[1]],
#                 SL.library = learners
#             )
#         }
#         if (cluster_opt == "noncluster.mlr") {
#             fit <- SuperLearner::SuperLearner(
#                 df_FE$Y,
#                 df_FE[, c(xnames), drop = FALSE],
#                 obsWeights = df_FE$wreg,
#                 family = family[[1]],
#                 SL.library = learners
#             )
#         }
#         
#         preds <- sapply(valid.list, function(validX) {
#             newX <- data.frame(validX[, c(xnames, varnames$W, Sname_dummies), drop = FALSE])
#             preds <- predict(fit, newX[, fit$varNames])$pred
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#     }
#     
#     
#     
#     # cwc with SL ------------------------
#     if (cluster_opt == "cwc") { # continuous outcome
#         # colnames(train) # note: train[, glue("{yname}"), drop=TRUE] doesn't work well
#         df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
#                              train[, c(glue("{xnames}_cwc"), glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
#                                        #glue("{xnames}_clmean"),
#                                        varnames$W
#                              ), drop = FALSE])
#         
#         fit <- SuperLearner::SuperLearner(
#             df_cwc$Y,
#             df_cwc[, -1, drop = FALSE],
#             family = family[[1]],
#             SL.library = learners,
#             env = environment(SuperLearner::SuperLearner)
#         )
#         
#         preds <- sapply(valid.list, function(validX) {
#             
#             newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), varnames$W), drop = FALSE]) 
#             validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
#             colnames(validX_cwc) <- glue("{xnames}_cwc")
#             newX <- newX %>% bind_cols(validX_cwc)
#             # colnames(newX)
#             preds <- predict(fit, newX[, fit$varNames])$pred
#             # preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
#             
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#         
#         
#     }
#     
#     # sufficient stats ----
#     if (cluster_opt == "sufficient_stats")  { # continuous outcome
#         if (family[[1]] == "binomial") {
#             df_ss <- data.frame(Y = train[, glue("{yname}"), drop=TRUE],
#                                 train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), glue("{yname}_clmean"), 
#                                          varnames$W), drop = FALSE])
#         }
#         if (family[[1]] == "gaussian") {
#             df_ss <- data.frame(Y = train[, glue("{yname}_cwc"), drop=TRUE],
#                                 train[,c(glue("{xnames}_cwc"), glue("{xnames}_clmean"), #glue("{yname}_clmean"), 
#                                          varnames$W), drop = FALSE])
#         } 
#         
#         fit <- SuperLearner::SuperLearner(
#             df_ss$Y,
#             df_ss[, -1, drop = FALSE],
#             family = family[[1]],
#             SL.library = learners,
#             env = environment(SuperLearner::SuperLearner)
#         )
#         
#         preds <- sapply(valid.list, function(validX) {
#             
#             newX <- data.frame(validX[, c(glue("{yname}_clmean"), xnames, glue("{xnames}_clmean"), varnames$W), drop = FALSE]) 
#             validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
#             colnames(validX_cwc) <- glue("{xnames}_cwc")
#             newX <- newX %>% bind_cols(validX_cwc)
#             # colnames(newX)
#             preds <- predict(fit, newX[, fit$varNames])$pred
#             
#             if (family[[1]] == "gaussian") {
#                 preds <- preds + validX[, glue("{yname}_clmean"), drop=TRUE]
#             }
#             
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#         
#         
#     }
#     
#     # cwc.FE ----
#     if (cluster_opt == "cwc.FE") { # continuous outcome
#         # colnames(train) # note: train[, glue("{yname}"), drop=TRUE] doesn't work well
#         if (family[[1]]=="gaussian") {
#             df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
#                                  train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
#                                            #glue("{xnames}_clmean"),
#                                            varnames$W
#                                  ), drop = FALSE])
#         }
#         if (family[[1]]=="binomial") {
#             df_cwc <- data.frame(Y = train[, glue("{yname}"), drop=TRUE], 
#                                  train[, c(glue("{xnames}_cwc"), Sname_dummies, glue("{yname}_clmean"), #glue("{varnames$Xnames}_clmean"),# only covariates' cluster means
#                                            #glue("{xnames}_clmean"),
#                                            varnames$W
#                                  ), drop = FALSE])
#         }
#         
#         
#         fit <- SuperLearner::SuperLearner(
#             df_cwc$Y,
#             df_cwc[, -1, drop = FALSE],
#             family = family[[1]],
#             SL.library = learners,
#             env = environment(SuperLearner::SuperLearner)
#         )
#         
#         preds <- sapply(valid.list, function(validX) {
#             
#             newX <- data.frame(validX[, c(xnames, glue("{xnames}_clmean"), glue("{yname}_clmean"), Sname_dummies, varnames$W), drop = FALSE]) 
#             validX_cwc <- validX[, xnames] - validX[, glue("{xnames}_clmean")]
#             colnames(validX_cwc) <- glue("{xnames}_cwc")
#             newX <- newX %>% bind_cols(validX_cwc)
#             # colnames(newX)
#             preds <- predict(fit, newX[, fit$varNames])$pred
#             
#             if (family[[1]]=="gaussian") {
#                 preds <- preds #+ validX[, glue("{yname}_clmean"), drop=TRUE]
#             }
#             
#             
#             if (!bounded) {
#                 return(preds)
#             }
#             bound(preds)
#         }, simplify = TRUE)
#         
#         
#     }
#     
#     out <- list(fit = fit, preds = preds)
#     return(out)
#     
#     
#     # old (original submission)
#     # if (cluster_opt == "FE.mlr") {
#     
#     # colnames(df_cwc)
#     # df_FE <- data.frame(df_lm, train[, Sname_dummies])
#     # df_cwc <- df_lm %>%
#     #     group_by(S) %>%
#     #     mutate(across(everything(), ~.x-mean(.x) ))
#     # df_cluster <- data.frame(df_cwc, train[, Sname_dummies])
#     
#     # fit <- SuperLearner::SuperLearner(
#     #     df_FE$Y,
#     #     df_cluster[, c(xnames, Sname_dummies), drop = FALSE],
#     #     family = family[[1]],
#     #     SL.library = learners,
#     #     env = environment(SuperLearner::SuperLearner)
#     # )
#     # preds <- sapply(valid.list, function(validX) {
#     #     newX <- data.frame(Y = validX[[yname]],
#     #                        validX[, xnames, drop = FALSE],
#     #                        S = validX[[Sname]] )
#     #     newX_cwc <- newX %>%
#     #         group_by(S) %>%
#     #         mutate(across(everything(), ~.x-mean(.x) ))
#     #     newX_cluster <- data.frame(newX_cwc, validX[, Sname_dummies])
#     #     
#     #     preds <- predict(fit, newX_cluster[, fit$varNames])$pred
#     #     
#     #     if (!bounded) {
#     #         return(preds)
#     #     }
#     #     bound(preds)
#     # }, simplify = TRUE)
#     # }
#     
#     
# }
