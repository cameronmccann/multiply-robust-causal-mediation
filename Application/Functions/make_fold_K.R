#' within cluster sample split and cross fit 
#' Create within-cluster V-fold cross-validation splits
#'
#' This function generates cross-validation folds by first splitting 
#' each cluster internally into \code{cv_folds}, then recombining the 
#' v-th fold from each cluster to form global folds. The result is that 
#' each validation fold contains observations from *all* clusters, and 
#' each cluster contributes to every fold.  
#'
#' @param data_in A data frame containing at least an \code{id} column 
#'   (unique row IDs) and the cluster variable specified by \code{Sname}.
#' @param Sname Character string giving the name of the cluster variable in \code{data_in}.
#' @param cv_folds Integer, number of cross-validation folds (default = 4).
#'
#' @return A list of length \code{cv_folds}, where each element is a fold 
#'   containing:
#'   \itemize{
#'     \item \code{validation_set} – vector of row indices (from \code{data_in$id}) 
#'       used for validation in this fold.
#'     \item \code{training_set} – vector of row indices (from \code{data_in$id}) 
#'       used for training in this fold.
#'   }
#'   Together, the folds cover all rows in \code{data_in}.
#'
#' @examples
#' library(origami)
#' set.seed(1)
#' df <- data.frame(
#'   id = 1:20,
#'   cluster = rep(letters[1:4], each = 5),
#'   x = rnorm(20)
#' )
#' folds <- make_fold_K(df, "cluster", cv_folds = 2)
#' str(folds[[1]])
#'
#' @export



make_fold_K <- function(data_in, Sname, Yname, cv_folds = 4) {
    
    # For each unique cluster, split its rows into V folds
    fold_K <- lapply(unique(data_in[[Sname]]), FUN = function(k) {
            
            # Assign integer cluster labels
            data_in$K <- match(data_in[[Sname]], unique(data_in[[Sname]]))
            # Map label to index (helps if clusters are character, instead of numeric)
            k_idx <- match(k, unique(data_in[[Sname]]))

            # Only split if cluster has at least one row
            if (nrow(data_in[data_in$K == k_idx, ]) >= 1) {

                # Build strata within cluster (only when feasible)
                strata_local <- NULL
                if (!is.null(Yname) && Yname %in% names(data_in)) {
                    y_k <- data_in[data_in$K == k_idx, Yname, drop = TRUE]
                    tab <- table(y_k)
                    if (length(tab) == 2L) {
                        if (min(tab) >= cv_folds) {
                            strata_local <- as.integer(as.factor(y_k))
                        } else {
                            warning(sprintf(
                                "Cluster '%s': minority count = %d < V = %d; not stratifying this cluster.",
                                as.character(k), min(tab), cv_folds
                            ))
                        }
                    } else {
                        warning(sprintf(
                            "Cluster '%s': binary outcome only has one value; not stratifying this cluster.",
                            as.character(k)
                        ))
                    }
                }

                # Create V-folds within this cluster (stratified when feasible)
                fk <- origami::make_folds(data_in[data_in$K == k_idx, ], # k,],
                                          fold_fun = origami::folds_vfold,
                                          V = cv_folds,
                                          strata_ids = strata_local)
                fold_k <- fk

                # Remap fold indices (relative to the cluster) back to global row IDs
                # v <- 1
                for (v in 1:cv_folds) {
                    fold_k[[v]]$validation_set <-
                        data_in$id[data_in$K == k_idx][fk[[v]]$validation_set]
                    fold_k[[v]]$training_set <-
                        data_in$id[data_in$K == k_idx][fk[[v]]$training_set]
                }
            }

            # NOTE: commented out code would handle tiny clusters (< 4 rows)
            # by reusing the whole cluster for both training and validation.

            # if (nrow(data_in[data_in$K==k, ]) < 4) {
            #   # if cluster size too small, no cluster split; use entire cluster as both training and valid
            #   fk <- origami::make_folds(
            #     data_in[data_in$K==k, ][sample(1:nrow(data_in[data_in$K==k, ]), cv_folds*2, replace = T), ],
            #                             fold_fun = origami::folds_vfold,
            #                             V = cv_folds
            #   )
            #   fold_k <- fk
            #   for(v in 1:cv_folds) {
            #     fold_k[[v]]$validation_set <- data_in$id[data_in$K==k]
            #     fold_k[[v]]$training_set <- data_in$id[data_in$K==k]
            #   }
            #
            # }

            return(fold_k)
    } )

    # Initialize global folds
    folds <- origami::make_folds(data_in,
                                 fold_fun = origami::folds_vfold,
                                 V = cv_folds)
    
    # For each v-th fold, combine v-th within-cluster folds across clusters
    for(v in 1:cv_folds) {
        folds[[v]]$validation_set <- unlist(lapply(1:length(fold_K), FUN = function(k = 1) {
            fold_K[[k]][[v]]$validation_set
            }))
        folds[[v]]$training_set <- unlist(lapply(1:length(fold_K), FUN = function(k = 1) {
            fold_K[[k]][[v]]$training_set
        }))
    }

  folds
}

# # tester
# make_fold_K <- function(data_in, Sname, Yname = NULL, cv_folds = 4) {
#     UC <- unique(data_in[[Sname]])
#     data_in$K <- match(data_in[[Sname]], UC)
#     
#     fold_K <- lapply(UC, function(k_label) {
#         k_idx <- match(k_label, UC)
#         idx_k <- which(data_in$K == k_idx)
#         if (length(idx_k) == 0) return(NULL)
#         
#         # Decide strata within this cluster
#         strata_local <- NULL
#         if (!is.null(Yname) && Yname %in% names(data_in)) {
#             y_k <- data_in[[Yname]][idx_k]
#             tab <- table(y_k)
#             if (length(tab) == 2L) {
#                 if (min(tab) >= cv_folds) {
#                     strata_local <- as.integer(as.factor(y_k))  # ok to stratify
#                 } else {
#                     warning(sprintf(
#                         "Cluster %s: minority count = %d < V = %d; not stratifying (still using V).",
#                         as.character(k_label), min(tab), cv_folds
#                     ))
#                 }
#             } else {
#                 warning(sprintf(
#                     "Cluster %s: outcome is single-class; cannot ensure both outcomes within folds.",
#                     as.character(k_label)
#                 ))
#             }
#         }
#         
#         # Make within-cluster folds (length should be exactly cv_folds)
#         fk <- origami::make_folds(
#             data_in[idx_k, , drop = FALSE],
#             fold_fun   = origami::folds_vfold,
#             V          = cv_folds,
#             strata_ids = strata_local
#         )
#         if (length(fk) != cv_folds) {
#             stop(sprintf("Internal: got V=%d folds for cluster %s (expected %d).", length(fk), as.character(k_label), cv_folds))
#         }
#         
#         # Map local indices back to global ids
#         for (v in seq_len(cv_folds)) {
#             val_idx <- if (!is.null(fk[[v]]$validation_set)) fk[[v]]$validation_set else fk[[v]]$validation
#             trn_idx <- if (!is.null(fk[[v]]$training_set))   fk[[v]]$training_set   else fk[[v]]$training
#             fk[[v]]$validation_set <- data_in$id[idx_k][val_idx]
#             fk[[v]]$training_set   <- data_in$id[idx_k][trn_idx]
#         }
#         fk
#     })
#     
#     # Initialize and combine v-th folds across clusters
#     folds <- origami::make_folds(data_in, fold_fun = origami::folds_vfold, V = cv_folds)
#     for (v in seq_len(cv_folds)) {
#         folds[[v]]$validation_set <- unlist(lapply(fold_K, function(fk) fk[[v]]$validation_set), use.names = FALSE)
#         folds[[v]]$training_set   <- unlist(lapply(fold_K, function(fk) fk[[v]]$training_set),   use.names = FALSE)
#     }
#     folds
# }

