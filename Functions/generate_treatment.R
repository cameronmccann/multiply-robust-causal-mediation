#' Generate Treatment Assignments
#'
#' This function generates treatment assignments in a clustered setting either 
#' by using a model-based propensity score (default) or by completely random 
#' assignment (like a randomized experiment).
#'
#' @param data_list A list containing at least the element \code{data}, which is 
#'   a \code{data.frame} with variables \code{X}, \code{Z}, and \code{school}.
#' @param nj_sizes A numeric vector of length \code{J} (number of schools) 
#'   indicating the number of individuals in each school.
#' @param icca A numeric value for the intra-class correlation for \code{A}.
#' @param quadratic.A Logical. If \code{TRUE}, quadratic terms for \code{X} 
#'   (transformed as (\code{X}^2 - 1)/sqrt(4)) are used. Default is \code{FALSE}.
#' @param num_x Number of columns in \code{X}. Default is 3.
#' @param randomize Logical. If \code{TRUE}, assignments are completely 
#'   randomized with probability 0.5 for treatment vs control, overriding the 
#'   model-based propensity score. Default is \code{FALSE}.
#'
#' @return Returns \code{data_list} with updated treatment assignments 
#'   (\code{A}) and the true propensity scores (\code{ps_true}). Also returns 
#'   the coefficients used to generate \code{A} (\code{a_x}, \code{a_z}) and 
#'   the \code{icca} and \code{quadratic.A} values.
#'
#' @examples
#' # Example usage:
#' # data_list <- list(data = data.frame(X = matrix(rnorm(300), ncol=3), 
#' #                                    Z = rnorm(100), 
#' #                                    school = rep(1:10, each=10)))
#' # nj_sizes <- rep(10, 10)
#' # new_data_list <- generate_treatment(data_list, nj_sizes, icca=0.1, 
#' #                                    quadratic.A=FALSE, num_x=3,
#' #                                    randomize=TRUE)
#'
#' @export
generate_treatment <- function(data_list, 
                               nj_sizes, 
                               icca, 
                               quadratic.A = FALSE, 
                               num_x = 3, 
                               randomize = FALSE) {
    
    # If randomize is TRUE, generate random A (like a randomized experiment)
    if (isTRUE(randomize)) {
        
        N <- nrow(data_list$data)
        # Assign each individual to treatment with probability 0.5
        data_list$data$A <- rbinom(N, 1, 0.5)
        
        # Set the true propensity score to 0.5 for everyone in the randomized scenario
        data_list$data$ps_true <- rep(0.5, N)
        
        # For consistency, return placeholders (or NA) for a_x, a_z
        # or keep them the same as the default. 
        # Here we just store NA since they're not used.
        return(
            modifyList(data_list, list(
                a_x = NA_real_,
                a_z = NA_real_,
                icca = icca,
                quadratic.A = quadratic.A
            ))
        )
        
    } else {
        
        # --- Original logic: Generate A based on a model with cluster effects ---
        
        # Store generation parameters
        gen_a <- list(
            icca = icca, 
            a_x = sqrt(0.15 * 1 / num_x), 
            a_z = sqrt(0.4 / 1)
        )
        
        J <- length(unique(data_list$data$school))
        N <- nrow(data_list$data)
        
        # Generate cluster-level random effects for 'A'
        ab <- unlist(
            purrr::map(seq_len(J), ~rep(rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])), 
                                        each = nj_sizes[.x]))
        )
        
        # Compute the linear predictor for 'A'
        if (!quadratic.A) {
            Xlinear <- data_list$data$X
            a_given <- ab + gen_a[["a_x"]] * rowSums(Xlinear) + gen_a[["a_z"]] * data_list$data$Z
        } else {
            # Include quadratic terms if specified
            Xquad <- (data_list$data$X^2 - 1) / sqrt(4)
            a_given <- ab + gen_a[["a_x"]] * rowSums(Xquad) + gen_a[["a_z"]] * data_list$data$Z
        }
        
        # Compute the true propensity score
        ps_true <- pnorm(a_given, mean = 0, sd = sqrt(1 - gen_a$icca))
        
        # Generate binary treatment 'A' using the true propensity score
        data_list$data$A <- rbinom(N, 1, ps_true)
        
        # Add true propensity score to data
        data_list$data$ps_true <- ps_true
        
        # Return updated data_list with parameters used
        return(
            modifyList(data_list, list(
                a_x = gen_a[["a_x"]], 
                a_z = gen_a[["a_z"]], 
                icca = icca, 
                quadratic.A = quadratic.A
            ))
        )
    }
}
