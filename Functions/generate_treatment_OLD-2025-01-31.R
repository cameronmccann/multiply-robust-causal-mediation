#' @title generate_treatment 
#' 
#' @description Brief description of what the function does.
#' 
#' @param param1 Description of the first parameter, including its type (e.g., numeric, character).
#' @param param2 Description of the second parameter. Mention any special behavior, such as default values.
#' @param ... Description of additional arguments (if applicable). 
#' 
#' @return Description of the output value or object the function returns.
#' 
#' @details Additional details about the function, such as implementation notes or edge cases.
#' 
#' @examples
#' # Example usage
#' example_result <- example_function(10, 20)
#' print(example_result)
#'
#' @export
generate_treatment <- function(data_list, nj_sizes, icca, quadratic.A = FALSE, num_x = 3 #, # a_x = 0.15, # a_z = sqrt(0.4 / 1)
                               ) {
    
    gen_a <- list(
        icca = icca,                                       # Intra-class correlation for 'A'
        a_x = sqrt(0.15 * 1 / num_x), #sqrt(a_x * 1 / num_x),               # Effect of 'X' on 'A'
        a_z = sqrt(0.4 / 1) # a_z                          # Effect of 'Z' on 'A'
    )
    
    J <- length(unique(data_list$data$school))
    N <- nrow(data_list$data)
    
    # Generate cluster-level random effects for 'A'
    ab <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])), each = nj_sizes[.x])))
    
    # Compute the linear predictor for 'A'
    if (quadratic.A == FALSE) {
        Xlinear <- data_list$data$X
        a_given <- ab + gen_a[["a_x"]] * rowSums(Xlinear) + gen_a[["a_z"]] * data_list$data$Z
    } else {
        # Include quadratic terms if specified
        Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
        a_given <- ab + gen_a[["a_x"]] * rowSums(Xquad) + gen_a[["a_z"]] * data_list$data$Z
    }
    
    # Compute the true propensity score 
    ps_true <- pnorm(a_given, mean = 0, sd = sqrt(1 - gen_a$icca))
    # ps_true <- pnorm(a_given, mean = 0, sd = sqrt(1 - gen_a[["icca"]]))
    # prob1 <- 1 * ps_true + (1 - 1) * (1 - ps_true) # make a=1: prob1 <- a * ps_true + (1 - a) * (1 - ps_true)
    
    # Note: check overlap assumption 
    
    # Generate binary treatment 'A' using the true propensity score 
    data_list$data$A <- rbinom(N, 1, ps_true)
    # data_list$data$A <- rbinom(N, 1, prob1)
    
    # prob1 <- pnorm(a_given, mean = 0, sd = sqrt(1 - gen_a$icca))
    # data_list$data$A <- rbinom(N, 1, prob1) #(a * prob1 + (1 - a) * (1 - prob1)))
    
    # Add true propensity score to data 
    data_list$data$ps_true <- ps_true
    
    return(modifyList(data_list, list(
        a_x = gen_a[["a_x"]], 
        a_z = gen_a[["a_z"]], 
        icca = icca, 
        quadratic.A = quadratic.A
    )))
}




