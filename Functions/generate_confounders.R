#' @title generate_confounders 
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
#' @importFrom mvtnorm rmvnorm 
#' @export
generate_confounders <- function(data_list, 
                                 nj_sizes,
                                 num_x = 3,
                                 iccx = 0.2,
                                 x_z = 0) {
    
    
    # library(mvtnorm)
    # Required library
    if (!requireNamespace("mvtnorm", quietly = TRUE)) {
        stop("Package 'mvtnorm' is required.")
    }
    
    J <- length(data_list$nj_sizes)
    N <- nrow(data_list$data)
    
    # Generate cluster-level unobserved confounder 'Z'
    if (!"Z" %in% names(data_list$data)) {
        data_list$data$Z <- unlist(map(1:J, ~ rep(rnorm(1), each = nj_sizes[.x])))
    }
    
    # Generate individual-level confounders 'X'
    gen_x <- list(iccx = iccx, x_z = x_z)
    
    # Generate cluster-level random effects for 'X'
    xb <- mvtnorm::rmvnorm(
        n = J,
        mean = rep(0, num_x),
        sigma = diag((1 - gen_x[["x_z"]] ^ 2) * gen_x[["iccx"]], nrow = num_x)
    )[data_list[["data"]]$school,]
    
    # Generate individual-level random effects for 'X'
    xe <- mvtnorm::rmvnorm(
        n = N,
        mean = rep(0, num_x),
        sigma = diag(1 - gen_x[["iccx"]], nrow = num_x)
    )
    
    # Compute 'X' as a function of 'Z' and random effects
    x <- gen_x[["x_z"]] * data_list[["data"]]$Z + xb + xe
    
    # Name the columns as x1, x2, x3
    colnames(x) <- paste0("x", 1:num_x)
    
    # # Add new X variables to the data frame
    data_list[["data"]]$X <- x
    # # Add each new variable (X1, X2, ...) as a separate column in the data frame
    # for (i in 1:num_x) {
    #     data_list$data[[paste0("X", i)]] <- x[, i]
    # }
    
    return(modifyList(data_list, list(
        # data = data_list[["data"]],
        # x = x,
        # xb = xb,
        # xe = xe,
        num_x = num_x, 
        x_z = x_z
    )))
}
