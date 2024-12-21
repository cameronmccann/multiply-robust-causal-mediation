#' @title generate_outcome 
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
#' @seealso [my()]
#' @export
generate_outcome <- function(data_list, iccy = 0.2, yintercept = 1, 
                             y_on_a = 0.2, 
                             y_on_m = 1, 
                             y_on_am = 0, 
                             y_on_az = 0.2, 
                             y_on_mz = 0.2, 
                             y_on_anj = 0.2,
                             num_x = 3,
                             # y_on_x = sqrt(0.15 / num_x),
                             # y_on_z = sqrt(0.4),
                             quadratic.Y = FALSE, 
                             int.XZ = TRUE, 
                             Yfamily = "gaussian", if.null = FALSE) {
    
    # Define parameters for the outcome model
    gen_y <- list(
        iccy = iccy,             # Intra-class correlation for 'Y'
        yintercept = yintercept,          # Intercept
        y_on_a = y_on_a,            # Effect of 'A' on 'Y'
        y_on_m = y_on_m,             # Effect of 'M' on 'Y'
        y_on_am = y_on_am,            # Interaction effect of 'A' and 'M' on 'Y'
        y_on_az = y_on_az,           # Interaction effect of 'A' and 'Z' on 'Y'
        y_on_mz = y_on_mz,          # Interaction effect of 'M' and 'Z' on 'Y'
        y_on_anj = y_on_anj,          # Interaction effect of 'A' and cluster size on 'Y'
        y_on_x = sqrt(0.15 / num_x), # y_on_x,  # Effect of 'X' on 'Y'
        y_on_z = sqrt(0.4) # y_on_z        # Effect of 'Z' on 'Y'
    )
    
    J <- length(unique(data_list$data$school))
    N <- nrow(data_list$data)
    
    
    # Set interaction effects to zero if specified
    # y_on_amint <- 0  # Not directly used in this code
    
    # Generate cluster-level random effects for 'Y'
    yb <- rnorm(J, sd = sqrt(gen_y[["iccy"]]))[data_list$data$school]
    
    # If generating data under the null hypothesis, set effects to zero
    if (if.null == TRUE) {
        gen_y <- list(
            iccy = iccy,
            yintercept = 1,
            y_on_a = 0,
            y_on_m = 0,
            y_on_am = 0,
            y_on_az = 0,
            y_on_mz = 0,
            y_on_anj = 0,
            y_on_x = y_on_x, # sqrt(0.15 / num_x),
            y_on_z = y_on_z  # sqrt(0.4)
        )
    }
    
    # Skipping this part
    # If interaction between 'A' and cluster size is not included
    if (int.XZ == FALSE) {
        gen_y[c("y_on_anj")] <- 0 # NOTE : derlete
    }
    
    # Compute the linear predictor for 'Y'
    if (quadratic.Y == TRUE) {
        # Include quadratic terms if specified
        Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
        y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xquad) +
            gen_y[["y_on_z"]] * data_list$data$Z
    } else {
        # Linear terms only
        Xlinear <- data_list$data$X
        y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xlinear) +
            gen_y[["y_on_z"]] * data_list$data$Z
    }
    
    
    # Generate outcome 'Y'
    if (Yfamily == "gaussian") {
        # For continuous outcome
        # latent <- gen_y[["y_on_m"]] * data_list$data$M + gen_y[["y_on_a"]] * data_list$data$A + gen_y[["y_on_am"]] * data_list$data$A * data_list$data$M + gen_y[["y_on_az"]] * data_list$data$A * data_list$data$Z + gen_y[["y_on_mz"]] * data_list$data$M * data_list$data$Z + gen_y[["y_on_anj"]] * data_list$data$A * data_list$data$W_nj + y_given
        # condmy <- latent
        condmy <- my(m = data_list$data$M, a = data_list$data$A, z = data_list$data$Z, nj = data_list$data$W_nj, given = y_given, gen_y = gen_y, binary = FALSE)
        data_list$data$Y <- condmy + rnorm(N, sd = sqrt(1 - gen_y[["iccy"]]))
    } else if (Yfamily == "binomial") {
        # For binary outcome
        condmy <- my(m = data_list$data$M, a = data_list$data$A, z = data_list$data$Z, nj = data_list$data$W_nj, given = y_given, gen_y = gen_y, binary = TRUE)
        # latent <- gen_y[["y_on_m"]] * data_list$data$M + gen_y[["y_on_a"]] * data_list$data$A + gen_y[["y_on_am"]] * data_list$data$A * data_list$data$M + gen_y[["y_on_az"]] * data_list$data$A * data_list$data$Z + gen_y[["y_on_mz"]] * data_list$data$M * data_list$data$Z + gen_y[["y_on_anj"]] * data_list$data$A * data_list$data$W_nj + y_given
        # condmy <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_y[["iccy"]]))
        data_list$data$Y <- rbinom(N, 1, condmy)
    }
    
    return(modifyList(data_list, list(
        iccy = iccy, 
        yintercept = yintercept, 
        y_on_a = gen_y[["y_on_a"]],
        y_on_m = gen_y[["y_on_m"]],
        y_on_am = gen_y[["y_on_am"]],
        y_on_az = gen_y[["y_on_az"]],
        y_on_mz = gen_y[["y_on_mz"]],
        y_on_anj = gen_y[["y_on_anj"]],
        y_on_x = gen_y[["y_on_x"]], 
        y_on_z = gen_y[["y_on_z"]], 
        quadratic.Y = quadratic.Y, 
        int.XZ = int.XZ, 
        Yfamily = Yfamily, 
        y_given = y_given
    )))
    
}
