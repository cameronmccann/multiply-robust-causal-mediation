#' @title generate_mediator 
#' 
#' @description Generates a mediator variable 'M' either as binomial or gaussian, 
#'  influenced by specified covariates and interactions.
#' 
#' @param data_list A list containing a data frame with variables 'school', 'A', 'Z', 'X', and 'W_nj'.
#' @param nj_sizes Vector of cluster sizes (length equals the number of unique schools).
#' @param iccm Numeric. Intra-class correlation for 'M'.
#' @param num_x Numeric. Number of X covariates.
#' @param m_on_a Numeric. Effect of 'A' on 'M'.
#' @param m_on_az Numeric. Interaction effect of 'A' and 'Z' on 'M'.
#' @param m_on_anj Numeric. Interaction effect of 'A' and cluster size on 'M'.
#' @param quadratic.M Logical. If TRUE, includes quadratic terms for 'X' in the model for 'M'.
#' @param int.XZ Logical. If FALSE, sets the 'm_on_anj' effect to 0.
#' @param Mfamily Character. Either "binomial" or "gaussian". Determines the distribution of the mediator 'M'.
#' 
#' @return The modified `data_list` with the generated mediator 'M' and associated parameters.
#' 
#' @details Additional details about the function, such as implementation notes or edge cases.
#' 
#' @examples
#' # Assuming you have a suitable data_list and nj_sizes:
#' # data_list <- list(data = data.frame(
#' #   school = rep(1:5, each = 20),
#' #   A = rbinom(100, 1, 0.5),
#' #   Z = rnorm(100),
#' #   X = matrix(rnorm(300), ncol = 3), 
#' #   W_nj = rep(1:20, 5)
#' # ))
#' # nj_sizes <- rep(20, 5)
#' # example_result <- generate_mediator(data_list, nj_sizes, Mfamily = "binomial")
#' # head(example_result$data$M)
#'
#' @seealso [pm1()]
#' @export

generate_mediator <- function(data_list, nj_sizes, iccm = 0.2, num_x = 3, m_on_a = 0.2,
                              m_on_az = 0.2, m_on_anj = 0.2, 
                              m_on_x = sqrt(0.15 / num_x), m_on_z = sqrt(0.4),
                              quadratic.M = FALSE, 
                              # int.XZ = TRUE, 
                              Mfamily = "binomial") {
    
    # Input validation for Mfamily
    if (!Mfamily %in% c("binomial", "gaussian")) {
        stop("Mfamily must be either 'binomial' or 'gaussian'")
    }
    
    gen_m <- list(
        iccm = iccm,         # Intra-class correlation for 'M'
        m_on_a = m_on_a,        # Effect of 'A' on 'M'
        m_on_x = sqrt(0.15 / num_x), # m_on_x, # Effect of 'X' on 'M'
        m_on_z = sqrt(0.4), # m_on_z,  # Effect of 'Z' on 'M'
        m_on_az = m_on_az,       # Interaction effect of 'A' and 'Z' on 'M'
        m_on_anj = m_on_anj      # Interaction effect of 'A' and cluster size on 'M'
    )
    
    J <- length(unique(data_list$data$school))
    N <- sum(nj_sizes) #N <- nrow(data_list$data)
    
    # # skipping this part
    # # # If interaction between 'A' and cluster size is not included
    # if (int.XZ == FALSE) {
    #     gen_m[c("m_on_anj")] <- 0 # NOTE: delete 
    # }
    
    # Generate cluster-level random effects for mediators
    mb <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm"]])), each = nj_sizes[.x])))
    
    # Generate mediator 'M'
    if (quadratic.M == TRUE) {
        # Include quadratic terms if specified
        Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
        
        # Compute the linear predictor for 'M1'
        m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xquad) +
            gen_m[["m_on_z"]] * data_list$data$Z
    } else {
        # Linear terms only
        Xlinear <- data_list$data$X
        
        # Compute the linear predictor for 'M1'
        m_given <- mb + gen_m[["m_on_x"]] * rowSums(Xlinear) +
            gen_m[["m_on_z"]] * data_list$data$Z # m_latent relabel 
    }
    
    # Generate mediator values
    if (Mfamily == "binomial") {
        # # try plogis 
        # latent <- gen_m[["m_on_a"]] * data_list$data$A +
        #     gen_m[["m_on_az"]] * data_list$data$A * data_list$data$Z +
        #     gen_m[["m_on_anj"]] * data_list$data$A * data_list$data$W_nj +
        #     m_given
        # prob1 <- plogis(latent)
        # data_list$data$M <- rbinom(N, size = 1, prob = prob1 + rnorm(N, 0, sd = sqrt(1 - gen_m[["iccm"]])))

        #
        probm <- pm1(1, data_list$data$A, data_list$data$Z, data_list$data$W_nj, m_given, gen_m)
        data_list$data$M <- rbinom(N, size = 1, prob = probm)
        
        # latent <- gen_m[["m_on_a"]] * data_list$data$A + gen_m[["m_on_az"]] * data_list$data$A*data_list$data$Z + gen_m[["m_on_anj"]] * data_list$data$A*data_list$data$W_nj + m_given
        # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
        # # m * prob1 + (1 - m) * (1 - prob1) # m = 1 so can drop
        # data_list$data$M <- rbinom(N, 1, prob = prob1)
    } else if (Mfamily == "gaussian") {
        # mean_m <- m_given # m_latent
        mean_m <- m_given + 
            gen_m[["m_on_a"]] * data_list$data$A +
            gen_m[["m_on_az"]] * data_list$data$A * data_list$data$Z +
            gen_m[["m_on_anj"]] * data_list$data$A * data_list$data$W_nj
        
        data_list$data$M <- rnorm(N, mean = mean_m, sd = sqrt(1 - gen_m$iccm))
    }
    
    modifyList(data_list, list(
        iccm = iccm,
        m_on_a = gen_m[["m_on_a"]],
        m_on_x = gen_m[["m_on_x"]],
        m_on_z = gen_m[["m_on_z"]],
        m_on_az = gen_m[["m_on_az"]],
        m_on_anj = gen_m[["m_on_anj"]],
        quadratic.M = quadratic.M,
        Mfamily = Mfamily,
        m_given = m_given
    ))
}


##################################### END ######################################




# Alternative code for function  ------------------------------------------
# This function results in different mediator values when continuous. Investigate prior to using. 


#' @title generate_mediator 
#' 
#' @description Generates a mediator variable 'M' either as binomial (Bernoulli) or Gaussian, 
#'  influenced by specified covariates and interactions.
#' 
#' @param data_list A list containing a data frame with variables 'school', 'A', 'Z', 'X', and 'W_nj'.
#' @param nj_sizes Vector of cluster sizes (length equals the number of unique schools).
#' @param iccm Numeric. Intra-class correlation for 'M'.
#' @param num_x Numeric. Number of X covariates.
#' @param m_on_a Numeric. Effect of 'A' on 'M'.
#' @param m_on_az Numeric. Interaction effect of 'A' and 'Z' on 'M'.
#' @param m_on_anj Numeric. Interaction effect of 'A' and cluster size on 'M'.
#' @param quadratic.M Logical. If TRUE, includes quadratic terms for 'X' in the model for 'M'.
#' @param int.XZ Logical. If FALSE, sets the 'm_on_anj' effect to 0.
#' @param Mfamily Character. Either "binomial" or "gaussian". Determines the distribution of the mediator 'M'.
#' 
#' @return The modified `data_list` with the generated mediator 'M' and associated parameters.
#' 
#' @details For "binomial" M, probabilities are derived from a latent linear predictor and mapped through the normal CDF.
#'  For "gaussian" M, values are sampled from a normal distribution with mean defined by the linear predictor.
#' 
#' @examples
#' # Assuming you have a suitable data_list and nj_sizes:
#' # data_list <- list(data = data.frame(
#' #   school = rep(1:5, each = 20),
#' #   A = rbinom(100, 1, 0.5),
#' #   Z = rnorm(100),
#' #   X = matrix(rnorm(300), ncol = 3), 
#' #   W_nj = rep(1:20, 5)
#' # ))
#' # nj_sizes <- rep(20, 5)
#' # example_result <- generate_mediator(data_list, nj_sizes, Mfamily = "binomial")
#' # head(example_result$data$M)
#'
#' @seealso [pm1()]
#' @export
# generate_mediator <- function(data_list, nj_sizes, iccm = 0.2, num_x = 3, m_on_a = 0.2,
#                               m_on_az = 0.2, m_on_anj = 0.2, quadratic.M = FALSE, int.XZ = FALSE,
#                               Mfamily = c("binomial", "gaussian")) {
# 
#     Mfamily <- match.arg(Mfamily)
# 
#     gen_m <- list(
#         iccm = iccm,            # Intra-class correlation for 'M'
#         m_on_a = m_on_a,        # Effect of 'A' on 'M'
#         m_on_x = sqrt(0.15 / num_x), # Effect of 'X' on 'M'
#         m_on_z = sqrt(0.4),     # Effect of 'Z' on 'M'
#         m_on_az = m_on_az,      # Interaction effect of 'A' and 'Z' on 'M'
#         m_on_anj = m_on_anj     # Interaction effect of 'A' and cluster size on 'M'
#     )
# 
#     J <- length(unique(data_list$data$school))
#     N <- nrow(data_list$data)
# 
#     # If interaction between 'A' and cluster size is not included
#     if (!int.XZ) {
#         gen_m[["m_on_anj"]] <- 0
#     }
# 
#     # Generate cluster-level random effects for mediators
#     mb <- purrr::map_dbl(1:J, ~ rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm"]])))
#     mb <- unlist(purrr::map2(1:J, nj_sizes, ~rep(mb[.x], each = .y)))
# 
#     # Construct linear predictors for M
#     if (quadratic.M) {
#         # Quadratic terms for X
#         Xquad <- (data_list$data$X ^ 2 - 1) / sqrt(4)
#         x_effect <- gen_m[["m_on_x"]] * rowSums(Xquad)
#     } else {
#         Xlinear <- data_list$data$X
#         x_effect <- gen_m[["m_on_x"]] * rowSums(Xlinear)
#     }
# 
#     # Base linear predictor (without A)
#     m_given <- mb + x_effect + gen_m[["m_on_z"]] * data_list$data$Z
# 
#     # Full linear predictor including A and interactions
#     latent <- gen_m[["m_on_a"]] * data_list$data$A +
#         gen_m[["m_on_az"]] * (data_list$data$A * data_list$data$Z) +
#         gen_m[["m_on_anj"]] * (data_list$data$A * data_list$data$W_nj) +
#         m_given
# 
#     # Generate M based on Mfamily
#     if (Mfamily == "binomial") {
#         # For binomial M, use a probit model
#         # M ~ Bernoulli(prob), where prob = pnorm(latent, 0, sqrt(1-iccm))
#         prob <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
#         data_list$data$M <- rbinom(N, 1, prob = prob)
#     } else {
#         # For Gaussian M, M ~ Normal(latent, sqrt(1-iccm))
#         data_list$data$M <- rnorm(N, mean = latent, sd = sqrt(1 - gen_m[["iccm"]]))
#     }
# 
#     return(modifyList(data_list, list(
#         iccm = iccm,
#         m_on_a = gen_m[["m_on_a"]],
#         m_on_x = gen_m[["m_on_x"]],
#         m_on_z = gen_m[["m_on_z"]],
#         m_on_az = gen_m[["m_on_az"]],
#         m_on_anj = gen_m[["m_on_anj"]],
#         quadratic.M = quadratic.M,
#         m_given = m_given,
#         Mfamily = Mfamily
#     )))
# }

