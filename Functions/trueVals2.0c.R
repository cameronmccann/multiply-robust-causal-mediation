#' @title trueVals2.0c
#'
#' @description
#' Computes the true potential outcomes under interventions on treatment (\code{A}) 
#' and mediator (\code{M}) for the given data configuration. It returns these true 
#' values at both the individual and cluster levels, enabling the calculation of 
#' natural direct/indirect effects in a clustered setting.
#'
#' @param data_list A list containing the generated dataset (\code{data}), cluster sizes, 
#'                  and all generation parameters. This should be the output from one 
#'                  of the data generation functions (e.g., \code{generate_data2.0c()}).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{truevals_individual}}{A named list of true expected outcomes at the individual level 
#'                                     for each scenario \code{Y(a0, gm(a1))}.}
#'   \item{\code{truevals_cluster}}{A named list of true expected outcomes at the cluster level for 
#'                                  each scenario \code{Y(a0, gm(a1))}.}
#' }
#'
#' @details
#' This function calculates the expected outcomes under the four potential-outcome scenarios:
#' \itemize{
#'   \item \(\text{Y}(a0=0, \text{gm}(a1=0))\): Set \code{A = 0} and \code{M} to the distribution 
#'         it would have under \code{A = 0}.
#'   \item \(\text{Y}(a0=0, \text{gm}(a1=1))\): Set \code{A = 0} and \code{M} to the distribution 
#'         it would have under \code{A = 1}.
#'   \item \(\text{Y}(a0=1, \text{gm}(a1=0))\): Set \code{A = 1} and \code{M} to the distribution 
#'         it would have under \code{A = 0}.
#'   \item \(\text{Y}(a0=1, \text{gm}(a1=1))\): Set \code{A = 1} and \code{M} to the distribution 
#'         it would have under \code{A = 1}.
#' }
#'
#' For **binary mediators**, the function sums over \code{M = 0, 1} with respective probabilities.  
#' For **continuous mediators**, it integrates (via closed-form or Monte Carlo) over the distribution of \code{M}.  
#' For **binary outcomes**, \code{Y} is derived from a probit link, i.e. \code{pnorm(latentY)}.  
#' For **continuous outcomes**, the linear predictor \code{latentY} is used directly.
#'
#' Additional logic is included for the special case of a **continuous mediator** combined with a 
#' **binary outcome**, where a large population dataset is generated (by calling \code{generate_data2.0c()} 
#' internally) to approximate the integral. This provides a Monte Carlo estimate of the expected 
#' outcome \(\mathbb{E}[Y]\).
#'
#' @import dplyr
#' @importFrom stats dnorm pnorm
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' # Typically called internally. Example call:
#' data_list <- generate_data2.0c(J = 50, Mfamily = "binomial", Yfamily = "binomial")
#' result <- trueVals2.0c(data_list)
#' result$truevals_individual
#' }
#'
#' @seealso 
#' \code{\link{trueVals2.0b}}, 
#' \code{\link{generate_data2.0c}}
#'
#' @export
trueVals2.0c <- function(data_list) {
    
    library(dplyr)
    library(glue)
    
    # -------------------------------------------------------------------------
    # 1. EXTRACT COMPONENTS FROM data_list
    # -------------------------------------------------------------------------
    data <- data_list$data
    nj_sizes <- data_list$nj_sizes
    Mfamily <- data_list$Mfamily
    Yfamily <- data_list$Yfamily
    iccm <- data_list$iccm
    iccy <- data_list$iccy
    
    # Extract mediator parameters
    gen_m <- list(
        iccm = iccm,
        m_on_a = data_list$m_on_a,
        m_on_x = data_list$m_on_x,
        m_on_z = data_list$m_on_z,
        m_on_az = data_list$m_on_az,
        m_on_anj = data_list$m_on_anj
    )
    
    # Extract outcome parameters
    gen_y <- list(
        iccy = iccy,
        yintercept = data_list$yintercept,
        y_on_a = data_list$y_on_a,
        y_on_m = data_list$y_on_m,
        y_on_am = data_list$y_on_am,
        y_on_az = data_list$y_on_az,
        y_on_mz = data_list$y_on_mz,
        y_on_anj = data_list$y_on_anj,
        y_on_x = data_list$y_on_x,
        y_on_z = data_list$y_on_z
    )
    
    m_given <- data_list$m_given
    y_given <- data_list$y_given
    
    # Attach cluster sizes to the data 
    data <- data |> mutate(nj = nj_sizes[school])
    
    # -------------------------------------------------------------------------
    # 2. HELPER FUNCTIONS
    # -------------------------------------------------------------------------
    
    # Calculate latent mean for M
    calc_m_latent <- function(a, z, nj, given) {
        latent_m <- gen_m$m_on_a * a +
            gen_m$m_on_az * a * z +
            gen_m$m_on_anj * a * nj +
            given
        return(latent_m)
    }
    
    # Calculate latent outcome Y
    calc_y_latent <- function(m, a, z, nj, given) {
        latent_y <- gen_y$y_on_m * m +
            gen_y$y_on_a * a +
            gen_y$y_on_am * a * m +
            gen_y$y_on_az * a * z +
            gen_y$y_on_mz * m * z +
            gen_y$y_on_anj * a * nj +
            given
        return(latent_y)
    }

    # -------------------------------------------------------------------------
    # 3. FUNCTION TO COMPUTE E[Y(a0, gm(a1))] FOR EACH OBSERVATION
    # -------------------------------------------------------------------------
    compute_expected_y <- function(a0_val, a1_val, data) {
        z <- data$Z
        nj <- data$nj
        given_m <- m_given
        given_y <- y_given
        
        # Compute mediator parameters
        m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)
        
        if (Mfamily == "binomial") {
            # ══════════════════════════════
            # Binary mediator:
            # p(M=1) = pnorm(m_latent)
            # p(M=0) = 1 - p(M=1)
            # E[Y] = p(M=0)*E[Y|M=0] + p(M=1)*E[Y|M=1]
            # ══════════════════════════════
            p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - iccm))
            p_m0 <- 1 - p_m1
            
            # For each M = 0/1, compute latent Y
            y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, nj = nj, given = given_y)
            y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, nj = nj, given = given_y)
            
            # Then get E[Y|M=0], E[Y|M=1] depending on outcome family
            if (Yfamily == "binomial") {
                E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
                E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
            } else {
                # If Y is gaussian
                E_y_m0 <- y_latent_m0
                E_y_m1 <- y_latent_m1
            }
            
            E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
            
        } else {
            # ══════════════════════════════
            # Continuous mediator:
            # M ~ Normal(m_latent, var=1 - iccm)
            # We must integrate or do a linear approximation
            # ══════════════════════════════
            var_m <- 1 - iccm
            mean_m <- m_latent
            
            if (Yfamily == "gaussian") {
                # Continuous M, continuous Y => linear 
                # E[Y] = E[E[Y|M]] = E[y_latent(M)]
                y_latent_mean_m <- calc_y_latent(m = mean_m, a = a0_val, z = z, nj = nj, given = given_y)
                E_y <- y_latent_mean_m
            } else {
                # Continous M, binary Y => handled later (next step)
                E_y <- NULL
            }
        }
        
        return(E_y)
    }
    
    # -------------------------------------------------------------------------
    # 4. LOOP OVER (a0, a1) = (0,0), (0,1), (1,0), (1,1) & COMPUTE TRUE EXPECTATIONS
    # -------------------------------------------------------------------------
    a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
    truevals_individual <- list()
    truevals_cluster    <- list()

    for (j in 1:nrow(a_vals)) {
        a0_val <- a_vals$a0[j]
        a1_val <- a_vals$a1[j]
        label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
        
        # ══════════════════════════════
        # Continuous M, Binary Y => Monte Carlo approximation
        # done by generating a large population with generate_data2.0c(...).
        # ══════════════════════════════
        if (Mfamily == "gaussian" & Yfamily == "binomial") {
            
            # Generate a large "population" (J=10,000) to approximate integral
            large_J <- 10000
            big_njrange <- c(data_list$njrange[1], data_list$njrange[2]) 
            
            pop_result <- generate_data2.0c( 
                include_truevals = FALSE, # 
                J = large_J,
                njrange = big_njrange,
                Mfamily = "gaussian",
                Yfamily = "binomial",
                if.null = FALSE, # CHANGE
                seed = seed,
                num_x = num_x, 
                # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
                # a_z = sqrt(0.4 / 1),
                x_z = x_z, 
                m_on_a = data_list$m_on_a, 
                m_on_az = data_list$m_on_az, 
                m_on_anj = data_list$m_on_anj, 
                # m_on_x = sqrt(0.15 / num_x),
                # m_on_z = sqrt(0.4),
                # int.XZ = TRUE,       # DELETE
                yintercept = data_list$yintercept, 
                y_on_a = data_list$y_on_a,
                y_on_m = data_list$y_on_m, 
                y_on_am = data_list$y_on_am, 
                y_on_az = data_list$y_on_az, 
                y_on_mz = data_list$y_on_mz, 
                y_on_anj = data_list$y_on_anj, 
                # y_on_x = sqrt(0.15 / num_x),
                # y_on_z = sqrt(0.4),
                quadratic.A = data_list$quadratic.A, 
                quadratic.M = data_list$quadratic.M, 
                quadratic.Y = data_list$quadratic.Y, 
                iccx = iccx, 
                icca = data_list$icca, 
                iccm = data_list$iccm, 
                iccy = data_list$iccy 
            )
            
            # Add the cluster sizes to the newly generated dataset
            pop_result$data <- pop_result$data |> 
                mutate(nj = nj_sizes[school])
            
            # Compute the mediator latent mean for M under (A=a1_val)
            m_mu <- calc_m_latent(
                a = a1_val, 
                z = pop_result$data$Z, 
                nj = pop_result$data$nj, 
                given = pop_result$parameters$m_given
            )
            
            # Compute the latent Y for (A=a0_val, M=m_mu)
            y_latent <- calc_y_latent(
                m = m_mu,
                a = a0_val,
                z = pop_result$data$Z,
                nj = pop_result$data$nj,
                given = pop_result$parameters$y_given
            )
            
            # Convert latent Y 
            E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
            
            # Individual-level expected outcome:
            truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
            
            # Cluster-level expected outcome:
            cluster_means <- pop_result$data  |>
                mutate(E_y = E_y_each) |>
                group_by(school) |>
                summarize(cluster_avg = mean(E_y, na.rm = TRUE))
            truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
            
        } else {
            # ══════════════════════════════
            # If Continuous M, Binary/Continuous Y or 
            # Continuous M, Continuous Y
            # ══════════════════════════════
            E_y_each <- compute_expected_y(a0_val, a1_val, data)
            
            # Individual-level average
            truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
            
            # Cluster-level average
            cluster_means <- data  |>
                mutate(E_y = E_y_each) |>
                group_by(school) |>
                summarize(cluster_avg = mean(E_y, na.rm = TRUE))
            truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
        }
    }
    
    # -------------------------------------------------------------------------
    # 5. RETURN THE TRUE VALUES
    # -------------------------------------------------------------------------
    return(
        list(
            truevals_individual = truevals_individual,
            truevals_cluster    = truevals_cluster
        )
    )
}










#' 
#' 
#' #' NOTE: THIS IS MODIFIED TO WORK ON ORIGINAL trueVals2.0 function 
#' #' @title trueVals2.0c
#' #' 
#' #' @description Computes the true potential outcomes under interventions on treatment (A) and mediator (M) 
#' #' for the given data configuration. It then returns these true values at both the individual and cluster levels.
#' #' 
#' #' @param data_list A list containing the generated dataset and all parameters used in generation. 
#' #'  This should be the output from the data generation functions (`trueVals2.0b()`).
#' #' 
#' #' @return A list with two elements:
#' #' \describe{
#' #'   \item{truevals_individual}{A named list of true expected outcomes at the individual level for each scenario (Y(a0, gm(a1))).}
#' #'   \item{truevals_cluster}{A named list of true expected outcomes at the cluster level for each scenario (Y(a0, gm(a1))).}
#' #' }
#' #' 
#' #' @details This function calculates the expected outcomes under four potential outcome scenarios:
#' #' - \(Y(a0=0, gm(a1=0))\): Set A=0 and mediator at the distribution induced by A=0.
#' #' - \(Y(a0=0, gm(a1=1))\): Set A=0 and mediator at the distribution induced by A=1.
#' #' - \(Y(a0=1, gm(a1=0))\): Set A=1 and mediator at the distribution induced by A=0.
#' #' - \(Y(a0=1, gm(a1=1))\): Set A=1 and mediator at the distribution induced by A=1.
#' #' 
#' #' For binary mediators, this involves summation over M=0,1. 
#' #' For continuous mediators, it involves integration over the mediator distribution. 
#' #' For binary outcomes, probabilities are computed using a probit link. 
#' #' For continuous outcomes, the linear predictor directly gives expected values.
#' #' 
#' #' @import dplyr
#' #' @importFrom stats dnorm pnorm
#' #' @importFrom utils combn
#' #' 
#' #' @examples
#' #' # This is typically not called directly by the user. It is called inside `trueVals2.0b()`:
#' #' # result <- trueVals2.0b(J = 50, Mfamily = "binomial", Yfamily = "binomial")
#' #' # result$truevals
#' #' 
#' #' @seealso \code{\link{trueVals2.0b}}
#' #' @export
#' trueVals2.0c <- function(data_list) {
#'     library(dplyr)
#'     library(glue)
#'     
#'     # Extract necessary components
#'     data <- data_list$data
#'     nj_sizes <- data_list$nj_sizes
#'     Mfamily <- data_list$Mfamily
#'     Yfamily <- data_list$Yfamily
#'     iccm <- data_list$iccm
#'     iccy <- data_list$iccy
#'     
#'     # Extract mediator parameters
#'     gen_m <- list(
#'         iccm = iccm,
#'         m_on_a = data_list$m_on_a,
#'         m_on_x = data_list$m_on_x,
#'         m_on_z = data_list$m_on_z,
#'         m_on_az = data_list$m_on_az,
#'         m_on_anj = data_list$m_on_anj
#'     )
#'     
#'     # Extract outcome parameters
#'     gen_y <- list(
#'         iccy = iccy,
#'         yintercept = data_list$yintercept,
#'         y_on_a = data_list$y_on_a,
#'         y_on_m = data_list$y_on_m,
#'         y_on_am = data_list$y_on_am,
#'         y_on_az = data_list$y_on_az,
#'         y_on_mz = data_list$y_on_mz,
#'         y_on_anj = data_list$y_on_anj,
#'         y_on_x = data_list$y_on_x,
#'         y_on_z = data_list$y_on_z
#'     )
#'     
#'     m_given <- data_list$m_given
#'     y_given <- data_list$y_given
#'     
#'     data <- data  |> 
#'         mutate(nj = nj_sizes[school])
#'     
#'     # Helper functions:
#'     ## Calculate M latent mean
#'     calc_m_latent <- function(a, z, nj, given) {
#'         # Latent M: includes main effects and interactions handled in data generation:
#'         # M distribution:
#'         # For binary M: p(M=1)=pnorm(latent)
#'         # For gaussian M: M ~ Normal(latent, 1 - iccm)
#'         latent_m <- gen_m$m_on_a * a +
#'             gen_m$m_on_az * a * z +
#'             gen_m$m_on_anj * a * nj +
#'             given
#'         return(latent_m)
#'     }
#'     
#'     ## Calculate Y latent
#'     calc_y_latent <- function(m, a, z, nj, given) {
#'         latent_y <- gen_y$y_on_m * m +
#'             gen_y$y_on_a * a +
#'             gen_y$y_on_am * a * m +
#'             gen_y$y_on_az * a * z +
#'             gen_y$y_on_mz * m * z +
#'             gen_y$y_on_anj * a * nj +
#'             given
#'         return(latent_y)
#'     }
#'     
#'     # Compute E[Y(a0, gm(a1))] for each observation
#'     compute_expected_y <- function(a0_val, a1_val, data) {
#'         z <- data$Z
#'         nj <- data$nj
#'         given_m <- m_given
#'         given_y <- y_given
#'         
#'         # Compute mediator parameters
#'         m_latent <- calc_m_latent(a = a1_val, z = z, nj = nj, given = given_m)
#'         
#'         if (Mfamily == "binomial") {
#'             # M in {0,1}
#'             p_m1 <- pnorm(m_latent, mean = 0, sd = sqrt(1 - iccm))
#'             p_m0 <- 1 - p_m1
#'             
#'             # Compute E[Y|M=0,A=a0_val] and E[Y|M=1,A=a0_val]
#'             # For Y:
#'             # If Y is binary: E[Y|...] = pnorm(y_latent,0,sqrt(1-iccy))
#'             # If Y is continuous: E[Y|...] = y_latent
#'             y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, nj = nj, given = given_y)
#'             y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, nj = nj, given = given_y)
#'             
#'             if (Yfamily == "binomial") {
#'                 E_y_m0 <- pnorm(y_latent_m0, 0, sqrt(1 - iccy))
#'                 E_y_m1 <- pnorm(y_latent_m1, 0, sqrt(1 - iccy))
#'             } else {
#'                 # Continuous Y
#'                 E_y_m0 <- y_latent_m0
#'                 E_y_m1 <- y_latent_m1
#'             }
#'             
#'             E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
#'             
#'         } else {
#'             # M is continuous
#'             # M ~ Normal(m_latent, var=1 - iccm)
#'             var_m <- 1 - iccm
#'             mean_m <- m_latent
#'             
#'             if (Yfamily == "gaussian") {
#'                 # Continuous M, continuous Y:
#'                 # E[Y] = E[E[Y|M]] = E[y_latent(M)]
#'                 # If linear, E[Y] = y_latent(E[M]) since it's linear in M.
#'                 # Substitute M = mean_m directly:
#'                 y_latent_mean_m <- calc_y_latent(m = mean_m, a = a0_val, z = z, nj = nj, given = given_y)
#'                 E_y <- y_latent_mean_m
#'             } else {
#'                 E_y <- NULL
#'             }
#'         }
#'         
#'         return(E_y)
#'     }
#'     
#'     # We consider (a0,a1) in {0,1}x{0,1}
#'     a_vals <- expand.grid(a0 = c(0, 1), a1 = c(0, 1))
#'     truevals_individual <- list()
#'     truevals_cluster <- list()
#'     
#'     for (j in 1:nrow(a_vals)) {
#'         a0_val <- a_vals$a0[j]
#'         a1_val <- a_vals$a1[j]
#'         label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
#'         
#'         # E_y_each <- compute_expected_y(a0_val, a1_val, data)
#'         # 
#'         # truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#'         # 
#'         # cluster_means <- data  |>
#'         #     mutate(E_y = E_y_each) |>
#'         #     group_by(school) |>
#'         #     summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#'         # truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#'         
#'         if (Mfamily == "gaussian" & Yfamily == "binomial") {
#'             
#'             # Generate pop data (J = 10,000) 
#'             large_J <- 10000
#'             big_njrange <- c(data_list$njrange[1], data_list$njrange[2]) # c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()
#'             
#'             pop_result <- generate_data2.0c( # generate_data2.0(
#'                 include_truevals = FALSE, 
#'                 
#'                 J = large_J,
#'                 njrange = big_njrange,
#'                 Mfamily = "gaussian",
#'                 Yfamily = "binomial",
#'                 
#'                 if.null = FALSE, # CHANGE
#'                 seed = seed,
#'                 num_x = num_x, # 3,
#'                 # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
#'                 # a_z = sqrt(0.4 / 1),
#'                 x_z = x_z, #0,
#'                 m_on_a = data_list$m_on_a, #0.2,
#'                 m_on_az = data_list$m_on_az, #0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
#'                 m_on_anj = data_list$m_on_anj, #0.2,                  # Interaction effect of 'A' and cluster size on 'M'
#'                 # m_on_x = sqrt(0.15 / num_x),
#'                 # m_on_z = sqrt(0.4),
#'                 int.XZ = TRUE,       # DELETE
#'                 yintercept = data_list$yintercept, # 1,
#'                 y_on_a = data_list$y_on_a, # 0.5,
#'                 y_on_m = data_list$y_on_m, #1,
#'                 y_on_am = data_list$y_on_am, # 0,
#'                 y_on_az = data_list$y_on_az, # 0.2,
#'                 y_on_mz = data_list$y_on_mz, # 0.2,
#'                 y_on_anj = data_list$y_on_anj, # 0.2,
#'                 # y_on_x = sqrt(0.15 / num_x),
#'                 # y_on_z = sqrt(0.4),
#'                 quadratic.A = data_list$quadratic.A, # FALSE,
#'                 quadratic.M = data_list$quadratic.M, # FALSE,
#'                 quadratic.Y = data_list$quadratic.Y, # FALSE,
#'                 iccx = iccx, #0.2,
#'                 icca = data_list$icca, # 0.2,
#'                 iccm = data_list$iccm, # 0.2,
#'                 iccy = data_list$iccy # 0.2
#'             )
#'             
#'             # add njs to data 
#'             pop_result$data <- pop_result$data |> 
#'                 mutate(nj = nj_sizes[school])
#'             
#'             
#'             m_mu <- calc_m_latent(a = a1_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$m_given)
#'             
#'             y_latent <- calc_y_latent(m = m_mu, a = a0_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$y_given)
#'             
#'             E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
#'             
#'             
#'             # individual
#'             truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#'             
#'             # cluster 
#'             cluster_means <- pop_result$data  |>
#'                 mutate(E_y = E_y_each) |>
#'                 group_by(school) |>
#'                 summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#'             truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#'             
#'         } else {
#'             
#'             E_y_each <- compute_expected_y(a0_val, a1_val, data)
#'             
#'             truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#'             
#'             cluster_means <- data  |>
#'                 mutate(E_y = E_y_each) |>
#'                 group_by(school) |>
#'                 summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#'             truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#'         }
#'             
#'         # if (Mfamily != "gaussian" & Yfamily != "binomial") {
#'         #     # a0_val <- a_vals$a0[j]
#'         #     # a1_val <- a_vals$a1[j]
#'         #     # label <- glue("Y(a0={a0_val}, gm(a1={a1_val}))")
#'         #     
#'         #     E_y_each <- compute_expected_y(a0_val, a1_val, data)
#'         #     
#'         #     truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#'         #     
#'         #     cluster_means <- data  |>
#'         #         mutate(E_y = E_y_each) |>
#'         #         group_by(school) |>
#'         #         summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#'         #     truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#'         #     
#'         # } else if (Mfamily == "gaussian" & Yfamily == "binomial") {
#'         #     
#'         #     # Generate pop data (J = 10,000) 
#'         #     large_J <- 10000
#'         #     big_njrange <- c(data_list$parameters$njrange[1], data_list$parameters$njrange[2]) # njrange # c()
#'         #     
#'         #     pop_result <- generate_data2.0b( # generate_data2.0(
#'         #         J = large_J,
#'         #         njrange = big_njrange,
#'         #         Mfamily = "gaussian",
#'         #         Yfamily = "binomial",
#'         #         
#'         #         if.null = FALSE, # CHANGE
#'         #         seed = seed,
#'         #         num_x = 3,
#'         #         # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
#'         #         # a_z = sqrt(0.4 / 1),
#'         #         x_z = 0,
#'         #         m_on_a = 0.2,
#'         #         m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
#'         #         m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
#'         #         # m_on_x = sqrt(0.15 / num_x),
#'         #         # m_on_z = sqrt(0.4),
#'         #         int.XZ = TRUE,       # DELETE
#'         #         yintercept = 1,
#'         #         y_on_a = 0.5,
#'         #         y_on_m = 1,
#'         #         y_on_am = 0,
#'         #         y_on_az = 0.2,
#'         #         y_on_mz = 0.2,
#'         #         y_on_anj = 0.2,
#'         #         # y_on_x = sqrt(0.15 / num_x),
#'         #         # y_on_z = sqrt(0.4),
#'         #         quadratic.A = FALSE,
#'         #         quadratic.M = FALSE,
#'         #         quadratic.Y = FALSE,
#'         #         iccx = 0.2,
#'         #         icca = 0.2,
#'         #         iccm = 0.2,
#'         #         iccy = 0.2
#'         #     )
#'         #     
#'         #     # add njs to data 
#'         #     pop_result$data <- pop_result$data |> 
#'         #         mutate(nj = nj_sizes[school])
#'         #     
#'         #     
#'         #     m_mu <- calc_m_latent(a = a1_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$m_given)
#'         #     
#'         #     y_latent <- calc_y_latent(m = m_mu, a = a0_val, z = pop_result$data$Z, nj = pop_result$data$nj, given = pop_result$parameters$y_given)
#'         #     
#'         #     E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
#'         #     
#'         #     
#'         #     # individual
#'         #     truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
#'         #     
#'         #     # cluster 
#'         #     cluster_means <- pop_result$data  |>
#'         #         mutate(E_y = E_y_each) |>
#'         #         group_by(school) |>
#'         #         summarize(cluster_avg = mean(E_y, na.rm = TRUE))
#'         #     truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
#'         # }
#'         
#'     }
#'     
#'     return(list(
#'         # truevals_individual = NULL, 
#'         # truevals_cluster = NULL
#'         truevals_individual = truevals_individual,
#'         truevals_cluster = truevals_cluster
#'     ))
#' }
