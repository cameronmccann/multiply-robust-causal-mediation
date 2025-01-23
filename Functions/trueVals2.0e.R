#' @title trueVals2.0e
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
#' result <- trueVals2.0e(data_list)
#' result$truevals_individual
#' }
#'
#' @seealso 
#' \code{\link{trueVals2.0b}}, 
#' \code{\link{generate_data2.0c}}
#'
#' @export
trueVals2.0e <- function(data_list, 
                         from_GenData_2.0 = FALSE # added argument to easily change GenData_2.0() data to this func
) {
    
    # library(dplyr)
    # library(glue)
    
    
    # 1. Extract components from data_list ------------------------------------
    if (from_GenData_2.0 == TRUE) {
        data <- data_list$datobs
        # Change X headers 
        colnames(data) <- gsub(pattern = "^X\\.", replacement = "X", colnames(data))
        
        nj_sizes <- data_list$nj_sizes
        Mfamily <- "binomial"    # <- ADJUST LATER
        Yfamily <- data_list$Yfamily
        iccm <- data_list$iccm
        iccy <- data_list$iccy
        seed <- data_list$seedone
        iccx <- data_list$iccx
        x_z <- data_list$x_z
        num_x <- data_list$num_x
        
        # Extract mediator parameters
        gen_m <- list(
            iccm = iccm,
            m_on_a = data_list$gen_m$m1_on_a,
            m_on_x = data_list$gen_m$m1_on_x,
            m_on_z = data_list$gen_m$m1_on_z,
            m_on_az = data_list$gen_m$m1_on_az,
            m_on_anj = data_list$gen_m$m1_on_anj
        )
        
        # Extract outcome parameters
        gen_y <- list(
            iccy = iccy,
            yintercept = data_list$gen_y$yintercept,
            y_on_a = data_list$gen_y$y_on_a,
            y_on_m = data_list$gen_y$y_on_m1,
            y_on_am = data_list$gen_y$y_on_am1,
            y_on_az = data_list$gen_y$y_on_az,
            y_on_mz = data_list$gen_y$y_on_m1z,
            y_on_anj = data_list$gen_y$y_on_anj,
            y_on_x = data_list$gen_y$y_on_x,
            y_on_z = data_list$gen_y$y_on_z
        )
        
        m_given <- data_list$m1_given
        y_given <- data_list$y_given
        
        # # Attach cluster sizes to the data 
        # data <- data |> dplyr::mutate(nj = nj_sizes[school])
        # Use scaled version of cluster size (W_nj) instead of raw 
        W_nj <- data_list$data$W_nj
        
    }
    if (from_GenData_2.0 == FALSE) {
        
        data <- data_list$data
        nj_sizes <- data_list$nj_sizes
        Mfamily <- data_list$Mfamily
        Yfamily <- data_list$Yfamily
        iccm <- data_list$iccm
        iccy <- data_list$iccy
        seed <- data_list$seed
        iccx <- data_list$iccx
        x_z <- data_list$x_z
        num_x <- data_list$num_x
        
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
        
        # # Attach cluster sizes to the data 
        # data <- data |> dplyr::mutate(nj = nj_sizes[school])
        # Use scaled version of cluster size (W_nj) instead of raw 
        W_nj <- data_list$data$W_nj
    }
    
    
    # 2. Helper functions -----------------------------------------------------
    
    # Calculate latent mean for M
    calc_m_latent <- function(a, z, W_nj, given) {
        latent_m <- gen_m$m_on_a * a +
            gen_m$m_on_az * a * z +
            gen_m$m_on_anj * a * W_nj +
            given
        return(latent_m)
    }
    
    # Calculate latent outcome Y
    calc_y_latent <- function(m, a, z, W_nj, given) {
        latent_y <- gen_y$y_on_m * m +
            gen_y$y_on_a * a +
            gen_y$y_on_am * a * m +
            gen_y$y_on_az * a * z +
            gen_y$y_on_mz * m * z +
            gen_y$y_on_anj * a * W_nj +
            given
        return(latent_y)
    }
    
    
    # 3. Function to compute E[Y(a0, gm(a1))] for each observation ------------
    compute_expected_y <- function(a0_val, a1_val, data) {
        
        ##--- NEW CODE for cluster-level RE ----------------------------------##
        # We add exactly one random effect for M per cluster, and one for Y.
        # Because we want *expected* Y, we won't physically rbinom or rnorm 
        # each subject. Instead, we incorporate the cluster RE into the 
        # latent predictor the same way you do i.i.d. errors, but at cluster level.
        cl <- data$school
        n_clusters <- length(unique(cl))
        # random effect for M:
        mb_vec   <- rnorm(n_clusters, mean=0, sd=sqrt(iccm))
        mb_subj  <- mb_vec[cl]  # each subject i in cluster j gets the same offset
        # random effect for Y:
        yb_vec   <- rnorm(n_clusters, mean=0, sd=sqrt(iccy))
        yb_subj  <- yb_vec[cl]
        ##----------------------------------------------------------------------##
        
        
        z <- data$Z
        W_nj <- data$W_nj
        given_m <- m_given
        given_y <- y_given
        
        # Compute mediator parameters
        m_latent <- calc_m_latent(a = a1_val, z = z, W_nj = W_nj, given = given_m)
        # add error term to m_latent for mediator 
        
        if (Mfamily == "binomial") {
            # ══════════════════════════════
            # Binary mediator:
            # p(M=1) = pnorm(m_latent)
            # p(M=0) = 1 - p(M=1)
            # E[Y] = p(M=0)*E[Y|M=0] + p(M=1)*E[Y|M=1]
            # ══════════════════════════════
            p_m1 <- pnorm(m_latent + mb_subj, mean = 0, sd = sqrt(1 - iccm))
            p_m0 <- 1 - p_m1
            
            # For each M = 0/1, compute latent Y
            y_latent_m0 <- calc_y_latent(m = 0, a = a0_val, z = z, W_nj = W_nj, given = given_y)
            y_latent_m1 <- calc_y_latent(m = 1, a = a0_val, z = z, W_nj = W_nj, given = given_y)
            
            # Then get E[Y|M=0], E[Y|M=1] depending on outcome family
            if (Yfamily == "binomial") {
                E_y_m0 <- pnorm(y_latent_m0 + yb_subj, 0, sqrt(1 - iccy))
                E_y_m1 <- pnorm(y_latent_m1 + yb_subj, 0, sqrt(1 - iccy))
            } else {
                # If Y is gaussian
                E_y_m0 <- (y_latent_m0 + yb_subj)
                E_y_m1 <- (y_latent_m1 + yb_subj)
            }
            
            E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
            
        } else {
            # ══════════════════════════════
            # Continuous mediator:
            # M ~ Normal(m_latent, var=1 - iccm)
            # We must integrate or do a linear approximation
            # ══════════════════════════════
            # var_m <- 1 - iccm # delete if var_m is not used anywhere 
            # Ma <- m_latent + rnorm(length(m_latent), sd = sqrt(1 - iccm)) # added error term 
            Ma <- (m_latent + mb_subj) + rnorm(nrow(data), sd=sqrt(1 - iccm))
            
            if (Yfamily == "gaussian") {
                # Continuous M, continuous Y => linear 
                # E[Y] = E[E[Y|M]] = E[y_latent(M)]
                y_latent_mean_m <- calc_y_latent(m = Ma, a = a0_val, z = z, W_nj = W_nj, given = given_y)
                E_y <- (y_latent_mean_m + yb_subj) + rnorm(nrow(data), sd=sqrt(1 - iccy)) #y_latent_mean_m #
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
    truevals_cluster <- list()
    
    for (j in 1:nrow(a_vals)) {
        a0_val <- a_vals$a0[j]
        a1_val <- a_vals$a1[j]
        label <- glue::glue("Y(a0={a0_val}, gm(a1={a1_val}))")
        
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
                include_overlapMsg = FALSE, 
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
                iccx = data_list$iccx, 
                icca = data_list$icca, 
                iccm = data_list$iccm, 
                iccy = data_list$iccy 
            )
            
            # # Add the cluster sizes to the newly generated dataset
            # pop_result$data <- pop_result$data |> 
            #     dplyr::mutate(nj = nj_sizes[school]) # I believe this is unnecessary since we are usin W_nj
            
            # Compute the mediator latent mean for M under (A=a1_val)
            m_mu <- calc_m_latent(
                a = a1_val, 
                z = pop_result$data$Z, 
                W_nj = pop_result$data$W_nj, 
                given = pop_result$parameters$m_given
            )
            
            # Compute the latent Y for (A=a0_val, M=m_mu)
            y_latent <- calc_y_latent(
                m = m_mu,
                a = a0_val,
                z = pop_result$data$Z,
                W_nj = pop_result$data$W_nj,
                given = pop_result$parameters$y_given
            )
            
            # Convert latent Y 
            E_y_each <- pnorm(y_latent, mean = 0, sd = sqrt(1 - iccy))
            
            # Individual-level expected outcome:
            truevals_individual[[label]] <- mean(E_y_each, na.rm = TRUE)
            
            # Cluster-level expected outcome:
            cluster_means <- pop_result$data  |>
                dplyr::mutate(E_y = E_y_each) |>
                dplyr::group_by(school) |>
                dplyr::summarize(cluster_avg = mean(E_y, na.rm = TRUE))
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
                dplyr::mutate(E_y = E_y_each) |>
                dplyr::group_by(school) |>
                dplyr::summarize(cluster_avg = mean(E_y, na.rm = TRUE))
            truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm = TRUE)
        }
    }
    
    # -------------------------------------------------------------------------
    # 5. RETURN THE TRUE VALUES
    # -------------------------------------------------------------------------
    # return(
    #     list(
    #         truevals_individual = truevals_individual,
    #         truevals_cluster    = truevals_cluster
    #     )
    # )
    return(
        list(
            truevals_individual = truevals_individual,
            truevals_cluster    = truevals_cluster,
            pop_data = if (exists("pop_result") && !is.null(pop_result$data)) pop_result$data else NULL
        )
    )
}

#' 
#' #' @title trueVals2.0e
#' #'
#' #' @description
#' #' Computes the true potential outcomes under interventions on treatment (\code{A}) 
#' #' and mediator (\code{M}) for the given data configuration. It returns these true 
#' #' values at both the individual and cluster levels, enabling the calculation of 
#' #' natural direct/indirect effects in a clustered setting.
#' #'
#' #' @param data_list A list containing the generated dataset (\code{data}), cluster sizes, 
#' #'                  and all generation parameters. This should be the output from one 
#' #'                  of the data generation functions (e.g., \code{generate_data2.0c()}).
#' #'
#' #' @return A list with two elements:
#' #' \describe{
#' #'   \item{\code{truevals_individual}}{A named list of true expected outcomes at the individual level 
#' #'                                     for each scenario \code{Y(a0, gm(a1))}.}
#' #'   \item{\code{truevals_cluster}}{A named list of true expected outcomes at the cluster level for 
#' #'                                  each scenario \code{Y(a0, gm(a1))}.}
#' #' }
#' #'
#' #' @details
#' #' This function calculates the expected outcomes under the four potential-outcome scenarios:
#' #' \itemize{
#' #'   \item \(\text{Y}(a0=0, \text{gm}(a1=0))\): Set \code{A = 0} and \code{M} to the distribution 
#' #'         it would have under \code{A = 0}.
#' #'   \item \(\text{Y}(a0=0, \text{gm}(a1=1))\): Set \code{A = 0} and \code{M} to the distribution 
#' #'         it would have under \code{A = 1}.
#' #'   \item \(\text{Y}(a0=1, \text{gm}(a1=0))\): Set \code{A = 1} and \code{M} to the distribution 
#' #'         it would have under \code{A = 0}.
#' #'   \item \(\text{Y}(a0=1, \text{gm}(a1=1))\): Set \code{A = 1} and \code{M} to the distribution 
#' #'         it would have under \code{A = 1}.
#' #' }
#' #'
#' #' For **binary mediators**, the function sums over \code{M = 0, 1} with respective probabilities.  
#' #' For **continuous mediators**, it still uses a normal approximation but crucially adds one 
#' #' cluster random effect per cluster.  
#' #' For **binary outcomes**, \code{Y} is derived from a probit link, i.e. \code{pnorm(latentY + clusterRE)}.  
#' #' For **continuous outcomes**, the linear predictor \code{latentY} plus cluster RE is used directly.
#' #'
#' #' @import dplyr
#' #' @importFrom stats dnorm pnorm rnorm
#' #' @importFrom utils combn
#' #'
#' #' @export
#' trueVals2.0e <- function(data_list, 
#'                          from_GenData_2.0 = FALSE # additional argument if needed
#' ) {
#'     # library(dplyr)
#'     # library(glue)
#'     
#'     # 1. Extract components from data_list ------------------------------------
#'     if (from_GenData_2.0 == TRUE) {
#'         data <- data_list$datobs
#'         colnames(data) <- gsub(pattern = "^X\\.", replacement = "X", colnames(data))
#'         
#'         nj_sizes <- data_list$nj_sizes
#'         Mfamily  <- "binomial"  # ADJUST if needed
#'         Yfamily  <- data_list$Yfamily
#'         iccm     <- data_list$iccm
#'         iccy     <- data_list$iccy
#'         seed     <- data_list$seedone
#'         iccx     <- data_list$iccx
#'         x_z      <- data_list$x_z
#'         num_x    <- data_list$num_x
#'         
#'         # Mediator params
#'         gen_m <- list(
#'             iccm    = iccm,
#'             m_on_a  = data_list$gen_m$m1_on_a,
#'             m_on_x  = data_list$gen_m$m1_on_x,
#'             m_on_z  = data_list$gen_m$m1_on_z,
#'             m_on_az = data_list$gen_m$m1_on_az,
#'             m_on_anj= data_list$gen_m$m1_on_anj
#'         )
#'         # Outcome params
#'         gen_y <- list(
#'             iccy = iccy,
#'             yintercept = data_list$gen_y$yintercept,
#'             y_on_a  = data_list$gen_y$y_on_a,
#'             y_on_m  = data_list$gen_y$y_on_m1,
#'             y_on_am = data_list$gen_y$y_on_am1,
#'             y_on_az = data_list$gen_y$y_on_az,
#'             y_on_mz = data_list$gen_y$y_on_m1z,
#'             y_on_anj= data_list$gen_y$y_on_anj,
#'             y_on_x  = data_list$gen_y$y_on_x,
#'             y_on_z  = data_list$gen_y$y_on_z
#'         )
#'         m_given <- data_list$m1_given
#'         y_given <- data_list$y_given
#'         W_nj    <- data_list$data$W_nj
#'         
#'     } else {
#'         
#'         data <- data_list$data
#'         nj_sizes <- data_list$nj_sizes
#'         Mfamily <- data_list$Mfamily
#'         Yfamily <- data_list$Yfamily
#'         iccm <- data_list$iccm
#'         iccy <- data_list$iccy
#'         seed <- data_list$seed
#'         iccx <- data_list$iccx
#'         x_z <- data_list$x_z
#'         num_x <- data_list$num_x
#'         
#'         # Extract mediator parameters
#'         gen_m <- list(
#'             iccm = iccm,
#'             m_on_a = data_list$m_on_a,
#'             m_on_x = data_list$m_on_x,
#'             m_on_z = data_list$m_on_z,
#'             m_on_az = data_list$m_on_az,
#'             m_on_anj = data_list$m_on_anj
#'         )
#'         
#'         # Extract outcome parameters
#'         gen_y <- list(
#'             iccy = iccy,
#'             yintercept = data_list$yintercept,
#'             y_on_a = data_list$y_on_a,
#'             y_on_m = data_list$y_on_m,
#'             y_on_am = data_list$y_on_am,
#'             y_on_az = data_list$y_on_az,
#'             y_on_mz = data_list$y_on_mz,
#'             y_on_anj = data_list$y_on_anj,
#'             y_on_x = data_list$y_on_x,
#'             y_on_z = data_list$y_on_z
#'         )
#'         
#'         m_given <- data_list$m_given
#'         y_given <- data_list$y_given
#'         
#'         # # Attach cluster sizes to the data 
#'         # data <- data |> dplyr::mutate(nj = nj_sizes[school])
#'         # Use scaled version of cluster size (W_nj) instead of raw 
#'         W_nj <- data_list$data$W_nj
#'     }
#'     
#'     # 2. Helper functions -----------------------------------------------------
#'     
#'     calc_m_latent <- function(a, z, W_nj, given) {
#'         latent_m <- gen_m$m_on_a * a +
#'             gen_m$m_on_az * a * z +
#'             gen_m$m_on_anj * a * W_nj +
#'             given
#'         return(latent_m)
#'     }
#'     
#'     calc_y_latent <- function(m, a, z, W_nj, given) {
#'         latent_y <- gen_y$y_on_m * m +
#'             gen_y$y_on_a * a +
#'             gen_y$y_on_am * a * m +
#'             gen_y$y_on_az * a * z +
#'             gen_y$y_on_mz * m * z +
#'             gen_y$y_on_anj * a * W_nj +
#'             given
#'         return(latent_y)
#'     }
#'     
#'     # 3. Function to compute E[Y(a0, gm(a1))] for each observation ------------
#'     compute_expected_y <- function(a0_val, a1_val, data) {
#'         
#'         ##--- NEW CODE for cluster-level RE ----------------------------------##
#'         # We add exactly one random effect for M per cluster, and one for Y.
#'         # Because we want *expected* Y, we won't physically rbinom or rnorm 
#'         # each subject. Instead, we incorporate the cluster RE into the 
#'         # latent predictor the same way you do i.i.d. errors, but at cluster level.
#'         cl       <- data$school
#'         n_clusters <- length(unique(cl))
#'         # random effect for M:
#'         mb_vec   <- rnorm(n_clusters, mean=0, sd=sqrt(iccm))
#'         mb_subj  <- mb_vec[cl]  # each subject i in cluster j gets the same offset
#'         # random effect for Y:
#'         yb_vec   <- rnorm(n_clusters, mean=0, sd=sqrt(iccy))
#'         yb_subj  <- yb_vec[cl]
#'         ##----------------------------------------------------------------------##
#'         
#'         z       <- data$Z
#'         W_nj    <- data$W_nj
#'         given_m <- m_given
#'         given_y <- y_given
#'         
#'         # Mediator linear predictor *without* cluster RE:
#'         m_latent <- calc_m_latent(a = a1_val, z = z, W_nj = W_nj, given = given_m)
#'         
#'         # If mediator is binomial:
#'         if (Mfamily == "binomial") {
#'             
#'             # Probability M=1 is pnorm( m_latent + clusterRE_for_M, sd=sqrt(1-iccm) )
#'             p_m1 <- pnorm(m_latent + mb_subj, mean = 0, sd = sqrt(1 - iccm))
#'             p_m0 <- 1 - p_m1
#'             
#'             # Then for each M=0 or 1, we get outcome latent:
#'             y_latent_m0 <- calc_y_latent(m=0, a=a0_val, z=z, W_nj=W_nj, given=given_y)
#'             y_latent_m1 <- calc_y_latent(m=1, a=a0_val, z=z, W_nj=W_nj, given=given_y)
#'             
#'             if (Yfamily == "binomial") {
#'                 # E[Y|M=0] = pnorm( y_latent_m0 + clusterRE_for_Y, sd=sqrt(1-iccy) )
#'                 E_y_m0 <- pnorm(y_latent_m0 + yb_subj, 0, sqrt(1 - iccy))
#'                 E_y_m1 <- pnorm(y_latent_m1 + yb_subj, 0, sqrt(1 - iccy))
#'             } else {
#'                 # continuous Y => add cluster RE
#'                 E_y_m0 <- (y_latent_m0 + yb_subj)  # no extra sd if we only want the mean
#'                 E_y_m1 <- (y_latent_m1 + yb_subj)
#'             }
#'             
#'             # Weighted by p(M=0) / p(M=1)
#'             E_y <- p_m0 * E_y_m0 + p_m1 * E_y_m1
#'             
#'         } else {
#'             # Mfamily == "gaussian"
#'             # M_i = (m_latent + clusterRE_for_M) + subject-level noise ...
#'             # But for the *expected value*, we incorporate the cluster shift but 
#'             # keep the average over subject-level noise the same as your i.i.d. code.
#'             
#'             # So effectively: 
#'             #   M_i ~ Normal(m_latent + mb_subj, 1 - iccm)
#'             # We can simulate one draw OR just note the mean is (m_latent + mb_subj).
#'             # minimal approach: replicate your existing "Ma" but add cluster RE:
#'             Ma <- (m_latent + mb_subj) + rnorm(nrow(data), sd=sqrt(1 - iccm))
#'             
#'             # Then plug into Y
#'             y_latent <- calc_y_latent(m = Ma, a = a0_val, z = z, W_nj = W_nj, given = given_y)
#'             
#'             if (Yfamily == "gaussian") {
#'                 # Y_i = y_latent + yb_subj + subject noise
#'                 # The expected Y_i = y_latent + yb_subj (since the mean of subject noise is 0)
#'                 E_y <- (y_latent + yb_subj)
#'             } else {
#'                 # Yfamily == "binomial"
#'                 # Probability = pnorm( y_latent + clusterRE_for_Y, 0, sqrt(1 - iccy) )
#'                 E_y <- pnorm(y_latent + yb_subj, 0, sqrt(1 - iccy))
#'             }
#'         }
#'         
#'         return(E_y)
#'     }
#'     
#'     # -------------------------------------------------------------------------
#'     # 4. LOOP OVER (a0, a1) = (0,0), (0,1), (1,0), (1,1) & COMPUTE TRUE EXPECTATIONS
#'     # -------------------------------------------------------------------------
#'     a_vals <- expand.grid(a0 = c(0,1), a1 = c(0,1))
#'     truevals_individual <- list()
#'     truevals_cluster    <- list()
#'     
#'     for (j in 1:nrow(a_vals)) {
#'         a0_val <- a_vals$a0[j]
#'         a1_val <- a_vals$a1[j]
#'         label  <- glue::glue("Y(a0={a0_val}, gm(a1={a1_val}))")
#'         
#'         # Compute the "E[Y(a0, gm(a1))]" for each subject:
#'         E_y_each <- compute_expected_y(a0_val, a1_val, data)
#'         
#'         # (a) Individual-level average
#'         truevals_individual[[label]] <- mean(E_y_each, na.rm=TRUE)
#'         
#'         # (b) Cluster-level average
#'         cluster_means <- data %>%
#'             dplyr::mutate(E_y = E_y_each) %>%
#'             dplyr::group_by(school) %>%
#'             dplyr::summarize(cluster_avg = mean(E_y, na.rm=TRUE))
#'         truevals_cluster[[label]] <- mean(cluster_means$cluster_avg, na.rm=TRUE)
#'     }
#'     
#'     # -------------------------------------------------------------------------
#'     # 5. Return the True Values
#'     # -------------------------------------------------------------------------
#'     return(list(
#'         truevals_individual = truevals_individual,
#'         truevals_cluster    = truevals_cluster
#'     ))
#' }


##################################### END #####################################

#' 
#' 
#' #' @title trueVals2.0e
#' #'
#' #' @description
#' #' Computes the true potential outcomes under interventions on treatment (\code{A}) 
#' #' and mediator (\code{M}) for the given data configuration. It returns these true 
#' #' values at both the individual and cluster levels, enabling the calculation of 
#' #' natural direct/indirect effects in a clustered setting.
#' #'
#' #' @param data_list A list containing the generated dataset (\code{data}), cluster sizes, 
#' #'                  and all generation parameters. This should be the output from one 
#' #'                  of the data generation functions (e.g., \code{generate_data2.0c()}).
#' #'
#' #' @return A list with two elements:
#' #' \describe{
#' #'   \item{\code{truevals_individual}}{A named list of true expected outcomes at the individual level 
#' #'                                     for each scenario \code{Y(a0, gm(a1))}.}
#' #'   \item{\code{truevals_cluster}}{A named list of true expected outcomes at the cluster level for 
#' #'                                  each scenario \code{Y(a0, gm(a1))}.}
#' #' }
#' #'
#' #' @details
#' #' This function calculates the expected outcomes under the four potential-outcome scenarios:
#' #' \itemize{
#' #'   \item \(\text{Y}(a0=0, \text{gm}(a1=0))\): Set \code{A = 0} and \code{M} to the distribution 
#' #'         it would have under \code{A = 0}.
#' #'   \item \(\text{Y}(a0=0, \text{gm}(a1=1))\): Set \code{A = 0} and \code{M} to the distribution 
#' #'         it would have under \code{A = 1}.
#' #'   \item \(\text{Y}(a0=1, \text{gm}(a1=0))\): Set \code{A = 1} and \code{M} to the distribution 
#' #'         it would have under \code{A = 0}.
#' #'   \item \(\text{Y}(a0=1, \text{gm}(a1=1))\): Set \code{A = 1} and \code{M} to the distribution 
#' #'         it would have under \code{A = 1}.
#' #' }
#' #'
#' #' For **binary mediators**, the mediator is generated via a probit link 
#' #' (i.e., \code{M=1} with probability \code{pnorm(...)}).  
#' #' For **continuous mediators**, \code{M} is the latent predictor plus random noise.  
#' #' For **binary outcomes**, \code{Y} is also derived from a probit link.  
#' #' For **continuous outcomes**, the linear predictor plus noise is used directly.
#' #'
#' #' The key difference from \code{trueVals2.0e()} is that this function uses a 
#' #' **large Monte Carlo approach** for *all* mediator/outcome family combinations, ensuring 
#' #' proper integration over the cluster-level random effects.
#' #'
#' #' @import dplyr
#' #' @importFrom stats pnorm rnorm
#' #' @importFrom utils combn
#' #'
#' #' @export
#' trueVals2.0e <- function(data_list, 
#'                          J_big = 10000,     # large number of clusters for MC
#'                          from_GenData_2.0 = FALSE) {
#'     
#'     # 1. Extract relevant parameters from data_list -----------------------------
#'     if (from_GenData_2.0 == TRUE) {
#'         # If it came from some older function with slightly different naming:
#'         data <- data_list$datobs
#'         colnames(data) <- gsub(pattern = "^X\\.", replacement = "X", colnames(data))
#'         nj_sizes <- data_list$nj_sizes
#'         Mfamily  <- "binomial"  # might override if needed
#'         Yfamily  <- data_list$Yfamily
#'         iccm     <- data_list$iccm
#'         iccy     <- data_list$iccy
#'         seed     <- data_list$seedone
#'         iccx     <- data_list$iccx
#'         x_z      <- data_list$x_z
#'         num_x    <- data_list$num_x
#'         
#'         gen_m <- list(iccm = iccm,
#'                       m_on_a  = data_list$gen_m$m1_on_a,
#'                       m_on_x  = data_list$gen_m$m1_on_x,
#'                       m_on_z  = data_list$gen_m$m1_on_z,
#'                       m_on_az = data_list$gen_m$m1_on_az,
#'                       m_on_anj= data_list$gen_m$m1_on_anj)
#'         
#'         gen_y <- list(iccy = iccy,
#'                       yintercept = data_list$gen_y$yintercept,
#'                       y_on_a  = data_list$gen_y$y_on_a,
#'                       y_on_m  = data_list$gen_y$y_on_m1,
#'                       y_on_am = data_list$gen_y$y_on_am1,
#'                       y_on_az = data_list$gen_y$y_on_az,
#'                       y_on_mz = data_list$gen_y$y_on_m1z,
#'                       y_on_anj= data_list$gen_y$y_on_anj,
#'                       y_on_x  = data_list$gen_y$y_on_x,
#'                       y_on_z  = data_list$gen_y$y_on_z)
#'         
#'         # Not strictly used below, but we keep them:
#'         m_given <- data_list$m1_given
#'         y_given <- data_list$y_given
#'         
#'     } else {
#'         # Standard data_list from generate_data2.0c()
#'         # data      <- data_list$data
#'         # nj_sizes  <- data_list$nj_sizes
#'         # Mfamily   <- data_list$Mfamily
#'         # Yfamily   <- data_list$Yfamily
#'         # iccm      <- data_list$iccm
#'         # iccy      <- data_list$iccy
#'         # seed      <- data_list$seed
#'         # iccx      <- data_list$iccx
#'         # x_z       <- data_list$x_z
#'         # num_x     <- data_list$num_x
#'         data <- data_list$data
#'         nj_sizes <- data_list$parameters$nj_sizes
#'         Mfamily <- data_list$parameters$Mfamily
#'         Yfamily <- data_list$parameters$Yfamily
#'         iccm <- data_list$parameters$iccm
#'         iccy <- data_list$parameters$iccy
#'         seed <- data_list$parameters$seed
#'         iccx <- data_list$parameters$iccx
#'         x_z <- data_list$parameters$x_z
#'         num_x <- data_list$parameters$num_x
#'         
#'         # Extract mediator parameters
#'         gen_m <- list(
#'             iccm = data_list$parameters$iccm,
#'             m_on_a = data_list$parameters$m_on_a,
#'             m_on_x = data_list$parameters$m_on_x,
#'             m_on_z = data_list$parameters$m_on_z,
#'             m_on_az = data_list$parameters$m_on_az,
#'             m_on_anj = data_list$parameters$m_on_anj
#'         )
#'         
#'         # Extract outcome parameters
#'         gen_y <- list(
#'             iccy = data_list$parameters$iccy,
#'             yintercept = data_list$parameters$yintercept,
#'             y_on_a = data_list$parameters$y_on_a,
#'             y_on_m = data_list$parameters$y_on_m,
#'             y_on_am = data_list$parameters$y_on_am,
#'             y_on_az = data_list$parameters$y_on_az,
#'             y_on_mz = data_list$parameters$y_on_mz,
#'             y_on_anj = data_list$parameters$y_on_anj,
#'             y_on_x = data_list$parameters$y_on_x,
#'             y_on_z = data_list$parameters$y_on_z
#'         )
#'         
#'         # For reference, not essential if we do a large MC:
#'         m_given <- data_list$parameters$m_given
#'         y_given <- data_list$parameters$y_given
#'     }
#'     
#'     # Keep the random seed for reproducibility:
#'     if (!is.null(seed)) set.seed(seed)
#'     
#'     # 2. Define a helper to generate the large MC population & compute E[Y] -----
#'     #    This function returns a numeric value: the mean of Y(a0, M(a1)).
#'     
#'     large_mc_estimate <- function(a0_val, a1_val) {
#'         # (a) Generate large J_big cluster sizes:
#'         min_nj  <- min(nj_sizes)
#'         max_nj  <- max(nj_sizes)
#'         big_nj  <- sample(min_nj:max_nj, size=J_big, replace=TRUE)
#'         big_N   <- sum(big_nj)
#'         
#'         # (b) Make a data.frame with a row per individual
#'         big_data <- data.frame(
#'             cluster_j = unlist(mapply(rep, 1:J_big, big_nj)),
#'             id_within = unlist(lapply(big_nj, seq_len))
#'         )
#'         
#'         # We'll define some simplistic confounders (X) and cluster-level Z,
#'         # approximating your 'generate_data2.0c()' logic. 
#'         # If you want them correlated or want X ICC, replicate exactly your approach.
#'         
#'         # cluster-level Z ~ N(0,1)
#'         z_j    <- rnorm(J_big)
#'         big_data$Z <- z_j[big_data$cluster_j]
#'         
#'         # cluster size scaled
#'         W_nj <- (big_nj[big_data$cluster_j] - min_nj)/(max_nj - min_nj)
#'         big_data$W_nj <- W_nj
#'         
#'         # generate X (num_x confounders). We'll do a simple approach:
#'         # Possibly correlated with Z if x_z != 0. For brevity, do uncorrelated:
#'         for (xx in seq_len(num_x)) {
#'             big_data[[paste0("X",xx)]] <- rnorm(big_N)
#'         }
#'         
#'         # (c) Draw cluster-level random effects for the mediator (mb_j):
#'         mb_j <- rnorm(J_big, 0, sqrt(gen_m$iccm))
#'         big_data$mb <- mb_j[big_data$cluster_j]
#'         
#'         # Build the linear predictor for M, forcibly set A = a1_val
#'         # We'll mimic your generate_mediator() formula:
#'         m_lp <- with(big_data, {
#'             mb +
#'                 gen_m$m_on_a  * a1_val +
#'                 gen_m$m_on_az * (a1_val * Z) +
#'                 gen_m$m_on_anj* (a1_val * W_nj) +
#'                 gen_m$m_on_x  * ( (X1 + X2 + X3) ) + # sum of all X if num_x=3
#'                 gen_m$m_on_z  * Z
#'         })
#'         
#'         # Now generate M(a1) properly, respecting the residual variance (1 - iccm)
#'         if (Mfamily == "binomial") {
#'             # prob = pnorm(m_lp, mean=0, sd=sqrt(1 - iccm))
#'             p_m <- pnorm(m_lp, 0, sqrt(1 - gen_m$iccm))
#'             M_a1 <- rbinom(big_N, 1, p_m)
#'         } else {
#'             # continuous M
#'             M_a1 <- m_lp + rnorm(big_N, 0, sqrt(1 - gen_m$iccm))
#'         }
#'         
#'         big_data$M_a1 <- M_a1
#'         
#'         # (d) Draw cluster-level random effects for the outcome (yb_j):
#'         yb_j <- rnorm(J_big, 0, sqrt(gen_y$iccy))
#'         big_data$yb <- yb_j[big_data$cluster_j]
#'         
#'         # Build the linear predictor for Y, forcibly set A = a0_val, but use M(a1_val)
#'         y_lp <- with(big_data, {
#'             yb +
#'                 gen_y$yintercept +
#'                 gen_y$y_on_a  * a0_val +
#'                 gen_y$y_on_m  * M_a1 +
#'                 gen_y$y_on_am * (a0_val * M_a1) +
#'                 gen_y$y_on_az * (a0_val * Z) +
#'                 gen_y$y_on_mz * (M_a1 * Z) +
#'                 gen_y$y_on_anj* (a0_val * W_nj) +
#'                 # plus X terms
#'                 gen_y$y_on_x  * ((X1 + X2 + X3)) + # sum if you have 3 X's
#'                 gen_y$y_on_z  * Z
#'         })
#'         
#'         # Generate Y given that linear predictor
#'         if (Yfamily == "binomial") {
#'             # prob = pnorm(y_lp, mean=0, sd=sqrt(1 - iccy))
#'             p_y <- pnorm(y_lp, 0, sqrt(1 - gen_y$iccy))
#'             Y_a0a1 <- rbinom(big_N, 1, p_y)
#'         } else {
#'             # continuous Y
#'             Y_a0a1 <- y_lp + rnorm(big_N, 0, sqrt(1 - gen_y$iccy))
#'         }
#'         
#'         big_data$Y_a0a1 <- Y_a0a1
#'         
#'         # (e) Finally, compute the mean at the individual and cluster levels
#'         mean_indiv    <- mean(big_data$Y_a0a1)
#'         cluster_means <- big_data %>%
#'             dplyr::group_by(cluster_j) %>%
#'             dplyr::summarize(cluster_avg = mean(Y_a0a1)) %>%
#'             dplyr::ungroup()
#'         mean_cluster  <- mean(cluster_means$cluster_avg)
#'         
#'         return(list(indiv=mean_indiv, cluster=mean_cluster))
#'     }
#'     
#'     # 3. Loop over (a0,a1) = (0,0),(0,1),(1,0),(1,1) ----------------------------
#'     a_vals <- expand.grid(a0=c(0,1), a1=c(0,1))
#'     truevals_individual <- list()
#'     truevals_cluster    <- list()
#'     
#'     for (rr in seq_len(nrow(a_vals))) {
#'         a0_val <- a_vals$a0[rr]
#'         a1_val <- a_vals$a1[rr]
#'         label  <- paste0("Y(a0=", a0_val, ", gm(a1=", a1_val, "))")
#'         
#'         # do the large MC for this scenario
#'         ans   <- large_mc_estimate(a0_val, a1_val)
#'         truevals_individual[[label]] <- ans$indiv
#'         truevals_cluster[[label]]    <- ans$cluster
#'     }
#'     
#'     # 4. Return the results ----------------------------------------------------
#'     return(list(
#'         truevals_individual = truevals_individual,
#'         truevals_cluster    = truevals_cluster
#'     ))
#' }
