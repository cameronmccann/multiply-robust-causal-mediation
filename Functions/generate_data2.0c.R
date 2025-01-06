#' @title generate_data2.0c
#'
#' @description
#' Generates a clustered dataset with an individual-level treatment (\code{A}), a mediator (\code{M}), 
#' an outcome (\code{Y}), individual-level covariates (\code{X}), and a cluster-level variable (\code{Z}). 
#' Supports both binary and continuous mediators/outcomes, incorporates optional quadratic terms for 
#' treatment/mediator/outcome, and calculates true potential outcome values and mediation effects (if requested).
#'
#' The function proceeds by:
#' \enumerate{
#'   \item Generating clusters with a specified size range.
#'   \item Creating individual- and cluster-level covariates, including correlated \code{X} and \code{Z} if desired.
#'   \item Simulating a treatment variable \code{A} with optional intraclass correlation.
#'   \item Simulating the mediator \code{M} given \code{A}, \code{Z}, cluster size, and user-specified interactions.
#'   \item Simulating the outcome \code{Y} given \code{A}, \code{M}, \code{Z}, cluster size, user-specified interactions, etc.
#'   \item (Optionally) Computing the true potential outcomes and mediation effects by calling \code{trueVals2.0c()}.
#' }
#'
#' @param J Integer. Number of clusters (default: 100).
#' @param njrange Integer vector of length 2. Range (min, max) for cluster sizes (default: \code{c(50, 100)}).
#' @param Mfamily Character. Family for the mediator (\code{"gaussian"} or \code{"binomial"}). Defaults to \code{"binomial"}.
#' @param Yfamily Character. Family for the outcome (\code{"gaussian"} or \code{"binomial"}). Defaults to \code{"binomial"}.
#' @param if.null Logical. If \code{TRUE}, generate data under the null hypothesis (i.e., no effects). Defaults to \code{FALSE}.
#' @param seed Integer. Random seed for reproducibility (default: 123456).
#' @param num_x Integer. Number of individual-level covariates (\code{X}). Defaults to 3.
#' @param x_z Numeric. Correlation between \code{X} and the cluster-level variable \code{Z} (default: 0).
#' @param m_on_a Numeric. Main effect of treatment \code{A} on mediator \code{M} (default: 0.2).
#' @param m_on_az Numeric. Interaction effect of \code{A} and \code{Z} on \code{M} (default: 0.2).
#' @param m_on_anj Numeric. Interaction effect of \code{A} and cluster size (\code{nj}) on \code{M} (default: 0.2).
#' @param m_on_x Numeric. Effect of each individual-level covariate \code{X} on \code{M}. 
#'   Defaults to \code{sqrt(0.15 / num_x)}.
#' @param m_on_z Numeric. Effect of cluster-level variable \code{Z} on \code{M}. Defaults to \code{sqrt(0.4)}.
#' @param int.XZ Logical. If \code{TRUE}, include an interaction term between \code{X} and \code{Z} in mediator/outcome models. 
#'   Defaults to \code{TRUE}.
#' @param yintercept Numeric. Intercept in the outcome \code{Y} model (default: 1).
#' @param y_on_a Numeric. Main effect of \code{A} on \code{Y} (default: 0.5).
#' @param y_on_m Numeric. Main effect of \code{M} on \code{Y} (default: 1).
#' @param y_on_am Numeric. Interaction effect of \code{A} and \code{M} on \code{Y} (default: 0).
#' @param y_on_az Numeric. Interaction effect of \code{A} and \code{Z} on \code{Y} (default: 0.2).
#' @param y_on_mz Numeric. Interaction effect of \code{M} and \code{Z} on \code{Y} (default: 0.2).
#' @param y_on_anj Numeric. Interaction effect of \code{A} and cluster size (\code{nj}) on \code{Y} (default: 0.2).
#' @param y_on_x Numeric. Effect of each individual-level covariate \code{X} on \code{Y}. 
#'   Defaults to \code{sqrt(0.15 / num_x)}.
#' @param y_on_z Numeric. Effect of \code{Z} on \code{Y}. Defaults to \code{sqrt(0.4)}.
#' @param quadratic.A Logical. If \code{TRUE}, include quadratic term for \code{A} in the mediator/outcome models (default: FALSE).
#' @param quadratic.M Logical. If \code{TRUE}, include quadratic term for \code{M} in the outcome model (default: FALSE).
#' @param quadratic.Y Logical. If \code{TRUE}, include quadratic term for \code{Y} itself (used in some advanced simulations) (default: FALSE).
#' @param iccx Numeric. Intra-class correlation for the \code{X} variables (default: 0.2).
#' @param icca Numeric. Intra-class correlation for treatment \code{A} (default: 0.2).
#' @param iccm Numeric. Intra-class correlation for mediator \code{M} (default: 0.2).
#' @param iccy Numeric. Intra-class correlation for outcome \code{Y} (default: 0.2).
#' @param include_truevals Logical. If \code{TRUE}, call \code{trueVals2.0c()} to compute the true potential outcomes and 
#'   mediation effects (default: TRUE).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{data}}{A data frame containing the simulated data: 
#'                      \code{A} (treatment), \code{M} (mediator), \code{Y} (outcome), 
#'                      \code{Z} (cluster-level var), \code{X} (individual-level covariates), 
#'                      \code{school} (cluster ID), etc.}
#'   \item{\code{truevals}}{(Optional) A list of the true potential outcomes under different 
#'                          \code{(A, M)} interventions if \code{include_truevals = TRUE}.}
#'   \item{\code{effects}}{A list of mediation effects (PNDE, PNIE, TNDE, TNIE) at both individual and cluster levels, 
#'                         if \code{include_truevals = TRUE}. Otherwise, this may be \code{NULL}.}
#'   \item{\code{overlap}}{A list containing diagnostic plots and summary for propensity scores (\code{ps_true}) 
#'                         and stabilized IPTW (\code{iptw_true}). Useful for checking overlap/outliers.}
#'   \item{\code{parameters}}{A list of the input parameters and additional generated values (like \code{nj_sizes}) 
#'                            for documentation and reproducibility.}
#' }
#'
#' @details
#' This function is a wrapper that calls several helper functions:
#' \itemize{
#'   \item \code{generate_clusters()}: Builds the cluster IDs and sets cluster sizes in \code{njrange}.
#'   \item \code{generate_confounders()}: Creates individual-level confounders \code{X} (optionally correlated with \code{Z}).
#'   \item \code{generate_treatment()}: Simulates the binary (or continuous) treatment \code{A} with optional ICC.
#'   \item \code{generate_mediator()}: Simulates \code{M} given \code{A}, \code{Z}, cluster size, etc., 
#'                                     using the family (\code{binomial} or \code{gaussian}).
#'   \item \code{generate_outcome()}: Constructs outcome \code{Y} based on \code{A}, \code{M}, \code{Z}, 
#'                                    interactions, ICC, etc., again depending on the family chosen.
#'   \item \code{trueVals2.0c()}: (Optional) Computes the true potential outcomes for \code{Y(a0, gm(a1))}, 
#'                                cluster-level means, and resulting mediation effects.
#' }
#'
#' Propensity score (\code{ps_true}) and IPTW weights (\code{iptw_true}) are also computed for 
#' reference and diagnostic plotting. 
#'
#' @examples
#' # Generate data with smaller clusters and continuous M, continuous Y:
#' result <- generate_data2.0c(
#'   J = 50, 
#'   njrange = c(10, 20), 
#'   Mfamily = "gaussian", 
#'   Yfamily = "gaussian"
#' )
#' str(result$data)  # Inspect the generated data
#'
#' @import dplyr
#' @import MASS
#' @importFrom stats rnorm rbinom
#' @importFrom utils set.seed
#' @export
generate_data2.0c <- function(J = 100,                        # Number of clusters
                              njrange = c(50, 100),            # Range for cluster sizes
                              Mfamily = "binomial",            # Family for mediator ('gaussian' or 'binomial')
                              Yfamily = "binomial",            # Family for outcome ('gaussian' or 'binomial')
                              if.null = FALSE, 
                              seed = 123456,                   # Seed for reproducibility
                              num_x = 3,                       # Number of individual-level confounders
                              # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
                              # a_z = sqrt(0.4 / 1), 
                              x_z = 0,                         # Correlation between 'X' and 'Z'
                              m_on_a = 0.2,                    # Effect of 'A' on 'M'
                              m_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'M'
                              m_on_anj = 0.2,                  # Interaction: 'A' x cluster size on 'M'
                              m_on_x = sqrt(0.15 / num_x),     # Effect of 'X' on 'M'
                              m_on_z = sqrt(0.4),              # Effect of 'Z' on 'M'
                              int.XZ = TRUE,                   # Include X:Z interaction in mediator/outcome model
                              yintercept = 1,                  # Intercept for outcome model
                              y_on_a = 0.5,                    # Effect of 'A' on 'Y'
                              y_on_m = 1,                      # Effect of 'M' on 'Y'
                              y_on_am = 0,                     # Interaction: 'A' x 'M' on 'Y'
                              y_on_az = 0.2,                   # Interaction: 'A' x 'Z' on 'Y'
                              y_on_mz = 0.2,                   # Interaction: 'M' x 'Z' on 'Y'
                              y_on_anj = 0.2,                  # Interaction: 'A' x cluster size on 'Y'
                              y_on_x = sqrt(0.15 / num_x),     # Effect of 'X' on 'Y'
                              y_on_z = sqrt(0.4),              # Effect of 'Z' on 'Y'
                              quadratic.A = FALSE,             # Include quadratic term for 'A'
                              quadratic.M = FALSE,             # Include quadratic term for 'M'
                              quadratic.Y = FALSE,             # Include quadratic term for 'Y'
                              iccx = 0.2,                      # Intra-class correlation for 'X'
                              icca = 0.2,                      # Intra-class correlation for 'A'
                              iccm = 0.2,                      # Intra-class correlation for 'M'
                              iccy = 0.2,                      # Intra-class correlation for 'Y'
                              include_truevals = TRUE,         # Whether or not to compute true values
                              include_overlapMsg = TRUE        # Whether or not to display messages about PS overlap in console
) {               
    # -----------------------------------------------------
    # 1. CLUSTER GENERATION
    # -----------------------------------------------------
    set.seed(seed)  
    
    # Generate initial cluster structure 
    # (cluster IDs, cluster sizes) based on J and njrange
    data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)
    
    # -----------------------------------------------------
    # 2. GENERATE CONFOUNDERS (X and Z)
    # -----------------------------------------------------
    data_list <- generate_confounders(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        num_x = num_x,
        iccx = iccx,
        x_z = x_z
    )
    
    # -----------------------------------------------------
    # 3. GENERATE TREATMENT (A)
    # -----------------------------------------------------
    data_list <- generate_treatment(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        icca = icca,
        quadratic.A = quadratic.A,
        num_x = num_x# , 
        # a_x = data_list$a_x,
        # a_z = a_z, 
    )
    
    # -----------------------------------------------------
    # 4. DIAGNOSTIC PLOTS OF THE PROPENSITY SCORE
    # -----------------------------------------------------
    # Overlap plot (density of ps_true by treatment group)
    overlap_plot <- ggplot(data_list$data, aes(x = ps_true, color = factor(A), fill = factor(A))) +
        geom_density(alpha = 0.5) +
        labs(
            title = "Density Plot of ps_true by Treatment Group (A)",
            x = "True Propensity Score (ps_true)",
            y = "Density",
            fill = "Treatment (A)"
        ) +
        theme_minimal() +
        theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold")
        )
    
    # Overlap plot on the logit scale
    overlap_plot_logit <- ggplot(data_list$data, aes(x = qlogis(ps_true), fill = factor(A))) +
        geom_density(alpha = 0.5) +
        labs(
            title = "Density Plot of Logit(ps_true) by Treatment Group (A)",
            x = "Logit of the True Propensity Score",
            y = "Density",
            fill = "Treatment (A)"
        ) +
        theme_minimal()
    
    # Summaries of extreme PS values (below 0.01 or above 0.99)
    n_ps_below_001 <- sum(data_list$data$ps_true < 0.01, na.rm = TRUE)
    n_ps_above_099 <- sum(data_list$data$ps_true > 0.99, na.rm = TRUE)
    pct_ps_below_001 <- 100 * n_ps_below_001 / nrow(data_list$data)
    pct_ps_above_099 <- 100 * n_ps_above_099 / nrow(data_list$data)
    
    # Combine into a message string
    ps_msg <- paste0(
        "Number of PSs < 0.01: ", n_ps_below_001, " (", 
        round(pct_ps_below_001, 2), "%); ",
        "Number of PSs > 0.99: ", n_ps_above_099, " (",
        round(pct_ps_above_099, 2), "%)"
    )
    if (include_overlapMsg == TRUE) {
        message(ps_msg) # Print info about overlap to console
    }
    
    # Create an IPTW variable for each observation
    data_list$data <- data_list$data %>%
        mutate(
            iptw_true = ifelse(A == 1, 1 / ps_true, 1 / (1 - ps_true))
        )
    
    # Summaries of extreme IPTW values (below 1st pct or above 99th pct)
    first_percentile <- quantile(data_list$data$iptw_true, probs = 0.01, na.rm = TRUE)
    ninety_ninth_percentile <- quantile(data_list$data$iptw_true, probs = 0.99, na.rm = TRUE)
    
    n_iptw_below_1p <- sum(data_list$data$iptw_true < first_percentile, na.rm = TRUE)
    n_iptw_above_99p <- sum(data_list$data$iptw_true > ninety_ninth_percentile, na.rm = TRUE)
    pct_iptw_below_1p <- 100 * n_iptw_below_1p / nrow(data_list$data)
    pct_iptw_above_99p <- 100 * n_iptw_above_99p / nrow(data_list$data)
    
    # Combine into a message string
    iptw_msg <- paste0(
        "Number of cases < 1st percentile of IPTW (", 
        round(first_percentile, 4), "): ", n_iptw_below_1p, " (",
        round(pct_iptw_below_1p, 2), "%); ",
        "Number of cases > 99th percentile of IPTW (", 
        round(ninety_ninth_percentile, 4), "): ", n_iptw_above_99p, " (",
        round(pct_iptw_above_99p, 2), "%)"
    )
    if (include_overlapMsg == TRUE) {
        message(iptw_msg)  # Print info about IPTW extremes to console
    }
    
    # -----------------------------------------------------
    # 5. GENERATE MEDIATOR (M)
    # -----------------------------------------------------
    data_list <- generate_mediator(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        iccm = iccm,
        num_x = num_x,
        m_on_a = m_on_a,
        m_on_az = m_on_az,
        m_on_anj = m_on_anj,
        # m_on_x = m_on_x, 
        # m_on_z = m_on_z,
        quadratic.M = quadratic.M,
        int.XZ = int.XZ, 
        Mfamily = Mfamily
    )
    
    # -----------------------------------------------------
    # 6. GENERATE OUTCOME (Y)
    # -----------------------------------------------------
    data_list <- generate_outcome(
        data_list = data_list,
        iccy = iccy,
        yintercept = yintercept,
        y_on_a = y_on_a,
        y_on_m = y_on_m,
        y_on_am = y_on_am,
        y_on_az = y_on_az,
        y_on_mz = y_on_mz,
        y_on_anj = y_on_anj,
        num_x = num_x,
        # y_on_x = y_on_x,
        # y_on_z = y_on_z,
        quadratic.Y = quadratic.Y,
        int.XZ = int.XZ,
        Yfamily = Yfamily,
        if.null = if.null
    )

    # -----------------------------------------------------
    # 7. (OPTIONAL) COMPUTE TRUE POTENTIAL OUTCOMES
    # -----------------------------------------------------
    # If include_truevals is TRUE, compute Y(a0, gm(a1)) values
    # and obtain mediation effects (PNDE, PNIE, etc.).
    if (include_truevals == TRUE) {
        true_vals <- trueVals2.0c(data_list = data_list)
    } else {
        true_vals <- NULL
    }
    
    # -----------------------------------------------------
    # 8. CALCULATE MEDIATION EFFECTS (IF TRUE VALUES AVAILABLE)
    # -----------------------------------------------------
    
    
    # Extract the relevant potential outcomes from true_vals
    if (!is.null(true_vals)) {
        # Individual-level potential outcomes
        y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
        y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
        y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
        y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
        
        # Compute individual-level mediation effects
        pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
        pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
        tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
        tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
        
        # Cluster-level potential outcomes
        y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
        y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
        y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
        y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
        
        # Compute cluster-level mediation effects
        pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
        pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
        tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
        tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
    } else {
        # If we didn't compute trueVals2.0c, set effects to NULL
        pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
        pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
    }
    
    # -----------------------------------------------------
    # 9. POST-PROCESSING: SEPARATE OUT X COLUMNS IF NEEDED
    # -----------------------------------------------------
    datobs <- data_list$data
    if (is.matrix(datobs$X)) {
        # If X was stored as a matrix, split it into X1, X2, ..., X_{num_x}
        for (i in seq_len(num_x)) {
            datobs[[paste0("X", i)]] <- datobs$X[, i]
        }
        datobs$X <- NULL
    }
    data_list$data <- datobs
    rm(datobs)
    
    # -----------------------------------------------------
    # 10. PREPARE FINAL OUTPUT
    # -----------------------------------------------------
    result_data <- list(
        data = data_list$data,
        truevals = true_vals,
        effects = list(
            individual = list(
                pnde = pnde_ind, 
                tnie = tnie_ind, 
                tnde = tnde_ind, 
                pnie = pnie_ind
            ),
            cluster = list(
                pnde = pnde_cluster, 
                tnie = tnie_cluster, 
                tnde = tnde_cluster, 
                pnie = pnie_cluster
            )
        ),
        overlap = list(
            overlap_plot = overlap_plot,
            overlap_plot_logit = overlap_plot_logit,
            ps_summary = ps_msg,
            iptw_summary = iptw_msg
        ),
        parameters = list(
            J = J,
            njrange = njrange,
            nj_sizes = data_list$nj_sizes,
            y_given = data_list$y_given,
            m_given = data_list$m_given,
            seed = seed,
            num_x = num_x,
            iccx = iccx,
            x_z = x_z,
            icca = icca,
            quadratic.A = quadratic.A,
            iccm = iccm,
            m_on_a = m_on_a,
            m_on_az = m_on_az,
            m_on_anj = m_on_anj,
            quadratic.M = quadratic.M,
            int.XZ = int.XZ,
            iccy = iccy,
            yintercept = yintercept,
            y_on_a = y_on_a,
            y_on_m = y_on_m,
            y_on_am = y_on_am,
            y_on_az = y_on_az,
            y_on_mz = y_on_mz,
            y_on_anj = y_on_anj,
            quadratic.Y = quadratic.Y,
            Yfamily = Yfamily,
            if.null = if.null
        )
    )
    
    return(result_data)
}





#' 
#' #' NOTE: THIS IS MODIFIED TO WORK ON ORIGINAL generate_data2.0 function (note: ps overlap plot was added to this function)
#' #' @title generate_data2.0c
#' #' 
#' #' @description This function generates clustered data with an individual-level treatment (A), mediator (M), and outcome (Y), 
#' #' as well as individual-level covariates (X) and a cluster-level variable (Z). 
#' #' It can handle binary or continuous mediators and outcomes, incorporates optional 
#' #' quadratic terms, and computes the true potential outcomes and mediation effects.
#' #' 
#' #' @param J Number of clusters (default: 100).
#' #' @param njrange Range for cluster sizes (default: c(50, 100)).
#' #' @param Mfamily Family for mediator ('gaussian' or 'binomial', default: "binomial").
#' #' @param Yfamily Family for outcome ('gaussian' or 'binomial', default: "binomial").
#' #' @param if.null Generate data under null hypothesis if TRUE (default: FALSE).
#' #' @param seed Seed for reproducibility (default: 123456).
#' #' @param num_x Number of individual-level confounders (default: 3).
#' #' @param x_z Correlation between 'X' and 'Z' (default: 0).
#' #' @param m_on_a Effect of 'A' on 'M' (default: 0.2).
#' #' @param m_on_az Interaction effect of 'A' and 'Z' on 'M' (default: 0.2).
#' #' @param m_on_anj Interaction effect of 'A' and cluster size on 'M' (default: 0.2).
#' #' @param int.XZ Interaction between 'X' and 'Z' (default: TRUE).
#' #' @param yintercept Intercept for outcome model (default: 1).
#' #' @param y_on_a Effect of 'A' on 'Y' (default: 0.5).
#' #' @param y_on_m Effect of 'M' on 'Y' (default: 1).
#' #' @param y_on_am Interaction effect of 'A' and 'M' on 'Y' (default: 0).
#' #' @param y_on_az Interaction effect of 'A' and 'Z' on 'Y' (default: 0.2).
#' #' @param y_on_mz Interaction effect of 'M' and 'Z' on 'Y' (default: 0.2).
#' #' @param y_on_anj Interaction effect of 'A' and cluster size on 'Y' (default: 0.2).
#' #' @param quadratic.A Include quadratic terms for 'A' (default: FALSE).
#' #' @param quadratic.M Include quadratic terms for 'M' (default: FALSE).
#' #' @param quadratic.Y Include quadratic terms for 'Y' (default: FALSE).
#' #' @param iccx Intra-class correlation for 'X' (default: 0.2).
#' #' @param icca Intra-class correlation for 'A' (default: 0.2).
#' #' @param iccm Intra-class correlation for 'M' (default: 0.2).
#' #' @param iccy Intra-class correlation for 'Y' (default: 0.2).
#' #' 
#' #' @return A list containing:
#' #' \describe{
#' #'   \item{data}{A complete dataset with all variables (A, M, Y, X, Z, and cluster indices).}
#' #'   \item{truevals}{True values for potential outcomes under different treatment/mediator interventions, computed by `trueVals2.0()`.}
#' #'   \item{effects}{Calculated mediation effects at both individual and cluster levels (PNDE, PNIE, TNDE, TNIE).}
#' #'   \item{parameters}{A list of parameters used to generate the data.}
#' #' }
#' #' 
#' #' @details This function is a wrapper that calls several helper functions:
#' #' - `generate_clusters()`: Creates clusters with specified size ranges.
#' #' - `generate_confounders()`: Generates individual- and cluster-level confounders.
#' #' - `generate_treatment()`: Simulates the treatment variable A.
#' #' - `generate_mediator()`: Simulates the mediator variable M.
#' #' - `generate_outcome()`: Simulates the outcome variable Y.
#' #' - `trueVals2.0()`: Calculates the true potential outcomes and mediation effects for the given setup.
#' #' 
#' #' Both binary and continuous mediators/outcomes are supported. Quadratic terms for A, M, and Y can be included, 
#' #' as well as interactions between X and Z.
#' #' 
#' #' @import dplyr
#' #' @import MASS
#' #' @importFrom stats rnorm rbinom
#' #' @importFrom utils set.seed
#' #'
#' #' @examples
#' #' # Example usage
#' #' result <- generate_data2.0c(J = 50, num_x = 2, Mfamily="gaussian", Yfamily="gaussian")
#' #' head(result$data)
#' #' 
#' #' @export
#' generate_data2.0c <- function(J = 100,                        # Number of clusters
#'                               njrange = c(50, 100),            # Range for cluster sizes
#'                               Mfamily = "binomial",            # Family for mediator ('gaussian' or 'binomial')
#'                               Yfamily = "binomial",            # Family for outcome ('gaussian' or 'binomial')
#'                               if.null = FALSE, 
#'                               seed = 123456,                   # Seed for reproducibility
#'                               num_x = 3,                       # Number of individual-level confounders
#'                               # a_x = 0.15, # sqrt(0.15 * 1 / num_x)
#'                               # a_z = sqrt(0.4 / 1), 
#'                               x_z = 0,                         # Correlation between 'X' and 'Z'
#'                               m_on_a = 0.2,                    # Effect of 'A' on 'M'
#'                               m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
#'                               m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
#'                               m_on_x = sqrt(0.15 / num_x), 
#'                               m_on_z = sqrt(0.4),
#'                               int.XZ = TRUE,                  # Interaction between 'X' and 'Z'
#'                               yintercept = 1,                  # Intercept for outcome model
#'                               y_on_a = 0.5,                    # Effect of 'A' on 'Y'
#'                               y_on_m = 1,                      # Effect of 'M' on 'Y'
#'                               y_on_am = 0,                     # Interaction effect of 'A' and 'M' on 'Y'
#'                               y_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'Y'
#'                               y_on_mz = 0.2,                   # Interaction effect of 'M' and 'Z' on 'Y'
#'                               y_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'Y'
#'                               y_on_x = sqrt(0.15 / num_x),
#'                               y_on_z = sqrt(0.4),
#'                               quadratic.A = FALSE,             # Include quadratic terms for 'A'
#'                               quadratic.M = FALSE,             # Include quadratic terms for 'M'
#'                               quadratic.Y = FALSE,             # Include quadratic terms for 'Y'
#'                               iccx = 0.2,                      # Intra-class correlation for 'X'
#'                               icca = 0.2,                      # Intra-class correlation for 'A'
#'                               iccm = 0.2,                      # Intra-class correlation for 'M'
#'                               iccy = 0.2,                      # Intra-class correlation for 'Y'
#'                               include_truevals = TRUE # Whether or not to compute true values 
#' ) {               
#'     # Set Seed
#'     set.seed(seed)
#'     
#'     # Generate Clusters
#'     data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)
#'     
#'     # Generate Confounders
#'     data_list <- generate_confounders(
#'         data_list = data_list,
#'         nj_sizes = data_list$nj_sizes,
#'         num_x = num_x,
#'         iccx = iccx,
#'         x_z = x_z
#'     )
#'     
#'     # Generate Treatment
#'     data_list <- generate_treatment(
#'         data_list = data_list,
#'         nj_sizes = data_list$nj_sizes,
#'         icca = icca,
#'         quadratic.A = quadratic.A,
#'         num_x = num_x# , 
#'         # a_x = data_list$a_x,
#'         # a_z = a_z, 
#'     )
#'     
#'     # Create Overlap Plot
#'     overlap_plot <- ggplot(data_list$data, aes(x = ps_true, color = factor(A), fill = factor(A))) +
#'         geom_density(alpha = 0.5) +
#'         labs(
#'             title = "Density Plot of ps_true by Treatment Group (A)",
#'             x = "True Propensity Score (ps_true)",
#'             y = "Density",
#'             fill = "Treatment (A)"
#'         ) +
#'         theme_minimal() +
#'         theme(
#'             legend.position = "top",
#'             plot.title = element_text(hjust = 0.5, face = "bold")
#'         )
#'     # Create Overlap Plot with Logit PS
#'     overlap_plot_logit <- ggplot(data_list$data, aes(x = qlogis(ps_true), fill = factor(A))) +
#'         geom_density(alpha = 0.5) +
#'         labs(
#'             title = "Density Plot of Logit(ps_true) by Treatment Group (A)",
#'             x = "Logit of the True Propensity Score",
#'             y = "Density",
#'             fill = "Treatment (A)"
#'         ) +
#'         theme_minimal()
#'     
#'     # Summarize extreme PS values
#'     n_ps_below_001 <- sum(data_list$data$ps_true < 0.01, na.rm = TRUE)
#'     n_ps_above_099 <- sum(data_list$data$ps_true > 0.99, na.rm = TRUE)
#'     
#'     # Calculate percentages
#'     pct_ps_below_001 <- 100 * n_ps_below_001 / nrow(data_list$data)
#'     pct_ps_above_099 <- 100 * n_ps_above_099 / nrow(data_list$data)
#'     
#'     # Combine into a message string
#'     ps_msg <- paste0(
#'         "Number of PSs < 0.01: ", n_ps_below_001, " (", 
#'         round(pct_ps_below_001, 2), "%); ",
#'         "Number of PSs > 0.99: ", n_ps_above_099, " (",
#'         round(pct_ps_above_099, 2), "%)"
#'     )
#'     # Print to console
#'     message(ps_msg)
#'     
#'     # Create an IPTW variable
#'     data_list$data <- data_list$data %>%
#'         mutate(
#'             iptw_true = ifelse(A == 1, 1 / ps_true, 1 / (1 - ps_true))
#'         )
#'     
#'     # Identify 1st and 99th percentiles
#'     first_percentile <- quantile(data_list$data$iptw_true, probs = 0.01, na.rm = TRUE)
#'     ninety_ninth_percentile <- quantile(data_list$data$iptw_true, probs = 0.99, na.rm = TRUE)
#'     
#'     # Count outliers below 1st percentile & above 99th percentile
#'     n_iptw_below_1p <- sum(data_list$data$iptw_true < first_percentile, na.rm = TRUE)
#'     n_iptw_above_99p <- sum(data_list$data$iptw_true > ninety_ninth_percentile, na.rm = TRUE)
#'     
#'     # Calculate percentages for IPTW outliers
#'     pct_iptw_below_1p <- 100 * n_iptw_below_1p / nrow(data_list$data)
#'     pct_iptw_above_99p <- 100 * n_iptw_above_99p / nrow(data_list$data)
#'     
#'     # Combine into a message string
#'     iptw_msg <- paste0(
#'         "Number of cases < 1st percentile of IPTW (", 
#'         round(first_percentile, 4), "): ", n_iptw_below_1p, " (",
#'         round(pct_iptw_below_1p, 2), "%); ",
#'         "Number of cases > 99th percentile of IPTW (", 
#'         round(ninety_ninth_percentile, 4), "): ", n_iptw_above_99p, " (",
#'         round(pct_iptw_above_99p, 2), "%)"
#'     )
#'     # Print to console
#'     message(iptw_msg)
#'     
#'     
#'     
#'     # Generate Mediator
#'     data_list <- generate_mediator(
#'         data_list = data_list,
#'         nj_sizes = data_list$nj_sizes,
#'         iccm = iccm,
#'         num_x = num_x,
#'         m_on_a = m_on_a,
#'         m_on_az = m_on_az,
#'         m_on_anj = m_on_anj,
#'         # m_on_x = m_on_x, 
#'         # m_on_z = m_on_z,
#'         quadratic.M = quadratic.M,
#'         int.XZ = int.XZ, 
#'         Mfamily = Mfamily
#'     )
#'     
#'     # Generate Outcome
#'     data_list <- generate_outcome(
#'         data_list = data_list,
#'         iccy = iccy,
#'         yintercept = yintercept,
#'         y_on_a = y_on_a,
#'         y_on_m = y_on_m,
#'         y_on_am = y_on_am,
#'         y_on_az = y_on_az,
#'         y_on_mz = y_on_mz,
#'         y_on_anj = y_on_anj,
#'         num_x = num_x,
#'         # y_on_x = y_on_x,
#'         # y_on_z = y_on_z,
#'         quadratic.Y = quadratic.Y,
#'         int.XZ = int.XZ,
#'         Yfamily = Yfamily,
#'         if.null = if.null
#'     )
#'     
#'     # Compute True Values (if possible)
#'     if (include_truevals == TRUE) {
#'         true_vals <- trueVals2.0c(data_list = data_list)
#'     } else {
#'         true_vals <- NULL
#'     }
#'     
#'     # if (Mfamily %in% c("binomial","gaussian") & Yfamily %in% c("binomial","gaussian")) {
#'     #     true_vals <- trueVals2.0b(data_list = data_list)
#'     # } else {
#'     #     true_vals <- NULL
#'     # }
#'     
#'     # Extract the relevant potential outcomes from true_vals
#'     if (!is.null(true_vals)) {
#'         # Individual-level true values
#'         y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
#'         y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
#'         y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
#'         y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
#'         
#'         # Compute individual-level mediation effects
#'         pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
#'         pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
#'         tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
#'         tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
#'         
#'         # Cluster-level true values
#'         y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
#'         y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
#'         y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
#'         y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
#'         
#'         # Compute cluster-level mediation effects
#'         pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
#'         pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
#'         tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
#'         tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
#'     } else {
#'         pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
#'         pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
#'     }
#'     # # Compute Effects if binary-binary (as previously)
#'     # if (Mfamily == "binomial" & Yfamily == "binomial" & !is.null(true_vals)) {
#'     #     y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
#'     #     y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
#'     #     y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
#'     #     y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
#'     #     
#'     #     # Individual-level effects
#'     #     pnde_ind <- y_a1_m0 - y_a0_m0
#'     #     pnie_ind <- y_a0_m1 - y_a0_m0
#'     #     tnde_ind <- y_a1_m1 - y_a0_m1
#'     #     tnie_ind <- y_a1_m1 - y_a1_m0
#'     #     
#'     #     # Cluster-level effects
#'     #     y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
#'     #     y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
#'     #     y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
#'     #     y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
#'     #     
#'     #     pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
#'     #     pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
#'     #     tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
#'     #     tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
#'     # } else {
#'     #     # For now, if continuous involved, skip automatic effect calculation or implement similarly
#'     #     pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
#'     #     pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
#'     # }
#'     
#'     # Update data with observed data
#'     datobs <- data_list$data
#'     if (is.matrix(datobs$X)) {
#'         for (i in 1:num_x) {
#'             datobs[[paste0("X", i)]] <- datobs$X[, i]
#'         }
#'         datobs$X <- NULL
#'     }
#'     data_list$data <- datobs
#'     rm(datobs)
#'     
#'     # Compile Final Output
#'     result_data <- list(
#'         data = data_list$data,
#'         truevals = true_vals,
#'         effects = list(
#'             individual = list(pnde = pnde_ind, tnie = tnie_ind, tnde = tnde_ind, pnie = pnie_ind),
#'             cluster = list(pnde = pnde_cluster, tnie = tnie_cluster, tnde = tnde_cluster, pnie = pnie_cluster)
#'         ),
#'         overlap = list(overlap_plot = overlap_plot, 
#'                        overlap_plot_logit = overlap_plot_logit, 
#'                        ps_summary    = ps_msg,
#'                        iptw_summary  = iptw_msg), 
#'         parameters = list(
#'             J = J, 
#'             njrange = njrange, 
#'             nj_sizes = data_list$nj_sizes, 
#'             y_given = data_list$y_given,
#'             m_given = data_list$m_given,
#'             seed = seed, 
#'             num_x = num_x, 
#'             iccx = iccx, x_z = x_z, 
#'             icca = icca, quadratic.A = quadratic.A, iccm = iccm, m_on_a = m_on_a, m_on_az = m_on_az, m_on_anj = m_on_anj,
#'             quadratic.M = quadratic.M, int.XZ = int.XZ, iccy = iccy, yintercept = yintercept,
#'             y_on_a = y_on_a, y_on_m = y_on_m, y_on_am = y_on_am, y_on_az = y_on_az, y_on_mz = y_on_mz,
#'             y_on_anj = y_on_anj, quadratic.Y = quadratic.Y, Yfamily = Yfamily, if.null = if.null
#'         )
#'     )
#'     
#'     return(result_data)
#' }
