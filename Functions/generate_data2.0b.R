#' NOTE: THIS IS MODIFIED TO WORK ON ORIGINAL generate_data2.0 function (note: ps overlap plot was added to this function)
#' @title generate_data2.0b
#' 
#' @description This function generates clustered data with an individual-level treatment (A), mediator (M), and outcome (Y), 
#' as well as individual-level covariates (X) and a cluster-level variable (Z). 
#' It can handle binary or continuous mediators and outcomes, incorporates optional 
#' quadratic terms, and computes the true potential outcomes and mediation effects.
#' 
#' @param J Number of clusters (default: 100).
#' @param njrange Range for cluster sizes (default: c(50, 100)).
#' @param Mfamily Family for mediator ('gaussian' or 'binomial', default: "binomial").
#' @param Yfamily Family for outcome ('gaussian' or 'binomial', default: "binomial").
#' @param if.null Generate data under null hypothesis if TRUE (default: FALSE).
#' @param seed Seed for reproducibility (default: 123456).
#' @param num_x Number of individual-level confounders (default: 3).
#' @param x_z Correlation between 'X' and 'Z' (default: 0).
#' @param m_on_a Effect of 'A' on 'M' (default: 0.2).
#' @param m_on_az Interaction effect of 'A' and 'Z' on 'M' (default: 0.2).
#' @param m_on_anj Interaction effect of 'A' and cluster size on 'M' (default: 0.2).
#' @param int.XZ Interaction between 'X' and 'Z' (default: TRUE).
#' @param yintercept Intercept for outcome model (default: 1).
#' @param y_on_a Effect of 'A' on 'Y' (default: 0.5).
#' @param y_on_m Effect of 'M' on 'Y' (default: 1).
#' @param y_on_am Interaction effect of 'A' and 'M' on 'Y' (default: 0).
#' @param y_on_az Interaction effect of 'A' and 'Z' on 'Y' (default: 0.2).
#' @param y_on_mz Interaction effect of 'M' and 'Z' on 'Y' (default: 0.2).
#' @param y_on_anj Interaction effect of 'A' and cluster size on 'Y' (default: 0.2).
#' @param quadratic.A Include quadratic terms for 'A' (default: FALSE).
#' @param quadratic.M Include quadratic terms for 'M' (default: FALSE).
#' @param quadratic.Y Include quadratic terms for 'Y' (default: FALSE).
#' @param iccx Intra-class correlation for 'X' (default: 0.2).
#' @param icca Intra-class correlation for 'A' (default: 0.2).
#' @param iccm Intra-class correlation for 'M' (default: 0.2).
#' @param iccy Intra-class correlation for 'Y' (default: 0.2).
#' 
#' @return A list containing:
#' \describe{
#'   \item{data}{A complete dataset with all variables (A, M, Y, X, Z, and cluster indices).}
#'   \item{truevals}{True values for potential outcomes under different treatment/mediator interventions, computed by `trueVals2.0()`.}
#'   \item{effects}{Calculated mediation effects at both individual and cluster levels (PNDE, PNIE, TNDE, TNIE).}
#'   \item{parameters}{A list of parameters used to generate the data.}
#' }
#' 
#' @details This function is a wrapper that calls several helper functions:
#' - `generate_clusters()`: Creates clusters with specified size ranges.
#' - `generate_confounders()`: Generates individual- and cluster-level confounders.
#' - `generate_treatment()`: Simulates the treatment variable A.
#' - `generate_mediator()`: Simulates the mediator variable M.
#' - `generate_outcome()`: Simulates the outcome variable Y.
#' - `trueVals2.0()`: Calculates the true potential outcomes and mediation effects for the given setup.
#' 
#' Both binary and continuous mediators/outcomes are supported. Quadratic terms for A, M, and Y can be included, 
#' as well as interactions between X and Z.
#' 
#' @import dplyr
#' @import MASS
#' @importFrom stats rnorm rbinom
#' @importFrom utils set.seed
#'
#' @examples
#' # Example usage
#' result <- generate_data2.0b(J = 50, num_x = 2, Mfamily="gaussian", Yfamily="gaussian")
#' head(result$data)
#' 
#' @export
generate_data2.0b <- function(J = 100,                        # Number of clusters
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
                             m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
                             m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
                             m_on_x = sqrt(0.15 / num_x), 
                             m_on_z = sqrt(0.4),
                             int.XZ = TRUE,                  # Interaction between 'X' and 'Z'
                             yintercept = 1,                  # Intercept for outcome model
                             y_on_a = 0.5,                    # Effect of 'A' on 'Y'
                             y_on_m = 1,                      # Effect of 'M' on 'Y'
                             y_on_am = 0,                     # Interaction effect of 'A' and 'M' on 'Y'
                             y_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'Y'
                             y_on_mz = 0.2,                   # Interaction effect of 'M' and 'Z' on 'Y'
                             y_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'Y'
                             y_on_x = sqrt(0.15 / num_x),
                             y_on_z = sqrt(0.4),
                             quadratic.A = FALSE,             # Include quadratic terms for 'A'
                             quadratic.M = FALSE,             # Include quadratic terms for 'M'
                             quadratic.Y = FALSE,             # Include quadratic terms for 'Y'
                             iccx = 0.2,                      # Intra-class correlation for 'X'
                             icca = 0.2,                      # Intra-class correlation for 'A'
                             iccm = 0.2,                      # Intra-class correlation for 'M'
                             iccy = 0.2                       # Intra-class correlation for 'Y'
) {               
    # Set Seed
    set.seed(seed)
    
    # Generate Clusters
    data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)
    
    # Generate Confounders
    data_list <- generate_confounders(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        num_x = num_x,
        iccx = iccx,
        x_z = x_z
    )
    
    # Generate Treatment
    data_list <- generate_treatment(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        icca = icca,
        quadratic.A = quadratic.A,
        num_x = num_x# , 
        # a_x = data_list$a_x,
        # a_z = a_z, 
    )
    
    # Create Overlap Plot
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
    # Create Overlap Plot with Logit PS
    overlap_plot_logit <- ggplot(data_list$data, aes(x = qlogis(ps_true), fill = factor(A))) +
        geom_density(alpha = 0.5) +
        labs(
            title = "Density Plot of Logit(ps_true) by Treatment Group (A)",
            x = "Logit of the True Propensity Score",
            y = "Density",
            fill = "Treatment (A)"
        ) +
        theme_minimal()
    
    # Summarize extreme PS values
    n_ps_below_001 <- sum(data_list$data$ps_true < 0.01, na.rm = TRUE)
    n_ps_above_099 <- sum(data_list$data$ps_true > 0.99, na.rm = TRUE)
    
    # Calculate percentages
    pct_ps_below_001 <- 100 * n_ps_below_001 / nrow(data_list$data)
    pct_ps_above_099 <- 100 * n_ps_above_099 / nrow(data_list$data)
    
    # Combine into a message string
    ps_msg <- paste0(
        "Number of PSs < 0.01: ", n_ps_below_001, " (", 
        round(pct_ps_below_001, 2), "%); ",
        "Number of PSs > 0.99: ", n_ps_above_099, " (",
        round(pct_ps_above_099, 2), "%)"
    )
    # Print to console
    message(ps_msg)
    
    # Create an IPTW variable
    data_list$data <- data_list$data %>%
        mutate(
            iptw_true = ifelse(A == 1, 1 / ps_true, 1 / (1 - ps_true))
        )
    
    # Identify 1st and 99th percentiles
    first_percentile <- quantile(data_list$data$iptw_true, probs = 0.01, na.rm = TRUE)
    ninety_ninth_percentile <- quantile(data_list$data$iptw_true, probs = 0.99, na.rm = TRUE)
    
    # Count outliers below 1st percentile & above 99th percentile
    n_iptw_below_1p <- sum(data_list$data$iptw_true < first_percentile, na.rm = TRUE)
    n_iptw_above_99p <- sum(data_list$data$iptw_true > ninety_ninth_percentile, na.rm = TRUE)
    
    # Calculate percentages for IPTW outliers
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
    # Print to console
    message(iptw_msg)
    
    
    
    # Generate Mediator
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
    
    # Generate Outcome
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
    
    # Compute True Values (if possible)
    if (Mfamily %in% c("binomial","gaussian") & Yfamily %in% c("binomial","gaussian")) {
        true_vals <- trueVals2.0b(data_list = data_list)
    } else {
        true_vals <- NULL
    }
    
    # Extract the relevant potential outcomes from true_vals
    if (!is.null(true_vals)) {
        # Individual-level true values
        y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
        y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
        y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
        y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
        
        # Compute individual-level mediation effects
        pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
        pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
        tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
        tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
        
        # Cluster-level true values
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
        pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
        pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
    }
    # # Compute Effects if binary-binary (as previously)
    # if (Mfamily == "binomial" & Yfamily == "binomial" & !is.null(true_vals)) {
    #     y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
    #     y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
    #     y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
    #     y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1))`
    #     
    #     # Individual-level effects
    #     pnde_ind <- y_a1_m0 - y_a0_m0
    #     pnie_ind <- y_a0_m1 - y_a0_m0
    #     tnde_ind <- y_a1_m1 - y_a0_m1
    #     tnie_ind <- y_a1_m1 - y_a1_m0
    #     
    #     # Cluster-level effects
    #     y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
    #     y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
    #     y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
    #     y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
    #     
    #     pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0
    #     pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0
    #     tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1
    #     tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0
    # } else {
    #     # For now, if continuous involved, skip automatic effect calculation or implement similarly
    #     pnde_ind <- pnie_ind <- tnde_ind <- tnie_ind <- NULL
    #     pnde_cluster <- pnie_cluster <- tnde_cluster <- tnie_cluster <- NULL
    # }
    
    # Update data with observed data
    datobs <- data_list$data
    if (is.matrix(datobs$X)) {
        for (i in 1:num_x) {
            datobs[[paste0("X", i)]] <- datobs$X[, i]
        }
        datobs$X <- NULL
    }
    data_list$data <- datobs
    rm(datobs)
    
    # Compile Final Output
    result_data <- list(
        data = data_list$data,
        truevals = true_vals,
        effects = list(
            individual = list(pnde = pnde_ind, tnie = tnie_ind, tnde = tnde_ind, pnie = pnie_ind),
            cluster = list(pnde = pnde_cluster, tnie = tnie_cluster, tnde = tnde_cluster, pnie = pnie_cluster)
        ),
        overlap = list(overlap_plot = overlap_plot, 
                       overlap_plot_logit = overlap_plot_logit, 
                       ps_summary    = ps_msg,
                       iptw_summary  = iptw_msg), 
        parameters = list(
            J = J, 
            njrange = njrange, 
            nj_sizes = data_list$nj_sizes, 
            y_given = data_list$y_given,
            m_given = data_list$m_given,
            seed = seed, 
            num_x = num_x, 
            iccx = iccx, x_z = x_z, 
            icca = icca, quadratic.A = quadratic.A, iccm = iccm, m_on_a = m_on_a, m_on_az = m_on_az, m_on_anj = m_on_anj,
            quadratic.M = quadratic.M, int.XZ = int.XZ, iccy = iccy, yintercept = yintercept,
            y_on_a = y_on_a, y_on_m = y_on_m, y_on_am = y_on_am, y_on_az = y_on_az, y_on_mz = y_on_mz,
            y_on_anj = y_on_anj, quadratic.Y = quadratic.Y, Yfamily = Yfamily, if.null = if.null
        )
    )
    
    return(result_data)
}
