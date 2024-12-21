#' @title generate_data 
#' 
#' @description Brief description of what the function does.
#' 
#' @param J Number of clusters (default: 100).
#' @param njrange Range for cluster sizes (default: c(50, 100)).
#' @param seed Seed for reproducibility (default: 123456).
#' @param num_x Number of individual-level confounders (default: 3).
#' @param iccx Intra-class correlation for 'X' (default: 0.2).
#' @param x_z Correlation between 'X' and 'Z' (default: 0).
#' @param icca Intra-class correlation for 'A' (default: 0.2).
#' @param quadratic.A Include quadratic terms for 'A' (default: FALSE).
#' @param iccm Intra-class correlation for 'M' (default: 0.2).
#' @param m_on_a Effect of 'A' on 'M' (default: 0.2).
#' @param m_on_az Interaction effect of 'A' and 'Z' on 'M' (default: 0.2).
#' @param m_on_anj Interaction effect of 'A' and cluster size on 'M' (default: 0.2).
#' @param quadratic.M Include quadratic terms for 'M' (default: FALSE).
#' @param int.XZ Interaction between 'X' and 'Z' (default: FALSE).
#' @param iccy Intra-class correlation for 'Y' (default: 0.2).
#' @param yintercept Intercept for outcome model (default: 1).
#' @param y_on_a Effect of 'A' on 'Y' (default: 0.5).
#' @param y_on_m Effect of 'M' on 'Y' (default: 1).
#' @param y_on_am Interaction effect of 'A' and 'M' on 'Y' (default: 0).
#' @param y_on_az Interaction effect of 'A' and 'Z' on 'Y' (default: 0.2).
#' @param y_on_mz Interaction effect of 'M' and 'Z' on 'Y' (default: 0.2).
#' @param y_on_anj Interaction effect of 'A' and cluster size on 'Y' (default: 0.2).
#' @param quadratic.Y Include quadratic terms for 'Y' (default: FALSE).
#' @param Yfamily Family for outcome ('gaussian' or 'binomial', default: "binomial").
#' @param if.null Generate data under null hypothesis if TRUE (default: FALSE).
#' 
#' @return A list containing:
#' \describe{
#'   \item{data}{Complete dataset with all variables.}
#'   \item{truevals}{True values for potential outcomes.}
#'   \item{effects}{Calculated effects at individual and cluster levels.}
#'   \item{parameters}{Parameters used to generate the data.}
#' }
#' 
#' @details This function is a wrapper that depends on several other functions for generating specific parts of the data:
#' - `generate_clusters()`: Creates clusters with specified size ranges.
#' - `generate_confounders()`: Adds individual- and cluster-level confounders.
#' - `generate_treatment()`: Simulates treatment variables.
#' - `generate_mediators()`: Simulates mediating variables.
#' - `generate_outcome()`: Simulates outcome variables.
#' - `trueVals()`: Calculates true potential outcomes.
#' 
#' It relies on the following packages:
#' - `dplyr`: For data manipulation.
#' - `MASS`: For multivariate normal distribution.
#' - `stats`: For statistical functions.
#' - `utils`: For general utility functions.
#' 
#' @import dplyr
#' @import MASS
#' @importFrom stats rnorm rbinom
#' @importFrom utils set.seed
#' 
#' @examples
#' # Example usage
#' result <- generate_data(J = 50, num_x = 2)
#' head(result$data)
#' 
#' @seealso 
#' @export
generate_data <- function(J = 100,                        # Number of clusters
                          njrange = c(50, 100),            # Range for cluster sizes
                          Mfamily = "binomial",            # Family for mediator ('gaussian' or 'binomial')
                          Yfamily = "binomial",            # Family for outcome ('gaussian' or 'binomial')
                          if.null = FALSE, 
                          seed = 123456,                   # Seed for reproducibility
                          num_x = 3,                       # Number of individual-level confounders
                          x_z = 0,                         # Correlation between 'X' and 'Z'
                          m_on_a = 0.2,                    # Effect of 'A' on 'M'
                          m_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'M'
                          m_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'M'
                          int.XZ = FALSE,                  # Interaction between 'X' and 'Z'
                          yintercept = 1,                  # Intercept for outcome model
                          y_on_a = 0.5,                    # Effect of 'A' on 'Y'
                          y_on_m = 1,                      # Effect of 'M' on 'Y'
                          y_on_am = 0,                     # Interaction effect of 'A' and 'M' on 'Y'
                          y_on_az = 0.2,                   # Interaction effect of 'A' and 'Z' on 'Y'
                          y_on_mz = 0.2,                   # Interaction effect of 'M' and 'Z' on 'Y'
                          y_on_anj = 0.2,                  # Interaction effect of 'A' and cluster size on 'Y'
                          quadratic.A = FALSE,             # Include quadratic terms for 'A'
                          quadratic.M = FALSE,             # Include quadratic terms for 'M'
                          quadratic.Y = FALSE,             # Include quadratic terms for 'Y'
                          iccx = 0.2,                      # Intra-class correlation for 'X'
                          icca = 0.2,                      # Intra-class correlation for 'A'
                          iccm = 0.2,                      # Intra-class correlation for 'M'
                          iccy = 0.2                       # Intra-class correlation for 'Y'
                          ) {               # Generate data under null hypothesis if TRUE
    
    # ══════════════════════════════
    #     Step 0: Set Seed for Reproducibility
    # ══════════════════════════════
    set.seed(seed)
    
    # ══════════════════════════════
    #     Step 1: Generate Clusters   
    # ══════════════════════════════
    data_list <- generate_clusters(J = J, njrange = njrange, seed = seed)
    # data_list contains:
    # - data: Data frame with 'id', 'school', and 'W_nj'
    # - nj_sizes: Vector of cluster sizes
    # - njrange: Range of cluster sizes
    
    # ══════════════════════════════
    #     Step 2: Generate Confounders
    # ══════════════════════════════
    data_list <- generate_confounders(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        num_x = num_x,
        iccx = iccx,
        x_z = x_z
    )
    # Confounders added:
    # - Z: Cluster-level unobserved confounder
    # - X: Matrix of individual-level confounders
    
    # ══════════════════════════════
    #     Step 3: Generate Treatment   
    # ══════════════════════════════
    data_list <- generate_treatment(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        icca = icca,
        quadratic.A = quadratic.A,
        num_x = num_x
    )
    # Treatment 'A' added to data_list$data
    
    # ══════════════════════════════
    #     Step 4: Generate Mediator
    # ══════════════════════════════
    data_list <- generate_mediator(
        data_list = data_list,
        nj_sizes = data_list$nj_sizes,
        iccm = iccm,
        num_x = num_x,
        m_on_a = m_on_a,
        m_on_az = m_on_az,
        m_on_anj = m_on_anj,
        quadratic.M = quadratic.M,
        int.XZ = int.XZ, 
        Mfamily = Mfamily
    )
    # Mediator 'M' added to data_list$data
    
    # ══════════════════════════════
    #     Step 5: Generate Outcome
    # ══════════════════════════════
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
        quadratic.Y = quadratic.Y,
        int.XZ = int.XZ,
        Yfamily = Yfamily,
        if.null = if.null
    )
    # Outcome 'Y' added to data_list$data
    
    # ══════════════════════════════
    #     Step 6: Compute True Values
    # ══════════════════════════════
    if (Mfamily == "binomial" & Yfamily == "binomial") {
        true_vals <- trueVals(data_list = data_list)
        # true_vals contains true values for individual and cluster levels
    } else {
        true_vals <- NULL
        pnde_ind <- NULL
        pnie_ind <- NULL
        tnde_ind <- NULL
        tnie_ind <- NULL
        pnde_cluster <- NULL
        pnie_cluster <- NULL
        tnde_cluster <- NULL
        tnie_cluster <- NULL
    }
    # true_vals <- trueVals(data_list = data_list)
    # # true_vals contains true values for individual and cluster levels

    # ══════════════════════════════
    #     Step 7: Add Effects Calculation
    # ══════════════════════════════
    if (Mfamily == "binomial" & Yfamily == "binomial") {
        # Individual-level true values
        y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
        y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
        y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
        y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1)`
        
        # Compute effects for individual-level outcomes
        pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
        pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
        tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
        tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
        
        # Cluster-level true values (averages)
        y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
        y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
        y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
        y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
        
        # Compute effects for cluster-level outcomes
        pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0  # Pure Natural Direct Effect
        pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0  # Pure Natural Indirect Effect
        tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1  # Total Natural Direct Effect
        tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0  # Total Natural Indirect Effect
    }
    
    if (Mfamily == "gaussian" | Yfamily == "gaussian") {
        print("Function does not support computing true values for gaussian distributions yet")
    }
    # # Individual-level true values
    # y_a0_m0 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=0))`
    # y_a1_m0 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=0))`
    # y_a0_m1 <- true_vals$truevals_individual$`Y(a0=0, gm(a1=1))`
    # y_a1_m1 <- true_vals$truevals_individual$`Y(a0=1, gm(a1=1)`
    # 
    # # Compute effects for individual-level outcomes
    # pnde_ind <- y_a1_m0 - y_a0_m0  # Pure Natural Direct Effect
    # pnie_ind <- y_a0_m1 - y_a0_m0  # Pure Natural Indirect Effect
    # tnde_ind <- y_a1_m1 - y_a0_m1  # Total Natural Direct Effect
    # tnie_ind <- y_a1_m1 - y_a1_m0  # Total Natural Indirect Effect
    # 
    # # Cluster-level true values (averages)
    # y_cl_a0_m0 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=0))`
    # y_cl_a1_m0 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=0))`
    # y_cl_a0_m1 <- true_vals$truevals_cluster$`Y(a0=0, gm(a1=1))`
    # y_cl_a1_m1 <- true_vals$truevals_cluster$`Y(a0=1, gm(a1=1))`
    # 
    # # Compute effects for cluster-level outcomes
    # pnde_cluster <- y_cl_a1_m0 - y_cl_a0_m0  # Pure Natural Direct Effect
    # pnie_cluster <- y_cl_a0_m1 - y_cl_a0_m0  # Pure Natural Indirect Effect
    # tnde_cluster <- y_cl_a1_m1 - y_cl_a0_m1  # Total Natural Direct Effect
    # tnie_cluster <- y_cl_a1_m1 - y_cl_a1_m0  # Total Natural Indirect Effect
    
    # Mfamily = "binomial"
    
    # ══════════════════════════════
    #     Step 8: Update Data with Observed Data
    # ══════════════════════════════
    datobs <- data_list$data
    # Split 'X' matrix into separate columns if 'X' is a matrix
    if (is.matrix(datobs$X)) {
        for (i in 1:num_x) {
            datobs[[paste0("X", i)]] <- datobs$X[, i]
        }
        datobs$X <- NULL  # Remove the original 'X' matrix
    }
    
    # Replace data in data_list with datobs
    data_list$data <- datobs
    
    # Remove temporary variable
    rm(datobs)
    
    # ══════════════════════════════
    #     Step 9: Compile Final Output
    # ══════════════════════════════
    result_data <- list(
        data = data_list$data,   # Complete dataset with all variables
        truevals = true_vals,    # True values for potential outcomes
        effects = list(
            individual = list(pnde = pnde_ind, tnie = tnie_ind, tnde = tnde_ind, pnie = pnie_ind),
            cluster = list(pnde = pnde_cluster, tnie = tnie_cluster, tnde = tnde_cluster, pnie = pnie_cluster)
        ),
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
