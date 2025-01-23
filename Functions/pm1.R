#---------------------------------------------------#
# [UPDATE INFO!]
# 
# QP Project 
# Data Generation for Simulation 1 
#' 
#' `pm1()` generates clustered data consisting of a level-1 treatment, 
#' 3 level-1 confounders, a level-1 mediator, and a level-1 outcome as well as 
#' a level-2 confounder, with control over the number of clusters, cluster size, 
#' and ICC. Specifically, the dataframe returned from this function consists of 
#' the following variables: observation ID (id); cluster ID (school); 
#' 3 level-1 confounders of T, M, Y relations (x1-x3); a level-2 confounder of 
#' T, M, Y relations (z); (t_ast); the true propensity score of an observation (ps_true); 
#' a level-1 treatment assignment (t); level-1 mediator value (m); and the level-1 outcome measure (y). 
#' 
#' @param num_clust Number of clusters
#' @param clust_size Cluster size for each cluster (i.e., number of observations per cluster)
#' @param num_x Number of x (level-1) confounders
#' @param iccx,icct,iccm,iccy The intraclass correlation (ICC) for covariate x, treatment, mediator, and outcome
#' @returns Returns a dataframe of generated data
#' @examples
#' genOneData_Sim1(num_clust = 30, clust_size = 30)
#' 
#' 
#' 
#' 

pm1 <- function(m, a, z, nj, given, gen_m) {
    
    latent <- gen_m[["m_on_a"]] * a +
        gen_m[["m_on_az"]] * a * z +
        gen_m[["m_on_anj"]] * a * nj +
        given
    prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
    # m * prob1 + (1 - m) * (1 - prob1)
    return(m * prob1 + (1 - m) * (1 - prob1))
    
    # tried using nearly exact func below (did not work 2025-01-14)
        # latent <- gen_m[["m_on_a"]] * a + gen_m[["m_on_az"]] * a*z + gen_m[["m_on_anj"]] * a*nj + given
        # prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
        # m * prob1 + (1 - m) * (1 - prob1)
    
    # # Add individual-level residual?
    # N <- length(a) 
    # latent <- gen_m[["m_on_a"]] * a +
    #     gen_m[["m_on_az"]] * a * z +
    #     gen_m[["m_on_anj"]] * a * nj +
    #     given
    # latent_m <- latent + rnorm(N, sd = sqrt(1 - gen_m[["iccm"]]))
    # prob1 <- pnorm(latent_m, mean = 0, sd = sqrt(1 - gen_m[["iccm"]]))
    # return(m * prob1 + (1 - m) * (1 - prob1))
}


##################################### END ######################################


# Dr Liu's 
# pm1 <- function(m1, a, z, nj, given, gen_m) {
#     latent <- gen_m[["m1_on_a"]] * a + gen_m[["m1_on_az"]] * a*z + gen_m[["m1_on_anj"]] * a*nj + given
#     prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm1"]]))
#     m1 * prob1 + (1 - m1) * (1 - prob1)
# }
