#---------------------------------------------------#
# [UPDATE INFO!]
# 
# QP Project 
# Data Generation for Simulation 1 
#' 
#' `my()` generates clustered data consisting of a level-1 treatment, 
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
my <- function(m, a, z, nj, given, gen_y, binary = TRUE) {
    latent <- gen_y[["y_on_m"]] * m + 
        gen_y[["y_on_a"]] * a + 
        gen_y[["y_on_am"]] * a * m + 
        gen_y[["y_on_az"]] * a * z + 
        gen_y[["y_on_mz"]] * m * z + 
        gen_y[["y_on_anj"]] * a * nj + 
        given
    
    if (binary) {
        # For binary outcomes, use logistic function
        # cond_mean <- plogis(latent)  # Equivalent to 1 / (1 + exp(-latent))
        cond_mean <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_y[["iccy"]]))
    }
    if (!binary) {
        cond_mean <- latent
    }
    
    cond_mean
}


##################################### END ######################################

