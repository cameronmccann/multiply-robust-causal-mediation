
# Last updated: 2025-01-14
# Recreating new data generation (staying very close to Dr Liu's code as possible)



GenData_2.0 <- function(
        seedone = 123,
        J = 100,
        nj = 30,
        quadratic.A = FALSE,
        quadratic.M = FALSE,
        quadratic.Y = FALSE,
        icca = 0.2,
        iccm = 0.2, 
        iccy = 0.2,
        x_z = 0,
        num_m = 2,
        num_x = 3,
        Yfamily = "gaussian",
        indep_M = FALSE,
        int.XZ = FALSE,
        if.null = FALSE
) {
    
    set.seed(seed = seedone)
    
    if (nj==5) { 
        njrange <- c(5, 20)
    }
    
    if (nj == 20) {
        njrange <- c(20, 40)
    }
    
    if (nj == 50) {
        njrange <- c(50, 100)
    }
    
    nj_sizes <- runif(J, njrange[1], njrange[2]) %>% round()
    
    N <- sum(nj_sizes)
    
    data <- data.frame(
        id = 1:N,
        school = unlist(map(1:J, ~rep(.x, each = nj_sizes[.x])))
    ) %>% 
        group_by(school) %>% 
        mutate(W_nj = (n()-njrange[1])/(njrange[2]-njrange[1]) )
    
    
    
    # Z (unobserved) cluster-level confounders -----------------
    # z <- rep(rnorm(J, sd = 1), each = nj)
    data$Z <- unlist(map(1:J, ~rep(rnorm(1), each = nj_sizes[.x])))
    
    # X individual-level confounders ------
    iccx <- 0.2
    # x_z <- 0.5
    gen_x <- list(iccx = iccx, x_z = x_z)
    
    xb <- mvtnorm::rmvnorm(n = J,
                           mean = rep(0, num_x),
                           sigma = diag((1 - gen_x[["x_z"]]^2)*gen_x[["iccx"]], nrow = num_x))[data$school, ] #[rep(1:J, each = nj), ]
    xe <- mvtnorm::rmvnorm(n = N,
                           mean = rep(0, num_x),
                           sigma = diag(1 - gen_x[["iccx"]], nrow = num_x))
    x <- gen_x[["x_z"]] * data$Z + xb + xe
    data$X <- x
    
    
    # Treatment ---------
    # icca <- 0.2
    gen_a <- list(icca = icca, a_x = sqrt(0.15 * 1 / num_x), a_z = sqrt(0.4 / 1))
    # ab <- rep(rnorm(J, sd = sqrt(gen_a[["icca"]])), each = nj)
    ab <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_a[["icca"]])), each = nj_sizes[.x])))
    if (quadratic.A == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        a_given <- ab +  gen_a[["a_x"]] * rowSums(Xlinear) + gen_a[["a_z"]] * data$Z
    }
    if (quadratic.A == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4) # mean(rnorm(.)^2) = 1; var(rnorm(.)^2) = 2
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_a[["a_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        a_given <- ab +  gen_a[["a_x"]] * rowSums(Xquad) + gen_a[["a_z"]] * data$Z
    }
    
    data$A <- rbinom(N, 1, pa(1, a_given, gen_a))
    
    # Mediators ------------------
    # iccm <- 0.2
    iccm1 <- iccm2 <- iccm
    gen_m <- list(iccm1 = iccm1, iccm2 = iccm2,
                  m1_on_a = 0.2, m1_on_x = sqrt(0.15 / num_x), m1_on_z = sqrt(0.4),
                  m1_on_az = 0.2, m1_on_anj = 0.2##,
                  # M2
                  ## m2_on_a = 0.2, m2_on_m1 = 0.2, m2_on_am1 = 0.2,
                  # m2_on_a = 0.2, m2_on_m1 = 0, m2_on_am1 = 0,
                  ## m2_on_x = sqrt(0.15 / num_x), m2_on_z = sqrt(0.4),
                  ## m2_on_az = 0.2, m2_on_anj = 0.2
    )
    # conditional independent mediators
    if (indep_M) {
        gen_m[c("m2_on_m1","m2_on_am1")] <- 0
    }
    
    if (int.XZ==FALSE) {
        # gen_m[c("m1_on_az","m2_on_az")] <- 0
        gen_m[c("m1_on_anj","m2_on_anj")] <- 0
    }
    # m1b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm1"]])), each = nj)
    # m2b <- rep(rnorm(J, sd = sqrt(gen_m[["iccm2"]])), each = nj)
    m1b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm1"]])), each = nj_sizes[.x])))
    # m2b <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_m[["iccm2"]])), each = nj_sizes[.x])))
    
    if (quadratic.M == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4)
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_m[["m1_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        # gen_m[["m2_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        
        m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xquad) +
            gen_m[["m1_on_z"]] * data$Z
        data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
        
        # m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xquad) +
        #     gen_m[["m2_on_z"]] * data$Z
        # data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
        
    }
    if (quadratic.M == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        m1_given <- m1b + gen_m[["m1_on_x"]] * rowSums(Xlinear) +
            gen_m[["m1_on_z"]] * data$Z
        # pm1(1, data$A, m1_given, gen_m) %>% quantile(.,c(0.1, 0.9))
        data$M1 <- rbinom(N, 1, prob = pm1(1, data$A, data$Z, data$W_nj, m1_given, gen_m) )
        
        # m2_given <- m2b + gen_m[["m2_on_x"]] * rowSums(Xlinear) +
        #     gen_m[["m2_on_z"]] * data$Z
        # data$M2 <- rbinom(N, 1, prob = pm2(1, data$M1, data$A, data$Z, data$W_nj, m2_given, gen_m) )
    }
    
    
    # Outcome -------------------
                                            # Note: oddly
    y_on_amint <- 0
    # iccy <- 0.2
    gen_y <- list(iccy = iccy, yintercept = 1,
                  # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
                  y_on_a = 0.2, y_on_m1 = 1, #y_on_m2 = 1,
                  y_on_am1 = 0, #y_on_am2 = 0, 
                  #y_on_m1m2 = 0.2, y_on_am1m2 = 0.2,
                  # y_on_m1m2 = 0, y_on_am1m2 = 0,
                  y_on_az = 0.2, y_on_m1z = 0.2, #y_on_m2z = 0.2,
                  y_on_anj = 0.2,  
                  y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
    )
    yb <- rnorm(J, sd = sqrt(gen_y[["iccy"]]))[data$school] #[rep(1:J, each = nj)]
    ## trying unlist() way of yb creates different Ys that still differ from GenData()
    # yb <- unlist(map(1:J, ~rep(rnorm(1, mean = 0, sd = sqrt(gen_y[["iccy"]])), each = nj_sizes[.x]))) ## trying something
    
    if (if.null==TRUE) {
        gen_y <- list(iccy = iccy, yintercept = 1,
                      # y_on_a = 0.2, y_on_m1 = 0.2, y_on_m2 = 0.2,
                      y_on_a = 0, y_on_m1 = 0, #y_on_m2 = 0,
                      y_on_am1 = 0, #y_on_am2 = 0, 
                      #y_on_m1m2 = 0, y_on_am1m2 = 0,
                      # y_on_m1m2 = 0, y_on_am1m2 = 0,
                      y_on_az = 0, y_on_m1z = 0, #y_on_m2z = 0,
                      y_on_anj = 0,  
                      y_on_x = sqrt(0.15 / num_x), y_on_z = sqrt(0.4)
        )
    }
    
    if (int.XZ==FALSE) {
        # gen_y[c("y_on_az","y_on_m1z","y_on_m2z")] <- 0
        gen_y[c("y_on_anj")] <- 0
    }
    
    if (quadratic.Y == TRUE) {
        Xquad <- (data$X ^ 2 - 1) / sqrt(4)
        # if (int.XZ) {
        #   Xquad <- Xquad * data$Z
        # }
        # previous
        # Xquad <- Xquad * data$Z
        # gen_y[["y_on_x"]] <- sqrt(0.8 * 1 / ncol(Xquad))
        
            y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xquad) +
            gen_y[["y_on_z"]] * data$Z
    }
    if (quadratic.Y == FALSE) {
        Xlinear <- data$X
        # if (int.XZ) {
        #   Xlinear <- data$X * data$Z
        # }
        
        y_given <- yb + gen_y[["yintercept"]] + gen_y[["y_on_x"]] * rowSums(Xlinear) +
            gen_y[["y_on_z"]] * data$Z
    }
    
    if (Yfamily == "gaussian") {
        condmy <- my(#m2 = data$M2, 
                     m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = FALSE)
        data$Y <- condmy + rnorm(N, sd = sqrt(1 - gen_y[["iccy"]]))
    }
    if (Yfamily == "binomial") {
        condmy <- my(#m2 = data$M2, 
            m1 = data$M1, data$A, data$Z, data$W_nj, given = y_given, gen_y, binary = TRUE)
        data$Y <- rbinom(N, 1, condmy)
    }
    
    datobs <- do.call(data.frame, data)
    
    
    # nj_sizes <- as.numeric(table(data$school))
    
    
    out <- mget(ls(envir = environment()))

    return(out)

}




# other func --------------------------------------------------------------
pa <- function(a, given, gen_a) {
    prob1 <- pnorm(given, mean = 0, sd = sqrt(1 - gen_a$icca))
    a * prob1 + (1 - a) * (1 - prob1)
}

pm1 <- function(m1, a, z, nj, given, gen_m) {
    latent <- gen_m[["m1_on_a"]] * a + gen_m[["m1_on_az"]] * a*z + gen_m[["m1_on_anj"]] * a*nj + given
    prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm1"]]))
    m1 * prob1 + (1 - m1) * (1 - prob1)
}

my <- function(#m2, 
               m1, a, z, nj, given, gen_y, binary = TRUE) {
    latent <- #gen_y[["y_on_m2"]] * m2 + 
        gen_y[["y_on_m1"]] * m1 + gen_y[["y_on_a"]] * a + 
        # gen_y[["y_on_am2"]] * a * m2 + 
        gen_y[["y_on_am1"]] * a * m1 + 
        # gen_y[["y_on_m1m2"]] * m1 * m2 + 
        # gen_y[["y_on_am1m2"]] * a * m1 * m2 + 
        gen_y[["y_on_az"]] * a * z + gen_y[["y_on_m1z"]] * m1 * z + 
        # gen_y[["y_on_m2z"]] * m2 * z + 
        gen_y[["y_on_anj"]] * a * nj +
        given
    
    if (binary) {
        cond_mean <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_y[["iccy"]]))
    }
    if (!binary) {
        cond_mean <- latent
    }
    
    cond_mean
}











