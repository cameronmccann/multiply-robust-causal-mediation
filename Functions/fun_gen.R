pa <- function(a, given, gen_a) {
  prob1 <- pnorm(given, mean = 0, sd = sqrt(1 - gen_a$icca))
  a * prob1 + (1 - a) * (1 - prob1)
}

pm1 <- function(m1, a, z, nj, given, gen_m) {
    latent <- gen_m[["m1_on_a"]] * a + gen_m[["m1_on_az"]] * a*z + gen_m[["m1_on_anj"]] * a*nj + given
    prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm1"]]))
    m1 * prob1 + (1 - m1) * (1 - prob1)
}

pm2 <- function(m2, m1, a, z, nj, given, gen_m) {
    latent <- gen_m[["m2_on_m1"]] * m1 + gen_m[["m2_on_a"]] * a +
        gen_m[["m2_on_am1"]] * a * m1 + gen_m[["m2_on_az"]] * a * z + gen_m[["m2_on_anj"]] * a*nj + 
        given
    
    prob1 <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_m[["iccm2"]]))
    m2 * prob1 + (1 - m2) * (1 - prob1)
}

pm2a <- function(m2, a, z, nj, m2_given, m1_given, gen_m) {
    pm2(m2, 1, a, z, nj, m2_given, gen_m) * pm1(1, a, z, nj, m1_given, gen_m) +
        pm2(m2, 0, a, z, nj, m2_given, gen_m) * pm1(0, a, z, nj, m1_given, gen_m)
}


my <- function(m2, m1, a, z, nj, given, gen_y, binary = TRUE) {
    latent <- gen_y[["y_on_m2"]] * m2 + gen_y[["y_on_m1"]] * m1 + gen_y[["y_on_a"]] * a + 
        gen_y[["y_on_am2"]] * a * m2 + gen_y[["y_on_am1"]] * a * m1 + gen_y[["y_on_m1m2"]] * m1 * m2 + 
        gen_y[["y_on_am1m2"]] * a * m1 * m2 + 
        gen_y[["y_on_az"]] * a * z + gen_y[["y_on_m1z"]] * m1 * z + gen_y[["y_on_m2z"]] * m2 * z +  gen_y[["y_on_anj"]] * a * nj +
        given
    
    if (binary) {
        cond_mean <- pnorm(latent, mean = 0, sd = sqrt(1 - gen_y[["iccy"]]))
    }
    if (!binary) {
        cond_mean <- latent
    }
    
    cond_mean
}

