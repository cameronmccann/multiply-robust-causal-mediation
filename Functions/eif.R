
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2025-01-03: did not modify anything yet  


# eif
eif <- function(data_in, varnames, a_c, a_mc, mu_mac, v_ac, mu_ac) {
    
    a_vals <- data.frame(a = c(1, 1, 0),
                         astar = c(0, 1, 0))
    
    A <- data_in[[varnames$A]]
    Y <- data_in[[varnames$Y]]
    
    # multiply-robust
    eifs <- list()
    # regression (G-computation)
    regs <- list()
    # weighting
    rmpw <- list()
    
    i <-1
    for (i in 1:nrow(a_vals)) {
        a <- a_vals$a[i]
        astar <- a_vals$astar[i]
        
        # Y(a,M(astar)) --------------
        if (a != astar) {
            # observed treatment
            # ipwA <- ipwA_c[, glue("ipwA({a0}|c)")]
            # p_a <- a_c[, glue("a({a}|c)")]
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            ipw_astar <- 1*(A==astar) / bound(a_c[, glue("a({astar}|c)")])
            # observed outcome
            mu <- mu_mac[, glue("mu(m,a{a},c)")]
            vbar <- v_ac[, glue("v_ac(c)")]
            # observed A,M distribution
            h_m <- ( a_c[, glue("a({a}|c)")]*a_mc[, glue("a({astar}|m,c)")] ) /
                (a_c[, glue("a({astar}|c)")]*a_mc[, glue("a({a}|m,c)")])
            
            
            eify <- 1*(A == a)*ipw_a*h_m*(Y - mu) / mean(1*(A == a)*ipw_a*h_m)
            eifm <- 1*(A == astar)*ipw_astar*(mu - vbar) / mean(1*(A == astar)*ipw_astar)
            eif <- eify + eifm + vbar
            
            eifs[[glue("Y({a},M({astar}))")]] <- eif
            
            rmpw[[glue("Y({a},M({astar}))")]] <- 1*(A == a)*ipw_a*h_m*(Y) / mean(1*(A == a)*ipw_a*h_m)
            regs[[glue("Y({a},M({astar}))")]] <- vbar
        }
        
        # Y(a) -------------
        if (a == astar) {
            # observed treatment
            # ipwA <- ipwA_c[, glue("ipwA({a0}|c)")]
            # p_a <- a_c[, glue("a({a}|c)")]
            ipw_a <- 1*(A==a) / bound(a_c[, glue("a({a}|c)")])
            # observed outcome
            mu_c <- mu_ac[, glue("mu(a{a},c)")]
            
            eifya <- 1*(A == a)*ipw_a*(Y - mu_c) / mean(1*(A == a)*ipw_a)
            
            eif_a <- eifya + mu_c
            
            eifs[[glue("Y({a},M({a}))")]] <- eif_a
            
            rmpw[[glue("Y({a},M({a}))")]] <- 1*(A == a)*ipw_a*(Y) / mean(1*(A == a)*ipw_a)
            regs[[glue("Y({a},M({a}))")]] <- mu_c
        }
    }
    
    
    list(eifs=eifs, #eify_ajo=eify_ajo, eifm_ajo=eifm_ajo, mu_a0c_jo=mu_a0c_jo,
         rmpw=rmpw, regs=regs)
}
