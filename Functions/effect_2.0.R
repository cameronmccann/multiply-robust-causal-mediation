
# {UPDATE DOCUMENTATION AT SOMEPOINT}
# As of 2025-01-16: only modified the to pull from thetas (OG commmented out below) 

effect_2.0 <- function(thetas) {
    thetas[["DE(,M(0))"]] <- thetas[["`Y(a0=1, gm(a1=0))`"]] - thetas[["`Y(a0=0, gm(a1=0))`"]]
    thetas[["IE(1,)"]] <- thetas[["`Y(a0=1, gm(a1=1))`"]] - thetas[["`Y(a0=1, gm(a1=0))`"]]
    
    thetas
}

# OG
# effect <- function(thetas) {
#     thetas[["DE(,M(0))"]] <- thetas[["Y(1,M(0))"]] - thetas[["Y(0,M(0))"]]
#     thetas[["IE(1,)"]] <- thetas[["Y(1,M(1))"]] - thetas[["Y(1,M(0))"]]
#     
#     thetas
# }
