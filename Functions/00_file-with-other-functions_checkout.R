# This comes from the end of the fit_pred.R file that had crossfit() in it




bound <- function(vals, tol = 0.025) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}

bound.precison <- function(vals, tol = 1e-4) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}


SL.xgboost.modified <- function(...) {
    SL.xgboost(..., ntrees = 100)
}
SL.ranger.modified <- function(...) {
    SL.ranger(...,num.trees = 200)
}

SL.gam.modified <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, 
                             ...) 
{
    Xoffset <- NULL
    if (length(grep("_clmean", colnames(X), value = T))>0) {
        Xoffset <- X[,grep("_clmean", colnames(X), value = T)]
        if (family=="binomial") {
            Xoffset <- bound.precision(qlogis(Xoffset))
        }
        # Xoffset <- grep("_clmean", colnames(X), value = T)
    }
    X <- X[, !grepl("_clmean", colnames(X))]
    
    cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
    if (sum(!cts.x) > 0) {
        gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                        colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                        ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
                                                                                                               !cts.x, drop = FALSE]), collapse = "+")))
    }
    else {
        gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                        colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                        ")", sep = ""), collapse = "+")))
    }
    if (sum(!cts.x) == length(cts.x)) {
        gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                                  collapse = "+"), sep = ""))
    }
    fit.gam <- gam::gam(gam.model, data = data.frame(X, Y=Y), family = family, offset=Xoffset,
                        control = gam::gam.control(maxit = 50, bf.maxit = 50), 
                        weights = obsWeights)
    if (packageVersion("gam") >= "1.15") {
        pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
    }
    else {
        stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
    }
    fit <- list(object = fit.gam)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.gam")
    return(out)
}


