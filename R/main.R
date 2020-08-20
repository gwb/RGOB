##library(rlang)
##library(stringr)
##library(magrittr)


## utils

##
## Desired call: gob(Y ~ A + B, dat)
##
## or maybe
##
## gob(r1= Y ~ A + B, r0 = Y~A + C, Z=Z, data=dat)
##

## note: do it for a prediction-unbiased pred.fn, then
## include "debiasing" part

#' Computes a point estimate and confidence interval using the GOB estimator
#'
#' @param form A formula or a call-formula (see details for `call-formula`).
#' @param Z A vector of assignments.
#' @param pred.fn.ls A list of learners for the treatment and control potential
#' outcomes (see details).
#' @param data (optional) A dataframe whose columns include all the variables
#' referenced in `form`.
#'
#' If `NULL`, the variables in `form` will be taken from the
#' calling environment.
#' @param alpha The confidence level (defaults to 0.95).
#' @examples
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
#' Z <- sample(c(0,1), N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)
#' dat <- data.frame(Yp=Y, D=X, E=B, F=C)
#' gob(lm(Y ~ X + B), Z)
#' gob(lm(Yp~ D + E), Z, data=dat)
#' @import rlang
#' @export
gob <- function(form, Z, pred.fn.ls=NULL, data=NULL, alpha=0.95) {
    ## Checks if call-form or form
    form.expr <- enexpr(form)    
    if(as_string(form.expr[[1]]) == '~') {
        ## formula form
        new_form <- form        
    } else {
        ## call form
        new_form <- formula(form.expr[[2]], env=caller_env())
        pred.fn.ls  <- match_pred_fn(form.expr, caller_env())
    }
    
    if(is.null(data)){
        ## The form uses variables described in the caller env
        
        if(is_dot_predictor_formula(new_form)){
            stop("dot is invalid in formula if `data` is NULL")
        }
        Y <- get_env(new_form)[[as_string(new_form[[2]])]]
        X <- build_predictor_dataframe(new_form)
        
        return(.gob(Y, X, Z, pred.fn.ls, alpha))
    }

    ## deals with the case where data is provided    
    Y <- eval(expr(data[, !!as_string(new_form[[2]])]))
    X <- data[, get_predictor_names(new_form), drop=FALSE]
    
    return(.gob(Y, X, Z, pred.fn.ls, alpha))    
}

.gob <- function(Y, X, Z, pred.fn.ls, alpha=0.95) {
    X.1 <- X[Z==1, , drop=FALSE]; Y.1 <- Y[Z==1]
    X.0 <- X[Z==0, , drop=FALSE]; Y.0 <- Y[Z==0]

    mu.hat.0.fn <- pred.fn.ls[[1]](Y.0,X.0)
    mu.hat.1.fn <- pred.fn.ls[[2]](Y.1,X.1)
    
    mu.hat.0 <- sapply(seq(nrow(X)), function(i) mu.hat.0.fn(X[i, , drop=FALSE]))
    mu.hat.1 <- sapply(seq(nrow(X)), function(i) mu.hat.1.fn(X[i, , drop=FALSE]))

    ## est
    tau.hat <- mean(mu.hat.1 - mu.hat.0)

    ## inference
    resid.0 <- Y.0 - sapply(seq(nrow(X.0)), function(i) mu.hat.0.fn(X[i, , drop=FALSE]))
    resid.1 <- Y.1 - sapply(seq(nrow(X.1)), function(i) mu.hat.1.fn(X[i, , drop=FALSE]))

    CI <- tau.hat + t.test(resid.1, resid.0, conf.level=alpha)$conf.int

    return(list(tau.hat, CI))
}



