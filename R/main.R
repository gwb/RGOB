

#' Computes a point estimate and confidence interval using the GOB estimator
#'
#' `gob` implements the Generalized Oaxaca-Blinder covariate adjustment of
#' Guo and Basse (2020). The function currently supports linear models
#' and generalized linear models out of the box; it allows allows the
#' specification of custom regression functions.
#'
#' Adjustment via linear and generalized linear models is as easy as:
#'
#' > gob(lm(Y ~ X1 + X2), Z)
#'
#' or 
#'
#' > gob(glm(Y ~ X1 + X2, family=gaussian), Z)
#'
#' for linear adjustment. Nonlinear adjustment via the `glm` function
#' is also straightforward, e.g.
#'
#' > gob(glm(Y ~ X1 + X2, family=Poisson), Z)
#'
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
#' @return A list whose first element is a point estimate, and second element
#' is a confidence interval.
#' @examples
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
#' Z <- sample(c(0,1), N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)
#' Yb <- ifelse(Y > 1.2, 1, 0)
#' dat <- data.frame(Yp=Y, D=X, E=B, F=C, Ybp=Yb)
#'
#' # Using lm for covariate adjustment
#' gob(lm(Y ~ X + B), Z)
#' gob(lm(Yp~ D + E), Z, data=dat)
#'
#' # Using logistic regression adjustment
#' gob(glm(Yb ~ X + B, family=binomial(link="logit")), Z)
#' gob(glm(Ybp ~ D + E, family=binomial(link="logit")), Z, data=dat)
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
        if(has_data_field(form.expr)) {
            warning(paste0("The call formula you provided contains data field `data=",
                           as_string(form.expr$data),
                           "'. The `data' argument should be provided *outside* of the ",
                           "lm(.) or glm(.) call. Any `data' field inside these calls has ",
                           "been ignored."))
            form.expr$data <- NULL
            
        }
        new_form <- formula(form.expr[[2]], env=caller_env())
        pred.fn.ls <- match_pred_fn(form.expr)
    }    

    ## DO SOME CHECKS TO MAKE SURE USER DOESN'T INCLUDE ASSIGNMENT IN
    ## FORMULA. 
    
    if(is.null(data)){
        ## The form uses variables described in the caller env        
        if(is_dot_predictor_formula(new_form)){
            stop("dot is invalid in formula if `data` is NULL")
        } else {
            Y <- get_env(new_form)[[as_string(new_form[[2]])]]
            X <- build_predictor_dataframe(new_form)
            
            return(.gob(Y, X, Z, pred.fn.ls, alpha))
        }
    } else {
        ## deals with the case where data is provided    
        Y <- eval(expr(data[, !!as_string(new_form[[2]])]))
        
        ##X <- data[, get_predictor_names(new_form), drop=FALSE]
        X <- build_predictor_dataframe_from_data(new_form, data)
        
        return(.gob(Y, X, Z, pred.fn.ls, alpha))

    }
}

#' Unsugared version of `gob`
#'
#' `.gob` does the actual work of computing the point estimate and
#' confidence interval. It should generally not be called directly
#' (and is in fact not exposed to the user).
#'
#' @param Y A vector of outcomes.
#' @param X A dataframe of covariates.
#' @param Z A binary vector of assignments.
#' @param pred.fn.ls A list of length 2.
#'
#' The first element is the regression function to apply to the
#' control units, while the second element is the regression function
#' to apply to the treated units.
#'
#' @param alpha The desired coverage of the confidence interval.
#' @return a list of length 2. The first element is the point estimate;
#' the second is the confidence interval.
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

    return(list("estimate"=tau.hat, "conf.int"=CI))
}



