
#' Replaces a call to a regression function with a call to the
#' associated prediction function.
#'
#' `match_pred_fn` takes an expression capturing a call to either
#' `lm` or `glm`, and the environment in which this expression should
#' be evaluated; it then replaces the call to `lm` or `glm` by a call to
#' `lm.pred` or `glm.pred`.
#'
#' @param form_call_expr An expression containing a call to `lm` or `glm`.
#' @param orig_env An environment.
#'
#' This is the environment in which the expression will be evaluated. It must
#' contain definitions for all the variables captured in the expression.
#'
#' @return A list of functions.
#'
#' Specifically, it returns the same as the functions `lm.res` and `glm.res`.
#'
#' @examples
#' \dontrun{
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N)
#' Z <- sample(c(0,1), size=N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5*Z
#'
#' lm_call <- expr(lm(Y ~ X + Z))
#' glm_call <- expr(glm(Y ~ X + Z))
#'
#' match_pred_fn(lm_call, current_env()) # equivalent to: lm.pred(Y ~ X + Z)
#' match_pred_fn(glm_call, current_env()) # equivalent to: glm.pred(Y ~ X + Z)
#' }
match_pred_fn <- function(form_call_expr, orig_env) {
    pred_hash <- list("lm" = "lm.pred", "glm" = "glm.pred")
    fn_name <- as_string(form_call_expr[[1]])
    if(!(fn_name %in% names(pred_hash))) {
        stop(paste0("Couldn't match function ", fn_name,
                    " to a known prediction function. Current options are ",
                    "`lm` and `glm`."))
    }
    pred_sym <- sym(pred_hash[[fn_name]])
    new_form_call_expr <- form_call_expr
    new_form_call_expr[[1]] <- pred_sym
    return(eval(new_form_call_expr, orig_env))
    ##return(eval(new_form_call_expr)) TODO: CHECK IF THAT IS NOT ENOUGH
}


#' Generates a function that generates a learner from a formula
#'
#' `lm.pred` takes a formula and returns a function that, when provided
#' data, returns a predictor function. See examples.
#'
#' The function can be used to generate a pair of predictors for use with
#' the `gob` function, e.g.
#'
#' > gob(Y~X, Z, lm.pred(Y~X))
#'
#' However, the `gob` function provides a shorthand for this. The above
#' call is exactly equivalent to:
#'
#' > gob(lm(Y~X), Z)
#'
#' Behind the scenes, the argument lm(Y~X) is not evaluated: instead, the
#' function name `lm` is matched to `lm.pred`, and the formula is extracted.
#' 
#' @param form A formula.
#'
#' The formula will be passed without changes to the `lm` function, so it must
#' be compatible with it.
#' 
#' @return A list of two functions
#'
#' The functions returned have two arguments -- a response vector `Y` and covariates
#' `X` (which may be a vector a dataframe) -- and they each return a predictor function;
#' that is, a function that takes a covariate value and returns a prediction. The
#' first function of the list returns a function that learns a predictor for the
#' control potential outcomes, while the second returns a function that learns a
#' predictor for the treatment potential outcomes.
#' @examples
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N)
#' Z <- sample(c(0,1), size=N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5*Z 
#' mu_hat_ls <- lm.pred(Y~X)
#' mu_hat_0 <- mu_hat_ls[[1]](Y[Z==0], X[Z==0])
#' mu_hat_1 <- mu_hat_ls[[2]](Y[Z==1], X[Z==1])
#' mu_hat_0(0.2) # 1.04
#' mu_hat_1(0.1) # 1.52
#' @export
lm.pred <- function(form) {
    ##res.lm <- lm(form, dat)
    form.expr <- enexpr(form)
    outcome.expr <- form.expr[[2]]
    build.pred.fn <- function(Y, X) {
        dat.Y <- data.frame(Y)
        names(dat.Y) <- as_string(outcome.expr) 
        if(is.vector(X)) {
            if(length(form.expr) != 3) {
                stop("invalid formula for vector")
            } else {
                dat.X <- data.frame("X" = X)
                if(as_string(form.expr[[3]]) != ".") {
                    names(dat.X) <- as_string(form.expr[[3]])
                }
            }
        } else {
            # X is a dataframe
            dat.X <- X
        }
        dat <- cbind(dat.Y, dat.X) # combined df with correct names
        reg_res <- lm(form, dat)

        pred_res <- .build_pred_fn(reg_res, names(dat.X))
        return(pred_res)
##        res <- function(x){
##            if(is.data.frame(x)) {
##                as.numeric(predict(lm.res, newdata=x))
##            } else {
##                new.dat.X <- data.frame("X" = x)
##                names(new.dat.X) = names(dat.X)
##                as.numeric(predict(lm.res, newdata=new.dat.X))
##            }            
##        }        
##        return(res)
    }
    return(c(build.pred.fn, build.pred.fn))
}

#' To be implemented soon.
#'
#' @param form A formula
#' @param ... Additional arguments to be passed to the glm function. See examples
#' @return A list of two functions.
#' @examples
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N)
#' Z <- sample(c(0,1), size=N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5*Z
#' mu_hat_ls <- glm.pred(Y~X, family=gaussian) 
#' @export
glm.pred <- function(form, ...) {
    outcome.expr <- form[[2]]
    build.pred.fn <- function(Y, X) {
        dat.Y  <- data.frame(Y)
        names(dat.Y) <- as_string(outcome.expr)
        if(is.vector(X)) {
            if(!is_one_predictor_formula(form)){
                stop(paste0("The formula provided: ", formula_to_string(form),
                            ", is incompatible with argument X being a vector"))
            } else {
                dat.X <- data.frame("X" = X)
                names(dat.X) <- get_predictor_names(form)
            }
        } else {
            ## X is a dataframe. No further processing needed.
            dat.X <- X
        }

        ## when running the glm, it's important that ... contain *named* arguments,
        ## if any.
        dat <- cbind(dat.Y, dat.X)
        reg_res <- glm(form, data=dat, ... )

        pred_res <- .build_pred_fn(reg_res, names(dat.X))
        return(pred_res)
    }    
        
    return(c(build.pred.fn, build.pred.fn))
}


.build_pred_fn <- function(reg_res, pred_names) {
    res <- function(x) {
        if(is.data.frame(x)) {
            ## no preprocessing needed
            return(as.numeric(predict(reg_res, newdata=x)))
        } else {
            new.dat.X <- data.frame("X" = x)
            names(new.dat.X) = pred_names
            return(as.numeric(predict(reg_res, newdata=new.dat.X)))
        }
    }
    return(res)
}
