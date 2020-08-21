
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
            dat.X <- X
        }
        dat <- cbind(dat.Y, dat.X)
        lm.res <- lm(form, dat)
        ##print(lm.res)
        res <- function(x){
            if(is.data.frame(x)) {
                as.numeric(predict(lm.res, newdata=x))
            } else {
                new.dat.X <- data.frame("X" = x)
                names(new.dat.X) = names(dat.X)
                as.numeric(predict(lm.res, newdata=new.dat.X))
            }            
        }
        return(res)
    }
    return(c(build.pred.fn, build.pred.fn))
}

#' To be implemented soon.
#'
#' @param form A formula
#' @return A list of two functions.
#' @export
glm.pred <- function(form) {
    return(NULL)
}
