
#' Replaces a call to a regression function with a call to the
#' associated prediction function.
#'
#' `match_pred_fn` takes an expression capturing a call to either
#' `lm` or `glm` and replaces that call with a call to
#' `lm.pred` or `glm.pred`.
#'
#' @param form_call_expr An expression containing a call to `lm` or `glm`.
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
#' match_pred_fn(lm_call) # equivalent to: lm.pred(Y ~ X + Z)
#' match_pred_fn(glm_call) # equivalent to: glm.pred(Y ~ X + Z)
#' }
match_pred_fn <- function(form_call_expr) {
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
    ##return(eval(new_form_call_expr, orig_env))
    return(eval(new_form_call_expr)) ##TODO: CHECK IF THAT IS NOT ENOUGH
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
#'
#' @family predictor builders
#' 
#' @export
lm.pred <- function(form) {
    outcome.expr <- form[[2]]
    
    ## The following function takes a vector `Y` and
    ## a vector or dataframe `X`, and returns a 
    ## predictor function based on the linear model.
    build.pred.fn <- function(Y, X) {
        ## Creates a dataframe that can be used in a regression
        dat.ls <- build_regression_data(form, Y, X)
        dat <- dat.ls$dat
        pred_names <- dat.ls$pred_names

        ## runs the regression with the created dataframe
        reg_res <- lm(form, dat)

        ## Constructs and returns a function `pred_res` that takes
        ## a covariate `x` (either a scalar, or a row of a dataframe)
        ## and returns the predicted value from the linear regression.
        pred_res <- .build_pred_fn(reg_res, pred_names)
        return(pred_res)
    }
    
    return(c(build.pred.fn, build.pred.fn))
}

#' Generates a function that generates a learner from a formula
#'
#' Similar to the `lm.pred` function, `glm.pred` takes a formula (and
#' optional additional arguments) and returns a function that, when provided
#' data, returns a predictor function. See examples.
#' 
#' @param form A formula
#' @param ... Additional named arguments to be passed to the glm function.
#'
#' It is important that the arguments be named. Passing positional arguments may
#' lead to errors.
#' 
#' @return A list of two functions.
#' @examples
#' library(RGOB)
#' N <- 100
#' X <- rnorm(N)
#' Z <- sample(c(0,1), size=N, replace=TRUE)
#' Y <- 1 + 0.2 * X + 0.5*Z
#' mu_hat_ls <- glm.pred(Y~X, family=gaussian)
#' 
#' @family predictor builders
#' @export
glm.pred <- function(form, ...) {
    ## The following function takes a vector `Y` and
    ## a vector or dataframe `X`, and returns a 
    ## predictor function based on a generalized linear model.
    build.pred.fn <- function(Y, X) {
        ## Creates a dataframe that can be used in a regression
        dat.ls <- build_regression_data(form, Y, X)
        dat <- dat.ls$dat
        pred_names <- dat.ls$pred_names

        ## runs the regression with the created dataframe
        reg_res <- glm(form, data=dat, ... )

        ## Construct the predictor functions
        pred_res <- .build_pred_fn(reg_res, pred_names)
        return(pred_res)
    }    
        
    return(c(build.pred.fn, build.pred.fn))
}

#' Constructs a dataframe for use in regression functions
#'
#' `build_regression_data` construct a dataframe `dat` from
#' `Y` and `X` for use in a regression such as `lm(form, data=dat)`.
#'
#' The names of the columns of the dataframe constructed must
#' correspond to the variables named in `form`, except in the case
#' where the formula `form` has a dot on the right-hand-side
#' (e.g. Y1 ~ .). If `X` is already a dataframe, not check is
#' done. If it is a vector, then it is converted into a dataframe.
#'
#' @param form A formula.
#' @param Y A vector.
#' @param X A vector or a dataframe.
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#' A <- c(1, 2)
#' B <- c(3, 4)
#' C <- c(5, 6)
#'
#' X <- data.frame(X1 = B, X2 = C)
#' 
#' build_regression_data(Y ~ X1, A, B)
#' # returns:
#' # Y X1
#' # 1 3
#' # 2 4 
#'
#' build_regression_data(Y ~ ., A, B)
#' # returns:
#' # Y X
#' # 1 3
#' # 2 4
#'
#' build_regression_data(Y ~ X1 + X2, A, X)
#' # returns:
#' # Y X1 X2 
#' # 1 3  5  
#' # 2 4  6
#'
#' build_regression_data(Y ~ X1, A, X)
#' # returns:
#' # Y X1 X2 
#' # 1 3  5  
#' # 2 4  6
#' }
#' 
build_regression_data <- function(form, Y, X) {
    ## Creates `dat.Y`
    ## This is a dataframe containing a single
    ## column of outcomes, with the appropriate name
    ## (the name is from `form`... it may not be `Y`)
    dat.Y <- data.frame(Y)
    names(dat.Y) <- as_string(form[[2]])

    ## Creates `dat.X`
    ## A dataframe containing the covariates, with the
    ## correct names, as supplied by `form`. Deals with
    ## both the case where `X` is a vector and the case
    ## where it is a dataframe.

    if(is.vector(X)) {

        ## The formula `form` cannot have more 
        ## than one named predictor, e.g. Y ~ X1 + X2
        if(length(form) != 3) {
            stop(paste0("The formula provided: ", formula_to_string(form),
                        ", is incompatible with argument X being a vector"))
        }

        ## This leaves two possibilities:
        ## (1) Y ~ X1
        ## or
        ## (2) Y ~ .
        ## In the case of (1), the name needs to be taken
        ## from `form`. In the case of (2), the name in
        ## `dat.X` doesn't matter so we call it `X`, which
        ## is arbitrary.        
        if(is_one_predictor_formula(form)) {
            ## The formula is of type (1)
            dat.X <- data.frame("X" = X)
            names(dat.X) <- get_predictor_names(form)
        } else {
            ## The formula is of type (2)
            dat.X <- data.frame("X" = X)
        }        
    } else {
        ## X is already a dataframe
        dat.X <- X
    }

    
    dat <- cbind(dat.Y, dat.X)    
    return(list(dat=dat, pred_names=names(dat.X)))    
}

#' Constructs a prediction function from a regression output
#'
#' @param reg_res The output object from a regression, e.g. `lm` or `glm`
#' @param pred_names The names of the predictors
#' @return A function that takes a scalar or data.frame row and returns
#' a prediction.
.build_pred_fn <- function(reg_res, pred_names) {
    res <- function(x) {
        if(is.data.frame(x)) {
            ## no preprocessing needed
            return(as.numeric(predict(reg_res, newdata=x, type="response")))
        } else {
            new.dat.X <- data.frame("X" = x)
            names(new.dat.X) = pred_names
            return(as.numeric(predict(reg_res, newdata=new.dat.X, type="response")))
        }
    }
    return(res)
}
