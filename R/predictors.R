
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

#' @export
glm.pred <- function(form) {
    return(NULL)
}
