
is_one_predictor_formula <- function(form) {
    if(length(form[[3]]) != 1) {
        return(FALSE)
    }
    if(as_string(form[[3]]) == ".") {
        return(FALSE)
    }

    return(TRUE)
}

is_dot_predictor_formula <- function(form) {    
    if(length(form[[3]]) == 1 && as_string(form[[3]]) == ".") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

get_predictor_names <- function(form) {
    if(is_one_predictor_formula(form)) {
        return(as_string(form[[3]]))
    }
    
    rec <- function(sub_form, acc) {
        if(is_symbol(sub_form)) {
            return(c(as_string(sub_form), acc))
        } else {
            ## sub_form is of the form A + ...           
            return(rec(sub_form[[2]], c(as_string(sub_form[[3]]), acc)))
        }
    }
    
    return(rec(form[[3]], NULL))
}

#' @importFrom magrittr %>%
build_predictor_dataframe <- function(form) {
    form_env <- get_env(form)
    predictor_names <- get_predictor_names(form)
    pred_df <-
        predictor_names %>%
        lapply(function(name) form_env[[name]]) %>%
        do.call("cbind", .) %>%
        magrittr::set_colnames(predictor_names) %>%
        as.data.frame

    return(pred_df)
}

build_predictor_dataframe_from_data <- function(form, data) {
    predictor_names <- get_predictor_names(form)
    
}

