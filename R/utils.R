
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

#' Returns a string version of a formula
#'
#' @param form A formula
#' @return a string representing the formula
#' @examples
#' formula_to_string(Y ~ X + Y) # returns: "Y ~ X + Y" 
formula_to_string <- function(form) {
    rec <- function(form) {
        if(!is.call(form)) {
            return(as_string(form))
        } else {
            return(c(rec(form[[2]]), as_string(form[[1]]), rec(form[[3]])))
        }
    }
   return(paste(rec(form), collapse=" "))
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
    return(data[,get_predictor_names(form)])
}

has_data_field <- function(call_form) {
    return("data" %in% names(call_form))
}
