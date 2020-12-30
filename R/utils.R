
#' Checks whether the formula has a single predictor (which is not a dot)
#'
#' @param form A formula
#' @return TRUE if formula has a single predictor, and that it is not a dot.
#' FALSE otherwise.
#' @examples
#' \dontrun{
#' is_one_predictor_formula(Y ~ X + Z) # returns: FALSE
#' is_one_predictor_formula(Y ~ .) # returns: FALSE
#' is_one_predictor_formula(Y ~ X) # returns: TRUE
#' }
is_one_predictor_formula <- function(form) {
    if(length(form[[3]]) != 1) {
        return(FALSE)
    }
    if(as_string(form[[3]]) == ".") {
        return(FALSE)
    }

    return(TRUE)
}

#' Checks whether the predictor side of a formula is a dot
#'
#' @param form A formula
#' @return TRUE if predictor side is a dot. FALSE otherwise.
#' @examples
#' \dontrun{
#' is_dot_predictor_formula(Y ~ .) # returns: TRUE
#' is_dot_predictor_formula(Y ~ X + Z) # returns: FALSE
#' }
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
#' \dontrun{
#' formula_to_string(Y ~ X + Y) # returns: "Y ~ X + Y"
#' }
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


#' Extracts the names of the predictors in a formula
#'
#' @param form A formula
#' @return a vector of string representing the names of the predictors
#' @examples
#' \dontrun{
#' get_predictor_names(Y ~ X + Z) # returns: c("X", "Z")
#' get_predictor_names(Y ~ X:C) # returns: c("X", "C")
#' }
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

#' Constructs a dataframe collecting the predictors from a formula
#'
#' `build_predictor_dataframe` searches the environment in which a
#' formula was defined and collects the vectors associated with the
#' predictors of that formula into a dataframe.
#' 
#' @param form A formula.
#'
#' @return A dataframe.
#'
#' Each column of the dataframe is the vector of values associated with a
#' predictor in `form`. 
#'
#' @examples
#' \dontrun{
#' library(RGOB)
#' A <- c(1,2,3,4)
#' X <- c(5,6,7,8)
#' Z <- c(1,0,1,0)
#' build_predictor_dataframe(A ~ X + Z)
#' # returns: 
#' # X Z
#' # 5 1
#' # 6 0
#' # 7 1
#' # 8 0
#' }
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


#' Constructs a dataframe collecting the predictors from a formula
#'
#' `build_predictor_dataframe_from_data` extracts a dataframe containing
#' the predictors in a formula, from a larger dataframe.
#' 
#' @param form A formula.
#' @param data A dataframe.
#' @return A dataframe.
#'
#' Each column of the dataframe is the vector of values associated with a
#' predictor in `form`, as extracted from `data`.
#'
#' If the formula has a dot as a predictor (e.g. Y ~ .), then the entire
#' dataframe `data` is returned minus the column containing the outcome.
#' See the examples.
#'
#' @examples
#' \dontrun{
#' library(RGOB)
#' dat <- data.frame(A = c(1,2,3,4)
#'                   X = c(5,6,7,8)
#'                   Z = c(1,0,1,0))
#' build_predictor_dataframe_from_data(A ~ X + Z, dat)
#' # returns: 
#' # X Z
#' # 5 1
#' # 6 0
#' # 7 1
#' # 8 0
#'
#' build_predictor_dataframe_from_data(A ~ ., dat) # equivalent to previous call.
#' }
#' @importFrom magrittr %>%
build_predictor_dataframe_from_data <- function(form, data) {
    if(!is_dot_predictor_formula(form)) {
        return(data[,get_predictor_names(form), drop=FALSE])
    }
    ## we need to select all entries of data *except* the outcome
    outcome.name <- as_string(form[[2]])
    outcome.idx <- which(names(data) == outcome.name)
    return(data[,-outcome.idx, drop=FALSE])
}


has_data_field <- function(call_form) {
    return("data" %in% names(call_form))
}
