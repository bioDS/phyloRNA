#' Replace values in a vector or a matrix
#'
#' Replace the values in a vector or a matrix `x`, specified by the vector of values
#' `values` with the values in the vector `replace`.
#'
#' @param x a vector or a matrix
#' @param values a vector of values in 'x' to be replaced by values in `replace`
#' @param replace replacement values
#' @return a vector a a matrix with replaced values
#'
#' @examples
#' vec = c("foo", "bar", "baz")
#' replace(vec, "bar", "spam")
#' replace(vec, c("foo", bar"), c("bar", "foo"))
#' replace(vec, "spam", "eggs")
#' replace(vec, c("foo", "bar", "baz"), c("spam", "spam", "spam"))
#'
#' @export
replace = function(x, values, replace){
    if(length(values) != length(replace))
        stop("The vector `values` and `replace` must have the same length!")

    match = match(x, values)
    x[!is.na(match)] = replace[match][!is.na(match)]
    x
    }


#' Replace missing categories in ordinal scale
#'
#' Rescale an ordinal scale to replace missing categories.
#' This will assure that the categories in the ordinal scale are sequential.
#'
#' @param x a vector or a matrix of categorical variables
#' @return a vector or a matrix where categories are replaced with
#' a sequential numeric vector
#'
#' @examples
#' foo = c(1, 3, 5)
#' bar = c(1, 2, 3)
#' identical(replace.ordinal(foo), bar)
#'
#' @export
replace.ordinal = function(x){
    categories = sort(unique.default(x))
    replace(x, categories, seq_along(categories))
    }
