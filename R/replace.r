#' Replace values in a vector or a matrix
#'
#' Replace the values in `x`, specified by the vector of values
#' `values` with the values in the vector `replace`.
#'
#' @param x an object
#' @param values a vector of values in 'x' to be replaced by values in `replace`
#' @param replace replacement values
#' @param ... further arguments passed to or from other methods
#' @return an object with replaced values
#'
#' @examples
#' vec = c("foo", "bar", "baz")
#' replace(vec, "bar", "spam")
#' replace(vec, c("foo", "bar"), c("bar", "foo"))
#' replace(vec, "spam", "eggs")
#' replace(vec, c("foo", "bar", "baz"), c("spam", "spam", "spam"))
#'
#' @export
replace = function(x, ...){
    UseMethod("replace", x)
    }


#' @rdname replace
#' @export
replace.default = function(x, values, replace, ...){
    if(length(values) != length(replace))
        stop("The vector `values` and `replace` must have the same length!")

    match = match(x, values)
    x[!is.na(match)] = replace[match][!is.na(match)]
    x
    }


#' @rdname replace
#' @export
replace.data.frame = function(x, values, replace, ...){
    x[] = lapply(x, replace.default, values, replace)
    x
    }


#' Replace missing categories on ordinal scale
#'
#' Rescale an ordinal scale to replace missing categories.
#' This will assure that the currently present ordinal categories are sequential.
#'
#' By default `replace_ordinal` function replaces the sorted unique elements in `x` with a numeric
#' series `1:n` where `n` is the number of categories in the data. The type of the object is
#' preserved, e.g., if the object type of `x` is integer vector, matrix or a data.frame containing
#' integers, the replacement series as well as the returning object will be integer as well.
#'
#' If `replace` has length 1 and is numeric (in the sense of `is.numeric`), the replacement
#' series will start with `replace`. Typical usage might be `replace = 0` for a series starting
#' with 0.
#'
#' Alternatively, user can provide vector of replacement values. This vector will be used in
#' provided order and the returned object will have an elements of the requested type. The vector
#' must be at least as long as the number of categories in `x`, but can be larger. This permits
#' replacement with arbitrary categories, such as with the build-in `letters`.
#'
#' @param x an object with ordinal categories
#' @param replace **optional** a positive integer or a vector of replacement values. See `Details`.
#' @return x where categories are replaced with an numeric series of with the provided replacement
#' values
#'
#' @examples
#' foo = c(1, 3, 5)
#' bar = c(1, 2, 3)
#' identical(replace_ordinal(foo), bar)
#'
#' foo = c(1, 3, 5)
#' bar = c(5, 6, 7)
#' identical(replace_ordinal(foo, 5), bar)
#'
#' foo = c(1, 5, 3)
#' bar = c("a", "c", "b")
#' identical(replace_ordinal(foo, letters), bar)
#'
#' @export
replace_ordinal = function(x, replace=NULL){
    categories = sort(unique(as.vector(unlist(x))))

    if(is.null(replace))
        replace = methods::as(seq_along(categories), typeof(categories))
    if(length(replace) == 1 && is.numeric(replace))
        replace = methods::as(seq(from=replace, along.with=categories), typeof(categories))
    if(length(replace) < length(categories))
        stop("Not enough replacement values.")
    replace = replace[seq_along(categories)]

    convert(replace(x, categories, replace), typeof(replace))
    }
