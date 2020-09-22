#' Remove constant
#'
#' Remove constant rows or columns from matrix.
#'
#' Constant row or column is such column that has only single unique value
#' (beside the unknown elements).
#'
#' @param data a data matrix
#' @param margin **optional** rows (1) or columns (2) that will be explored
#' @param unknown **optional** elements that are excluded from this comparison
#' (do not count among the unique values).
#' @return a data matrix without
#'
#' @export
remove_constant = function(data, margin=1, unknown="N"){
    lengths = apply(data, margin, unique)
    not_constant = sapply(lengths, function(x) sum(! x %in% unknown) > 1)

    if(margin == 1)
        return(data[not_constant,])
    if(margin == 2)
        return(data[, not_constant])
    }

