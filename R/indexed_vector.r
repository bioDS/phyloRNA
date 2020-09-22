#' Indexed vector
#'
#' @description
#' An indexed vector
#'
#' @details
#' A vector that remembers its original indices.
#'
#'
#'
IndexedVector = R6::R6Class("IndexedVector", list(
    #' @field values vector values
    values = NULL,
    #' @field indices vector indices
    indices = NULL,

    #' @description
    #' Create a new `IndexedVector` object.
    #'
    #' @param values a vector of values
    #' @param indices **optional** vector of indices. If not provided, `seq_along(values)` is used.
    #' @return a new `IndexedVector` object
    initialize = function(values, indices=NULL){
        self$values = vector
        self$indices = indices
        if(is_nn(indices))
            self$indices = seq_along(values)
        if(length(self$values) != length(self$indices))
            stop("Vector and indices must have the same length")
        },

    #' @description
    #' Subset the vector
    #'
    #' @details
    #' This is a standard subsetting, i.e., `1` returns the first element of the vector, not the one
    #' belonging to the first index.
    #'
    #' Note that since `IndexedVector` is an R6 object, this modifies the vector in place.
    #'
    #' @param indices indices specifying elements to extract
    subset = function(indices){
        self$values = self$vector[indices]
        self$indices = self$indices[indices]
        invisible(self)
        },

    #' @description
    #' A minimum of vector
    #'
    #' @details
    #' Method that calculate a minimum of vector values
    #'
    #' @return minimum of vector values
    min = function(){
        min(self$values)
        },

    #' @description
    #' Find indices of minimum values
    #'
    #' @details
    #' Find which values are minimum values. Returns vector current (not original) indices
    #'
    #' @param minimum **optional** min a precalculated minimum.
    #' @return current indices that have the minimum value
    which_min = function(minimum=NULL){
        if(is.null(minimum)) minimum = min(self$values)
        which(self$values == minimum)
        },

    #' @description
    #' Substract values from vector
    #'
    #' @details
    #' Substract values from vector, equivalent to calling `IndexedVector$values - vec`
    #'
    #' @param vec that is substracted from IndexedVector values
    substract = function(vec){
        self$values = self$vector - vec
        invisible(self)
        },

    #' @description
    #' Get length of the vector
    #'
    #' @details
    #' equivalent to calling `length(IndexedVector$values)`
    #'
    #' @return length of the IndexedVector
    len = function(){
        length(self$values)
        },

    #' @description
    #' Sum of the vector values
    #'
    #' @details
    #' equivalent to calling `sum(IndexedVector$values)`
    #'
    #' @return the sum of the IndexedVector
    sum = function(){
        sum(self$values)
        }
    ))
