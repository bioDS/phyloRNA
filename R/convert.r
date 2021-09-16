#' Convert object or its items into a requested type
#'
#' Convert a type of an object, such as `vector` or `matrix` or items contained in an object for
#' `list` and `data.frame` into a requested type.
#'
#' The `convert` function is a generalized `methods::as()` and its variants `as.character`,
#' `as.numeric, etc., to work for all basic R types and not just vectors. The basic types implied
#' here are `vector`, `matrix`, `data.frame`. Unlike `methods::as`, the object itself is not coerced
#' into a particular class, e.g, matrix won't become a list. Instead a numerical matrix might be
#' converted into a character matrix.
#'
#' @param x any R object
#' @param type R's (internal) storage type of object, such as `"logical"`, `"integer"`, `"double"`,
#' `"complex"`, `"character"` or `"raw"`.
#'
#' @seealso [base::mode()], [base::typeof()], [methods::is()], [methods::as()]
#'
#' @examples
#' # For vectors, convert is identical to as
#' foo = 1:3
#' identical(convert(foo, "character"), as(foo, "character"))
#'
#' # For matrices, convert is similar to mode, but a pure function
#' foo = matrix(1:4, 2, 2)
#' bar = foo
#' mode(bar) = "character"
#' identical(convert(foo, "character"), bar)
#'
#' # For data.frames and lists, its convert is equivalent of converting all their items.
#' foo = data.frame("a"=1:2, "b"=c(0.1, 0.2))
#' bar = convert(foo, "character")
#' identical(foo$a, c("1","2"))
#' identical(foo$b, c("0.1", "0.2"))
#'
#' @export
convert = function(x, type){
    UseMethod("convert", x)
    }


#' @rdname convert
#' @export
convert.default = function(x, type){
    if (typeof(x) == type) x else as(x, type)
    }


#' @rdname convert
#' @export
convert.list = function(x, type){
    x[] = lapply(x, function(y){convert(y, type)})
    x
    }


#' @rdname convert
#' @export
convert.data.frame = function(x, type){
    x[] = lapply(x, function(y){convert(y, type)})
    x
    }


#' @rdname convert
#' @export
convert.matrix = function(x, type){
    mode(x) = type
    x
    }
