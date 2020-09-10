#' Create directory
#'
#' A simple utility function that creates directory recursively. This simulates 
#' the behaviour of the Unix command \code{mkdir -p}.
#' If directory exists, no error or warning is reported, unlike with the base
#' function \code{dir.create} which throws an error.
#'
#' @param dir a directory to be created
#'
#' @export
mkdir = function(dir){
    if(!dir.exists(dir))
        dir.create(dir, recursive=TRUE)
    }


#' Get core part of the file name
#'
#' This is similar to the base function \code{basename}. The difference
#' is that this also removes the file type (or extension) and thus
#' works as a shorthand for many file operations.
#'
#' @param path a path to file
#' @return a core part of the file name
#'
#' @examples
#' corename("foo/bar/file.ext") # file
#' corename("file.ext1.ext2") # file.ext1
#'
#'
#' @export
corename = function(path){
    core = basename(path)
    core = tools::file_path_sans_ext(core)
    core
    }


#' Turn path into an absolute path
#'
#' Turn path into an aboslute path. The file or folder does not have to exists.
#'
#' The \code{tools::file_path_as_absolute} will turn a path into an aboslute path but only if
#' the path exists. This makes it unusable in certain cases, such as when a desired output path
#' needs to be passed to an external call.
#'
#' @param path a relative path
#' @return an absolute path
#'
#' @examples
#' abspath("foo.txt") # equal to file.path(getwd(), "foo.txt")
#' abspath("~/foo/bar/")
#'
#' @export
abspath = function(path){
    if(dir.exists(path) || file.exists(path)){
        return(tools::file_path_as_absolute(path))
        } else {
        dir = abspath(dirname(path))
        return(file.path(dir, basename(path)))
        }
    }


#' Check if value is NULL or NA
#'
#' Returns `TRUE` if object is either `NULL` or `NA`. 
#'
#' The no value `NULL` and missing value `NA` are both used to signify no value.
#' NULL is often used as an empty parameter in functions and `NA` is used for
#' the same reason in the [argparser::argparser] package (because setting value to `NULL`
#' in list and environments means to delete it).
#'
#' For this reason, a missing (unspecified) parameter can can reach function
#' as both `NULL` and `NA`. This is simple shorthand to test both conditions.
#'
#' @param x object to be tested
#' @return `TRUE` if the value is `NULL` or all values are `NA`
#'
#' @examples
#' is_nn(NULL)
#' is_nn(NA)
#' @export
is_nn = function(x){
    is.null(x) || all(is.na(x))
    }

#' Convert a value to NA
#'
#' Convert a value considered to be an unknown value to the standard `NA`
#'
#' @param x a vector, matrix or a data frame
#' @param empty a non-standard unknown value
#' @return an object where a non-standard unknown value is replaced by `NA`
#'
#' @examples
#' x = c("foo", "bar", "unknown")
#' phyloRNA:::empty_to_na(x, "unknown")
empty_to_na = function(x, empty){
    x[x == empty] = NA
    x
    }
