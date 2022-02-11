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
#' Convert a value considered to be an missing value to the standard `NA`
#'
#' @param x a vector, matrix or a data frame
#' @param missing a non-standard missing value
#' @return an object where a non-standard missing value is replaced by `NA`
#'
#' @examples
#' x = c("foo", "bar", "missing")
#' phyloRNA:::missing_to_na(x, "missing")
missing_to_na = function(x, missing){
    x[x == missing] = NA
    x
    }

#' Check if all specified files exist
#'
#' This function checks the existence of all files stored in a vector or a list.
#'
#' `all_files_exist` is a simple shorthand around [`base::file.exists`] wrapped with an additional
#' [`base::all`] to return a single value. This makes this function useful when checking that all
#' files required for or created by some function exist.
#'
#' @param x a vector or list of files.
#' @return a logical value indicating if all files exist.
#'
#' @examples
#' files = c(tempfile(), tempfile())
#' all_files_exist(files) # FALSE
#' file.create(files)
#' all_files_exist(files) # TRUE
#'
#' @export
all_files_exist = function(x){
    all(file.exists(unlist(x)))
    }


#' Read a column from a delimited file
#'
#' Read a k-th column from a delimited file.
#' @param x a path to a file
#' @param k **optional** a integer value of a column that will be returned
#' @param sep **optional** a separator that separate the columns
#' @return a character vector
read_column = function(x, k=1L, sep=" "){
    x = readLines(x)
    vapply(x, function(y){strsplit(x, split=sep)[[1]][k]}, character(1), USE.NAMES=FALSE)
    }
