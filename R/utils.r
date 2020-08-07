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
#' corename("/my/path/my.file.ext") # my.file
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
#' abspath("test.txt") # equal to file.path(getwd(), "test.txt")
#' abspath("~/my/stuff/")
#'
#' @export
abspath = function(path){
    if(dir.exists(path)){
        return(tools::file_path_as_absolute(path))
        } else {
        dir = abspath(dirname(path))
        return(file.path(dir, basename(path)))
        }
    }
