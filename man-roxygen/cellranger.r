#' @details
#' To mark a succesful run, the function creates a `completed` status file in the output directory.
#' If such file exists and `remake=TRUE`, the output directory is deleted and the output is
#' recreated.
#'
#' However, if the output directory `outdir` exists, but the status file does not, an error
#' is reported. This is to prevent cases when an incorrect existing `outdir` would be delted.
#'
#' @param outdir a non-existing output directory, see details
