#' Unzip file
#'
#' Extract a single gzipped file using the UNIX \code{gunzip} command.
#'
#' Base R includes tools that can read gzipped files, write gzipped files, extract gzipped archives,
#' but not actually decompress single gzipped file.
#'
#' @param file a compressed file
#' @param preserve whether to keep the compressed file
#' @param overwrite overwrite existing file
#'
#' @export
gunzip = function(file, preserve=FALSE, overwrite=FALSE){
    result = tools::file_path_sans_ext(file)
    if(!overwrite && file.exists(result))
        return(invisible())

    args = c(file, "-f")
    if(preserve) args = c(args, "-k")

    command = getOption("phyloRNA_gunzip")
    systemE(command, args)
    }
