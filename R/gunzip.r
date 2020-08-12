#' Unzip file
#'
#' Extract a single gzipped file using the UNIX `gunzip` command.
#'
#' Base R includes tools that can read gzipped files, write gzipped files, extract gzipped archives,
#' but not actually decompress single gzipped file. This function is a thin wrapper around the
#' UNIX `gunzip` command.
#̈́'
#' Currently supported extensions are `.gz` and `.z`. Other extensions will throw an error.
#' If the `output` is not specified, the extension is stripped from the file.
#'
#' @param input a compressed file
#' @param output an optional output file. If specified, the input file is preserved
#' irrespective of the `preserve` parameter and extracted to the output file.
#' @param preserve whether to keep the compressed file
#' @param overwrite overwrite existing file
#'
#' @examples
#' \dontrun{
#' # will remove foo.txt.gz and produce foo.txt:
#' gunzip("foo.txt.gz")
#' # preserves foo.txt.gz and produce bar.txt
#' gunzip("foo.txt.gz", "bar.txt")
#' }
#'
#' @export
gunzip = function(input, output=NULL, preserve=FALSE, overwrite=FALSE){
    supported_extensions = c(".gz", ".z")
    if(!tolower(tools::file_ext(input)) %in% supported_extensions)
        stop("Unknown extension. Only following extensions are supported: ", supported_extensions)

    if(!is_nn(output))
        args = c(input, "-c", ">", output)
        # NOTE: using just -c in both cases instead of trying to map to behaviour of gunzip
        # would produce a cleaner code.

    if(is_nn(output)){
        output = tools::file_path_sans_ext(input)
        args = c(input, "-f")
        if(preserve) args = c(args, "-k")
        }        

    if(!overwrite && file.exists(output))
        return(invisible())

    command = getOption("phyloRNA_gunzip")
    systemE(command, args)
    }
