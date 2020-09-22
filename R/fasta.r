#' Transform matrix into a fasta format
#'
#' Concatenate a character matrix rows or colums into an alignment in a fasta format.
#'
#' Each cell of the character matrix must have length of the string exactly 1, otherwise
#' different size of the sequences is produced.
#'
#' @param data a character matrix
#' @param margin **optional** whether rows (1) or columns (2) should be concatenated
#' @param file **optional** write the fasta file into the specified file instead
#' @return a character vector
#'
#' @export
fasta = function(data, margin=1, file=NULL){
    if(anyNA(data))
        stop("Data must not contain unknown values (NA).")

    data = apply(data, margin, function(x) paste0(x, collapse=""))
    data = paste0(">", names(data), "\n", data)

    if(!is.null(file) && is.character(file)){
        writeLines(data, file)
        return(invisible(data))
        }

    return(data)
    }
