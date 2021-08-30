#' Convert a table into a vector of sequences
#'
#' Convert a data.frame or matrix into a vector of sequences.
#' This is done by concatenating elements either row-wise (margin 1)
#' or column-wise (margin 2).
#'
#' @param x data.frame or matrix, symbols must have length 1
#' @param margin **optional** whether rows (1) or columns (2) should be
#' concatenated
#' @param na **optional** substitute `NA` for a missing/unknown symbol.
#' @return a vector of sequences
#'
#' @examples
#' mat = matrix(letters[1:4],2,2)
#' fasta = tab2seq(mat)
#' identical(fasta, c("ac", "bd"))
#'
#' @seealso
#' [seq2tab] to transform sequences into a table,
#' [read_fasta] to read sequences from a fasta file,
#' [write_fasta] to write sequences into a fasta file
#'
#' @export
tab2seq = function(x, margin=1, na=c("N","-","?")){
    na = match.arg(na)
    x[is.na(x)] = na

    if(any(nchardf(x) != 1))
        stop("The length of the elements in matrix must be 1")

    seq = apply(x, margin, function(y) paste0(y, collapse=""))
    seq
    }


nchardf = function(x){
    if(is.matrix(x))
        return(nchar(x))
    if(is.data.frame(x))
        return(vapply(x, nchar, integer(nrow(x))))
    stop("Unsupported type: only matrix and data.frame are supported")
    }


#' Convert sequences into a table
#'
#' Convert a named vector of sequences into a matrix by splitting up sequences
#' into individual positions.
#'
#' @param x a named vector of sequences, such as from `read_fasta`
#' @param margin **optional** whether rows sequences should be in rows (1)
#' or columns (2)
#' @return a matrix of fasta sequences
#'
#' @examples
#' seq = c("ac", "bd")
#' mat = seq2tab(seq)
#' identical(seq, c("ac","bd"))
#'
#' @seealso
#' [tab2seq] to transform table into a vector of sequences,
#' [read_fasta] to read sequences from a fasta file,
#' [write_fasta] to write sequences into a fasta file
#'
#' @export
seq2tab = function(x, margin=1){
    # match.arg cannot be used on a numeric vector
    if(!margin %in% c(1,2))
        stop("Margin must be either 1 or 2")
        
    data = strsplit(x, "")

    if(margin == 1)
        data = do.call(rbind, data)
    if(margin == 2)
        data = do.call(cbind, data)

    data
    }
