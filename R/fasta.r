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
fasta = function(data, margin=2, file=NULL){
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


#' Read fasta file
#'
#' Read a fasta file and return a named vector of sequences.
#'
#' @param file a fasta file
#' @return a named vector of sequences
#'
#' @examples
#' # Construct sequences as a named vector
#' seq1 = as.character(1:5)
#' names(seq1) = letters[1:5]
#'
#' # Write sequences in a fasta format
#' fasta = tempfile()
#' writeLines(paste0(">", names(seq1), "\n", seq1), fasta)
#' # or alternatively, write_fasta(seq1)
#'
#' # Read sequences with read_fasta
#' seq2 = read_fasta(fasta)
#' identical(seq1, seq2)
#'
#' @seealso
#' [write_fasta] to write sequences into a fasta file
#'
#' @export
read_fasta = function(file){
    text = readLines(file)
    starts = grep(">", text)
    stops = c(starts[-1]-1, length(text))

    seq = mapply(
        function(start, stop, text){
            seq = text[(start+1):stop]
            seq = gsub("[:blank:]*", "", seq)
            paste0(seq, collapse="")
            },
        starts, stops, MoreArgs=list(text)
        )
    names(seq) = sub("^>", "", text[starts])

    seq
    }


#' Write fasta file
#'
#' Write a named vector of sequences to a file in a fasta format.
#'
#' @param seq a named vector of sequences
#' @param file an output file
#'
#' @examples
#' # Construct sequences as a named vector
#' seq = as.character(1:5)
#' names(seq) = letters[1:5]
#'
#' # Write sequences to a temp file
#' fasta = tempfile()
#' write_fasta(seq, fasta)
#'
#' identical(paste0(">", names(seq), "\n", seq, collapse="\n"), readLines(fasta))
#'
#' @seealso
#' [read_fasta] to read sequences from a fasta file
#'
#' @export
write_fasta = function(seq, file){
    print(seq)
    if(!inherits(seq, "character"))
        stop("Invalid type. Sequences must be of a character type.")
    if(is.null(names(seq)))
        stop("Names are missing. Sequences must be named vector.")
    if(length(unique(names(seq))) != length(names(seq)))
        stop("Names must be unique for a valid fasta file.")

    text = paste0(">", names(seq), "\n", seq)
    writeLines(text, file)
    }


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

    if(any(nchar(x) != 1))
        stop("The length of the elements in matrix must be 1")

    seq = apply(x, margin, function(y) paste0(y, collapse=""))
    seq
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
