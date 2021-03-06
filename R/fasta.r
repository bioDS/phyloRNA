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
            seq = gsub("[[:blank:]]*", "", seq)
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
    if(!inherits(seq, "character"))
        stop("Invalid type. Sequences must be of a character type.")
    if(is.null(names(seq)))
        stop("Names are missing. Sequences must be named vector.")
    if(length(unique(names(seq))) != length(names(seq)))
        stop("Names must be unique for a valid fasta file.")

    text = paste0(">", names(seq), "\n", seq)
    writeLines(text, file)
    }
