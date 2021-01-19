#' Filter GTF file according to index file
#'
#' Fitler an annotation GTF file according to fasta index file.
#'
#' This might be useful when filtering a GTF so that only chromosomes or scaffolds of interest
#' are present.
#'
#' This code is a thin wrapper over the Python script `filter_gtf.py` found in the package's `inst`
#' folder.
#'
#' @param gtf an input GTF file
#' @param fai an input fasta index file
#' @param output **optional** an output file. If not specified, output is derived from the fasta
#' index file.
#' @export
filter_gtf = function(gtf, fai, output=NULL){
    args = c(
        file.path(find.package("phyloRNA"), "filter_gtf.py"),
        gtf,
        fai
        )

    if(!is_nn(output)) args = c(args, "--output", output)
        
    command = getOption("phyloRNA.python")
    systemE(command, args)
    }
