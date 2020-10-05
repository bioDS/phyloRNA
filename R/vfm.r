#' Make a variant call matrix
#'
#' Create a Variant Call Matrix file (VCM) by a taking the most common base for each barcode
#' according to reads in bam file and variants in a vcf file.
#'
#' This code is a thin wrapper over the Python script `vfm` found in the package's `inst`
#' folder.
#'
#' @template remake
#' @template nthreads
#'
#' @param bam an input bam file
#' @param vcf an input vcf file
#' @param barcodes a barcode file
#' @param output **optional an output file. If not specified `<bam>.vcm` in a current directory
#' is used.
#' @param varchunk **optional** a number of variants processed each iteration. The larger this
#' number is, the more variants are processed at once and distributed among processes. This should
#' reduce multiprocessing overhead and guarantee that there is enough data to send around as not
#' all variants might pass the filter. However, this also means that more information have to be
#' hold in memory, and read and written to a file at once.
#' @param chunksize **optional** a number of variants assigned to a process during each iteration.
#' Larger number reduce multiprocessing overhead but makes scheduling harder.
#' @param adaptive **optional** Use an adaptive chunksize calculation instead of a fixed number.
#' The adaptive approach calculates chunksize for each subset of variants that passed the filter.
#' This guarantee that the data are aways divided among the processes equally.
#' @param factor **optional** If an adaptive chunksize is chosen, the variants that passed
#' the filter are divided into a factor\*nthreads of equally sized subsets. Larger factor
#' trades a smaller overhead for a potential scheduling problems.
#' @param message **optional** Print a progress mesage.
#' @export
vfm = function(
    bam, vcf, barcodes, output=NULL, nthreads=16,
    varchunk=1000, chunksize=1, adaptive=FALSE, factor=4,
    remake=FALSE, message=FALSE){
    args = c(
        file.path(find.package("phyloRNA"), "vcm.py"),
        bam, vcf, barcodes,
        "--nthreads", nthreads,
        "--varchunk", varchunk,
        "--chunksize", chunksize,
        "--factor", factor
        )

    if(!is_nn(output)) args = c(args, "--output", output)
    if(adaptive) args = c(args, "--adaptive")
    if(remake) args = c(args, "--remale")
    if(message) args = c(args, "--message")

    command = getOption("phyloRNA.python")
    systemE(command, args)
    }
