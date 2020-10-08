#' Make a variant call matrix
#'
#' Create a Variant Call Matrix (vcm) by retaining the most frequent base of
#' reads for each cell contained in a bam file and at the position of detected variants
#' contained in the vcf file.
#'
#' This code is a thin wrapper over the Python script `vcm` found in the package's `inst`
#' folder.
#' 
#' This script reads a vcf file and then distribute the variants among the processes.
#' The varchunk, chunksize, adaptive and factor are parameters that govern how will
#' this distribution take place (see description of these parameters for more detail).
#' As scRNAseq are notoriously patchy, variants can take highly varied time dependent
#' on a number of reads mapped to that particular place. For this reason, the default
#' values should provide the best performance. However, there might be situations
#' where their modification would be beneficial.
#'
#'
#' @template remake
#' @template nthreads
#'
#' @param bam an input bam file
#' @param vcf an input vcf file
#' @param barcodes a barcode file
#' @param output **optional an output file. If not specified `<bam>.vcm` in a current directory
#' is used.
#' @param min_coverage **optional** a minimum coverage for a position to not be considered
#' unknown data
#' @param varchunk **optional** By default, the whole vcf file is processed at once.
#' Alternatively, this parameter can be specified to iterate over the vcf file and read
#' and process only a limited number of variants at once. This shouldl improve the performance
#' with a huge amount of cells as it decrease the size of the text that is being written
#' in a file. However, this can also decrease performance if a small number of variants
#' take a long time to process as other processes would wait for these.
#' @param chunksize **optional** Number of variants assigned to a process at once. The variants
#' can be divided into equally sized chunks that are then distributed among processes. Ideally,
#' if the time to process each variant is the same, a larger chunks should be assigned as this
#' reduce multiprocessing overhead. As this is not guranteed for the scRNAseq, only a single
#' variant is send to a process at once by default.
#' @param adaptive **optional** Use an adaptive chunksize calculation instead of a fixed number.
#' The adaptive approach calculates chunksize for each subset of variants that passed the filter.
#' This guarantee that the data are aways divided among the processes equally.
#' @param factor **optional** If an adaptive chunksize is chosen, the variants that passed
#' the filter are divided into a factor\*nthreads of equally sized subsets. Larger factor
#' trades a smaller overhead for a potential scheduling problems.
#' @param message **optional** Print a progress message.
#' @export
vcm = function(
    bam, vcf, barcodes,
    output=NULL, min_coverage=0,
    nthreads=16, remake=FALSE, message=FALSE,
    varchunk=NULL, chunksize=NULL,
    adaptive=FALSE, factor=NULL){
    args = c(
        file.path(find.package("phyloRNA"), "vcm.py"),
        bam, vcf, barcodes,
        "--min_coverage", min_coverage,
        "--nthreads", nthreads
        )

    if(!is_nn(output)) args = c(args, "--output", output)
    if(!is_nn(varchunk)) args = c(args, "--varchunk", varchunk)
    if(!is_nn(chunksize)) args = c(args, "--chunksize", chunksize)
    if(!is_nn(factor)) args = c(args, "--factor", factor)
    if(adaptive) args = c(args, "--adaptive")
    if(remake) args = c(args, "--remake")
    if(message) args = c(args, "--message")

    command = getOption("phyloRNA.python")
    systemE(command, args)
    }
