#' Make a varian frequency files
#'
#' Calculate the base frequency for every cell barcode in the bam file for every variant in the vcf.
#'
#' This code is a thin wrapper over the Python script `vff_make` found in the package's `inst`
#' folder.
#'
#'
#'
#' @param bam an input bam file
#' @param vcf an input vcf file
#' @param barcodes **optional** a barcode file
#' @param folder **optional** an output folder
#' @param nthreads **optional** number of threads to run in parallel
#'
#' @export
vff_make = function(bam, vcf, barcodes=NULL, folder=NULL, nthreads=16){
    args = c(
        file.path(find.package("phyloRNA"), "vff_make.py"),
        bam, vcf,
        "--pass_only",
        "--nthreads", nthreads
        )

    if(!is_nn(barcodes)) args = c(args, "--barcodes", barcodes)
    if(!is_nn(folder)) args = c(args, "--folder", folder)

    command = getOption("phyloRNA.python")
    systemE(command, args)
    }

#' Merge variant frequency files
#'
#' Merge the variatn frequency files previously created by the [vff_make] and outputs an alignment
#' table with the most frequent base for every cell at each position.
#'
#' This code is a thin wrapper over the Python script `vff_merge.py` found in the package's `inst`
#' folder.
#'
#' @param folder a folder with the variant frequency files
#' @param output a path to an output table
#'
#' @export
vff_merge = function(folder, output){
    args = c(
        file.path(find.package("phyloRNA"), "vff_merge.py"),
        folder, output
        )

    command = getOption("phyloRNA.python")
    systemE(command, args)
    }
