#' Remap BAM file to a new reference genome
#'
#' This function will remap a bam file previously mapped by the Cellranger software to a new genome.
#'
#' The `remap` function calls the [bamtofastq], [cellranger_mkref] and [cellranger_count] to
#' convert the bam file into fastq files, create the cellranger reference genome and perform
#' the expression analysis respectively. See the individual functions for details.
#'
#' The optional directories `fastqdir`, `refdir` and `countdir` are for outputs from the
#' above mentioned [bamtofastq], [cellranger_mkref] and [cellranger_count]. If not specified,
#' folders `fastqdir`, `ref` and `count` in the `outdir` directory are used. If these are
#' undesired, a path to a non-existing directory or directory with a previously existing run
#' must be used. For example when a multiple bam files are being re-mapped to the same reference,
#' all of them would share the same `refdir`.
#' See the [bamtofastq], [cellranger_mkref] and [cellranger_count] for details.
#'
#' Given the intended usage is to obtain a remapped bam file and a list of filtered barcodes,
#' `cbam` and `cbar` parameters are provided. If `TRUE`, the bam file and barcodes are copied
#' to the `outdir` folder as `prefix.bam` and `prefix.barcodes.txt` where `prefix` is the core
#' of the input file (see [corename]). An arbitrary path can also be provided.
#'
#' @template remake
#' @template nthreads
#'
#' @param input a bam file previously mapped by the Cellranger software
#' @param reference a reference genome fasta file (e.g., `.fna`)
#' @param annotation a GTF (`.gtf`) annotation file associated with the reference genome
#' @param outdir a directory where the 
#' @param fastqdir an optional directory for the [bamtofastq], see details
#' @param refdir an optional directory for the [cellranger_mkref], see details
#' @param countdir an optional directory for the [cellranger_count], see details
#' @param cbam a TRUE value or file path to a location where the re-mapped bam file will be copied 
#' @param cbar a TRUE value or file path to a location where the file with filtered barcodes
#' will be copied
#' @seealso [bamtofastq], [cellranger_mkref] and [cellranger_count] for details on the steps
#' performed by the `remap` function.
#' @export 
remap = function(
    input, reference, annotation, outdir,
    fastqdir=NULL, refdir=NULL, countdir=NULL,
    nthreads=4, remake=FALSE,
    cbam=FALSE, cbar=FALSE
    ){
    if(is_nn(refdir))
        refdir = file.path(outdir, "ref")
    if(is_nn(countdir))
        countdir = file.path(outdir, "count")
    if(is_nn(fastqdir))
        fastqdir = file.path(outdir, "fastq")

    mkdir(outdir)

    bamtofastq(
        input, fastqdir,
        nthreads = nthreads,
        remake = remake
        )
    cellranger_mkref(
        reference = reference,
        annotation = annotation,
        outdir = refdir,
        nthreads = nthreads,
        remake = remake
        )
    cellranger_count(
        fastqdir = fastqdir,
        refdir = refdir,
        outdir = countdir,
        nthreads = nthreads,
        remake = remake
        )

    prefix = corename(input)
    cellranger_bam = file.path(countdir, "outs", "possorted_genome_bam.bam")
    cellranger_bar = file.path(countdir, "outs", "filtered_feature_bc_matrix", "barcodes.tsv.gz")

    if(isTRUE(cbam))
        cbam = file.path(outdir, paste0(prefix, ".bam"))
    if(is.character(cbam))
        file.copy(cellranger_bam, cbam, overwrite=remake)

    if(isTRUE(cbar))
        cbar = file.path(outdir, paste0(prefix, ".barcodes.txt"))
    if(is.character(cbar))
        gunzip(cellranger_bar, cbar, overwrite=remake)
    }
