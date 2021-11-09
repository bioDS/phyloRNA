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
#' `copy_bam` and `copy_bar` parameters are provided. If `TRUE`, the bam file and barcodes are
#' copied to the `outdir` folder as `prefix.bam` and `prefix.barcodes.txt` where `prefix` is the
#' core of the input file (see [corename]). An arbitrary path can also be provided.
#'
#' @template remake
#' @template nthreads
#'
#' @param input a bam file previously mapped by the Cellranger software
#' @param reference a reference genome fasta file (e.g., `.fna`)
#' @param annotation a GTF (`.gtf`) annotation file associated with the reference genome
#' @param outdir a directory where the output will be generated
#' @param fastqdir **optional** a directory for the [bamtofastq], see details
#' @param refdir **optional a directory for the [cellranger_mkref], see details
#' @param countdir **optional** a directory for the [cellranger_count], see details
#' @param id **optional** an id that identifies the sample, this will be embedded in the
#'      read-groups (`RG` tag) of the header and reads of the bam-file. If not specified,
#'      it is derived from the input bam file
#' @param chemistry **optional** a 10X chemistry, use only when the autodetection is failing
#' @param copy_bam **optional** a TRUE value or file path to a location where the re-mapped bam file
#'      will be copied 
#' @param copy_bar **optional** a TRUE value or file path to a location where the file with filtered
#'      barcodes will be copied
#' @param copy_h5 **optional** a TRUE value or file path to a location where the `.h5` expression
#'      count matrix will be copied.
#' @seealso [bamtofastq], [cellranger_mkref] and [cellranger_count] for details on the steps
#'      performed by the `remap` function.
#' @export 
remap = function(
    input, reference, annotation, outdir,
    fastqdir=NULL, refdir=NULL, countdir=NULL,
    id=NULL, nthreads=4, chemistry="auto", remake=FALSE,
    copy_bam=FALSE, copy_bar=FALSE, copy_h5=FALSE
    ){
    prefix = corename(input)

    if(is.null(fastqdir))
        fastqdir = file.path(outdir, "fastq")
    if(is.null(refdir))
        refdir = file.path(outdir, "ref")
    if(is.null(countdir))
        countdir = file.path(outdir, "count")
    if(is.null(id)
        id = prefix

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
        id = id,
        fastqdir = fastqdir,
        refdir = refdir,
        outdir = countdir,
        chemistry = chemistry,
        nthreads = nthreads,
        remake = remake
        )

    cellranger_bam = file.path(countdir, "outs", "possorted_genome_bam.bam")
    cellranger_bar = file.path(countdir, "outs", "filtered_feature_bc_matrix", "barcodes.tsv.gz")
    cellranger_h5 = file.path(countdir, "outs", "filtered_feature_bc_matrix.h5")

    if(isTRUE(copy_bam))
        copy_bam = file.path(outdir, paste0(prefix, ".bam"))
    if(is.character(copy_bam))
        file.copy(cellranger_bam, copy_bam, overwrite=remake)

    if(isTRUE(copy_bar))
        copy_bar = file.path(outdir, paste0(prefix, ".barcodes.txt.gz"))
    if(is.character(copy_bar))
        file.copy(cellranger_bar, copy_bar, overwrite=remake)

    if(isTRUE(copy_h5))
        copy_h5 = file.path(outdir, paste0(prefix, ".h5"))
    if(is.character(copy_bar))
        file.copy(cellranger_h5, copy_h5, overwrite=remake)
    }
