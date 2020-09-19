#' Generate Cellranger Transcriptome Reference Genome
#'
#' Run `cellranger mkref` and generate the Cellranger Transcriptome Reference Genome.
#'
#' The *reference* which must include associated index file (`fai`) in the same directory.
#'
#' @template cellranger
#' @template remake
#' @template nthreads
#'
#' @param reference a reference genome fasta file (e.g., `.fna`)
#' @param annotation a GTF (`.gtf`) annotation file associated with the reference genome
#'
#' @seealso [cellranger_count] to use this reference directory to perform an expression analysis
#'          [bamtofastq] to transform mapped BAM file back into fasta files for remapping
#' @export
cellranger_mkref = function(reference, annotation, outdir, nthreads=4, remake=FALSE){
    statusfile = file.path(outdir, "completed")
    if(file.exists(statusfile) && !remake)
        return(invisible())
    if(file.exists(statusfile) && remake)
        unlink(outdir, recursive=TRUE, force=TRUE)
    if(dir.exists(outdir))
        stop("The directory ", outdir, " already exists.")
     
    args = c(
        "mkref",
        paste0("--genome=", basename(outdir)),
        paste0("--fasta=", abspath(reference)),
        paste0("--genes=", abspath(annotation)),
        paste0("--nthreads=", nthreads)
        )

    command = getOption("phyloRNA.cellranger")
    systemE(command, args, call=TRUE, dir=abspath(dirname(outdir)))
    file.create(statusfile)
    }


#' Estimate scRNAseq Gene Expression with Cellranger Count
#'
#' Perform mapping, cell estimation, filtering and gene expression analysis using the Cellranger
#' software.
#'
#' @template cellranger
#' @template remake
#' @template nthreads
#'
#' @param fastqdir a dir with fastq files prepared with [bamtofastq]
#' @param refdir a directory with the reference files created by the [cellranger_mkref]
#' @param chemistry **optional** a 10X chemistry, use only when the autodetection is failing
#'
#' @seealso [cellranger_mkref] to create the required reference genome directory (`refdir`)
#'          [bamtofastq] to transform mapped BAM file back into fasta files for remapping
#' @export
cellranger_count = function(fastqdir, refdir, outdir, chemistry="auto", nthreads=4, remake=FALSE){
    statusfile = file.path(outdir, "completed")
    if(file.exists(statusfile) && !remake)
        return(invisible())
    if(file.exists(statusfile) && remake)
        unlink(outdir, recursive=TRUE, force=TRUE)
    if(dir.exists(outdir))
        stop("The directory ", outdir, " already exists.")

    args = c(
        "count",
        paste0("--fastqs=", abspath(fastqdir)),
        paste0("--transcriptome=", abspath(refdir)),
        paste0("--id=", basename(outdir)),
        paste0("--localcores=", nthreads),
        paste0("--chemistry=", chemistry)
        )

    command = getOption("phyloRNA.cellranger")
    systemE(command, args, call=TRUE, dir=abspath(dirname(outdir)))
    file.create(statusfile)
    }
