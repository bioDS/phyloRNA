#' BAM to fastq
#'
#' Split a BAM aligned using the Cellranger software into individual `.fastq` files.
#' Typically this is done to re-map and re-analyze processed data.
#'
#' @template cellranger
#' @template nthreads
#' @template remake
#'
#' @param bam an input BAM file that was previously mapped using the Cellranger software
#'
#' @export
#'
#' @seealso [cellranger_count] to re-map and re-analyze `.fastq` files.
bamtofastq = function(bam, outdir, nthreads=12, remake=FALSE){
    statusfile = file.path(outdir, "completed")
    if(file.exists(statusfile) && !remake)
        return(invisible())
    if(file.exists(statusfile) && remake)
        unlink(outdir, recursive=TRUE, force=TRUE)
    if(dir.exists(outdir))
        stop("The directory ", outdir, " already exists.")

    args = c(
        paste0("--nthreads=", nthreads),
        bam,
        outdir
        )

    command = getOption("phyloRNA.bamtofastq")
    systemE(command, args=args)
    file.create(statusfile)
    }
