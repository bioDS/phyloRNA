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
#' @param id an id that identifies the sample, this will be embedded in the read-groups (`RG` tag)
#'    of the header and reads of the bam-file.
#' @param fastqdir a dir with fastq files prepared with [bamtofastq]
#' @param refdir a directory with the reference files created by the [cellranger_mkref]
#' @param chemistry **optional** a 10X chemistry, use only when the autodetection is failing
#' @param sample **optional** a file prefix to specify which sample to select,
#'    required if fastqdir contains fastq files for multiple samples
#'
#' @seealso [cellranger_mkref] to create the required reference genome directory (`refdir`)
#'          [bamtofastq] to transform mapped BAM file back into fasta files for remapping
#' @export
cellranger_count = function(
    id, fastqdir, refdir, outdir=".",
    chemistry="auto", nthreads=4, sample=NULL, remake=FALSE
    ){
    # Sets a reasonable defaults and behaviour for outdir
    if(abspath(outdir) == getwd()) # checks for "." "./" etc
        outdir = file.path(outdir, gsub(".", "_", id, fixed=TRUE))

    # define path to useful files: bam, h5 and barcodes
    outfiles = list(
        "bam" = file.path(outdir, "outs", "possorted_genome_bam.bam"),
        "h5" = file.path(outdir, "outs", "filtered_feature_bc_matrix.h5"),
        "barcodes" = file.path(outdir, "outs", "filtered_feature_bc_matrix", "barcodes.tsv.gz")
        )

    statusfile = file.path(outdir, paste0(id, ".completed"))

    # Early exit conditions:
    if(file.exists(statusfile) && !remake && all_files_exist(outfiles))
        return(invisible(outfiles))

    if(file.exists(statusfile) && !remake && !all_files_exist(outfiles))
        stop("The status file exists, but one or more of the output files",
             " (bam, h5 and barcodes) is missing. This suggest that there",
             " might be a problem with the run.")

    if(file.exists(statusfile) && remake)
        unlink(outdir, recursive=TRUE, force=TRUE)

    # Check if outdir and outpath already exists
    if(dir.exists(outdir))
        stop("The directory ", outdir, " already exists.")
        
    # This is the directory where cellranger actually saves files
    outpath = file.path(dirname(outdir), id)

    # If outpath is different for outdir and outpath exists, we need to make outpath unique:
    expanded = FALSE
    if(outdir != outpath && dir.exists(outpath)){
        tempdir = paste0("temp", random_string(8, 4, 4))
        outpath = file.path(dirname(outdir), paste0(tempdir, id))
        expanded = TRUE
        }

    # The outpath should be a unique non-existing directory
    if(dir.exists(outpath))
        stop("The directory ", outpath, " already exists. This is unexpected.")

    args = c(
        "count",
        paste0("--fastqs=", abspath(fastqdir)),
        paste0("--transcriptome=", abspath(refdir)),
        paste0("--id=", id),
        paste0("--localcores=", nthreads),
        paste0("--chemistry=", chemistry)
        )
    if(!is.null(sample))
        args = c(args, paste0("--sample=", sample))

    command = getOption("phyloRNA.cellranger")
    systemE(command, args, call=TRUE, dir=abspath(dirname(outpath)))

    # Rename the output folder if necessary
    if(outpath != outdir)
        file.rename(outpath, outdir)

    # Clean the outpath folder if expanded with tempdir
    if(expanded)
        unlink(dirname(outpath))

    file.create(statusfile)

    invisible(outfiles)
    }
