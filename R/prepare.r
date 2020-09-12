#' Prepare sam/bam file
#'
#' Prepare sam/bam file from scRNAseq for further analysis, such as the SNV detection.
#'
#' @template io
#' @template reference
#' @template vcf
#' @param barcodes **optional** file with cell barcodes to preserve
#' @param outdir **optional** an output directory for intermediate files
#' @template remake
prepare = function(input, output, reference, vcf, barcodes=NULL, outdir=NULL, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    if(!is_nn(outdir))
        outdir = file.path(dirname(output), "gatk")

    bam = GATKR6$new(
        bam = input,
        reference = reference,
        vcf = vcf,
        remake = remake,
        outdir = outdir
        )

    barcodes = readLines(barcodes)

    # Filter barcodes first, this will reduce the total size
    if(!is_nn(barcodes))
        bam$FilterSamReadsTag("CB", barcodes)

    # sort, split cigar, recalibrate:
    bam$SortSam()$SplitNCigarReads()$Recalibrate()
    file.copy(bam$bam, output, overwrite=TRUE)
    }
