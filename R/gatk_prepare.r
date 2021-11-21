#' Prepare sam/bam file according to GATK
#'
#' Prepare sam/bam file from scRNAseq according to GATK best practices for further analysis,
#' such as the SNV detection.
#'
#' This is a convenience function that chains individual [GATK] calls to prepare a mapped
#' sam/bam file for further SNV detection. Following steps are performed:
#'
#' * **optional** `FilterSamReadsTag`-- a  barcode fitering step to preserve only high-quality cells
#' * `SortSam` -- sort reads according to coordinates
#' * `SplitNCigarReads` -- split reads around the N position in their CIGAR tag (see the GATK
#' documentation for more information)
#' * `BaseRecalibrator` -- recalibrates the base quality scores after the `SplitNCigarReads`
#' * `ApplyBQSR` -- applies the recalibrated base quality scores from `BaseRecalibrator`
#'
#' Note that in contrast to the GATK best practices for the RNAseq, the `MarkDuplicates`
#' step is not performed as in scRNAseq, all sequences are barcoded.
#'
#' @param barcodes **optional** file with cell barcodes to preserve
#' @template io
#' @template reference
#' @template vcf
#' @template outdir
#' @param clean **optional** clean intermediate files
#' @template remake
#'
#' @seealso [GATK] and [GATKR6] for a binding to individual GATK functions,
#' [gatk_snv] for another convenience function build on GATK calls
#' @export
gatk_prepare = function(
    input, output, reference, vcf,
    barcodes = NULL, outdir = NULL,
    clean = TRUE, remake = FALSE
    ){
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

    # Filter barcodes first, this will reduce the total size
    if(!is_nn(barcodes)){
        barcodes = readLines(barcodes)
        bam$FilterSamReadsTag("CB", barcodes)
        }

    # sort, split cigar, recalibrate:
    bam$SortSam()$SplitNCigarReads()$Recalibrate()

    if(clean)
        bam$clean()

    file.copy(bam$bam, output, overwrite=TRUE)
    file.remove(bam$bam)

    if(clean && dir.exists(outdir) && length(dir(outdir)) == 0)
        unlink(outdir)

    invisible()
    }
