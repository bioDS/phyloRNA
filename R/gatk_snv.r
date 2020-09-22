#' Identify and filter SNVs
#'
#' Identify and filter single nucleotide variations in an input bam file.
#'
#' This is a convenience function that chains individual [GATK] calls to identify and filter
#' SNVs. Following steps are performed:
#'
#' * `Mutect2` -- calls variant using the GATK Mutect2 variant caller
#' * `FilterMutectalls` -- filter the raw output from Mutect2
#' * `vcftools` -- a non GATK call to preserve only single nucleotide variants
#'
#' @param input a sam/bam file prepared for variant calling
#' @param output an output gzipped VCF file containing only SNVs
#' @template reference
#' @template outdir
#' @template remake
#'
#' @seealso [GATK] and [GATKR6] for a binding to indiviual GATK functions,
#' [gatk_prepare] for another convenience function build on GATK calls,
#' [vcftools_filter()] for more information on the `vcftools` call used in this function
#' @export
gatk_snv = function(input, reference, output, outdir=NULL, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    if(!is_nn(outdir))
        outdir = file.path(dirname(output), "snv")

    mkdir(outdir)    
    core = corename(input)

    vcf = file.path(outdir, paste0(core, ".vcf.gz"))
    vcf_filtered = file.path(outdir, paste0(core, ".filtered.vcf.gz"))
    vcf_snv = file.path(outdir, paste0(core, ".snv.vcf.gz"))

    gatk_BuildBamIndex(input, remake)
    gatk_Mutect2(input, reference, vcf, remake)
    gatk_FilterMutectCalls(vcf, reference, vcf_filtered, remake)
    vcftools_filter(vcf_filtered, vcf_snv, remake)
    file.copy(vcf_snv, output, overwrite=TRUE)
    }
