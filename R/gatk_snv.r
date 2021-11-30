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
#' @param bam a sam/bam file prepared for variant calling
#' @template reference
#' @param vcf an output gzipped `vcf` file containing only SNVs
#' @param normal **optional** matched normal samples, either `sam/bam` files or normal sample names.
#' If `sam` or `bam` files are provided, the sample names are retrieved from the files using the
#' `gatk_GetSampleName`.
#' @param pon **optional** Panel of Normals, see [`gatk_make_pon`]
#' @param germline **optional**  a vcf file of germline population contaning allelic fractions
#' @param ps **optional** a parameter string passed to GATK Mutect2 call
#' @template outdir
#' @template remake
#'
#' @seealso
#' [GATK] and [GATKR6] for a binding to indiviual GATK functions,
#' [gatk_prepare] and [gatk_make_pon] for other convenience functions build on GATK calls,
#' [vcftools_filter()] for more information on the `vcftools` call used in this function
#' @export
gatk_snv = function(
    bam, reference, vcf,
    normal=NULL, pon=NULL, germline=NULL, ps=NULL,
    outdir=NULL, remake=FALSE
    ){
    if(!remake && file.exists(vcf))
        return(invisible())

    if(is.null(outdir))
        outdir = file.path(dirname(vcf), "snv")

    mkdir(outdir)    
    core = corename(bam)

    vcf_called = file.path(outdir, paste0(core, ".vcf.gz"))
    vcf_filtered = file.path(outdir, paste0(core, ".filtered.vcf.gz"))
    vcf_snv = file.path(outdir, paste0(core, ".snv.vcf"))

    if(!is.null(normal) && all(endsWith(normal, ".sam") | endsWith(normal, ".bam"))){
        bam = c(bam, normal)
        normal = sapply(normal, gatk_GetSampleName)
        }

    Map(gatk_BuildBamIndex, bam, remake)
    gatk_Mutect2(bam, reference, vcf_called, normal, pon, germline, ps, remake)
    gatk_FilterMutectCalls(vcf_called, reference, vcf_filtered, remake)
    vcftools_filter(vcf_filtered, vcf_snv, remake)
    file.copy(vcf_snv, vcf, overwrite=TRUE)
    }
