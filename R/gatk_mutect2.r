#' Mapping to the Mutect2
#'
#' Mapping to the Genome Analysis ToolKit (GATK) Mutect2 somatic mutation caller.
#'
#' Somatic mutations include short nucleid variations (SNV) and insertion-deletions (inserts).
#'
#' @param bam one or more sam/bam files to where mutations are to be called and optionally
#' matched normal samples when `Mutect2` is called in the "Tumor with Matched Normal" mode
#' @template reference
#' @param vcf an output `vcf` file with called mutations
#' @param normal **optional** sample names (`RG` sam/bam tag) of the matched normal samples 
#' @param pon **optional** the Panel of Normals (see [`gatk_make_pon`])
#' @param germline **optional** a vcf file of germline population contaning allelic fractions
#' @param ps **optional** a parameter string passed to `gatk Mutect2` call.
#' @template remake
#' @name gatk_mutect2
#' @seealso
#' [GATK] a GATK binding
#' [GATKR6] a GATK binding in the form of R6 class
#' [gatk_snv] a convenience function simplifying SNV calling with `gatk_Mutect2`
#' [gatk_prepare] and [gatk_make_pon] for other convenience functions utilizing the GATK calls
#' @export
gatk_Mutect2 = function(
    bam, reference, vcf,
    normal=NULL, pon=NULL, germline=NULL,
    ps=NULL, remake=FALSE
    ){
    if(!remake && file.exists(vcf))
        return(invisible())
    args = c(
        "Mutect2",
        paste("-I", bam), # for multiple bam files
        "-R", reference,
        "-O", vcf,
        "--disable-adaptive-pruning", # suggested for RNA
        ps
        )

    if(!is.null(normal))
        args = c(args, paste("-normal", normal))
    if(!is.null(pon))
        args = c(args, "--panel-of-normals", pon)
    if(!is.null(germline))
        args = c(args, "--germline-resource", germline)

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }
