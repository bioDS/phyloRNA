#' Mapping to the Mutect2
#'
#' Mapping to the Genome Analysis ToolKit (GATK) Mutect2 somatic mutation caller.
#'
#' Somatic mutations include short nucleid variations (SNV) and insertion-deletions (inserts).
#'
#' @param bam one or more sam/bam files to where mutations are to be called
#' @template reference
#' @param vcf an output `vcf` file with called mutations
#' @param pon *optional* the Panel of Normals (see [`gatk_make_pon`])
#' @param germline *optional* a vcf file of germline population contaning allelic fractions
#' @param ps *optional* a parameter string passed to `gatk Mutect2` call.
#' @template remake

#' Mapping to the Genome Analysis ToolKit (GATK)
#'
#' A group of functions that map to a functionality of the Broadinstitute's Genome Analysis ToolKit.
#'
#' @param input an input sam/bam or vcf file (in case of Mutect2 calls)
#' @param output an output sam/bam or vcf file (in case of Mutect2 calls)
#' @template reference
#' @template vcf
#' @param table  a table with recalibration information
#' @template remake
#' @name GATK
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
