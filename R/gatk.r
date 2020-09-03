#' Mapping to the Genome Analysis ToolKit (GATK)
#'
#' Group of function that map to a functionality of Broadinstitute's Genome Analysis ToolKit.
#'
#' @param input an input BAM file
#' @param output an output BAM file
#' @param reference a reference fasta (`.fas`) file to which the bam file was mapped
#' @param vcf a vcf file with known polymorphic sites
#' @param table  a table with recalibration information
#' @template remake
#' @name GATK
#' @seealso [GATKR6] a GATK binding in the form of R6 class
NULL


#' @describeIn GATK Mark duplicate reads in SAM/BAM file. These reads are then marked with a SAM tag
#' @export
gatk_MarkDuplicates = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    log = paste0(output, ".txt")
    args = c(
        "MarkDuplicates",
        "-I", input,
        "-O", output,
        "-M", log
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Sort SAM or BAM file according to coordinates.
#' @export
gatk_SortSam = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "SortSam",
        "-I", input,
        "-O", output,
        "-SORT_ORDER", "coordinate"
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Split reads that contains N in their CIGAR string
#' @export
gatk_SplitNCigarReads = function(input, output, reference, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "SplitNCigarReads",
        "-I", input,
        "-O", output,
        "-R", reference
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Recalibrate the base quality score and outputs a table of new recalibrated 
#' values
#' @export
gatk_BaseRecalibrator = function(
    input, reference, vcf, table, remake=FALSE
    ){
    if(!remake && file.exists(table))
        return(invisible())

    args = c(
        "BaseRecalibrator",
        "--input", input,
        "--output", table,
        "--reference", reference,
        "--known-sites", vcf
        )
    
    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Apply the base quality score recalibration according to the recalculated
#' scores from the `[gatk_BaseRecalibrator]` 
#' @export
gatk_ApplyBQSR = function(input, reference, table, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "ApplyBQSR",
        "--input", input,
        "--reference", reference,
        "--bqsr-recal-file", table,
        "--output", output
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Create an index for the Variant Coding File
#' @export
gatk_IndexFeatureFile = function(vcf, remake=FALSE){
    vcf_idx = paste0(vcf, ".idx")
    if(file.exists(vcf_idx))
        return(invisible())

    args = c("IndexFeatureFile", "--input", vcf)

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' @describeIn GATK Filter sam/bam file according to tag and its value
#' @param tag a name of tag
#' @param values one or multiple values of particular tag to keep
#' @export
gatk_FilterSamReadsTag = function(input, output, tag, values, remake=FALSE){
    if(file.exists(output))
        return(invisible())

    values = paste0("--TAG_VALUE", values)
    args = c(
        "FilterSamReads",
        "--INPUT", input,
        "--OUTPUT", output,
        "--TAG", tag,
        values
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }
