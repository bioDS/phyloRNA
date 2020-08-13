#' Mapping to the Genome Analysis ToolKit (GATK)
#'
#' Group of function that map to a functionality of Broadinstitute's Genome Analysis ToolKit.
#'
#' @param input an input bam file
#' @param output an output bam file
#' @param reference a reference fasta (`.fas`) file to which the bam file was mapped
#' @param vcf a vcf file with known polymorphic sites
#' @template remake
#' @name GATK
NULL

#' Mark duplicate reads
#'
#' runs the \code{MarkDuplicates} from the picard tools.
#'
#' @param input a BAM file to process
#' @param output a BAM file with marked duplicates
#' @param command a path to picard binary
#' @param remake whether to remake output if it already exists
gatk_mark_duplicates = function(input, output, remake=FALSE){
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


#' Sort reads
#'
#' runs the \code{SortSam} from the picard tools.
#'
#' @param input a BAM file to process
#' @param output a BAM file with sorted reads
#' @param command a path to picard binary
#' @param remake whether to remake output if it already exists
gatk_sort = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "SortSam",
        "-I", input,
        "-O", output
        "-SORT_ORDER", "coordinate"
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' Split reads according to Cigar coding
#'
#' runs SplitNCigarReads from GATK software package
#'
#' @param input a BAM file to process
#' @param output a BAM file with split reads
#' @param reference a reference fasta file to which BAM file was mapped
#' @param command a path to gatk binary
#' @param remake whether to remake output if it already exists
gatk_split_cigar = function(input, output, reference, remake=FALSE){
    if(!remake && file.exists(output))
        return()

    args = c(
        "SplitNCigarReads",
        "-I", input,
        "-O", output,
        "-R", reference
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' Recalibrate quality score
#'
#' runs BaseRecalibrator from the GATK software package
#'
#' @param input a BAM file to process
#' @param reference a reference fasta file to which BAM file was mapped
#' @param vcf a vcf file with known polymorphic sites
#' @param output a table with recalibration information
#' @param command a path to gatk binary
#' @param remake whether to remake output if it already exists
gatk_BaseRecalibrator = function(
    input, reference, vcf, output, remake=FALSE
    ){
    if(!remake && file.exists(output))
        return()

    args = c(
        "BaseRecalibrator",
        "--input", input,
        "--output", output,
        "--reference", reference,
        "--known-sites", vcf
        )
    
    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }


#' Apply base quality score recalibration
#'
#' runs applyBSQR from the GATK software package
#'
#' @param reads a BAM file to recalibrate
#' @param reference a reference fasta file to which BAM file was mapped
#' @param table with quality scores from BaseRecalibrator
#' @param output a BAM file with recalibrated reads
#' @param commandd a path to gatk binary
#' @param remake whether to remake output if it already exists
gatk_ApplyBQSR = function(reads, reference, table, output, remake=FALSE){
    if(!remake && file.exists(output))
        return()

    args = c(
        "ApplyBQSR",
        "--input", reads,
        "--reference", reference,
        "--bqsr-recal-file", table,
        "--output", output
        )

    command = getOption("phyloRNA.gatk")
    systemE(command=command, args=args)
    }
