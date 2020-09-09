#' GATK binding as an R6 class
#'
#' @description
#' A GATK binding in an object-oriented form as a R6 class.
#'
#' @details
#' The GATK is predominantly used in a pipeline by chaining individual GATK function calls
#' to clean a bam file and prepare it for further analysis. This GATK class utilize
#' the R6 method chaining to facilitate this usage.
#'
#' @examples
#' \dontrun{
#' bam = GATKR6$new("foo.bam", "bar.fas", "baz.vcf")
#' bam$SortSam()$SplitNCigarReads()$Recalibrate()
#' bam$bam
#' }
#'
#' @seealso [GATK] a group of functions that partially bind to the GATK toolkit
#' @export
GATKR6 = R6::R6Class("GATK",
    public = list(
        #' @field bam a path to a current bam file. Modified during each method call to point to
        #' the result of the last GATK action.
        bam = NULL,

        #' @field original a path to the original (unmodified) bam file
        original = NULL,

        #' @field reference a path to reference genome fasta (`.fas`) file
        reference = NULL,

        #' @field vcf a path to the Variant Coding File (VCF)
        vcf = NULL,

        #' @field remake whether to remake already existing files
        remake = NULL,
       
        #' @description
        #' Create a new GATK object
        #' @param bam an input bam file
        #' @param reference a path to reference genome fasta file (`.fas`).
        #' @param remake whether to remake already existing files
        #' @param vcf a path to the Variant Coding File (VCF)
        #' @return a new `GATK` object
        initialize = function(bam, reference, vcf, remake=FALSE){
            stopifnot(is.character(bam), length(bam) == 1)
            stopifnot(is.character(reference), length(reference) == 1)
            stopifnot(is.character(vcf), length(vcf) == 1)

            self$bam = bam
            self$original = bam
            self$reference = reference
            self$vcf = vcf
            self$remake = remake

            # index vcf file
            gatk_IndexFeatureFile(self$vcf, remake=self$remake)

            insivible(self)
            },
 
        #' @description
        #' Sort the SAM/BAM file
        #' @param output **optional** a path for an output bam file
        SortSam = function(output=NULL){
            if(is.null(output)) output = paste0(self$bam, ".sorted")
            self$sortsam = output

            gatk_SortSam(self$bam, output, self$remake)
            self$bam = output

            invisible(self)
            },

        #' @description
        #' Mark duplicates
        #' @param output **optional** a path for an output bam file
        MarkDuplicates = function(output=NULL){
            if(is.null(output)) output = paste0(self$bam, ".dedup")
            self$markduplicates = output

            gatk_MarkDuplicates(self$bam, self$markduplicates, self$remake)
            self$bam = self$markduplicates

            invisible(self)
            },

        #' @description
        #' Split the reads around the N in their CIGAR string.
        #' @param output **optional** a path for an output bam file 
        SplitNCigarReads = function(output=NULL){
            if(is.null(output)) output = paste0(self$bam, ".split")
            self$splitncigarreads = output

            gatk_SplitNCigarReads(self$bam, self$splitncigarreads, self$reference, self$remake)
            self$bam = self$splitncigarreads

            invisible(self)
            },

        #' @description
        #' Recalibrate the base quality score
        #' @param output **optional** a path for an output bam file
        #' @param table **optional** a path for an output recalibration table
        Recalibrate = function(output=NULL, table=NULL){
            if(is.null(output)) output = paste0(self$bam, ".recal")
            if(is.null(table)) table = paste0(self$bam, ".recal.txt")
            self$recalibrate = output
            self$table = table

            gatk_BaseRecalibrator(self$bam, self$reference, self$table, self$remake)
            gatk_ApplyBQSR(self$bam, self$reference, self$table, self$recalibrate, self$remake)
            self$bam = self$recalibrate

            invisible(self)
            },

        #' @description
        #' Filter the sam/bam file according to tag and its values.
        #' @param tag a name of the tag that will be used for filtering reads
        #' @param values one or multiple values of a chosen tag
        #' @param output **optional** a path for an output bam file
        FilterSamReadsTag = function(tag, values, output=NULL){
            if(is.null(output)) output = paste0(self$bam, ".filtered")
            self$filtersamreadstag = output

            gatk_FilterSamReadsTag(self$bam, self$filtersamreadstag, tag, values, self$remake)
            self$bam = self$filtersamreadstag

            invisible(self)
            }
        )
    )
