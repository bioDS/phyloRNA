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
#' @param output **optional** a path for an output bam file
#'
#' @examples
#' \dontrun{
#' bam = GATKR6$new("foo.bam", "bar.fas", "baz.vcf")
#' bam$SortSam()$SplitNCigarReads()$Recalibrate()
#' bam$bam
#' }
#'
#' @seealso [GATK] a group of functions that partially bind to the GATK toolkit,
#' [gatk_prepare] and [gatk_snv] for a convenience functions utilizing the GATK calls
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

        #' @field outdir an output directory for individual methods
        outdir = NULL,

        #' @field remake whether to remake already existing files
        remake = NULL,

        #' @field history a method call history
        history = list(),
       
        #' @description
        #' Create a new GATK object
        #'
        #' @param bam an input bam file
        #' @param outdir **optional** an output directory
        #' @template reference
        #' @template vcf
        #' @template remake
        #'
        #' @return a new `GATK` object
        initialize = function(bam, reference, vcf, outdir=NULL, remake=FALSE){
            stopifnot(is.character(bam), length(bam) == 1)
            stopifnot(is.character(reference), length(reference) == 1)
            stopifnot(is.character(vcf), length(vcf) == 1)

            if(is_nn(outdir))
                outdir = dirname(bam)

            self$bam = bam
            self$original = bam
            self$reference = reference
            self$vcf = vcf
            self$outdir = outdir
            self$remake = remake

            # index vcf file
            gatk_IndexFeatureFile(self$vcf, remake=self$remake)

            mkdir(outdir)

            invisible(self)
            },
 
        #' @description
        #' Sort the SAM/BAM file
        #'
        SortSam = function(output=NULL){
            if(is.null(output)) output = private$get_outfile("sorted")

            gatk_SortSam(self$bam, output, self$remake)
            private$add_to_history()
            self$bam = output

            invisible(self)
            },

        #' @description
        #' Mark duplicates
        #'
        MarkDuplicates = function(output=NULL){
            if(is.null(output)) output = private$get_outfile("dedup")

            gatk_MarkDuplicates(self$bam, output, self$remake)
            private$add_to_history()
            self$bam = output

            invisible(self)
            },

        #' @description
        #' Split the reads around the N in their CIGAR string.
        #'
        SplitNCigarReads = function(output=NULL){
            if(is.null(output)) output = private$get_outfile("split")

            gatk_SplitNCigarReads(self$bam, output, self$reference, self$remake)
            private$add_to_history()
            self$bam = output

            invisible(self)
            },

        #' @description
        #' Recalibrate the base quality score
        #'
        #' @param table **optional** an output path for a new base quality scores
        Recalibrate = function(output=NULL, table=NULL){
            if(is.null(output)) output = private$get_outfile("recal")
            if(is.null(table)) table =
                file.path(self$outdir, paste0(basename(self$bam), ".recal.txt"))

            gatk_BaseRecalibrator(self$bam, self$reference, self$vcf, table, self$remake)
            gatk_ApplyBQSR(self$bam, self$reference, table, output, self$remake)
            private$add_to_history("table"=table)
            self$bam = output

            invisible(self)
            },

        #' @description
        #' Filter the sam/bam file according to tag and its values.
        #'
        #' @param tag a name of the tag that will be used for filtering reads
        #' @param values one or multiple values of a chosen tag
        FilterSamReadsTag = function(tag, values, output=NULL){
            if(is.null(output)) output = private$get_outfile("filtered")

            gatk_FilterSamReadsTag(self$bam, output, tag, values, self$remake)
            private$add_to_history("tag"=tag, "values"=values)
            self$bam = output

            invisible(self)
            }
        ),

    private = list(
        get_outfile = function(add){
            ext = tools::file_ext(self$bam)
            if(tolower(ext) %in% c("sam", "bam")){
                outfile = paste(corename(self$bam), add, ext, sep=".")
                } else {
                outfile = paste(basename(self$bam), add, sep=".")
                }
            file.path(self$outdir, outfile)
            },

        add_to_history = function(...){
            call = sys.call(-1)
            env = parent.frame()
            history = list(
                "call" = call,
                "input" = parent.env(env)$self$bam,
                "output" = env$output
                )

            self$history = c(self$history, list(c(history, list(...))))
            }
        )
    )
