#' Change sam/bam tag values
#'
#' Change tag value in sam/bam file using regular expression.
#'
#' This code is a thin wrapper over the Python script `bamtagregex.py` found
#' in the package's `inst` folder.
#'
#' The `bamtagregex.py` uses the amazing `pysam` package to manipulate the sam/bam files.
#' The Python's `pysam` package is powered by the `htslib` library. the same library
#' that powers `samtools`.
#' In R, there is a similar package to `pysam`, the `Rsamtools` found in bioconductor. But this
#' library reads the whole file at once (instead of using memory efficient iterators), which
#' makes working with a large bam files less than ideal.
#'
#' @template io
#' @template remake
#' @param tag a tag to modify
#' @param pattern a regex pattern
#' @param replace a string to replace matched pattern
#'
#' @examples
#' \dontrun{
#' # Replace the end of 10x barcode from "-1" to "-baz"
#' bamtagregex("foo.bam", "bar.bam", "CB", "-1", "-baz")
#' }
#'
#' @export
bamtagregex = function(input, output, tag, pattern, replace, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        file.path(find.package("phyloRNA"), "bamtagregex.py"),
        input, output,
        paste0("--tag=", tag),
        paste0("--pattern=", pattern),
        paste0("--replace=", replace)
        )

    command = getOption("phyloRNA.python")
    systemE(command, args)
    }
