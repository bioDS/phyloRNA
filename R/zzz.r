#' phyloRNA: A package for phylogenetical analysis of RNA data
#'
#' A package with functions to facilitate creating pipelines for the phylogenetic analysis
#' of RNA, especially single-cell RNA, data.
#'
#' @section External software:
#' The phyloRNA package performs a number of calls to external programs. By default, the package
#' assumes that the path to the binaries is in the PATH environment variable and so that calling
#' only the name of the program is required (such as `samtools` instead of `path/to/samtools`).
#' If this is not true or the binary or symlink to it exists under a different name, you might
#' need to modify following [options()]:
#' \itemize{
#'     \item `phyloRNA_cellranger` a path to the 10x Genomic's cellranger software
#'     \item `phyloRNA_gatk` a path to the Broad Institute Genome Analysis Toolkit (GATK)
#'     }
#' @docType package
#' @name phyloRNA
NULL

phyloRNA_options = list(
    phyloRNA.cellranger = "cellranger",
    phyloRNA.bamtofastq = "bamtofastq",
    phyloRNA.gatk = "gatk",
    phyloRNA.vcftools = "vcftools",
    phyloRNA.python = "python3",
    phyloRNA.gunzip = "gunzip",
    phyloRNA.gzip = "gzip"
    )


.onLoad = function(libname, pkgname){
    opts = options()
    unset_opts = !(names(phyloRNA_options) %in% names(opts))
    if (any(unset_opts)) options(phyloRNA_options[unset_opts])

    invisible()
    }
