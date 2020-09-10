#' Filter VCF file
#'
#' Filter a VCF file using the external `vcftools` software, removing indels and keeping
#' keeping only those SNVs that passed all filters.
#'
#' @param input an input VCF file
#' @param output an output VCF file
#' @template remake
#' @export
vcftools_filter = function(input, output, remake=FALSE){
    if(!remake && file.exists(output))
        return(invisible())

    args = c(
        "--gzvcf", input,
        "--remove-filtered-all",
        "--remove-indels",
        "--recode",
        "--stdout", ">", output
        )

    command = getOption("phyloRNA.vcftools")
    systemE(command=command, args=args)
    }
