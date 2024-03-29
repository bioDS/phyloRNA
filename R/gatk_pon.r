#' Make a Panel of Normals using the GATK toolkit
#'
#' Construct a somatic Panel of Normals according to GATK best practices.
#'
#' @name gatk_pon
#'
#' @param bam one or more input sam/bam files
#' @param reference a reference genome that was used during mapping of the sam/bam files
#' @param vcf a variant call file, either one or more vcf to be merged into a database
#' (for `gatk_GenomicsDBImport`) or an output vcf with Panel of Normals (for `gatk_make_pon` and
#' `gatk_CreateSomaticPanelOfNormals`)
#' @param database a name of the database file created by `gatk_GenomicsDBImport` and used in
#' `gatk_CreateSomaticPanelOfNormals`. 
#' @param outdir **optional** outdir an output directory for intermediate files
#' @param intervals **optional** a character string or a file (one line per file) of genomic
#' segments over which to operate. If not provided, these itnervals are derived from the reference
#' `.fai` file.
#' @template remake
NULL

#' @rdname gatk_pon
#'
#' @details 
#' `gatk_make_pon` is a convenience function that creates the Panel of Normals out of one or
#' multiple `bam` files according to GATK best practices. The input `bam` files are first mapped
#' using the `gatk_Mutect2` in a tumour-only mode, the resulting `vcf` files are merged into
#' a database using the `gatk_GenomicDBIImport` and then a the Panel of Normals is created using the
#' `gatk_CreateSomaticPanelOfNormals`.
#'
#' @export
gatk_make_pon = function(bam, reference, vcf, outdir="pon", intervals=NULL, remake=FALSE){
    if(!remake && file.exists(vcf))
        return(invisible())

    mkdir(outdir)
    vcfs = file.path(outdir, paste0(corename(bam), ".vcf"))
    database = file.path(outdir, "panel_of_normals.db")

    # Create partial for a cleaner calling
    gatk_Mutect2_partial = function(x, y){
        gatk_Mutect2(bam=x, reference=reference, vcf=y, ps="-max-mnp-distance 0", remake=remake)
        }

    Map(gatk_Mutect2_partial, bam, vcfs)

    # Connect them into genomic database
    gatk_GenomicsDBImport(vcfs, reference=reference, database=database, intervals=intervals,
                          remake=remake)
    # Create PON VCF from database
    gatk_CreateSomaticPanelOfNormals(database=database, reference=reference, vcf=vcf)
    }


#' @rdname gatk_pon
#'
#' @details
#' `gatk_GenomicsDBImport` merges multiple variant call files (`vcf`), such as those created by
#' `gatk_Mutect2`, into a database. This database can be used to create a Panel of Normals using
#' the `gatk_CreateSomaticPanelOfNormals` call.
#'
#' @export
gatk_GenomicsDBImport = function(vcf, reference, database, intervals=NULL, remake=FALSE){
    if(!remake && file.exists(database))
        return(invisible())

    if(is.null(intervals)){
        chromosomes = read_column(paste0(reference, ".fai"), sep="\t")
        intervals = tempfile(fileext=".intervals")
        writeLines(chromosomes, intervals)
        on.exit(file.remove(intervals))
        }
        

    args = c(
        "GenomicsDBImport",
        paste("-V", vcf),
        "-R", reference,
        "--genomicsdb-workspace-path", database,
        "--intervals", intervals
        )
    command = getOption("phyloRNA.gatk")
    systemE(command, args)
    }



#' @rdname gatk_pon
#'
#' @details
#' `gatk_CreateSomaticPanelOfNormals` creates a Panel of Normals `vcf` out of a provided database.
#' Panel of Normals are typically created out of multiple guranteed normal samples that use the same
#' same sequencing technology as the tumour sample. Panel of Normals are used in the `gatk_Mutect2`
#' to inform the caller about technological errors and properties and also to filter out
#' pre-existing variants.
#'
#' @export
gatk_CreateSomaticPanelOfNormals = function(database, reference, vcf, remake=FALSE){
    if(!remake && file.exists(vcf))
        return(invisible())

    args = c(
        "CreateSomaticPanelOfNormals",
        "-R", reference,
        "-V", paste0("gendb://", database),
        "-O", vcf
        )
    command = getOption("phyloRNA.gatk")
    systemE(command, args)
    }   
