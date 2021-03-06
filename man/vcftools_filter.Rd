% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcftools.r
\name{vcftools_filter}
\alias{vcftools_filter}
\title{Filter VCF file}
\usage{
vcftools_filter(input, output, remake = FALSE)
}
\arguments{
\item{input}{an input VCF file}

\item{output}{an output VCF file}

\item{remake}{\strong{optional} remake the output if it already exists}
}
\description{
Filter a VCF file using the external \code{vcftools} software, removing indels and keeping
keeping only those SNVs that passed all filters.
}
