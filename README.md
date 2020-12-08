# phyloRNA
phyloRNA is an utility package that maps functionality of a commonly used software to easy building phylogenetic trees from scRNAseq data.

## Installation
```
devtools::install_github("biods/phyloRNA")
```

### External software
For full functionality, following external software is also required:

* [cellranger]("https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/what-is-cell-ranger") : analyse 10X scRNA-seq data
* [bamtofastq]("https://support.10xgenomics.com/docs/bamtofastq") : convert previously mapped BAM into fastq files
* [gatk]("https://gatk.broadinstitute.org/hc/en-us") : various BAM processing and variant detection
* [vcftools]("https://vcftools.github.io") : filter out Variant Coding Files (VCF)
* [python3]("https://www.python.org/") : utilize the [pysam]("https://pysam.readthedocs.io") library for following scripts:
  -  `vcm.py`: call scRNA-seq variants
  - `bamtagregex.py`: regex for sam/bam tags
* [gzip]("http://www.gzip.org/") : compress output stream from `vcftools`

Install `cellranger` and `bamtofastq` from respective websites.
To install `gatk`, `vcftools` and `python3`, you might consider the [conda]("conda.io") package manager.
On UNIX-based OS, `gzip` should be already part of your distribution.
Note that cellranger does not support Mac or Windows OS.

Once `python3` is installed, you can install `pysam` by runing:
```
pip3 install pysam
```

## Examples:
Here are but few examples of `phyloRNA` functionality:

#### Prepare `foo.bam` according to GATK best practices:
```
library("phyloRNA")
bam = GATKR6$new("foo.bam", "bar.fas","baz.vcf")
bam$(SortSam()$SplitNCigarReads()$Recalibrate()
```

#### Preprocess 10X expression data:
```
library("phyloRNA")
expr = expr_read10xh5("foo.h5")
expr = expr_quality_filter(expr, minUMI=500, minGene=250)
expr = expr_normalize(expr)
expr = expr_scale(data)
```

#### Remove columns and rows of data matrix with small amount of data
```
library("phyloRNA")
result = densest_subset(foo, density=0.5)

# delted rows
result$deleted_rows

# deleted columns
result$deleted_columns

# filtered matrix:
result$result
```
