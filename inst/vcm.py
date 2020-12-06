#!/usr/bin/env python3
"""Create a Variant Call Matrix (VFM) by retaining the most frequent base of reads for each cell
contained in a BAM file and at the position of detected variants contained in the VCF file."""
import os
import sys
import collections
import argparse
import textwrap
import itertools
from functools import partial
from multiprocessing import Pool
import pysam


def parse_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent("""\
            Create a Variant Call Matrix (vcm) by retaining the most frequent base of
            reads for each cell contained in a bam file and at the position of detected variants
            contained in the vcf file.
            
            This script reads a vcf file and then distribute the variants among the processes.
            The varchunk, chunksize, adaptive and factor are parameters that govern how will
            this distribution take place (see description of these parameters for more detail).
            As scRNAseq are notoriously patchy, variants can take highly varied time dependent
            on a number of reads mapped to that particular place. For this reason, the default
            values should provide the best performance. However, there might be situations
            where their modification would be beneficial.
            """)
        )
    parser.add_argument("bam", type=str, help="A sam or a bam file that will be parsed")
    parser.add_argument("vcf", type=str, help="A variant calling file to be summarized")
    parser.add_argument("barcodes", type=str, help="File with barcodes")
    parser.add_argument("--output", type=str,
                        help=("An output file. If not specified, a `<bam>.vcm` in a current"
                              " directory is used"))
    parser.add_argument("--min_coverage", type=int, default=0,
                        help="A minimum coverage for a position to not be considered unknown")
    parser.add_argument("--min_mapq", type=int, default=60,
                        help="Minimum mapping quality")
    parser.add_argument("--nthreads", type=intplus, default=4,
                        help="Number of threads (or processes to be precise) to run in paralell")
    parser.add_argument("--varchunk", type=intplus,
                        help="""\
        By default, the whole vcf file is processed at once.
        Alternatively, this parameter can be specified to iterate over the vcf file and read
        and process only a limited number of variants at once. This shouldl improve the performance
        with a huge amount of cells as it decrease the size of the text that is being written
        in a file. However, this can also decrease performance if a small number of variants
        take a long time to process as other processes would wait for these.
        """)
    parser.add_argument("--chunksize", type=intplus, default=1,
                        help="""\
        Number of variants assigned to a process at once. The variants can be divided into
        equally sized chunks that are then distributed among processes. Ideally, if the time to
        process each variant is the same, a larger chunks should be assigned as this reduce
        multiprocessing overhead. As this is not guranteed for the scRNAseq, only a single
        variant is send to a process at once.""")
    parser.add_argument("--adaptive", action="store_true", default=False,
                        help=("Use an adaptive chunksize calculation. The adaptive approach"
                              " calculates chunksize for each subset of variants that passed"
                              " the filter. This guarantee that the data are always divided"
                              " among the processes equally."))
    parser.add_argument("--factor", type=intplus, default=4,
                        help=("If an adaptive chunksize is chosen, the variants that passed"
                              " the filter are divided into a factor*nthreads equally sized"
                              " subsets. Larger factor trades a smaller overhead for a potential"
                              " scheduling problems."))
    parser.add_argument("--remake", action="store_true", default=False,
                        help="Remake files if they already exists.")
    parser.add_argument("--message", action="store_true", default=False,
                        help="Print a progress message")
    args = parser.parse_args()
    return args


Variant = collections.namedtuple("Variant", "contig, pos, ref, start, stop")
Read = collections.namedtuple("Read", "cb, aligned_pairs, query_sequence")

def main():
    """The main loop"""
    args = parse_args()

    if not args.output:
        args.output = vcm_path(args.bam, ".")
    if not args.remake and os.path.isfile(args.output):
        print(f"File {args.output} already exists.")
        return

    barcodes = read_barcodes(args.barcodes)
    index_bam(args.bam, args.nthreads)

    with pysam.VariantFile(args.vcf, "rb", duplicate_filehandle=True) as vcfile, \
        open(args.output, "wt") as vcmfile, \
        Pool(args.nthreads, initializer=init_alignment_file, initargs=[args.bam]) as pool:

        vcmfile.write(vcm_header(barcodes))
        variants_iter = vcfile.fetch()

        process_variant_partial = partial(
            process_variant,
            barcodes=barcodes,
            min_coverage=args.min_coverage,
            min_mapq=args.min_mapq
            )

        while True:
            variants = itertools.islice(variants_iter, args.varchunk)
            variants = list(variants)

            if not variants:
                break # vcf exhausted

            if args.adaptive:
                args.chunksize = calculate_chunksize(args.nthreads, len(variants))

            if args.message:
                print(f"Processing variants: {variants[0].contig}:{variants[0].pos}"
                      f" to {variants[-1].contig}:{variants[-1].pos}")

            variants = filter(passed_filter, variants)
            variants = map(variant2tuple, variants)

            lines = pool.imap(process_variant_partial, variants, chunksize=args.chunksize)
            vcmfile.writelines(lines)


def intplus(value):
    """
    Raises an argparse.ArgumentTypeError if the argument is not a strictly positive integer.

    The main use of this function is as an argparse type for parameter parsing.
    """
    ivalue = int(value)
    if ivalue <= 0:
        raise argparse.ArgumentTypeError(f"{value} is not a strictly positive integer!")
    return ivalue


def process_variant(variant, barcodes, min_coverage=0, min_mapq=60):
    """
    Process a variant for all barcodes.

    A a global variable BAMFILE of type pysam.AlignmentFile, must be defined!
    """
    reads = BAMFILE.fetch(variant.contig, variant.start, variant.stop)
    reads = [read2tuple(read) for read in reads
             if read.has_tag("CB") and read.mapping_quality >= min_mapq]
    bases = [process_barcode(barcode, reads, variant, min_coverage) for barcode in barcodes]
    line = vcm_line(variant, bases)
    return line


def index_bam(bam, nthreads):
    """Index BAM file."""
    index = bam + ".bai"
    if not os.path.isfile(index):
        pysam.index(bam, "-@", str(nthreads))


def init_alignment_file(bam):
    """
    Initialize a global variable BAMFILE as a pointer a pysam.AlignmentFile
    which is opened.
    """
    global BAMFILE
    BAMFILE = pysam.AlignmentFile(bam, "rb", duplicate_filehandle=True)


def variant2tuple(variant):
    """
    Convert a pysam.VariantRecord into a named tuple.

    This conversion is required as the pysam.VariantRecord is an unpickable c-type, which means
    that it cannot be send over to a child process from the main one. Converting to a named tuple
    prevents this issue.
    """
    return Variant(variant.contig, variant.pos, variant.ref, variant.start, variant.stop)


def read2tuple(read):
    """
    Convert a pysam.AlignedSegment into a named tuple.

    This conversion is required to speed up the computation as accessing
    information from a pysam.AlignedSegment is IO bound operation.
    This slows down significantly multiprocessing operation.
    """
    return Read(read.get_tag("CB"), read.get_aligned_pairs(), read.query_sequence)


def calculate_chunksize(n_workers, len_iterable, factor=4):
    """
    Calculate chunksize argument for Pool-methods.

    Resembles source-code within `multiprocessing.pool.Pool._map_async`.
    """
    chunksize, extra = divmod(len_iterable, n_workers * factor)
    if extra:
        chunksize += 1
    return chunksize


def process_barcode(barcode, reads, variant, min_coverage=0):
    """Get the most frequent base for a barcode given reads and variant."""
    reads = get_reads_with_barcode(reads, barcode)
    base = get_most_common_base(reads, variant, min_coverage=min_coverage)
    return base


def read_barcodes(barcodes):
    """Read barcode file"""
    with open(barcodes, "rt") as file:
        text = file.readlines()
    text = [line.rstrip("\n") for line in text]
    return text


def passed_filter(variant):
    """Check whether the variant passed various filtering criterias"""
    return "PASS" in variant.filter


def get_reads_with_barcode(reads, barcode):
    """Filter reads according to their barcode"""
    return [read for read in reads if read.cb == barcode]


def get_most_common_base(reads, variant, unknown="N", min_coverage=0):
    """Calculate the most common base"""
    try:
        bases = [get_base(read, variant.start) for read in reads]
    except VariantReadError as error:
        sys.stderr.write(variant_to_text(variant))
        raise error
    if len(bases) < min_coverage:
        return unknown

    frequencies = collections.Counter(bases)
    if not frequencies:
        return unknown

    most_common = frequencies.most_common(2)
    if most_common[0][0]:
        return most_common[0][0]
    if len(most_common) == 1:
        return unknown
    return most_common[1][0]


def get_base(read, position):
    """Get bases at certain position from selected reads"""
    for pair in read.aligned_pairs:
        if pair[1] == position:
            return read.query_sequence[pair[0]] if pair[0] is not None else None

    sys.stderr.write(read)
    raise VariantReadError("Position was not found in the read. This shouldn't happen!")


def vcm_path(filepath, folder=None):
    """Construct a Variant Frequency File path based on an input name"""
    path = os.path.splitext(filepath)[0]
    if folder:
        path = os.path.basename(path)
        path = os.path.join(folder, path)
    path = os.path.abspath(path + ".vcm")
    return path


def vcm_line(variant, bases):
    """Construct a Variant Call Matrix line"""
    bstring = "\t".join(bases)
    return f"{variant.contig}\t{variant.pos}\t{variant.ref}\t{bstring}\n"


def vcm_header(barcodes):
    """Construct a Variant Frequency File header"""
    bstring = "\t".join(barcodes)
    return f"Contig\tPosition\tReference\t{bstring}\n"


def variant_to_text(variant):
    """Collect relevant details of variant into text string"""
    text = ("Variant (contig, start, stop, position, ref, value):\n"
            f"{variant.contig} {variant.start} {variant.stop} {variant.pos} {variant.ref}\n")
    return text


def mkdir(folder):
    """Create directory"""
    if not os.path.exists(folder):
        os.makedirs(folder)


class VariantReadError(ValueError):
    """Custom exception for when something wrong happens when parsing reads or variants"""
    pass


if __name__ == "__main__":
    main()
