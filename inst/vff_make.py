#!/usr/bin/env python3
"""Create a Variant Frequency File (VFF) by calculating the base frequency of reads contained in a
BAM file at positions the positions of detected variants contained in the VCF file."""
import os
import sys
import collections
import argparse
from functools import partial
from multiprocessing import Pool
import pysam


def parse_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description=("Create a Variant Frequency File (VFF) by calculating the base frequency"
                     "of reads in BAM file at positions contained in the VCF file.")
        )
    parser.add_argument("bam", type=str, help="SAM or BAM file that will be parsed")
    parser.add_argument("vcf", type=str, help="A variatn calling file to be summarized")
    parser.add_argument("--folder", type=str, help="Folder with output VFF file for each barcode")
    parser.add_argument("--vff", type=str, help="An output VFF file, in case barcodes are not used")
    parser.add_argument("--pass_only", action="store_true",
                        help="Filter the VCF so only PASSed variants are considered")
    parser.add_argument("--nthreads", type=int, help="Number of threads to run in paralell")
    parser.add_argument("--remake", action="store_true",
                        help="Remake files if they already exists.")
    group = parser.add_mutually_exclusive_group(required=False)
    group.add_argument("--barcodes", type=str, help="File with barcodes")
    group.add_argument("--barcode", type=str, help="Pass only a single barcode")
    args = parser.parse_args()
    return args

Paths = collections.namedtuple("Paths", "bam vcf folder")
Settings = collections.namedtuple("Settings", "pass_only remake message")

def main(
        bam,
        vcf,
        folder=None,
        vff=None,
        pass_only=True,
        barcodes=None,
        barcode=None,
        nthreads=None,
        remake=False):
    # TODO: too many arguments? Maybe there is a better way to handle this?
    # pylint: disable=too-many-arguments,missing-docstring
    if folder is None:
        folder = os.path.abspath(os.path.splitext(bam)[0])

    mkdir(folder)
    paths = Paths(bam, vcf, folder)
    settings = Settings(pass_only, remake, True)

    if barcode:
        process_barcode(barcode, paths, settings)


    elif barcodes:
        barcodes = read_barcodes(barcodes)
        process_barcode_partial = partial(
            process_barcode,
            paths=paths,
            settings=settings
            )
        with Pool(nthreads) as pool:
            pool.map(process_barcode_partial, barcodes)

    else:
        vff = vff_path(folder, os.path.splitext(bam)[0])
        if remake or not os.path.isfile(vff):
            make_vff(vff, bam, vcf, pass_only)


def read_barcodes(barcodes):
    """Read barcode file"""
    with open(barcodes, "rt") as file:
        text = file.readlines()
    text = [line.rstrip("\n") for line in text]
    return text



def process_barcode(barcode, paths, settings):
    """Process barcode (cell) and create variant frequency file"""
    vff = vff_path(paths.folder, barcode)
    if settings.remake or not os.path.isfile(vff):
        if settings.message:
            print("Processing barcode: ", barcode)
        make_vff(vff, paths.bam, paths.vcf, settings.pass_only, barcode)


def make_vff(vff, bam, vcf, pass_only=True, barcode=None):
    """Make a Variant Frequency File"""
    with pysam.AlignmentFile(bam, "rb", duplicate_filehandle=True) as bamfile, \
        pysam.VariantFile(vcf, "rb", duplicate_filehandle=True) as vcfile, \
        open(vff, "wt")  as vffile:
        vffile.write(vff_header())
        process_variants(vffile, bamfile, vcfile, barcode=barcode, pass_only=pass_only)


def process_variants(vffile, bamfile, vcfile, pass_only=True, barcode=None):
    """Process VCF variants"""
    for variant in vcfile.fetch(reopen=True):
        if pass_only and not passed_filter(variant):
            continue
        # pysam has 0-based indexing
        # VCF file is 1-based indexing (variant.pos)
        # variant.start is the correct position of variant in 0-based indexing
        # Note that with fetch, both start and end must be provided
        reads = bamfile.fetch(variant.contig, variant.start, variant.stop, multiple_iterators=True)
        reads = [read for read in reads]
        if barcode:
            reads = get_reads_with_barcode(reads, barcode)
        frequencies = get_base_frequencies_for_variant(reads, variant)
        vffile.write(vff_line(variant, frequencies))


def passed_filter(variant):
    """Check whether the variant passed various filtering criterias"""
    return "PASS" in variant.filter


def get_reads_with_barcode(reads, barcode):
    """Filter reads according to their barcode"""
    return [read for read in reads if (read.has_tag("CB") and read.get_tag("CB") == barcode)]

def get_base_frequencies_for_variant(reads, variant, bases="ACGT"):
    """Calculate the base frequency for variant for selected reads"""
    # According to pyling, the function name doesn't confirm to the snake-case.
    # I disagree.
    # pylint: disable=invalid-name
    try:
        read_bases = [get_bases(read, variant.start) for read in reads]
    except VariantReadError as error:
        sys.stderr.write(variant_to_text(variant))
        raise error
    frequencies = empty_counter(bases)
    frequencies.update(read_bases)
    return frequencies


def empty_counter(bases="ACGT"):
    """Initialize an empty counter with particular items"""
    return collections.Counter(dict.fromkeys(bases, 0))


def get_bases(read, position):
    """Get bases at certain position from selected reads"""
    for pair in read.get_aligned_pairs():
        if pair[1] == position:
            return read.query_sequence[pair[0]] if pair[0] is not None else None

    sys.stderr.write(read_to_text(read))
    raise VariantReadError("Position was not found in the read. This shouldn't happen!")


def vff_base_frequency_string(bases, frequencies):
    """Construct a string of base frequencies for VFF file"""
    freqlist = [frequencies[base] for base in bases]
    freqlist.append(sum(frequencies.values()))
    return "\t".join(map(str, freqlist))


def vff_path(folder, name):
    """Construct a Variant Frequency File path"""
    path = os.path.join(folder, name) + ".vff"
    path = os.path.abspath(path)
    return path


def vff_line(variant, frequencies, bases="ACGT"):
    """Construct a Variant Frequency File line"""
    bstring = vff_base_frequency_string(bases, frequencies)
    return f"{variant.contig}\t{variant.pos}\t{variant.ref}\t{bstring}\n"


def vff_header(bases="ACTG"):
    """Construct a Variant Frequency File header"""
    bstring = "\t".join(bases)
    return f"Contig\tPosition\tReference\t{bstring}\tTotal\n"


def variant_to_text(variant):
    """Collect relevant details of variant into text string"""
    alts = "".join(variant.alts)
    text = ("Variant (contig, start, stop, position, ref, value):\n"
            f"{variant.contig} {variant.start} {variant.stop} {variant.pos} {variant.ref} {alts}\n")
    return text


def read_to_text(read):
    """Collect relevant details of read into text string"""
    text = ("Read (contig, from, to):\n"
            f"{read.reference_name} {read.reference_start} {read.reference_end}\n"
            f"Pairs: {read.get_aligned_pairs()}\n"
            f"Sequence: {read.query_sequence}\n")
    return text


def mkdir(folder):
    """Create directory"""
    if not os.path.exists(folder):
        os.makedirs(folder)


class VariantReadError(ValueError):
    """Custom exception for when something wrong happens when parsing reads or variants"""
    pass


if __name__ == "__main__":
    args = parse_args()
    main(**vars(args))
