#!/usr/bin/env python3
"""Merge Variant Frequency Files (VFFs) into a single SNV table. From each VFF, only the most
frequent base is taken. Additionally, if the coverage (number of reads at particular position)
is lower than some value, or alternatively, if the frequency of the most-frequent base
is lower than some value, N is used instead of that base."""
import os
import os.path
import argparse
from collections import namedtuple
import bitarray as bt

def parse_args():
    """Pare command line arguments"""
    parser = argparse.ArgumentParser(
        description=("Merge Variant Frequency Files (VFFs) by extracting the most frequent base for"
                     " for each contig and position.")
        )
    parser.add_argument("folder", type=str, help="A folder with VFF files")
    parser.add_argument("output", type=str, help="An output table with merged sequences")
    parser.add_argument("--min_coverage", type=int, default=0,
                        help=("Minimum coverage at given position. If the coverage is not reached,"
                              " position is assumed to be unknown and represented as N"))
    parser.add_argument("--min_frequency", type=int, default=0,
                        help=("Minimum base frequency at given position. If the frequency of"
                              " the most frequent base is not reached, position is assumed to be"
                              " unknown and represented as N. This is a more constricting variant"
                              " of the \"min_coverage\" requirement."))
    args = parser.parse_args()
    return args


VFFline = namedtuple(
    "VFFline",
    ["Contig", "Position", "Reference", "A", "C", "T", "G", "Coverage"]
    )

def main(folder, output, min_coverage=0, min_frequency=0):
    # pylint: disable=missing-docstring
    files = os.listdir(folder)
    names = list() # no need to be here
    sequences = list() # no need to be here
    contigs, positions = get_contigs_and_positions_from_vff(os.path.join(folder, files[0]))

    # TODO: move this to function?
    for file in files:
        filepath = os.path.join(folder, file)
        names.append(os.path.splitext(file)[0])
        # TODO: Check that contig and position match those found in the first file!
        sequences.append(get_sequence_from_vff(filepath, min_coverage, min_frequency))

    write_merged_table(output, names, contigs, positions, sequences)


def get_contigs_and_positions_from_vff(path):
    """Get contigs and positions from the VFF file."""
    # Pylint things that the name of the function does not conform to the snake_case naming.
    # I disagree.
    # pylint: disable=invalid-name
    contigs = list()
    positions = list()
    with open(path, "rt") as vff:
        next(vff) # skip header
        for line in vff:
            vffline = parse_vff_line(line)
            contigs.append(vffline.Contig)
            positions.append(vffline.Position)
    return (contigs, positions)


def get_sequence_from_vff(path, min_coverage=0, min_frequency=0):
    """Get an encoded sequence iterator from the VFF file."""
    # TODO: prealocate list
    seq = list()
    with open(path, "rt") as vff:
        # TODO: check if header against fields in VFFline
        header = vff.readline()
        for line in vff:
            # TODO: Check if position is correct
            vffline = parse_vff_line(line)
            base = get_base(vffline, min_coverage, min_frequency)
            seq.append(base)

    seq = get_encoded_iterator(seq)
    return seq


def parse_vff_line(line):
    """Parse a VFF line and returns a named tuple containing all fields."""
    fields = line.rstrip("\n").split("\t")
    if len(fields) != len(VFFline._fields):
        raise ValueError("Parsed line has incorrect number of fields:\n", line)
    return VFFline._make(fields)


def get_base(vffline, min_coverage=0, min_frequency=0):
    """Get the most frequent base in a particular position that passes the minimum coverage
    and frequency criterias."""
    base, frequency = get_max_base_freq(vffline)

    if int(vffline.Coverage) <= min_coverage:
        return "N"
    if frequency <= min_frequency:
        return "N"
    return base


def get_max_base_freq(vffline):
    """Get the most frequent base at its frequency from the VFF line"""
    bases = "ACTG"
    freqs = [int(getattr(vffline, base)) for base in bases]
    freq = max(freqs)
    base = bases[freqs.index(freq, )]
    return (base, freq)


def get_encoded_iterator(items):
    """Encode items with an encoding and return a decoding iterator."""
    encoding = get_encoding()
    encoded = bt.bitarray()
    encoded.encode(encoding, items)
    iterator = encoded.iterdecode(encoding)
    return iterator


def get_encoding():
    """Construct a 3 bit encoding for a 4 standard bases and an unknown position"""
    encoding = {
        "A":bt.bitarray("000"),
        "C":bt.bitarray("001"),
        "T":bt.bitarray("010"),
        "G":bt.bitarray("011"),
        "N":bt.bitarray("100")
        }
    return encoding


def write_merged_table(filepath, names, contigs, positions, sequences):
    """Write a table constructed by merging VFFs"""
    with open(filepath, "wt") as out:
        header = "Contig\tPosition\t" + "\t".join(names) + "\n"
        out.write(header)

        for contig, position in zip(contigs, positions):
            try:
                line = f"{contig}\t{position}\t" \
                    + "\t".join([next(sequence) for sequence in sequences]) \
                    + "\n"
                out.write(line)
            except StopIteration:
                break


if __name__ == "__main__":
    args = parse_args()
    main(**vars(args))
