#!/usr/bin/env python3
"""Filter out contigs in a gtf file according to fai file"""
import argparse
import os

def parse_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
                description="Filter out contigs in a gtf file according to fai file"
                )
    parser.add_argument("gtf", type=str, help="an input GTF file")
    parser.add_argument("fai", type=str, help="an input fasta index file")
    parser.add_argument("--output", type=str, help="an output filtered GTF file")
    args = parser.parse_args()
    return args


def filter_gtf():
    """Filter out gtf file according to fasta index file"""
    args = parse_args()

    if not args.output:
        args.output = os.path.splitext(args.fai)[0] + ".gtf"

    chromosomes = get_chromosomes(args.fai)

    with open(args.gtf, "r") as gtf, \
         open(args.output, "w") as gtfo:
        for line in gtf:
            if line[0] == "#":
                gtfo.write(line)
            elif line.split("\t")[0] in chromosomes:
                gtfo.write(line)
            else:
                pass


def get_chromosomes(fai):
    """Get chromosome information from fasta index file"""
    with open(fai, "r") as file:
        text = file.readlines()
    chromosomes = [line.split("\t")[0] for line in text]
    return chromosomes


if __name__ == "__main__":
    filter_gtf()
