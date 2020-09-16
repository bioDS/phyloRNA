#!/usr/bin/env python3
"""Manipulate with SAM/BAM tags using regular expression"""
import argparse
import re
import pysam


def parse_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description="Manipulate with SAM/BAM tags using regular expression"""
        )
    parser.add_argument("input", type=str, help="an input SAM/BAM file")
    parser.add_argument("output", type=str, help="an output SAM/BAM file")
    parser.add_argument("--tag", type=str, help="tag that will be modified", required=True)
    parser.add_argument("--pattern", type=str, help="a regular expression", required=True)
    parser.add_argument("--replace", type=str, help="a replacement string", required=True)
    args = parser.parse_args()
    return args


def bamtagregex():
    """Manipulate with SAM/BAM tags using regular expression"""
    args = parse_args()
    pattern = re.compile(args.pattern)

    with pysam.AlignmentFile(args.input, "rb") as inbam, \
         pysam.AlignmentFile(args.output, "wb", template=inbam) as outbam:
        for read in inbam.fetch(until_eof=True):
            read_sub_tag(read, args.tag, pattern, args.replace)
            outbam.write(read)


def read_sub_tag(read, tag, pattern, replace):
    """Replace tag value according to the pattern."""
    if read.has_tag(tag):
        value = read.get_tag(tag)
        value = pattern.sub(replace, value)
        read.set_tag(tag, value)


if __name__ == "__main__":
    bamtagregex()
