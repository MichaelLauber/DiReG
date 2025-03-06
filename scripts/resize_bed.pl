#!/usr/bin/env perl
use strict;
use warnings;
use POSIX qw(floor);

# Usage message
sub usage {
    die "Usage: $0 <input.bed> <size> [shift]\n".
        "  <input.bed> : BED file with at least 3 columns (chrom, start, end)\n".
        "  <size>      : Desired fixed length for each region (e.g., 200)\n".
        "  [shift]     : Optional shift offset to add to the center (default = 0).\n";
}

# Check arguments
usage() unless (@ARGV >= 2);
my $infile = shift @ARGV;
my $size = shift @ARGV;
my $shift = (@ARGV) ? shift @ARGV : 0;

# Open the input file
open(my $fh, "<", $infile) or die "Cannot open $infile: $!";

while (my $line = <$fh>) {
    chomp $line;
    # Skip header or comment lines if desired (you can modify this as needed)
    if ($line =~ /^#/) {
        print "$line\n";
        next;
    }
    my @fields = split /\t/, $line;
    # Require at least 3 columns: chrom, start, end
    if (@fields < 3) {
        warn "Skipping line (not enough columns): $line\n";
        next;
    }
    my ($chrom, $start, $end) = @fields[0,1,2];

    # Calculate the midpoint.
    # (BED format is 0-indexed; here we assume the arithmetic is as desired.)
    my $center = floor( ($start + $end) / 2 );

    # Apply shift to the center if provided.
    my $new_center = $center + $shift;

    # Compute new start and end such that the region is exactly $size long.
    # Here we use floor(size/2) for the left half.
    my $half = floor($size/2);
    my $new_start = $new_center - $half;
    my $new_end = $new_start + $size;

    # Replace the original start and end with the new coordinates.
    $fields[1] = $new_start;
    $fields[2] = $new_end;

    # Print the modified BED line.
    print join("\t", @fields), "\n";
}
close($fh);
