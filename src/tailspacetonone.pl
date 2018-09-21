#! /usr/bin/perl -w
use strict;
use Getopt::Std;

use vars qw($opt_i $opt_o $opt_m $opt_r $opt_t);
getopts('i:o:m:r:t:');

open IN,"$opt_i";
while(<IN>){
	chomp $_;
	print "$_\n";
}
close IN;
