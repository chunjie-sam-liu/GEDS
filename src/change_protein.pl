#! /usr/bin/perl -w
use strict;
use Getopt::Std;

use vars qw($opt_i $opt_o $opt_m);
getopts('i:o:m:');

open IN,"<$opt_o";
my %hash;
while(<IN>){
	chomp $_;
	$_ =~ /(\S*)\s*(\S*)\s*\S*/;
	$hash{$1}=$2;
}
close IN;
open IN,"<$opt_i";
my $i=1;my $up;my $symbol;my $protein;my $expr;
while(<IN>){
	chomp $_;
	if($i==1){
		print "$_\n";
		$i=2;
	}
	else{
		$_ =~ /\S*\s*(\S*)\s*(\S*)\s*(.*)/;
		$symbol=$1;
		$protein=$2;
		$expr=$3;
		if($protein =~ /(.*)_(.*)/){
			$up=uc($1).uc($2);
		}
		else{
			$up=uc($protein);
		}
		if($hash{$up}){
			print "$hash{$up}\t$up\t$expr\n";
		}
		else{
			print "\n";
		}
	}
}
close IN;
