open IN ,"</data/shiny-data/GSEXPR/mRNA/Homo-sapiens-genes-grch38.p5.txt.mod";
while(<IN>){
	chomp $_;
	$_ =~ /(\S*)\s*\S*\s*\S*\s*(\S*)/;
	$hash{$1}=$2;
}
close IN;
open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_ensembl.txt";
open OUT,">/data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_ID_converted.txt";
while(<IN>){
	chomp $_;
	if($hash{$_}){
		print OUT "$_\t$hash{$_}\n";
	}
}
close IN;
