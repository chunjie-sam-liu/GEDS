open IN,"<CCLE_cellline";
$i=1;
while(<IN>){
    chomp $_;
    $hash{$_}=$i;
    $i++;
}
close IN;
open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/sort_CCLE_cellline_inform";
while(<IN>){
    chomp $_;
    if(/(\S*)\t(.*)/){
        print "$1\t$2\t$hash{$1}\n";
    }
}
close IN;