open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_ID_converted.txt";
while(<IN>){
    chomp $_;
    $_ =~ /(\S*)\s*(\S*)/;
    $hash{$1}=$2;
}
close IN;
open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_DepMap_18q3_RNAseq_RPKM_20180718.xls";
open OUT,">/data/shiny-data/GSEXPR/mRNA/CCLE/filtered_CCLE";
$i=1;
while(<IN>){
    chomp $_;
    $_ =~ /(\S*)\s*(\S*)\s*(.*)/;
    if($i == 1){
        print OUT "Symbol name\t$3\n";
        $i=2;
    }
    $ensembl=$1;$express=$3;
    $ensembl =~ /(\S*)\.\S*/;
    if(($hash{$1}) && ($hash{$1} ne "-")){
        print OUT "$hash{$1}\t$express\n";
    }
}
close IN;