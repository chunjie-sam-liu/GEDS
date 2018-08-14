open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/handwrite_final_CCLE_cellline_inform";
while(<IN>){
    chomp $_;
    $_ =~ /(\S*)\t(.*)\t.*/;
    $hash{$1}=$2;
}
close IN;
open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_cellline";
open OUT,">/data/shiny-data/GSEXPR/mRNA/CCLE/CCLE_cellline_inform";
while(<IN>){
    chomp $_;
    if($hash{$_}){
        print OUT "$_\t$hash{$_}\n";
    }
    else{
        print OUT "$_\n";
    }
}
close IN;