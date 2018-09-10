open IN1,"<hpa_tissue_pair_list";
open IN2,"<hpa_cellline_pair_list";
open OUT1,">hpa_tissue.sh";
open OUT2,">hpa_cellline.sh";
while(<IN1>){
    chomp $_;
    $_ =~ /(.*)\t(\S*)/;
    print OUT1 "grep -w \"$1\" hpa_rna_tissue.tsv >tissueresult/$2\n";
}
while(<IN2>){
    chomp $_;
    $_ =~ /(.*)\t(\S*)/;
    print OUT2 "grep -w \"$1\" hpa_rna_cellline.tsv >celllineresult/$2\n";
}
close IN1;
close IN2;
close OUT1;
close OUT2;