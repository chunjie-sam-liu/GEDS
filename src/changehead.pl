open IN,"</data/shiny-data/GSEXPR/mRNA/CCLE/new_head";
$head=undef;
$i=1;
while(<IN>){
    chomp $_;
    if($i==1){
        $i=2;
    }
    else{
        $head=$head."\t".$_;    
    }
}
close IN;
open IN,"<filtered_CCLE";
$i=1;
while(<IN>){
    chomp $_;
    if($i==1){
        print "Symbol name$head\n";
        $i=2;
    }
    else{
        @test=split(/\s+/,$_);
        foreach $key (@test){
                print "$key\t";
        }
        print "\n";
    }
}
close IN;