open IN, "<final_CCLE_cellline_inform";
$i=1;
while(<IN>){
    chomp $_;
    $_ =~ /\S*\t.*\t(.*)\t(.*)/;
    if($i == 1){
        $tissue=$1;
        $number=$2;
        print "awk -F \"\\t\" \'\{print \$1";
        $i=2;
    }
    else{
        if($1 eq $tissue){
            print "\"\\t\"\$$number";
            $number=$2;
        }
        else{
            print "\"\\t\"\$$number\}\' final_filtered_CCLE >result/$tissue\n";
            print "awk -F \"\\t\" \'\{print \$1";
            $tissue=$1;
            $number=$2;
        }
    }
}
print "\"\\t\"\$$number\}\' final_filtered_CCLE >result/$tissue\n";
close IN;