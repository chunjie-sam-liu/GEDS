cat file_list |while read line;do sed s/\"//g $line >cut_$line;done
cat file_list |while read line;do perl changeprotein.pl -i $line -o ../protein_annotation_full >change_$line;done
cat file_list |while read line;do perl  ~/scripts/spacetotab.pl -i $line -o tab_$line;done
cat file_list |while read line;do sed s/\\t$//g $line >tab_$line ;done
