awk -F "." '{print $1}' CCLE_DepMap_18q3_RNAseq_RPKM_20180718.xls >CCLE_ensembl.txt
perl /data/xiamx/github/GSEXPR/src/id-convert.pl
perl /data/xiamx/github/GSEXPR/src/CCLE_filter.pl
awk -F "\t" '{print $1"\t"$2"\t"$5}' CCLE_sample_info_file_2012-10-18.txt |sort -t $'\t' -k 3 |sed s/_/\ /g >info_CCLE
head -n 1 filtered_CCLE |sed s/\\t/\\n/g |awk '{print $1}' >CCLE_cellline
perl /data/xiamx/github/GSEXPR/src/informcollect.pl
sort -t $'\t' -k 3 CCLE_cellline_inform >sort_CCLE_cellline_inform
#vi the sort_CCLE_cellline_inform for the inform not abtained from the files before
perl /data/xiamx/github/GSEXPR/src/informcollect_new.pl
sort -t $'\t' -k 3 CCLE_cellline_inform >sort_CCLE_cellline_inform
perl /data/xiamx/github/GSEXPR/src/cut.pl |sort -t $'\t' -k 3,3 >/data/shiny-data/GSEXPR/mRNA/CCLE/final_CCLE_cellline_inform
#vi the fortibble for unknown name cellline by the cellline name in files
awk -F "\t" '{print $2}' CCLE_cellline_inform >new_head
perl /data/xiamx/github/GSEXPR/src/changehead.pl >final_filtered_CCLE
perl /data/xiamx/github/GSEXPR/src/awk_shell.pl >CCLE_cut.sh
sh CCLE_cut.sh
