awk -F "\t" '{print $3}' /data/shiny-data/GSEXPR/mRNA/hpa/hpa_rna_tissue.tsv |sort -u >/data/shiny-data/GSEXPR/mRNA/hpa/hpa_tissue_list
sed s/\ /_/g hpa_tissue_list >hpa_tissue_file_list
paste hpa_tissue_list hpa_tissue_file_list>hpa_tissue_pair_list
awk -F "\t" '{print $3}' /data/shiny-data/GSEXPR/mRNA/hpa/hpa_rna_cellline.tsv |sort -u >/data/shiny-data/GSEXPR/mRNA/hpa/hpa_cellline_list
sed s/\ /_/g hpa_cellline_list >hpa_cellline_file_list
paste hpa_cellline_list hpa_cellline_file_list>hpa_cellline_pair_list
#edit the hpa_cellline_pair_list (cellline fHDF/TERT166, hTEC/SVTERT24-B, U-266/70 and U-266/84) because of /
mkdir celllineresult
mkdir tissueresult
perl /data/xiamx/github/GSEXPR/src/hpa_cut.pl