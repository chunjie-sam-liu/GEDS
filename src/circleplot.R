library(magrittr)
library(circlize)
file <- readr::read_tsv("/home/xiamx/for_circle_all")
file %>% dplyr::filter(source %in% c("GTEx","CCLE","MCLP")) -> a
a %>% dplyr::mutate(tmp = ifelse(source == "GTEx",1,0)) %>% dplyr::mutate(tmp2 = ifelse(source == "CCLE",2,0)) %>% dplyr::mutate(tmp3 = ifelse(source == "MCLP",3,0)) %>% dplyr::select(X1,GTEx=tmp,CCLE=tmp2,MCLP=tmp3) -> c
b <- matrix(c(c$GTEx,c$CCLE,c$MCLP),nrow = 3, ncol = 75,byrow=TRUE)
colnames(b) <- c$X1
rownames(b) <- c %>% dplyr::select(-X1) %>% colnames()
b2<-prop.table(b,margin=2)

chordDiagram(b2,order = union(c("MCLP","CCLE","GTEx"), colnames(b2)),annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
circlize::circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
  niceFacing=TRUE,adj=c(0,0.5),cex = 0.5 )},bg.border=NA)
