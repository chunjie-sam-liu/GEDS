a <- readr::read_tsv("/home/xiamx/for_circle")
a %>% dplyr::mutate(tmp = ifelse(mRNA == "1",1,0)) %>% dplyr::mutate(tmp2 = ifelse(protein == "2",2,0)) %>% dplyr::mutate(tmp3 = ifelse(miRNA == "3",3,0)) %>% dplyr::select(X1,mRNA=tmp,protein=tmp2,miRNA=tmp3) ->c
b <- matrix(c(c$mRNA,c$protein,c$miRNA),nrow=3,ncol=219,byrow=TRUE)
colnames(b) <- c$X1
rownames(b) <- c %>% dplyr::select(-X1) %>% colnames()
b2<-prop.table(b,margin=2)

chordDiagram(b2,order = union(c("miRNA","protein","mRNA"), colnames(b2)),annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
  niceFacing=TRUE,adj=c(0,0.5),cex = 0.5 )},bg.border=NA)
