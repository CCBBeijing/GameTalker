library(org.Hs.eg.db)
library(clusterProfiler)

hum_res4 <- list()
for (i in 1:4) {
  gene_name<-as.character(hum_4[[i]])
  
  gene_name<-sapply(gene_name,function(x){
    strsplit(x,"[.]")[[1]][1]
  })
  
  gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                  toType = c("SYMBOL","ENTREZID"),
                  OrgDb = org.Hs.eg.db)
  
  
  ego1<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"
  hum_res4[[i]] <-  as.matrix(ego1@result)[,1:6] 
}

