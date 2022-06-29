

gene_22 <- rownames(dat_sample)[N_22]
gene_21 <- rownames(dat_sample)[N_21]
gene_20 <- rownames(dat_sample)[N_20]

gene_12 <- rownames(dat_sample)[N_12]
gene_11 <- rownames(dat_sample)[N_11]
gene_10 <- rownames(dat_sample)[N_10]


gene_02 <- rownames(dat_sample)[N_02]
gene_01 <- rownames(dat_sample)[N_01]
gene_00 <- rownames(dat_sample)[N_00]




library(clusterProfiler)
library(org.Hs.eg.db)


#####################################
marker<-gene_22


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_22<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
               pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
               readable= TRUE)#"none"

#####################################
marker<-gene_21


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_21<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"


#####################################
marker<-gene_20


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_20<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"








#####################################
marker<-gene_12


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_12<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"

#####################################
marker<-gene_11


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_11<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"


#####################################
marker<-gene_10


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_10<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"




#####################################
marker<-gene_02


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_02<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"

#####################################
marker<-gene_01


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_01<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"


#####################################
marker<-gene_00


gene_name<-as.character(marker)

gene_name<-sapply(gene_name,function(x){
  strsplit(x,"[.]")[[1]][1]
})



gene.df <- bitr(gene_name, fromType = "ENSEMBL",
                toType = c("SYMBOL","ENTREZID"),
                OrgDb = org.Hs.eg.db)


ego_00<-enrichGO(gene=gene.df$ENTREZID,OrgDb=org.Hs.eg.db,ont = "BP",
                 pAdjustMethod ="BH", pvalueCutoff =1,qvalueCutoff=1,
                 readable= TRUE)#"none"





###########################################
###########################################

go_22 <- data.frame(ego_22)
go_21 <- data.frame(ego_21)
go_20 <- data.frame(ego_20)

go_12 <- data.frame(ego_12)
go_11 <- data.frame(ego_11)
go_10 <- data.frame(ego_10)

go_02 <- data.frame(ego_02)
go_01 <- data.frame(ego_01)
go_00 <- data.frame(ego_00)



go_name <- c(go_22$Description[1:4],go_21$Description[1:4],go_20$Description[1:4],
             go_12$Description[c(4,5,6,8)],go_11$Description[1:4],go_10$Description[1:4],
             go_02$Description[c(1,2,3,5)],go_01$Description[c(1,2,5,7)],go_00$Description[1:4])
             
             
             
             
g_22 <- c()
g_21 <- c()             
g_20 <- c()             
             
g_12 <- c()
g_11 <- c()             
g_10 <- c()              
             
g_02 <- c()
g_01 <- c()             
g_00 <- c()   


for(i in 1:length(go_name)){
  
  gg <- go_name[i]

  n_22 <- match(gg,go_22$Description)
  n_21 <- match(gg,go_21$Description)
  n_20 <- match(gg,go_20$Description)
  
  n_12 <- match(gg,go_12$Description)
  n_11 <- match(gg,go_11$Description)
  n_10 <- match(gg,go_10$Description)
  
  n_02 <- match(gg,go_02$Description)
  n_01 <- match(gg,go_01$Description)
  n_00 <- match(gg,go_00$Description)
  
  
  if(is.na(n_22)==T){
    g1_22 <- NA
    g_22 <- c(g_22,g1_22)
  }else{
    g1_22 <- go_22$p.adjust[n_22]
    g_22 <- c(g_22,g1_22)
  }
  
  
  if(is.na(n_21)==T){
    g1_21 <- NA
    g_21 <- c(g_21,g1_21)
  }else{
    g1_21 <- go_21$p.adjust[n_21]
    g_21 <- c(g_21,g1_21)
  }
  
  
  if(is.na(n_20)==T){
    g1_20 <- NA
    g_20 <- c(g_20,g1_20)
  }else{
    g1_20 <- go_20$p.adjust[n_20]
    g_20 <- c(g_20,g1_20)
  }

  if(is.na(n_12)==T){
    g1_12 <- NA
    g_12 <- c(g_12,g1_12)
  }else{
    g1_12 <- go_12$p.adjust[n_12]
    g_12 <- c(g_12,g1_12)
  }
  
  
  if(is.na(n_11)==T){
    g1_11 <- NA
    g_11 <- c(g_11,g1_11)
  }else{
    g1_11 <- go_11$p.adjust[n_11]
    g_11 <- c(g_11,g1_11)
  }


  if(is.na(n_10)==T){
    g1_10 <- NA
    g_10 <- c(g_10,g1_10)
  }else{
    g1_10 <- go_10$p.adjust[n_10]
    g_10 <- c(g_10,g1_10)
  }
  
  
  if(is.na(n_02)==T){
    g1_02 <- NA
    g_02 <- c(g_02,g1_02)
  }else{
    g1_02 <- go_02$p.adjust[n_02]
    g_02 <- c(g_02,g1_02)
  }
  
  
  
  if(is.na(n_01)==T){
    g1_01 <- NA
    g_01 <- c(g_01,g1_01)
  }else{
    g1_01 <- go_01$p.adjust[n_01]
    g_01 <- c(g_01,g1_01)
  }
  
  
  if(is.na(n_00)==T){
    g1_00 <- NA
    g_00 <- c(g_00,g1_00)
  }else{
    g1_00 <- go_00$p.adjust[n_00]
    g_00 <- c(g_00,g1_00)
  }
  
}



plot_go <- cbind(g_22,g_21,g_20,
                 g_12,g_11,g_10,
                 g_02,g_01,g_00)


rownames(plot_go) <- go_name
colnames(plot_go) <- c("Synergism","Commensalism to Tumor","Altruism for Tumor",
                       "Commensalism from Tumor","Co-existence","Amensalism from Tumor",
                       "Predation toward Tumor","Amensalism to Tumor","Antagonism")




save(plot_go,file="plot_go.RData")

load("plot_go.RData")

library(RColorBrewer)
library(pheatmap)
#rev(brewer.pal(n = 7, name =
 #                "Reds")))(100)
pdf("plot_go.pdf",width=16,height = 8)
p1 <-pheatmap(
  mat = t(plot_go), # 热图的数据源，要保证为数值矩阵，类型为numeric
  color = colorRampPalette((brewer.pal(n = 7, name =
                                         "RdYlBu")))(100),
  scale = "none", # 数值标准化，可以规定按行（row）或按列（column）
  cluster_rows = F, #  是否按行聚类
  cluster_cols = F, #  是否按列聚类
  legend = TRUE, #  图例是否显示
  legend_breaks = NA, # 图例是否断点标注
  legend_labels = NA, # 图例断点标注的标题
  show_rownames = TRUE, # 是否显示行名
  show_colnames = TRUE, # 是否显示列名
  main = NA, #  热图标题
  border=T,
  border_color = "white",
  fontsize = 10, #  热图字体大小
  fontsize_row = 10, #  热图行名字体大小，默认为fontsize
  fontsize_col = 16, #  热图列名字体大小，默认为fontsize
  angle_col = 90, #  热图列名角度
  display_numbers = FALSE, #  矩阵的数值是否显示在热图上
  number_format = "%.2f", # 单元格中数值的显示方式
  fontsize_number = 8, #  显示在热图上的矩阵数值的大小，默认为0.8*fontsize
  gaps_row = NULL, #  在行未聚类时使用，确定将间隙放置在热图中的位置。
  gaps_col = NULL, #  在列未聚类时使用，确定将间隙放置在热图中的位置。
  labels_row = NULL, #  使用行标签代替行名
  labels_col = NULL, #  使用列标签代替列名
  filename = NA, #  保存路径和文件名
  width = NA, # 保存图片的宽度
  height = NA, #  保存图片的高度
  na_col = "white", #  对“NA”值对应的单元格填充颜
)
p1
dev.off()







