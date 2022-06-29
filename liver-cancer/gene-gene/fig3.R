
setwd("~/Documents/R-doc/wurongling/liver-cancer/gene-gene/ode2")


dat_sample <- read.csv(file = "data_sample.csv", row.names = 1, check.names = F)

dat_cluster <- read.csv(file = "~/Documents/R-doc/wurongling/liver-cancer/fig8-8/1/data_cluster.csv", row.names = 1, check.names = F)

num <- match(rownames(dat_cluster),rownames(dat_sample))


load("res1.RData")



N_22 <- c()
N_21 <- c()
N_20 <- c()

N_12 <- c()
N_11 <- c()
N_10 <- c()


N_02 <- c()
N_01 <- c()
N_00 <- c()


for(i in num){
  an <- as.numeric(res1[[i]][[1]][[5]][2])
  bn <- as.numeric(res1[[i]][[2]][[5]][2])
  
  if(an > 0.001 & bn > 0.001){
    N_22 <- c(N_22,i)
  }
  
  if(an > 0.001 & abs(bn) < 0.001){
    N_21 <- c(N_21,i)
  }
  
  if(an > 0.001 & bn <  -0.001){
    N_20 <- c(N_20,i)
  }
  
  
  if(abs(an) < 0.001 & bn > 0.001){
    N_12 <- c(N_12,i)
  }
  
  if(abs(an) < 0.001 & abs(bn) < 0.001){
    N_11 <- c(N_11,i)
  }
  
  if(abs(an) < 0.001 & bn <  -0.001){
    N_10 <- c(N_10,i)
  }
  
  if(an <  -0.001 & bn > 0.001){
    N_02 <- c(N_02,i)
  }
  
  if(an <  -0.001 & abs(bn) < 0.001){
    N_01 <- c(N_01,i)
  }
  
  if(an <  -0.001 & bn <  -0.001){
    N_00 <- c(N_00,i)
  }
  
}






dat_plot <- c()
dat_plot$name <- c("Synergism","Commensalism to Tumor","Altruism for Tumor",
                   "Commensalism from Tumor","Co-existence","Amensalism from Tumor",
                   "Predation toward Tumor","Amensalism to Tumor","Antagonism")

#dat_plot$number <- c(length(N_22),length(N_12),length(N_02),
  #                   length(N_21),length(N_11),length(N_01),
  #                   length(N_20),length(N_10),length(N_00))



dat_plot$number <- c(length(N_22)/dim(dat_cluster)[1],length(N_12)/dim(dat_cluster)[1],length(N_02)/dim(dat_cluster)[1],
                     length(N_21)/dim(dat_cluster)[1],length(N_11)/dim(dat_cluster)[1],length(N_01)/dim(dat_cluster)[1],
                     length(N_20)/dim(dat_cluster)[1],length(N_10)/dim(dat_cluster)[1],length(N_00)/dim(dat_cluster)[1])



dat_plot$ratio <- c(length(N_22)/dim(dat_cluster)[1],length(N_12)/dim(dat_cluster)[1],length(N_02)/dim(dat_cluster)[1],
                    length(N_21)/dim(dat_cluster)[1],length(N_11)/dim(dat_cluster)[1],length(N_01)/dim(dat_cluster)[1],
                    length(N_20)/dim(dat_cluster)[1],length(N_10)/dim(dat_cluster)[1],length(N_00)/dim(dat_cluster)[1])


dat_plot$ratio1 <- round(dat_plot$ratio,3)


dat_plot$Percent <- paste(round(dat_plot$ratio*100,2),"%",sep="")

dat_plot <- as.data.frame(dat_plot)

write.csv (dat_plot,file="dat_plot_frequency.csv",row.names = F,quote = F)



dat_plot$name <- factor(dat_plot$name, levels=c("Synergism","Commensalism to Tumor","Altruism for Tumor",
                                                "Commensalism from Tumor","Co-existence","Amensalism from Tumor",
                                                "Predation toward Tumor","Amensalism to Tumor","Antagonism"), ordered=TRUE)

library(ggplot2)
pdf("fig8-32_frequency.pdf")
ggplot(dat_plot, aes(x=name, y=number)) + 
  geom_bar(position=position_dodge(), stat="identity", fill = '#39cccc')+
  labs(x="",y = "Frequency")+
  #stat_summary(aes(label = scales::comma(..y..)), fun = 'sum', 
  #     geom = 'text', col = 'black', vjust = 0, label = Percent) +
  geom_text(aes(label=ratio1),position=position_dodge(0.9),vjust=-0.2)+ 
  guides(fill=FALSE)+
  theme(panel.background=element_rect(fill='transparent', color='gray'),
        legend.key=element_rect(fill='transparent', color='transparent'),
        plot.title = element_text(size = 16,face = "bold", vjust = 0.5, hjust = 0.5),  
        legend.title = element_blank(),  
        legend.text = element_text(size = 6),     
        legend.position = 'right',               
        legend.key.size=unit(0.6,'cm'),
        axis.title.x=element_text(vjust=1,  
                                  size=12),  # X axis title
        axis.title.y=element_text(size=20),  # Y axis title
        axis.text.x=element_text(size=10, 
                                 angle = 45,
                                 vjust=1,hjust=1),  # X axis text
        axis.text.y=element_text(size=10))  

dev.off()


