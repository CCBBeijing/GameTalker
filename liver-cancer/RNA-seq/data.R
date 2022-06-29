setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8")

  
data<- read.csv("~/Documents/R-doc/wurongling/liver-cancer/data_rpkm.csv")

rownames(data)<- data[,1]
data <- data[,-1]


df <- data[which(rowSums(data)>0),]


num <- c()
for(i in 1:dim(df)[1]){
  
  data_H <- df[i,grep("H",colnames(df))]
  data_N <- df[i,grep("N",colnames(df))]
  
  if(length(which(data_H>0)) > 10 & length(which(data_N>0)) > 10 ){
    num <- c(num,i)
  }
  
}


df <- df[num,]

write.csv(df,file="data_cluster.csv",quote = F)





