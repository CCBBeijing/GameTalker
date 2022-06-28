setwd("E:/FL/拟南芥网络/幂函数")
data_cluster <- read.csv("data_cluster.csv",header=T,check.names=F)
rownames(data_cluster) <- data_cluster[,1]
data_cluster <- data_cluster[,-1]
data <- list()
for (i in 1:length(data_cluster[,1])) {
  data_ever <-matrix(NA,ncol = 10,nrow = 3)
  data_ever[1,] <- as.numeric(c(400,650,300,1550,1000,650,1350,500,350,1350
))
  k <- c()
  m <- c()
  for (j in 1:20) {
    if(j%%2==0){
      k <- c(k,data_cluster[i,j])
    }else{ m <- c(m,data_cluster[i,j]) }
    
  }
  data_ever[2,] <- as.numeric(m)
  data_ever[3,] <- as.numeric(k)
  data[[i]] <- data_ever
}
data_all <- list()
for (m in 1:length(data_cluster[,1])) {
  data_raw1 <-matrix(NA,ncol = 10,nrow = 4)
  rownames(data_raw1) <- c("tumor sizeo","H","N","t")
  
  
  data_raw1[1,] <- as.numeric(data[[m]][1,])
  data_raw1[2,] <- as.numeric(data[[m]][2,])
  data_raw1[3,] <- as.numeric(data[[m]][3,])
  
  data_raw2 <- data_raw1
  for (i in 1:3) {
    for (j in 1:10) {
      data_raw1[i,j] <- (data_raw2[i,j]-min(data_raw2[i,]))/(max(data_raw2[i,])-min(data_raw2[i,]))
    }
  }
  data_raw1[1:3,] <- data_raw1[1:3,]+1
  
  data_raw1[4,] <- apply(data_raw1[1:3,],2,sum)
  
  data_raw1[1,] <- data_raw1[1,order(data_raw1[4,],decreasing = F)]
  data_raw1[2,] <- data_raw1[2,order(data_raw1[4,],decreasing = F)]
  data_raw1[3,] <- data_raw1[3,order(data_raw1[4,],decreasing = F)]
  data_raw1[4,] <- data_raw1[4,order(data_raw1[4,],decreasing = F)]
  
  colnames(data_raw1) <- data_raw1[4,]
  
  data_raw1 <- log(data_raw1)
  
  data_raw1[4,] <- data_raw1[1,]
  data_all[[m]] <- data_raw1
}



