setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerC")




load("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB/M13_result/rep_3_k_17.RData")


cluster <- BIC[[2]][[1]]$clustered_data$cluster

uni_cluster <- unique(cluster )


data <- read.csv(file = "~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB/M13/dat_M13.csv", row.names = 1, check.names = F)


df <- log10(colSums(data))

T1 <- df[grep("H",names(df))]
T2 <- df[grep("N",names(df))]


dat_1 <- data[which(cluster==uni_cluster[1]),]
dat_2 <- data[which(cluster==uni_cluster[2]),]
dat_3 <- data[which(cluster==uni_cluster[3]),]
dat_4 <- data[which(cluster==uni_cluster[4]),]
dat_5 <- data[which(cluster==uni_cluster[5]),]
dat_6 <- data[which(cluster==uni_cluster[6]),]
dat_7 <- data[which(cluster==uni_cluster[7]),]
dat_8 <- data[which(cluster==uni_cluster[8]),]
dat_9 <- data[which(cluster==uni_cluster[9]),]



df <- dat_9

T1 <- log10(colSums(dat_9[,grep("H",colnames(df))])+1)
T2 <- log10(colSums(dat_9[,grep("N",colnames(df))])+1)


dat_H <- df[,grep("H",colnames(df))]
dat_N <- df[,grep("N",colnames(df))]



linear_equation <- function(par,x){
  y=par[1]+par[2]*x
  y
}

par_est <- function(par,x,y){
  sum( (y -   linear_equation(par,x))^2 )
}


f1 <- function(data,T1){
  y <- log10(colSums(data)+1)
  tmp <- c(0.1,0.1)
  r1 <- optim(tmp,par_est,x=T1,y=y, method = "Nelder-Mead",
              control = list(maxit = 30000000, trace = TRUE))
  T1_fit <- seq(min(T1),max(T1),length=30)
  result1 <-  linear_equation(r1$par,T1_fit)
  result1[which(result1 <0)] <- 0
  result1 
}


line_H <- rbind(f1(dat_H[1,],T1),
                f1(dat_H[2,],T1),
                f1(dat_H[3,],T1),
                f1(dat_H[4,],T1),
                f1(dat_H[5,],T1))

T1_fit <- seq(min(T1),max(T1),length=30)

colnames(line_H) <- T1_fit
rownames(line_H) <- c("IGHV320","IGHJ3P"," IGKV16","IGKV19","IGHV741")




line_N <- rbind(f1(dat_N[1,],T2),
                f1(dat_N[2,],T2),
                f1(dat_N[3,],T2),
                f1(dat_N[4,],T2),
                f1(dat_N[5,],T2))

T2_fit <- seq(min(T2),max(T2),length=30)

colnames(line_N) <- T2_fit

rownames(line_N) <- c("IGHV320","IGHJ3P"," IGKV16","IGKV19","IGHV741")

save(line_H,file="line_H.RData")
save(line_N,file="line_N.RData")

