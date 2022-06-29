setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB")

#8 #1
#9 #4
#10 #7
#11 #10
#12  # 13
#13 # 16
#14 #19
#15 #22
#16 #25
#17 #28
#18 #31
#19 #34
#20 #37
#21 #40
#22 #43
#23 #46
#24 #49
#25 #52

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



dat_1_H <- dat_1[,grep("H",colnames(dat_1))]
dat_1_N <- dat_1[,grep("N",colnames(dat_1))]



dat_2_H <- dat_2[,grep("H",colnames(dat_2))]
dat_2_N <- dat_2[,grep("N",colnames(dat_2))]


dat_3_H <- dat_3[,grep("H",colnames(dat_3))]
dat_3_N <- dat_3[,grep("N",colnames(dat_3))]

dat_4_H <- dat_4[,grep("H",colnames(dat_4))]
dat_4_N <- dat_4[,grep("N",colnames(dat_4))]



dat_5_H <- dat_5[,grep("H",colnames(dat_5))]
dat_5_N <- dat_5[,grep("N",colnames(dat_5))]


dat_6_H <- dat_6[,grep("H",colnames(dat_6))]
dat_6_N <- dat_6[,grep("N",colnames(dat_6))]


dat_7_H <- dat_7[,grep("H",colnames(dat_7))]
dat_7_N <- dat_7[,grep("N",colnames(dat_7))]


dat_8_H <- dat_8[,grep("H",colnames(dat_8))]
dat_8_N <- dat_8[,grep("N",colnames(dat_8))]


dat_9_H <- dat_9[,grep("H",colnames(dat_9))]
dat_9_N <- dat_9[,grep("N",colnames(dat_9))]



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
  result1 
}


line_H_1 <- f1(dat_1_H,T1)
line_H_2 <- f1(dat_2_H,T1)
line_H_3 <- f1(dat_3_H,T1)
line_H_4 <- f1(dat_4_H,T1)
line_H_5 <- f1(dat_5_H,T1)
line_H_6 <- f1(dat_6_H,T1)
line_H_7 <- f1(dat_7_H,T1)
line_H_8 <- f1(dat_8_H,T1)
line_H_9 <- f1(dat_9_H,T1)

line_H <- rbind(line_H_1,line_H_2,
                line_H_3,line_H_4,
                line_H_5,line_H_6,
                line_H_7,line_H_8,
                line_H_9)


EI <- sort(T1)
TT1  <- seq(min(EI),max(EI),length=30)

colnames(line_H) <- TT1

rownames(line_H) <- c("sM1","sM2","sM3","sM4","sM5","sM6","sM7","sM8","sM9")


save(line_H,file="line_H.RData")





line_N_1 <- f1(dat_1_N,T2)
line_N_2 <- f1(dat_2_N,T2)
line_N_3 <- f1(dat_3_N,T2)
line_N_4 <- f1(dat_4_N,T2)
line_N_5 <- f1(dat_5_N,T2)
line_N_6 <- f1(dat_6_N,T2)
line_N_7 <- f1(dat_7_N,T2)
line_N_8 <- f1(dat_8_N,T2)
line_N_9 <- f1(dat_9_N,T2)



line_N <- rbind(line_N_1,line_N_2,
                line_N_3,line_N_4,
                line_N_5,line_N_6,
                line_N_7,line_N_8,
                line_N_9)




EI <- sort(T2)
TT2  <- seq(min(EI),max(EI),length=30)


colnames(line_N) <- TT2

rownames(line_N) <- c("sM1","sM2","sM3","sM4","sM5","sM6","sM7","sM8","sM9")

save(line_N,file="line_N.RData")



