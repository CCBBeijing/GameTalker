setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerA")


load("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/result/rep_2_k_26.RData")
dat_26 <- BIC

cluster <- dat_26[[2]][[2]]$clustered_data$cluster


uni_cluster <- unique(cluster )

length(uni_cluster)

dat <- read.csv(file = "~/Documents/R-doc/wurongling/liver-cancer/fig8-8/1/data_cluster.csv", row.names = 1, check.names = F)


df <- log10(colSums(dat))

T1 <- df[grep("H",names(df))]
T2 <- df[grep("N",names(df))]




dat_1 <- dat[which(cluster==uni_cluster[1]),]
dat_2 <- dat[which(cluster==uni_cluster[2]),]
dat_3 <- dat[which(cluster==uni_cluster[3]),]
dat_4 <- dat[which(cluster==uni_cluster[4]),]
dat_5 <- dat[which(cluster==uni_cluster[5]),]
dat_6 <- dat[which(cluster==uni_cluster[6]),]
dat_7 <- dat[which(cluster==uni_cluster[7]),]
dat_8 <- dat[which(cluster==uni_cluster[8]),]
dat_9 <- dat[which(cluster==uni_cluster[9]),]
dat_10 <- dat[which(cluster==uni_cluster[10]),]
dat_11 <- dat[which(cluster==uni_cluster[11]),]
dat_12 <- dat[which(cluster==uni_cluster[12]),]
dat_13 <- dat[which(cluster==uni_cluster[13]),]
dat_14 <- dat[which(cluster==uni_cluster[14]),]
dat_15 <- dat[which(cluster==uni_cluster[15]),]
dat_16 <- dat[which(cluster==uni_cluster[16]),]
dat_17 <- dat[which(cluster==uni_cluster[17]),]
dat_18 <- dat[which(cluster==uni_cluster[18]),]
dat_19 <- dat[which(cluster==uni_cluster[19]),]
dat_20 <- dat[which(cluster==uni_cluster[20]),]
dat_21 <- dat[which(cluster==uni_cluster[21]),]
dat_22 <- dat[which(cluster==uni_cluster[22]),]
dat_23 <- dat[which(cluster==uni_cluster[23]),]
dat_24 <- dat[which(cluster==uni_cluster[24]),]


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



dat_10_H <- dat_10[,grep("H",colnames(dat_10))]
dat_10_N <- dat_10[,grep("N",colnames(dat_10))]



dat_11_H <- dat_11[,grep("H",colnames(dat_11))]
dat_11_N <- dat_11[,grep("N",colnames(dat_11))]


dat_12_H <- dat_12[,grep("H",colnames(dat_12))]
dat_12_N <- dat_12[,grep("N",colnames(dat_12))]



dat_13_H <- dat_13[,grep("H",colnames(dat_13))]
dat_13_N <- dat_13[,grep("N",colnames(dat_13))]


dat_14_H <- dat_14[,grep("H",colnames(dat_14))]
dat_14_N <- dat_14[,grep("N",colnames(dat_14))]


dat_15_H <- dat_15[,grep("H",colnames(dat_15))]
dat_15_N <- dat_15[,grep("N",colnames(dat_15))]


dat_16_H <- dat_16[,grep("H",colnames(dat_16))]
dat_16_N <- dat_16[,grep("N",colnames(dat_16))]


dat_17_H <- dat_17[,grep("H",colnames(dat_17))]
dat_17_N <- dat_17[,grep("N",colnames(dat_17))]

dat_18_H <- dat_18[,grep("H",colnames(dat_18))]
dat_18_N <- dat_18[,grep("N",colnames(dat_18))]

dat_19_H <- dat_19[,grep("H",colnames(dat_19))]
dat_19_N <- dat_19[,grep("N",colnames(dat_19))]


dat_20_H <- dat_20[,grep("H",colnames(dat_20))]
dat_20_N <- dat_20[,grep("N",colnames(dat_20))]


dat_21_H <- dat_21[,grep("H",colnames(dat_21))]
dat_21_N <- dat_21[,grep("N",colnames(dat_21))]

dat_22_H <- dat_22[,grep("H",colnames(dat_22))]
dat_22_N <- dat_22[,grep("N",colnames(dat_22))]

dat_23_H <- dat_23[,grep("H",colnames(dat_23))]
dat_23_N <- dat_23[,grep("N",colnames(dat_23))]


dat_24_H <- dat_24[,grep("H",colnames(dat_24))]
dat_24_N <- dat_24[,grep("N",colnames(dat_24))]





data <- dat_23_H


linear_equation <- function(par,x){
  y=par[1]+par[2]*x
  y
}

par_est <- function(par,x,y){
  sum( (y -   linear_equation(par,x))^2 )
}



f1 <- function(data,T1){
  y <- log10(colSums(data))
  tmp <- c(0.1,0.1)
  r1 <- optim(tmp,par_est,x=T1,y=y, method = "BFGS")
  T1_fit <- seq(min(T1),max(T2),length=30)
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
line_H_10 <- f1(dat_10_H,T1)
line_H_11 <- f1(dat_11_H,T1)
line_H_12 <- f1(dat_12_H,T1)
line_H_13 <- f1(dat_13_H,T1)
line_H_14 <- f1(dat_14_H,T1)
line_H_15 <- f1(dat_15_H,T1)
line_H_16 <- f1(dat_16_H,T1)
line_H_17 <- f1(dat_17_H,T1)
line_H_18 <- f1(dat_18_H,T1)
line_H_19 <- f1(dat_19_H,T1)
line_H_20 <- f1(dat_20_H,T1)
line_H_21 <- f1(dat_21_H,T1)
line_H_22 <- f1(dat_22_H,T1)
line_H_23 <- f1(dat_23_H,T1)
line_H_24 <- f1(dat_24_H,T1)


line_H <- rbind(line_H_1,line_H_2,
                line_H_3,line_H_4,
                line_H_5,line_H_6,
                line_H_7,line_H_8,
                line_H_9,line_H_10,
                line_H_11,line_H_12,
                line_H_13,line_H_14,
                line_H_15,line_H_16,
                line_H_17,line_H_18,
                line_H_19,line_H_20,
                line_H_21,line_H_22,
                line_H_23,line_H_24)


EI <- sort(T1)
TT1  <- seq(min(EI),max(EI),length=30)

colnames(line_H) <- TT1

rownames(line_H) <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10",
                      "M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24")


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
line_N_10 <- f1(dat_10_N,T2)
line_N_11 <- f1(dat_11_N,T2)
line_N_12 <- f1(dat_12_N,T2)
line_N_13 <- f1(dat_13_N,T2)
line_N_14 <- f1(dat_14_N,T2)
line_N_15 <- f1(dat_15_N,T2)
line_N_16 <- f1(dat_16_N,T2)
line_N_17 <- f1(dat_17_N,T2)
line_N_18 <- f1(dat_18_N,T2)
line_N_19 <- f1(dat_19_N,T2)
line_N_20 <- f1(dat_20_N,T2)
line_N_21 <- f1(dat_21_N,T2)
line_N_22 <- f1(dat_22_N,T2)
line_N_23 <- f1(dat_23_N,T2)
line_N_24 <- f1(dat_24_N,T2)


line_N <- rbind(line_N_1,line_N_2,
                line_N_3,line_N_4,
                line_N_5,line_N_6,
                line_N_7,line_N_8,
                line_N_9,line_N_10,
                line_N_11,line_N_12,
                line_N_13,line_N_14,
                line_N_15,line_N_16,
                line_N_17,line_N_18,
                line_N_19,line_N_20,
                line_N_21,line_N_22,
                line_N_23,line_N_24)


EI <- sort(T2)
TT2  <- seq(min(EI),max(EI),length=30)


colnames(line_N) <- TT2

rownames(line_N) <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10",
                      "M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24")

save(line_N,file="line_N.RData")


