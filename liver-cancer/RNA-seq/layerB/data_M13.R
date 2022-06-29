setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB")


load("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/result/rep_2_k_26.RData")
dat_26 <- BIC

cluster <- dat_26[[2]][[2]]$clustered_data$cluster

uni_cluster <- unique(cluster )
dat <- read.csv(file = "~/Documents/R-doc/wurongling/liver-cancer/fig8-8/1/data_cluster.csv", row.names = 1, check.names = F)

dat_13 <- dat[which(cluster==uni_cluster[13]),]

write.csv(dat_13,file = 'M13/dat_M13.csv')
