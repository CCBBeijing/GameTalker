setwd("~/Documents/R-doc/wurongling/liver-cancer/gene-gene/ode2")

rm(list = ls())
library(orthopolynom)
library(glmnet)
library(parallel)
library(pbapply)
library(ggplot2)
library(ggrepel)
library(igraph)
#library(plotrix)
source("ode2-1.R")
dat <- read.csv(file = "dat_fit.csv", row.names = 1, check.names = F)
dat <- dat[,seq(1,31,length=20)]*0.01
colnames(dat) = seq(11,41,length=20)



ode_result = get_ode_par(data = dat , times = seq(11,41,length=20), order = 4, reduction = F, parallel = F)


plot.dat = get_all_net(ode_result)
get_decomposition_plot(ode_result, plot.dat,2)












