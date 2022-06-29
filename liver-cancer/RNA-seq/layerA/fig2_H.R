
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerA")

library(orthopolynom)
library(glmnet)
library(parallel)
library(pbapply)
library(ggplot2)
library(ggrepel)
library(igraph)


source("~/Documents/R-doc/wurongling/liver-cancer/fig8-5/result/ode-1.R")

get_interaction <- function(data, col, reduction = F ){
  data <- t(data)
  n <- nrow(data)
  clean_data <- data
  gene_list <- list()
  m <- clean_data[,col]
  M <- clean_data[,-col]
  x_matrix <- M
  x_matrix <- as.matrix(x_matrix)
  if (reduction!=FALSE) {
    vec <- sapply(1:length(M[1,]),function(c)cor(m,M[,c]))
    x_matrix <- M[,which( vec %in% -sort(-vec)[1:(n/log(n))] )]
    x_matrix <- as.matrix(x_matrix)
  }
  name <- colnames(clean_data)
  ridge1_cv <- cv.glmnet(x = x_matrix, y = m,alpha = 0)
  best_ridge_coef <- abs(as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1])
  
  fit_res <- cv.glmnet(x = x_matrix, y = m,alpha = 0.4,
                       penalty.factor = 1/best_ridge_coef,
                       keep = TRUE)
  best_alasso_coef1 <- coef(fit_res, s = fit_res$lambda.min)
  
  return_obj = list(ind.name = name[col],
                    dep.name = best_alasso_coef1@Dimnames[[1]][best_alasso_coef1@i[-1]+1],
                    coefficent = best_alasso_coef1@x[-1])
  
  if ( length(return_obj$dep.name)==0 ) {
    tmp = cor(m,M)
    return_obj$dep.name = colnames(tmp)[which.max(abs(tmp))]
    return_obj$coefficent = tmp[which.max(abs(tmp))]
  }
  return(return_obj)
}

#load("line_H.RData")
dat <- line_H

relationship = pblapply(1:nrow(dat),function(c)get_interaction(dat,c,reduction = T))

result1_H = qsODE_all(dat, 1, c(3,15), Time = TT1,LOP_order = 3)#
result2_H = qsODE_all(dat, 2, c(11,13,14,18), Time = TT1,LOP_order = 3)#
result3_H = qsODE_all(dat, 3, c(1,15), Time = TT1,LOP_order = 3,maxit = 300)
result4_H = qsODE_all(dat, 4, c(11,13,14,18), Time = TT1,LOP_order = 3)
result5_H = qsODE_all(dat, 5, c(11,13,14), Time = TT1,LOP_order = 3,maxit = 1000000)
result6_H = qsODE_all(dat, 6, c(10,11,18), Time = TT1,LOP_order = 3)
result7_H = qsODE_all(dat, 7, c(10,13), Time = TT1,LOP_order = 3)
result8_H = qsODE_all(dat, 8, c(11,13,14,18), Time = TT1,LOP_order = 3)
result9_H = qsODE_all(dat, 9, c(11,13,14), Time = TT1,LOP_order = 3)
result10_H = qsODE_all(dat, 10, c(11,13,14,18), Time = TT1,LOP_order = 3)
result11_H = qsODE_all(dat, 11, c(4,8,10,14), Time = TT1,LOP_order = 3)
result12_H = qsODE_all(dat, 12, c(11,13,14,18), Time = TT1,LOP_order = 3)
result13_H = qsODE_all(dat, 13, c(10,14,18), Time = TT1,LOP_order = 3)
result14_H = qsODE_all(dat, 14, c(11,13,18), Time = TT1,LOP_order = 3)
result15_H = qsODE_all(dat, 15, c(1,3), Time = TT1,LOP_order = 3)
result16_H = qsODE_all(dat, 16, c(1,3,15), Time = TT1,LOP_order = 3)
result17_H = qsODE_all(dat, 17, c(1,3,15), Time = TT1,LOP_order = 3)
result18_H = qsODE_all(dat, 18, c(10,11,13), Time = TT1,LOP_order = 3)
result19_H = qsODE_all(dat, 19, c(11,13,14,18), Time = TT1,LOP_order = 3)
result20_H = qsODE_all(dat, 20, c(1,3,15), Time = TT1,LOP_order = 3)
result21_H = qsODE_all(dat, 21, c(1,3,15), Time = TT1,LOP_order = 3)
result22_H = qsODE_all(dat, 22, c(1,3,15), Time = TT1,LOP_order = 3)
result23_H = qsODE_all(dat, 23, c(11,13,18), Time = TT1,LOP_order = 3)
result24_H = qsODE_all(dat, 24, c(1,3,15), Time = TT1,LOP_order = 3)



result_H <- list()
result_H[[1]] <- result1_H
result_H[[2]] <- result2_H
result_H[[3]] <- result3_H
result_H[[4]] <- result4_H
result_H[[5]] <- result5_H
result_H[[6]] <- result6_H
result_H[[7]] <- result7_H
result_H[[8]] <- result8_H
result_H[[9]] <- result9_H
result_H[[10]] <- result10_H
result_H[[11]] <- result11_H
result_H[[12]] <- result12_H
result_H[[13]] <- result13_H
result_H[[14]] <- result14_H
result_H[[15]] <- result15_H
result_H[[16]] <- result16_H
result_H[[17]] <- result17_H
result_H[[18]] <- result18_H
result_H[[19]] <- result19_H
result_H[[20]] <- result20_H
result_H[[21]] <- result21_H
result_H[[22]] <- result22_H
result_H[[23]] <- result23_H
result_H[[24]] <- result24_H

save(result_H,file="result_H.RData")

load("result_H.RData")

ode_result <- result_H


network_conversion <- function(result, j = NULL){
  n = ncol(result$fit.df)
  lop_order = length(result$parameters)/n
  
  if ( is.null(j) == TRUE ) {
    effect.mean = apply(result$fit.df,2,mean)
  } else {
    effect.mean = unlist(result$fit.df[j,])
  }
  output <- list(ind.name = colnames(result$fit.df)[1],
                 dep.name = colnames(result$fit.df)[-1],
                 #ind.par = result$parameters[1:lop_order],
                 #dep.par = matrix(result$parameters[-(1:lop_order)],nrow = (n-1)),
                 effect.mean = effect.mean,
                 effect.all = result$fit.df2)
  
  return(output)
}


net_all = list(mean.net = lapply(ode_result,network_conversion))



replace_character <- function(i){
  require(stringr)
  tmp <- str_remove_all(i, "]")
  tmp1 <- str_remove(tmp, "\\(")
  tmp2 <- as.numeric(unlist(strsplit(tmp1,split='\\,')))
  return(tmp2)
}

get_after <- function(i){
  temp <- matrix(NA,nrow = length(i[[2]]),ncol=3)
  temp[,1] <- i[[2]]
  temp[,2] <- i[[1]]
  temp[,3] <- i[[3]][2:(length(i[[2]])+1)]
  
  colnames(temp) <- c('from','to','dep_effect')
  temp <- data.frame(temp)
  temp[,3] <- as.numeric(as.character(temp[,3]))
  return(temp)
}

get_max_effect <- function(k){
  after <- do.call(rbind,lapply(k, get_after))
  max_dep_effect <- max(abs(after$dep_effect))
  
  temp <- aggregate(dep_effect ~ to, data = after, sum)
  all_dep_effect <- max(abs(temp$dep_effect))
  return(c(max_dep_effect,all_dep_effect))
}

max_effect = t(sapply(net_all,get_max_effect))

k=net_all$mean.net
color_size=2



extra <- sapply(k,function(c) c[[3]][1])

after <- do.call(rbind,lapply(k, get_after))

colfunc <- colorRampPalette(c("blue",
                              "#ffdead",
                              "#FF2442"))

links_col = data.frame(color = colfunc(color_size),
                       y = cut(seq(-max(max_effect[,1]),max(max_effect[,1]),length=color_size),color_size))

links_interval = t(sapply(1:color_size,function(c) replace_character(links_col$y[c])))

links <- after
colnames(links) <- c("from","to","weight")
for (i in 1:nrow(links)) {
  links$edge.colour[i] <- links_col$color[which(sapply(1:color_size,function(c)
    findInterval(links$weight[i],c(links_interval[c,])))==1)] #add colour for links
}


#links$edge.colour[grep("H",links$from)] <- "blue"
#links$edge.colour[grep("N",links$from)] <- "green3"


node_col <- data.frame(color = colfunc(color_size),
                       y = cut(seq(-max(max_effect[,2]),max(max_effect[,2]),length=color_size),color_size))
node_interval = t(sapply(1:color_size,function(c) replace_character(node_col$y[c])))
nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
colnames(nodes) <- c("id","name","ind_effect")
node2 <- aggregate(weight ~ to, data = links, sum)
nodes$influence  <- node2[match(nodes$id,node2[,1]),2]



#nodes$colour[grep("H",nodes$name)] <- "#3DB2FF"
#nodes$colour[grep("N",nodes$name)] <- "green3"


#for (i in 1:nrow(nodes)) {
#nodes$colour[i] <- node_col$color[which(sapply(1:color_size,function(c)
#findInterval(nodes$influence[i],c(node_interval[c,])))==1)] #add colour for links
#}


for (i in 1:nrow(nodes)) {
  nodes$colour[i] <-"grey"
  #findInterval(nodes$influence[i],c(node_interval[c,])))==1)] #add colour for links
}


#nodes$name = c("H1.a","H1.b","N1.a","N1.b","N1.c","N1.d")


#normalization
normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.2+0.8}

#final plot
links[,3] <- normalization(abs(links[,3]))
nodes[,3:4] <- normalization(abs(nodes[,3:4]))



net <- graph_from_data_frame( d=links,vertices = nodes,directed = T )


pdf("fig2_H.pdf",height = 12,width = 12)
plot.igraph(net,
            rescale = T,
            #edge.arrow.size=E(net)$weight,
            vertex.label=V(net)$name,
            vertex.label.color="black",
            vertex.shape="circle",
            vertex.label.cex=V(net)$ind_effect*0.8,
            vertex.size=V(net)$ind_effect*6,
            edge.curved=0.05,
            edge.color=E(net)$edge.colour,
            edge.frame.color=E(net)$edge.colour,
            edge.width=E(net)$weight*2,
            vertex.color=V(net)$colour,
            layout=layout.kamada.kawai,
            #main=title,
            margin=c(-0.05,-0.05,-0.05,-0.05)
)
dev.off()





