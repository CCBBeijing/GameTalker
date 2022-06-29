

setwd("~/Documents/R-doc/wurongling/liver-cancer/gene-gene/ode2")


gtf <- rtracklayer::import("~/Documents/Index/human/gencode/gencode.v38.primary_assembly.annotation.gtf.gz")
gtf <- as.data.frame(gtf)


gtf_gene <- gtf[which(gtf$type=="gene"),]




sum_fun <- function(x,y){
  d1 <- c()
  for(j in 1:length(x)){
    d1 <- c(d1,sum(x[j],y[j]))
  }
  d1
}


EI <- function(par,x){
  par[1]*(x^par[2])
}



loss <- function(y,x,par){
  sum((y-EI(par,x))^2)
}


dat <- read.csv(file = "data_sample.csv", row.names = 1, check.names = F)




fit1 <- function(y){
  data_H <- as.numeric(y[seq(1,20,by=2)])
  data_N <- as.numeric(y[seq(2,20,by=2)])
  
  x <- sum_fun(data_H,data_N)
  
  par <-c(0.1,0.1)
  op_H <- optim(par,loss,y=data_H,x=x,method="Nelder-Mead",control=list(maxit=100000000))
  op_N <- optim(par,loss,y=data_N,x=x,method="Nelder-Mead",control=list(maxit=100000000))
  
  par_H <- op_H$par
  par_N <- op_N$par
  
  result <- c(par_H,par_N)
  
  return(result)
}

df_par <- t(pbsapply(1:nrow(dat),function(c)fit1(as.numeric(dat[c,]))))

n <- c(1,8,52,53,86,101,129,214,221,222,230,261,265,280,300,
       317,406,425,432,426,439,504,508,514,656,663,747,992,805)


pdf("plot.pdf")
par(mfcol=c(3,3))
for(i in 1:1000){
  
  data_H <- as.numeric(dat[i,seq(1,20,by=2)])
  data_N <- as.numeric(dat[i,seq(2,20,by=2)])
  
  x <- sum_fun(data_H,data_N)
  
  plot(x,data_H,main=i,ylim=c(0,50),col="red")
  points(x,data_N,col="green3")
  
  t1 <- seq(min(x),max(x),seq=20)
  t <- c(min(x),t1,max(x))
  lines(t ,EI(df_par[i,1:2],t ),col="red")
  lines(t ,EI(df_par[i,3:4],t ),col="green3")
}

dev.off()


pdf("fig-8-33.pdf",height = 6.4,width = 12)

ra <- 1
dd <- 2
kk <- 2
rax <- 1


par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(0,258-4),ylim=c(4, 100))



#########################
###########################
sub_rc <-c(10,55,72, 100)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}
text(sub_rc[1]-12,sub_rc[2],expression("Individual Expression"),srt=90,cex=1.6,family="Times New Roman")

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}


i =1
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")



#######################


sub_rc <-c(10,55-45,72, 100-45)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}



i =8
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")




#########################
###########################
sub_rc <-c(10+62,55,72+62, 100)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}


i =406
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")



#######################


sub_rc <-c(10+62,55-45,72+62, 100-45)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}



i =504
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")

text(sub_rc[3],sub_rc[2]-7,expression("Expression Index" ),cex=1.6,family="Times New Roman")




#########################
###########################
sub_rc <-c(10+62+62,55,72+62+62, 100)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}


i =514
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")



#######################


sub_rc <-c(10+62+62,55-45,72+62+62, 100-45)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}



i =747
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")





#########################
###########################
sub_rc <-c(10+62+62+62,55,72+62+62+62, 100)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}


i =567
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")



#######################


sub_rc <-c(10+62+62+62,55-45,72+62+62+62, 100-45)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+dd+10/ra*i,sub_rc[1]+1,sub_rc[2]+dd+10/ra*i,font=2)
  #text(sub_rc[1]-4,sub_rc[2]+dd+10/ra*i,10*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:5){
  segments(sub_rc[1]+kk+10/rax*i,sub_rc[2],sub_rc[1]+kk+10/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+10/rax*i,sub_rc[2]-2,10*i,cex=1.2,font=1,family="Times New Roman")
}



i =656
data_H <- as.numeric(dat[i,seq(1,20,by=2)])
data_N <- as.numeric(dat[i,seq(2,20,by=2)])
x <- sum_fun(data_H,data_N)
name <- gtf_gene$gene_name[match(rownames(dat)[i],gtf_gene$gene_id)]
points(sub_rc[1]+x+dd,sub_rc[2]+data_H+dd,col="red")
points(sub_rc[1]+x+dd,sub_rc[2]+data_N+dd,col="blue",pch=3)

t <- c(min(x),seq(min(x),max(x),length=100),max(x))

lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,1:2],t ),col="red",lwd=3)
lines(sub_rc[1]+t+dd,sub_rc[2]+dd+EI(df_par[i,3:4],t ),col="blue",lwd=3)


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-2,name,cex=1.6,family="Times New Roman1")

dev.off()





