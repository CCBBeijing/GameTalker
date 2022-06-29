
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8")


dat <- read.csv(file = "data_cluster.csv", row.names = 1, check.names = F)


T1 <- log10(colSums(dat[,grep("H",colnames(dat))]))
T2 <- log10(colSums(dat[,grep("N",colnames(dat))]))

pdf("plot.pdf")

par(mfcol=c(3,3))


for(i in 1:1000){
  
  
  yy <- dat[i,]
  y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
  y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)
  linear_equation <- function(par,x){
    y=par[1]+par[2]*x
    y
  }
  
  loss <- function(y,x,par){
    sum((y-linear_equation(par,x))^2)
  }
  
  
  tmp <- c(0.1,0.1)
  r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")
  tmp <- c(0.1,0.1)
  r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")
  
  l1 <- linear_equation(r1$par,sort(T1))
  l2 <- linear_equation(r2$par,sort(T2))
  
  plot(T1,y1,main=i,col="red")
  lines(sort(T1),l1,col="red")
  
  plot(T2,y2,main=i,col="green")
  lines(sort(T2),l2,col="green")
}

dev.off()




linear_equation <- function(par,x){
  y=par[1]+par[2]*x
  y
}

loss <- function(y,x,par){
  sum((y-linear_equation(par,x))^2)
}




library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")



ra <- 80


pdf("fig8-8.pdf",height=6,width=15)

par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(14,330),ylim=c(2, 14.5))




gene <- 6
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 3.2#4.5/max(c(y1,y2))


sub_rc <-c(20,9,56, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}





for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.3*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.3*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.3*rat*i,3*i,cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+0.5,"FGR",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36,9,56+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}







##############################

##############################


gene <- 7
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 1.5#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36,9,56+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.6*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.6*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.6*rat*i,6*i,cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+0.5,"CFH",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36,9,56+36+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}







##############################

##############################


gene <- 8
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 2.5#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36+36+10+36,9,56+36+10+36+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.4*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.4*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.4*rat*i,4*i,cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+0.5,"FUCA2",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36+36+10+36,9,56+36+36+10+36+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}






##############################

##############################


gene <- 115
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 3#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36+36+10+36+36+10+36,9,56+36+10+36+36+10+36+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)



points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.3*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.3*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.3*rat*i,3*i,cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+0.5,"TMEM98",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36+36+10+36+36+10+36,9,56+36+36+10+36+36+10+36+36+10+36, 14)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.3*i*ra,sub_rc[2]-0.3,0.3*i+12.4,cex=1.2,font=1,family="Times New Roman")
}

text(10,8,"Expression of Individual Genes",srt=90,cex=1.6,font=1,family="Times New Roman")


##############################

##############################


#par(mar=c(0,0,0,0),oma=c(0,0,0,0))
#plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(14,330),ylim=c(2, 14.5))




gene <- 20
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 6#4.5/max(c(y1,y2))


sub_rc <-c(20,9-6,56, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)



points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+0.5+0.2*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.2*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.2*rat*i,2*i,cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+0.5,"RAD52",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36,9-6,56+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}

for(i in 4){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.8),cex=1.2,font=1,family="Times New Roman")
}




##############################

##############################



gene <- 38
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 3.5#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36,9-6,56+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.3*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.3*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.3*rat*i,3*i,cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+0.5,"CASP10",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36,9-6,56+36+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}

for(i in 4){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.8),cex=1.2,font=1,family="Times New Roman")
}



text(179,2,expression("Expression Index"),cex=1.6,font=1,family="Times New Roman")




##############################

##############################



gene <- 13
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 5.2#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36+36+10+36,9-6,56+36+10+36+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:4){
  segments(sub_rc[1],sub_rc[2]+0.5+0.2*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.2*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.2*rat*i,2*i,cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+0.5,"LAS1L",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36+36+10+36,9-6,56+36+36+10+36+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}


for(i in 4){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.8),cex=1.2,font=1,family="Times New Roman")
}




##############################

##############################



gene <- 24
g1 <- rownames(dat)[gene]
g1
yy <- dat[gene,]
y1 <- log10(as.numeric(yy[grep("H",colnames(dat))])+1)
y2 <- log10(as.numeric(yy[grep("N",colnames(dat))])+1)

rat <- 7#4.5/max(c(y1,y2))


sub_rc <-c(20+36+10+36+36+10+36+36+10+36,9-6,56+36+10+36+36+10+36+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


points(sub_rc[1]+(T1 - 5.4)*ra+1,sub_rc[2]+y1*rat+0.5-1/rat,col="red")

tmp <- c(0.1,0.1)
r1 <- optim(tmp,loss,x=T1,y=y1, method = "BFGS")

l1 <- linear_equation(r1$par,sort(T1))

Tn <- sort(T1)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l1*rat+0.5-1/rat,col="red",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}




for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+0.5+0.2*rat*i,sub_rc[1]+1.2,sub_rc[2]+0.5+0.2*rat*i,font=2)
  text(sub_rc[1]-4,sub_rc[2]+0.5+0.2*rat*i,2*i,cex=1.2,font=1,family="Times New Roman")
}

text(sub_rc[3],sub_rc[4]+0.5,"HS3ST1",cex=1.6,font=1,family="Times New Roman1")

sub_rc <-c(20+36+36+10+36+36+10+36+36+10+36,9-6,56+36+36+10+36+36+10+36+36+10+36, 14-6)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

points(sub_rc[1]+(T2 - 5.4)*ra+1,sub_rc[2]+y2*rat+0.5-1/rat,col="green3")

tmp <- c(0.1,0.1)
r2 <- optim(tmp,loss,x=T2,y=y2, method = "BFGS")

l2 <- linear_equation(r2$par,sort(T2))

Tn <- sort(T2)

lines(sub_rc[1]+(Tn- 5.4)*ra+1,sub_rc[2]+l2*rat+0.5-1/rat,col="green3",lwd=2)


for(i in 0:4){
  segments(sub_rc[1]+1+0.1*i*ra,sub_rc[2],sub_rc[1]+1+0.1*i*ra,sub_rc[2]+0.1,font=2)
  #text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}



for(i in 0){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}

for(i in 3){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}

for(i in 4){
  text(sub_rc[1]+1+0.1*i*ra,sub_rc[2]-0.3,expression(10^5.8),cex=1.2,font=1,family="Times New Roman")
}



dev.off()
