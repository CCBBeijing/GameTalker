
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerC")

load("result_H_log.RData")
load("result_N_log.RData")

result_H <- result_H_log
result_N <- result_N_log
#######################################

library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")


ra1 <- 1
kk1 <- 7
rat1 <- 4.5



ra2 <- 1
kk2 <- 7
rat2 <- 4.5

TT1 <- T1_fit
TT2 <- T2_fit

pdf("fig3.pdf",width=16,height =5)


par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(3,110),ylim=c(43, 82))


####################################################

####################################################
sub_rc <-c(5,65,22, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFEAD0")




for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(10^8),cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  text(sub_rc[1]-1.8,sub_rc[2]+kk1-3/ra1*i,expression(-10^3),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  text(sub_rc[1]-1.8,sub_rc[2]+kk1-3/ra1*i,expression(-10^6),cex=1.2,font=1,family="Times New Roman")
}




segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- T1_fit*rat1

plot_data <- result_H[[1]]$fit.df2
l1 <- plot_data[,2]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"5",family="Times New Roman",adj=0,cex=2)


l1 <- plot_data[,1]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)


text(sub_rc[3],sub_rc[4]+1,"IGHV320 (1)",cex=2,family="Times New Roman1")


text(sub_rc[1]-6,sub_rc[2]-2.5,"Expression of Individual Genes",srt=90,cex=2,family="Times New Roman")



####################################################
sub_rc <-c(5+17,65,22+17, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#DBF0F1")




for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
 # text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk2,sub_rc[3],sub_rc[2]+kk2,lwd=1,lty=2,col="gray")



times <- TT2*rat2

plot_data <- result_N[[1]]$fit.df2
l1 <- plot_data[,2]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30],"2",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"5",family="Times New Roman",adj=0,cex=2)



l1 <- plot_data[,1]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)






####################################################

####################################################
sub_rc <-c(5+17+2+17,65,22+17+2+17, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFEAD0")






for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- TT1*rat1

plot_data <- result_H[[2]]$fit.df2
l1 <- plot_data[,2]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"3",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"4",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,4]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"5",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,1]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)


text(sub_rc[3],sub_rc[4]+1,"IGHJ3P (2)",cex=2,family="Times New Roman1")


####################################################
sub_rc <-c(5+17+2+17+17,65,22+17+2+17+17, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#DBF0F1")

for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- (TT2)*rat2

plot_data <- result_N[[2]]$fit.df2
l1 <- plot_data[,2]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30],"5",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,1]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)







####################################################

####################################################
sub_rc <-c(5+17+2+17+17+2+17,65,22+17+2+17+17+2+17, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFEAD0")




for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}



segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- TT1*rat1

plot_data <- result_H[[3]]$fit.df2
l1 <- plot_data[,2]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"2",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"4",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,4]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"5",family="Times New Roman",adj=0,cex=2)


l1 <- plot_data[,1]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)


text(sub_rc[3],sub_rc[4]+1,"IGKV16 (3)",cex=2,family="Times New Roman1")


####################################################
sub_rc <-c(5+17+2+17+17+2+17+17,65,22+17+2+17+17+2+17+17, 80)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#DBF0F1")





for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}



segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- (TT2)*rat2

plot_data <- result_N[[3]]$fit.df2
l1 <- plot_data[,2]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30]-1,"2",family="Times New Roman",adj=0,cex=2)


l1 <- plot_data[,3]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30]+1,"5",family="Times New Roman",adj=0,cex=2)



l1 <- plot_data[,1]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)








####################################################

####################################################
sub_rc <-c(5,65-18,22, 80-18)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFEAD0")




for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}


for(i in 0){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^1),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}



for(i in 3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^3),cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,expression(10^8),cex=1.2,font=1,family="Times New Roman")
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  text(sub_rc[1]-1.8,sub_rc[2]+kk1-3/ra1*i,expression(-10^3),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  text(sub_rc[1]-1.8,sub_rc[2]+kk1-3/ra1*i,expression(-10^6),cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- TT1*rat1

plot_data <- result_H[[4]]$fit.df2
l1 <- plot_data[,2]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30]+0.5,"3",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30]-0.5,"5",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,1]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)


text(sub_rc[3],sub_rc[4]+1,"IGKV19 (4)",cex=2,family="Times New Roman1")


####################################################
sub_rc <-c(5+17,65-18,22+17, 80-18)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#DBF0F1")





for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}


for(i in 0){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^1),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}



for(i in 3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^3),cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- (TT2)*rat2

plot_data <- result_N[[4]]$fit.df2
l1 <- plot_data[,2]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30],"2",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"5",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,1]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)




####################################################

####################################################
sub_rc <-c(5+17+2+17,65-18,22+17+2+17, 80-18)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFEAD0")





for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}


for(i in 0){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^1),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}



for(i in 3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^3),cex=1.2,font=1,family="Times New Roman")
}



for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}


segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- TT1*rat1

plot_data <- result_H[[5]]$fit.df2
l1 <- plot_data[,2]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"2",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"3",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,1]/ra1
#l1[which(l1 > 10/ra1*10)] <- NA
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra1
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)


text(sub_rc[3],sub_rc[4]+1,"IGHV741 (5)",cex=2,family="Times New Roman1")



text(sub_rc[3],sub_rc[2]-4,"Expression Index",cex=2,family="Times New Roman")

####################################################
sub_rc <-c(5+17+17+2+17,65-18,22+17+17+2+17, 80-18)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#DBF0F1")


for(i in 0:3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  #text(sub_rc[1]+0.5+0.2*i*23,sub_rc[2]-1,0.2*i+12.6,cex=1,font=1,family="Times New Roman")
}


for(i in 0){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^1),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}



for(i in 3){
  segments(sub_rc[1]+0.5+1*i*rat1,sub_rc[2],sub_rc[1]+0.5+1*i*rat1,sub_rc[2]+0.5,font=2)
  text(sub_rc[1]+0.5+1*i*rat1,sub_rc[2]-1,expression(10^3),cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1+4/ra1*i,sub_rc[1],sub_rc[2]+kk1+4/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1+4/ra1*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+kk1-3/ra1*i,sub_rc[1],sub_rc[2]+kk1-3/ra1*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+kk1-3/ra1*i,-3*i,cex=1.2,font=1,family="Times New Roman")
}



segments(sub_rc[1],sub_rc[2]+kk1,sub_rc[3],sub_rc[2]+kk1,lwd=1,lty=2,col="gray")



times <- (TT2)*rat2

plot_data <- result_N[[5]]$fit.df2
l1 <- plot_data[,2]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk2,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk2+l1[30]+0.2,"1",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,3]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"2",family="Times New Roman",adj=0,cex=2)

l1 <- plot_data[,4]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="green3",lwd=4,lty=1)
text(sub_rc[1]+5+0.5,sub_rc[2]+kk1+l1[30],"4",family="Times New Roman",adj=0,cex=2)


l1 <- plot_data[,1]/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="red",lwd=4,lty=1)

l1 <- plot_data$y/ra2
lines(times+sub_rc[1]+0.5,l1+sub_rc[2]+kk1,col="blue",lwd=4)








dev.off()

