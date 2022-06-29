setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/result")

save(T1,file="T1.RData")
save(T2,file="T2.RData")
rat <- 42
ra <- 3
#######################################

library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")


pdf("fig1.pdf",width=12,height =10)

par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(2,128),ylim=c(2, 120))


####################################################

####################################################
sub_rc <-c(5,105,20, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_1_H+1)
plot_par1 <- H_1$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M1 (631)",cex=1.2)


sub_rc <-c(5+15,105,20+15, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_1_N+1)
plot_par2 <- N_1$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}



####################################################

sub_rc <-c(5+32,105,20+32, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_2_H+1)
plot_par1 <- H_2$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M2 (6340)",cex=1.2)


sub_rc <-c(5+32+15,105,20+32+15, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_2_N+1)
plot_par2 <- N_2$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}




####################################################

sub_rc <-c(5+32+32,105,20+32+32, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_3_H+1)
plot_par1 <- H_3$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M3 (1445)",cex=1.2)


sub_rc <-c(5+32+32+15,105,20+32+32+15, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_3_N+1)
plot_par2 <- N_3$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}





####################################################

sub_rc <-c(5+32+32+32,105,20+32+32+32, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_4_H+1)
plot_par1 <- H_4$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M4 (1654)",cex=1.2)


sub_rc <-c(5+32+32+32+15,105,20+32+32+32+15, 120)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_4_N+1)
plot_par2 <- N_4$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}










####################################################

####################################################
sub_rc <-c(5,105-20,20, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_5_H+1)
plot_par1 <- H_5$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M5 (2808)",cex=1.2)


sub_rc <-c(5+15,105-20,20+15, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_5_N+1)
plot_par2 <- N_5$par[1:2]

Tn2 <- order(T2)
for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

####################################################

sub_rc <-c(5+32,105-20,20+32, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_6_H+1)
plot_par1 <- H_6$par[1:2]

Tn1 <- order(T1)
for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
 # text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M6 (254)",cex=1.2)


sub_rc <-c(5+32+15,105-20,20+32+15, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_6_N+1)
plot_par2 <- N_6$par[1:2]

Tn2 <- order(T2)
for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}





####################################################

sub_rc <-c(5+32+32,105-20,20+32+32, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_7_H+1)
plot_par1 <- H_7$par[1:2]

Tn1 <- order(T1)


for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

text(sub_rc[3],sub_rc[4]+1.5,"M7 (97)",cex=1.2)


sub_rc <-c(5+32+32+15,105-20,20+32+32+15, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_7_N+1)
plot_par2 <- N_7$par[1:2]

Tn2 <- order(T2)
for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}



####################################################

sub_rc <-c(5+32+32+32,105-20,20+32+32+32, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_8_H+1)
plot_par1 <- H_8$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
 # text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M8 (875)",cex=1.2)


sub_rc <-c(5+32+32+32+15,105-20,20+32+32+32+15, 120-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_8_N+1)
plot_par2 <- N_8$par[1:2]

Tn2 <- order(T2)
for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}









####################################################

####################################################
sub_rc <-c(5,105-20-20,20, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_9_H+1)
plot_par1 <- H_9$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M9 (1637)",cex=1.2)


sub_rc <-c(5+15,105-20-20,20+15, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_9_N+1)
plot_par2 <- N_9$par[1:2]

Tn2 <- order(T2)
for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}



####################################################

sub_rc <-c(5+32,105-20-20,20+32, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_10_H+1)
plot_par1 <- H_10$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}





text(sub_rc[3],sub_rc[4]+1.5,"M10 (4400)",cex=1.2)


sub_rc <-c(5+32+15,105-20-20,20+32+15, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_10_N+1)
plot_par2 <- N_10$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}




####################################################

sub_rc <-c(5+32+32,105-20-20,20+32+32, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_11_H+1)
plot_par1 <- H_11$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
 # text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+1.5,"M11 (1742)",cex=1.2)


sub_rc <-c(5+32+32+15,105-20-20,20+32+32+15, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_11_N+1)
plot_par2 <- N_11$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}





####################################################

sub_rc <-c(5+32+32+32,105-20-20,20+32+32+32, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_12_H+1)
plot_par1 <- H_12$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M12 (1621)",cex=1.2)


sub_rc <-c(5+32+32+32+15,105-20-20,20+32+32+32+15, 120-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_12_N+1)
plot_par2 <- N_12$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}






####################################################

####################################################
sub_rc <-c(5,105-20-20-20,20, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_13_H+1)
plot_par1 <- H_13$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M13 (1075)",cex=1.2)


text(sub_rc[1]-5,sub_rc[4]+2.5,"Expression of Individual Modules",srt=90,cex=1.6,family="Times New Roman")


sub_rc <-c(5+15,105-20-20-20,20+15, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_13_N+1)
plot_par2 <- N_13$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

####################################################

sub_rc <-c(5+32,105-20-20-20,20+32, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_14_H+1)
plot_par1 <- H_14$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+1.5,"M14 (263)",cex=1.2)


sub_rc <-c(5+32+15,105-20-20-20,20+32+15, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_14_N+1)
plot_par2 <- N_14$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}


####################################################

sub_rc <-c(5+32+32,105-20-20-20,20+32+32, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_15_H+1)
plot_par1 <- H_15$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

text(sub_rc[3],sub_rc[4]+1.5,"M15 (439)",cex=1.2)


sub_rc <-c(5+32+32+15,105-20-20-20,20+32+32+15, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_15_N+1)
plot_par2 <- N_15$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}




####################################################

sub_rc <-c(5+32+32+32,105-20-20-20,20+32+32+32, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_16_H+1)
plot_par1 <- H_16$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M16 (1294)",cex=1.2)


sub_rc <-c(5+32+32+32+15,105-20-20-20,20+32+32+32+15, 120-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_16_N+1)
plot_par2 <- N_16$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}











####################################################

####################################################
sub_rc <-c(5,105-20-20-20-20,20, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_17_H+1)
plot_par1 <- H_17$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M17 (376)",cex=1.2)


sub_rc <-c(5+15,105-20-20-20-20,20+15, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_17_N+1)
plot_par2 <- N_17$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}


####################################################

sub_rc <-c(5+32,105-20-20-20-20,20+32, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_18_H+1)
plot_par1 <- H_18$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+1.5,"M18 (157)",cex=1.2)


sub_rc <-c(5+32+15,105-20-20-20-20,20+32+15, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_18_N+1)
plot_par2 <- N_18$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}





####################################################

sub_rc <-c(5+32+32,105-20-20-20-20,20+32+32, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_19_H+1)
plot_par1 <- H_19$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M19 (64)",cex=1.2)


sub_rc <-c(5+32+32+15,105-20-20-20-20,20+32+32+15, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_19_N+1)
plot_par2 <- N_19$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}





####################################################

sub_rc <-c(5+32+32+32,105-20-20-20-20,20+32+32+32, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_20_H+1)
plot_par1 <- H_20$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}


text(sub_rc[3],sub_rc[4]+1.5,"M20 (48)",cex=1.2)


sub_rc <-c(5+32+32+32+15,105-20-20-20-20,20+32+32+32+15, 120-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_20_N+1)
plot_par2 <- N_20$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}







####################################################

####################################################
sub_rc <-c(5,105-20-20-20-20-20,20, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_21_H+1)
plot_par1 <- H_21$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}

for(i in 1){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^2),cex=1.2,font=1,family="Times New Roman")
}

for(i in 2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  text(sub_rc[1]-2,sub_rc[2]+1+2*i*ra,expression(10^4),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M21 (21)",cex=1.2)




for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}


sub_rc <-c(5+15,105-20-20-20-20-20,20+15, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_21_N+1)
plot_par2 <- N_21$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}




for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



####################################################

sub_rc <-c(5+32,105-20-20-20-20-20,20+32, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_22_H+1)
plot_par1 <- H_22$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M22 (26)",cex=1.2)


for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}


sub_rc <-c(5+32+15,105-20-20-20-20-20,20+32+15, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_22_N+1)
plot_par2 <- N_22$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}



for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3]+1,sub_rc[2]-5,"Expression Index",cex=1.6,family="Times New Roman")


####################################################

sub_rc <-c(5+32+32,105-20-20-20-20-20,20+32+32, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_23_H+1)
plot_par1 <- H_23$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
 # text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}



text(sub_rc[3],sub_rc[4]+1.5,"M23 (22)",cex=1.2)


for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}


sub_rc <-c(5+32+32+15,105-20-20-20-20-20,20+32+32+15, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_23_N+1)
plot_par2 <- N_23$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}



for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}




####################################################

sub_rc <-c(5+32+32+32,105-20-20-20-20-20,20+32+32+32, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data1 <- log10(dat_24_H+1)
plot_par1 <- H_24$par[1:2]

Tn1 <- order(T1)

for(i in 1:dim(plot_data1)[1]){
  ll <- plot_data1[i,][Tn1]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#FCBBA1")
}

lines(sub_rc[1]+(T1[Tn1]-5.5)*rat+2.5,linear_equation(plot_par1,T1[Tn1])*ra+sub_rc[2]+1,col="#67000D",lwd=2)

for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0:2){
  segments(sub_rc[1]+0.3,sub_rc[2]+1+2*i*ra,sub_rc[1],sub_rc[2]+1+2*i*ra,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+1+2*i*ra,expression(0),cex=1.2,font=1,family="Times New Roman")
}




text(sub_rc[3],sub_rc[4]+1.5,"M24 (1)",cex=1.2)

for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



sub_rc <-c(5+32+32+32+15,105-20-20-20-20-20,20+32+32+32+15, 120-20-20-20-20-20)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)


plot_data2 <- log10(dat_24_N+1)
plot_par2 <- N_24$par[1:2]

Tn2 <- order(T2)

for(i in 1:dim(plot_data2)[1]){
  
  ll <- plot_data2[i,][Tn2]
  ll[which(ll> 4.5)] <- NA
  lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,ll*ra+sub_rc[2]+1,col="#C6DBEF")
}


lines(sub_rc[1]+(T2[Tn2]-5.5)*rat+2.5,linear_equation(plot_par2,T2[Tn2])*ra+sub_rc[2]+1,col="#08306B",lwd=2)


for(i in 0:2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.3,font=2)
}

for(i in 0){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.5),cex=1.2,font=1,family="Times New Roman")
}


for(i in 1){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.6),cex=1.2,font=1,family="Times New Roman")
}


for(i in 2){
  segments(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2],sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]+0.05,font=2)
  text(sub_rc[1]+2.5+0.1*i*rat,sub_rc[2]-1.5,expression(10^5.7),cex=1.2,font=1,family="Times New Roman")
}



dev.off()


