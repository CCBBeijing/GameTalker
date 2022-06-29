
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB")


incoming_links_H <- c(rep(1,4),rep(2,3),rep(3,3),rep(4,3),rep(5,3),
                      rep(6,2), rep(7,3),rep(8,2),rep(9,3))


outgoing_links_H <- c(4,5,6,8,1,6,8,1,6,8,1,6,8,1,6,8,
                      1,8,1,6,8,1,6,1,6,8)


incoming_links_N <- c(rep(1,4),rep(2,2),rep(3,2),rep(4,2),rep(5,1),
                      rep(6,2), rep(7,2),rep(8,2),rep(9,2))


outgoing_links_N <- c(2,4,5,8,1,5,1,5,1,5,1,5,1,
                      1,5,1,5,1,5,1,5)


incoming_links_N_name <- as.numeric(names(table(incoming_links_N)))
incoming_links_N_num <- as.numeric(table(incoming_links_N))


outgoing_links_N_name <- as.numeric(names(table(outgoing_links_N)))
outgoing_links_N_num <- as.numeric(table(outgoing_links_N))


links_outgoing_N <- rep(0,9)
links_outgoing_N[outgoing_links_N_name] <- outgoing_links_N_num 


links_N <- cbind(incoming_links_N_num,links_outgoing_N,rowSums(cbind(incoming_links_N_num,links_outgoing_N)))

colnames(links_N) <- c("incoming","outgoing","all")



incoming_links_H_name <- as.numeric(names(table(incoming_links_H)))
incoming_links_H_num <- as.numeric(table(incoming_links_H))


outgoing_links_H_name <- as.numeric(names(table(outgoing_links_H)))
outgoing_links_H_num <- as.numeric(table(outgoing_links_H))


links_outgoing_H <- rep(0,9)
links_outgoing_H[outgoing_links_H_name] <- outgoing_links_H_num 



links_H <- cbind(incoming_links_H_num,links_outgoing_H,rowSums(cbind(incoming_links_H_num,links_outgoing_H)))

colnames(links_H) <- c("incoming","outgoing","all")


rowSums(cbind(links_H[,3],links_N[,3]))




library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")


n1 <- order(links_H[,3],decreasing = T)

#n1 <- c(10 , 5, 12 , 4, 11,  8 ,16 , 2,9,1 ,   3 , 6 ,  15 ,18 ,19, 20, 21, 23 ,24,  7, 13 ,14 ,17, 22)


n2 <- order(links_N[,3],decreasing = T)

#n2 <- c(5,  2 ,11,  4 ,10,  1 ,17, 21 , 6, 12, 22 , 3,  7,  8 , 9, 13, 14, 15, 16, 18, 19, 20, 23, 24)


pdf("M13_links.pdf",width = 6,height = 4)

par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(5,55),ylim=c(0, 27-15))


segments(5,11,20,11,lwd=2)
segments(20,1,20,11,lwd=2)



segments(40,11,40+15,11,lwd=2)
segments(40,1,40,11,lwd=2)

M_name <-  c("sM1","sM2","sM3","sM4","sM5","sM6","sM7","sM8","sM9")

for(i in 1:9){
  text(24.2,10.5-i,M_name[n1][i],cex=1.2,font=1,family="Times New Roman",adj=1)
}

for(i in 1:9){
  text(35.8,10.5-i,M_name[n2][i],cex=1.2,font=1,family="Times New Roman",adj=0)
}



for(i in 1:9){
  sub_rc <-c(20,10-i+1,20-links_H[n1[i],3], 10-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#F0FFF0")
  
  sub_rc <-c(20,10-i+1,20-links_H[n1[i],2], 10-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFC0CB")
  
  
}



for(j in 1:9){
  mm <- match(n1[j],n2)
  segments(24.5,10.5-j,35.5,10.5-mm)
}





for(i in 1:10){
  sub_rc <-c(40,10-i+1,40+links_N[n2[i],3], 10-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#F0FFF0")
  
  sub_rc <-c(40,10-i+1,40+links_N[n2[i],2], 10-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFC0CB")
  
}


for(i in 0:3){
  text(20-5*i,11.5,i*5,cex=1.2,font=1,family="Times New Roman")
  segments(20-5*i,11,20-5*i,11-0.3)
}


for(i in 0:3){
  text(40+5*i,11.5,i*5,cex=1.2,font=1,family="Times New Roman")
  segments(40+5*i,11,40+5*i,11-0.3)
}


text(20,0.5,"Tumor",cex=1.2,adj=1,font=1,family="Times New Roman")

text(40,0.5,"TME",cex=1.2,adj=0,font=1,family="Times New Roman")


dev.off()

