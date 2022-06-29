setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerA")


incoming_links_H <- c(rep(1,2),rep(2,4),rep(3,2),rep(4,4),rep(5,3),
                      rep(6,3), rep(7,2),rep(8,4),rep(9,3),rep(10,4),
                      rep(11,4),rep(12,4),rep(13,3),rep(14,3),rep(15,2),
                      rep(16,3),rep(17,3),rep(18,3),rep(19,4),rep(20,3),
                      rep(21,3),rep(22,3),rep(23,3),rep(24,3))


outgoing_links_H <- c(3,15,11,13,14,18,1,15,11,13,14,18,11,13,14,
                      10,11,18,10,13,11,13,14,18,11,13,14,11,13,14,18,
                      4,8,10,14,11,13,14,18,10,14,18,11,13,18,1,3,
                      1,3,15,1,3,15,10,11,13,11,13,14,18,1,3,15,
                      1,3,15,1,3,15,11,13,18,1,3,15)


incoming_links_N <- c(rep(1,2),rep(2,2),rep(3,3),rep(4,2),rep(5,3),
                      rep(6,3), rep(7,1),rep(8,1),rep(9,2),rep(10,3),
                      rep(11,2),rep(12,3),rep(13,1),rep(14,2),rep(15,1),
                      rep(16,1),rep(17,1),rep(18,1),rep(19,1),rep(20,1),
                      rep(21,3),rep(22,3),rep(23,3),rep(24,1))


outgoing_links_N <- c(8,16,9,11,8,13,16,9,11,4,9,11,
                      4,9,11,3,3,4,11,4,9,11,
                      4,9,4,9,11,3,8,13,3,
                      3,3,3,3,3,
                      4,9,11,4,9,11,4,9,11,3)


incoming_links_N_name <- as.numeric(names(table(incoming_links_N)))
incoming_links_N_num <- as.numeric(table(incoming_links_N))


outgoing_links_N_name <- as.numeric(names(table(outgoing_links_N)))
outgoing_links_N_num <- as.numeric(table(outgoing_links_N))


links_outgoing_N <- rep(0,24)
links_outgoing_N[outgoing_links_N_name] <- outgoing_links_N_num 


links_N <- cbind(incoming_links_N_num,links_outgoing_N,rowSums(cbind(incoming_links_N_num,links_outgoing_N)))

colnames(links_N) <- c("incoming","outgoing","all")



incoming_links_H_name <- as.numeric(names(table(incoming_links_H)))
incoming_links_H_num <- as.numeric(table(incoming_links_H))


outgoing_links_H_name <- as.numeric(names(table(outgoing_links_H)))
outgoing_links_H_num <- as.numeric(table(outgoing_links_H))


links_outgoing_H <- rep(0,24)
links_outgoing_H[outgoing_links_H_name] <- outgoing_links_H_num 



links_H <- cbind(incoming_links_H_num,links_outgoing_H,rowSums(cbind(incoming_links_H_num,links_outgoing_H)))

colnames(links_H) <- c("incoming","outgoing","all")


rowSums(cbind(links_H[,3],links_N[,3]))




library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")


nn <- order(links_H[,3],decreasing = T)

n1 <- c( 11, 13, 14 ,18 , 1,  3 ,15 ,10,  4 , 8 , 2, 12 ,19 , 5,  6 , 9, 16, 17, 20, 21, 22, 23, 24 , 7)


nn <- order(links_N[,3],decreasing = T)

n2 <- c( 3 , 9, 11,  4 , 8 ,  13 ,16 ,5 , 6, 10 ,12,21, 22 ,23,  1 , 2 ,14 , 7 ,15, 17, 18 ,19 ,20, 24)


pdf("links.pdf",width = 8,height = 6)

par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(5,53),ylim=c(-1, 27))


segments(5,25,25,25,lwd=2)
segments(25,0,25,25,lwd=2)



segments(38,25,38+15,25,lwd=2)
segments(38,0,38,25,lwd=2)

M_name <-  c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10",
             "M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24")

for(i in 1:24){
  text(27,24.5-i,M_name[n1][i],cex=1.2,font=1,family="Times New Roman")
}

for(i in 1:24){
  text(36,24.5-i,M_name[n2][i],cex=1.2,font=1,family="Times New Roman")
}



for(i in 1:24){
  sub_rc <-c(25,24-i+1,25-links_H[n1[i],3], 24-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#F0FFF0")
  
  sub_rc <-c(25,24-i+1,25-links_H[n1[i],2], 24-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFC0CB")
  
}



for(j in 1:24){
  mm <- match(n1[j],n2)
  segments(28.5,24.5-j,34.5,24.5-mm)
}





for(i in 1:24){
  sub_rc <-c(38,24-i+1,38+links_N[n2[i],3], 24-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#F0FFF0")
  
  sub_rc <-c(38,24-i+1,38+links_N[n2[i],2], 24-i)
  rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FFC0CB")
  
}


for(i in 0:4){
  text(25-5*i,26,i*5,cex=1.2,font=1,family="Times New Roman")
  segments(25-5*i,25,25-5*i,25-0.5)
}


for(i in 0:5){
  text(38+5*i,26,i*5,cex=1.2,font=1,family="Times New Roman")
  segments(38+5*i,25,38+5*i,25-0.5)
}


text(25,-1,"Tumor",cex=1.2,adj=1,font=1,family="Times New Roman")

text(38,-1,"TME",cex=1.2,adj=0,font=1,family="Times New Roman")


dev.off()

