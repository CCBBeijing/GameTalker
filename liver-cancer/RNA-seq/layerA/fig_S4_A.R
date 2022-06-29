
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/result")


rm(list = ls())

load("rep_2_k_20.RData")
dat_20 <- BIC
unique(dat_20[[2]][[1]]$clustered_data$cluster)
unique(dat_20[[2]][[2]]$clustered_data$cluster)

BIC_20 <- dat_20[[1]]

dat_20[[1]]


K_20 <- c(length(unique(dat_20[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_20[[2]][[2]]$clustered_data$cluster)))

BIC_20 <- dat_20[[1]]


##########################################



load("rep_2_k_21.RData")
dat_21 <- BIC
unique(dat_21[[2]][[1]]$clustered_data$cluster)
unique(dat_21[[2]][[2]]$clustered_data$cluster)



dat_21[[1]]


K_21 <- c(length(unique(dat_21[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_21[[2]][[2]]$clustered_data$cluster)))


BIC_21 <- dat_21[[1]]

##########################################



load("rep_2_k_22.RData")
dat_22 <- BIC
unique(dat_22[[2]][[1]]$clustered_data$cluster)
unique(dat_22[[2]][[2]]$clustered_data$cluster)



dat_22[[1]]


K_22 <- c(length(unique(dat_22[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_22[[2]][[2]]$clustered_data$cluster)))


BIC_22 <- dat_22[[1]]




##########################################

load("rep_2_k_25.RData")
dat_25 <- BIC
unique(dat_25[[2]][[1]]$clustered_data$cluster)
unique(dat_25[[2]][[2]]$clustered_data$cluster)



dat_25[[1]]


K_25 <- c(length(unique(dat_25[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_25[[2]][[2]]$clustered_data$cluster)))

BIC_25 <- dat_25[[1]]



##########################################

load("rep_2_k_26.RData")
dat_26 <- BIC
unique(dat_26[[2]][[1]]$clustered_data$cluster)
unique(dat_26[[2]][[2]]$clustered_data$cluster)



dat_26[[1]]


K_26 <- c(length(unique(dat_26[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_26[[2]][[2]]$clustered_data$cluster)))

BIC_26 <- dat_26[[1]]


##########################################

load("rep_2_k_28.RData")
dat_28 <- BIC
unique(dat_28[[2]][[1]]$clustered_data$cluster)
unique(dat_28[[2]][[2]]$clustered_data$cluster)



dat_28[[1]]


K_28 <- c(length(unique(dat_28[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_28[[2]][[2]]$clustered_data$cluster)))


BIC_28 <- dat_28[[1]]



##########################################

load("rep_2_k_29.RData")
dat_29 <- BIC
unique(dat_29[[2]][[1]]$clustered_data$cluster)
unique(dat_29[[2]][[2]]$clustered_data$cluster)



dat_29[[1]]


K_29 <- c(length(unique(dat_29[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_29[[2]][[2]]$clustered_data$cluster)))


BIC_29 <- dat_29[[1]]



##########################################

load("rep_2_k_31.RData")
dat_31 <- BIC
unique(dat_31[[2]][[1]]$clustered_data$cluster)
unique(dat_31[[2]][[2]]$clustered_data$cluster)



dat_31[[1]]


K_31 <- c(length(unique(dat_31[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_31[[2]][[2]]$clustered_data$cluster)))


BIC_31 <- dat_31[[1]]





K_BIC <- rbind(
  cbind(K_20,BIC_20[,2]),
  cbind(K_21,BIC_21[,2]),
  cbind(K_22,BIC_22[,2]),
  cbind(K_25,BIC_25[,2]),
  cbind(K_26,BIC_26[,2]),
  cbind(K_28,BIC_28[,2]),
  cbind(K_29,BIC_29[,2]),
  cbind(K_31,BIC_31[,2]))



K_11 <- -3100875
K_12 <- -3135262
K_13 <- -3155433
#K_18 <- -3558327
K_19 <- 	-3438647


K_20 <- -3565701
K_22 <- 	-3789857
K_24 <- 	-3837288
K_26 <- -3717477



plot(c(11,12,13,19,20,22,24,26),c(K_11,K_12,K_13,K_19,K_20,K_22,K_24,K_26),type="l",xlab="cluster",ylab="BIC")


plot(K_BIC[,1],K_BIC[,2])


dat_K_BIC <- data.frame(
  K = c(11,12,13,19,20,22,24,26),
  BIC = c(K_11,K_12,K_13,K_19,K_20,K_22,K_24,K_26)
)




library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")
options("scipen"=100, "digits"=4)

pdf("BIC.pdf",width=8,height =6)


par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(0.5,24),ylim=c(4, 15))

sub_rc <-c(5,5,24, 15)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

lines(sub_rc[1]+dat_K_BIC$K-10+1,sub_rc[4]+((dat_K_BIC$BIC)/100000+30),lwd=3)

v1 <- c()
for(i in 0:8){
  v1 <- c(v1,-200000-50000*i)
}

#v1 <- as.character(v1)

for(i in 0:4){
  segments(sub_rc[1],sub_rc[4]-2*i,sub_rc[1]+0.3,sub_rc[4]-2*i,font=2)
  text(sub_rc[1]-2,sub_rc[4]-2*i,as.numeric(-3000000-200000*i),cex=1.6,font=1,family="Times New Roman")
}


for(i in 0:3){
  segments(sub_rc[1]+5*i+1,sub_rc[2],sub_rc[1]+5*i+1,sub_rc[2]+0.3,font=2)
  text(sub_rc[1]+5*i+1,sub_rc[2]-0.4,10+5*i,cex=1.6,font=1,family="Times New Roman")
}

segments(sub_rc[1]+15,sub_rc[2],sub_rc[1]+15,sub_rc[4],lwd=3,lty=2,col="red")

#+((dat_K_BIC$BIC[6])/10000+20)


text(sub_rc[1]+15,sub_rc[2]-0.4,24,col="red",cex=1.6,font=1,family="Times New Roman")


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[2]-1,"No. of Modules",cex=2,font=1,family="Times New Roman")


text(sub_rc[1]-4.5,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],"BIC",cex=2,font=1,srt=90,family="Times New Roman")

dev.off()



