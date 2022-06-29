
setwd("~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB/M13_result")


rm(list = ls())

##############################


load("rep_3_k_8.RData")
dat_8 <- BIC
unique(dat_8[[2]][[1]]$clustered_data$cluster)
unique(dat_8[[2]][[2]]$clustered_data$cluster)
unique(dat_8[[2]][[3]]$clustered_data$cluster)
BIC_8 <- dat_8[[1]]

dat_8[[1]]


K_8 <- c(length(unique(dat_8[[2]][[1]]$clustered_data$cluster)),
         length(unique(dat_8[[2]][[2]]$clustered_data$cluster)),
         length(unique(dat_8[[2]][[3]]$clustered_data$cluster)))

BIC_8 <- dat_8[[1]]

##############################


load("rep_3_k_9.RData")
dat_9 <- BIC
unique(dat_9[[2]][[1]]$clustered_data$cluster)
unique(dat_9[[2]][[2]]$clustered_data$cluster)
unique(dat_9[[2]][[3]]$clustered_data$cluster)
BIC_9 <- dat_9[[1]]

dat_9[[1]]


K_9 <- c(length(unique(dat_9[[2]][[1]]$clustered_data$cluster)),
         length(unique(dat_9[[2]][[2]]$clustered_data$cluster)),
         length(unique(dat_9[[2]][[3]]$clustered_data$cluster)))

BIC_9 <- dat_9[[1]]


##############################


load("rep_3_k_10.RData")
dat_10 <- BIC
unique(dat_10[[2]][[1]]$clustered_data$cluster)
unique(dat_10[[2]][[2]]$clustered_data$cluster)
unique(dat_10[[2]][[3]]$clustered_data$cluster)
BIC_10 <- dat_10[[1]]

dat_10[[1]]


K_10 <- c(length(unique(dat_10[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_10[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_10[[2]][[3]]$clustered_data$cluster)))

BIC_10 <- dat_10[[1]]


##############################


load("rep_3_k_11.RData")
dat_11 <- BIC
unique(dat_11[[2]][[1]]$clustered_data$cluster)
unique(dat_11[[2]][[2]]$clustered_data$cluster)
unique(dat_11[[2]][[3]]$clustered_data$cluster)
BIC_11 <- dat_11[[1]]

dat_11[[1]]


K_11 <- c(length(unique(dat_11[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_11[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_11[[2]][[3]]$clustered_data$cluster)))

BIC_11 <- dat_11[[1]]


##############################


load("rep_3_k_12.RData")
dat_12 <- BIC
unique(dat_12[[2]][[1]]$clustered_data$cluster)
unique(dat_12[[2]][[2]]$clustered_data$cluster)
unique(dat_12[[2]][[3]]$clustered_data$cluster)
BIC_12 <- dat_12[[1]]

dat_12[[1]]


K_12 <- c(length(unique(dat_12[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_12[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_12[[2]][[3]]$clustered_data$cluster)))

BIC_12 <- dat_12[[1]]

##############################


load("rep_3_k_13.RData")
dat_13 <- BIC
unique(dat_13[[2]][[1]]$clustered_data$cluster)
unique(dat_13[[2]][[2]]$clustered_data$cluster)
unique(dat_13[[2]][[3]]$clustered_data$cluster)
BIC_13 <- dat_13[[1]]

dat_13[[1]]


K_13 <- c(length(unique(dat_13[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_13[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_13[[2]][[3]]$clustered_data$cluster)))

BIC_13 <- dat_13[[1]]


##############################


load("rep_3_k_14.RData")
dat_14 <- BIC
unique(dat_14[[2]][[1]]$clustered_data$cluster)
unique(dat_14[[2]][[2]]$clustered_data$cluster)
unique(dat_14[[2]][[3]]$clustered_data$cluster)
BIC_14 <- dat_14[[1]]

dat_14[[1]]


K_14 <- c(length(unique(dat_14[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_14[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_14[[2]][[3]]$clustered_data$cluster)))

BIC_14 <- dat_14[[1]]

##############################

load("rep_3_k_15.RData")
dat_15 <- BIC
unique(dat_15[[2]][[1]]$clustered_data$cluster)
unique(dat_15[[2]][[2]]$clustered_data$cluster)
unique(dat_15[[2]][[3]]$clustered_data$cluster)
BIC_15 <- dat_15[[1]]

dat_15[[1]]


K_15 <- c(length(unique(dat_15[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_15[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_15[[2]][[3]]$clustered_data$cluster)))

BIC_15 <- dat_15[[1]]




#######################################


load("rep_3_k_16.RData")
dat_16 <- BIC
unique(dat_16[[2]][[1]]$clustered_data$cluster)
unique(dat_16[[2]][[2]]$clustered_data$cluster)
unique(dat_16[[2]][[3]]$clustered_data$cluster)
BIC_16 <- dat_16[[1]]

dat_16[[1]]


K_16 <- c(length(unique(dat_16[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_16[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_16[[2]][[3]]$clustered_data$cluster)))

BIC_16 <- dat_16[[1]]


#######################################

load("rep_3_k_17.RData")
dat_17 <- BIC
unique(dat_17[[2]][[1]]$clustered_data$cluster)
unique(dat_17[[2]][[2]]$clustered_data$cluster)
unique(dat_17[[2]][[3]]$clustered_data$cluster)
BIC_17 <- dat_17[[1]]

dat_17[[1]]


K_17 <- c(length(unique(dat_17[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_17[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_17[[2]][[3]]$clustered_data$cluster)))

BIC_17 <- dat_17[[1]]

#######################################

load("rep_3_k_18.RData")
dat_18 <- BIC
unique(dat_18[[2]][[1]]$clustered_data$cluster)
unique(dat_18[[2]][[2]]$clustered_data$cluster)
unique(dat_18[[2]][[3]]$clustered_data$cluster)
BIC_18 <- dat_18[[1]]

dat_18[[1]]


K_18 <- c(length(unique(dat_18[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_18[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_18[[2]][[3]]$clustered_data$cluster)))

BIC_18 <- dat_18[[1]]


##############################


load("rep_3_k_19.RData")
dat_19 <- BIC
unique(dat_19[[2]][[1]]$clustered_data$cluster)
unique(dat_19[[2]][[2]]$clustered_data$cluster)
unique(dat_19[[2]][[3]]$clustered_data$cluster)
BIC_19 <- dat_19[[1]]

dat_19[[1]]


K_19 <- c(length(unique(dat_19[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_19[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_19[[2]][[3]]$clustered_data$cluster)))

BIC_19 <- dat_19[[1]]


##############################


load("rep_3_k_20.RData")
dat_20 <- BIC
unique(dat_20[[2]][[1]]$clustered_data$cluster)
unique(dat_20[[2]][[2]]$clustered_data$cluster)
unique(dat_20[[2]][[3]]$clustered_data$cluster)
BIC_20 <- dat_20[[1]]

dat_20[[1]]


K_20 <- c(length(unique(dat_20[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_20[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_20[[2]][[3]]$clustered_data$cluster)))

BIC_20 <- dat_20[[1]]

##############################

load("rep_3_k_21.RData")
dat_21 <- BIC
unique(dat_21[[2]][[1]]$clustered_data$cluster)
unique(dat_21[[2]][[2]]$clustered_data$cluster)
unique(dat_21[[2]][[3]]$clustered_data$cluster)
BIC_21 <- dat_21[[1]]

dat_21[[1]]


K_21 <- c(length(unique(dat_21[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_21[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_21[[2]][[3]]$clustered_data$cluster)))

BIC_21 <- dat_21[[1]]


##############################


load("rep_3_k_22.RData")
dat_22 <- BIC
unique(dat_22[[2]][[1]]$clustered_data$cluster)
unique(dat_22[[2]][[2]]$clustered_data$cluster)
unique(dat_22[[2]][[3]]$clustered_data$cluster)
BIC_22 <- dat_22[[1]]

dat_22[[1]]


K_22 <- c(length(unique(dat_22[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_22[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_22[[2]][[3]]$clustered_data$cluster)))

BIC_22 <- dat_22[[1]]



##############################


load("rep_3_k_23.RData")
dat_23 <- BIC
unique(dat_23[[2]][[1]]$clustered_data$cluster)
unique(dat_23[[2]][[2]]$clustered_data$cluster)
unique(dat_23[[2]][[3]]$clustered_data$cluster)
BIC_23 <- dat_23[[1]]

dat_23[[1]]


K_23 <- c(length(unique(dat_23[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_23[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_23[[2]][[3]]$clustered_data$cluster)))

BIC_23 <- dat_23[[1]]



##############################


load("rep_3_k_24.RData")
dat_24 <- BIC
unique(dat_24[[2]][[1]]$clustered_data$cluster)
unique(dat_24[[2]][[2]]$clustered_data$cluster)
unique(dat_24[[2]][[3]]$clustered_data$cluster)
BIC_24 <- dat_24[[1]]

dat_24[[1]]


K_24 <- c(length(unique(dat_24[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_24[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_24[[2]][[3]]$clustered_data$cluster)))

BIC_24 <- dat_24[[1]]



##############################


load("rep_3_k_25.RData")
dat_25 <- BIC
unique(dat_25[[2]][[1]]$clustered_data$cluster)
unique(dat_25[[2]][[2]]$clustered_data$cluster)
unique(dat_25[[2]][[3]]$clustered_data$cluster)
BIC_25 <- dat_25[[1]]

dat_25[[1]]


K_25 <- c(length(unique(dat_25[[2]][[1]]$clustered_data$cluster)),
          length(unique(dat_25[[2]][[2]]$clustered_data$cluster)),
          length(unique(dat_25[[2]][[3]]$clustered_data$cluster)))

BIC_25 <- dat_25[[1]]



data <- read.csv(file = "~/Documents/R-doc/wurongling/liver-cancer/fig8-8/network/layerB/M13/dat_M13.csv", row.names = 1, check.names = F)


K_BIC <- rbind(cbind(K_8,BIC_8[,3]),
               cbind(K_9,BIC_9[,3]),
               cbind(K_10,BIC_10[,3]),
               cbind(K_11,BIC_11[,3]),
               cbind(K_12,BIC_12[,3]),
               cbind(K_13,BIC_13[,3]),
               cbind(K_14,BIC_14[,3]),
  cbind(K_15,BIC_15[,3]),
  cbind(K_16,BIC_16[,3]),
  cbind(K_17,BIC_17[,3]),
  cbind(K_18,BIC_18[,3]),
  cbind(K_19,BIC_19[,3]),
  cbind(K_20,BIC_20[,3]),
  cbind(K_21,BIC_21[,3]),
  cbind(K_22,BIC_22[,3]),
  cbind(K_23,BIC_23[,3]),
  cbind(K_24,BIC_24[,3]),
  cbind(K_25,BIC_25[,3]))


BB <- c()
for(i in 1:dim(K_BIC)[1]){
  old_par <- 6+K_BIC[i,1]*6
  B1 <-  2*(K_BIC[i,2])+log(nrow(data))*old_par*100
  BB <- c(BB,B1)
}




K_BIC1 <- data.frame(
  K = K_BIC[,1],
  BIC = BB
)


plot(K_BIC1$K,K_BIC1$BIC)








K_2 <- 		-107630
K_3 <- 		-114773
K_4 <-				-118908
K_5 <-		-121471
K_6 <-				-123431
K_7 <-				-124149
K_8 <-					-124237
  K_9 <-				-124454
  K_10 <-				-123829
  K_11 <-			-122834
  K_12 <-				-120714

plot(c(2:12),c(K_2,K_3,K_4,K_5,K_6,K_7,K_8,K_9,K_10,K_11,K_12),type="l",xlab="cluster",ylab="BIC")


dat_K_BIC <- data.frame(
  K = c(2:12),
  BIC = c(K_2,K_3,K_4,K_5,K_6,K_7,K_8,K_9,K_10,K_11,K_12)
)







library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")
options("scipen"=100, "digits"=4)

pdf("BIC_M13.pdf",width=8,height =6)

raw_k <- 2
cl <- 9
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(3,17),ylim=c(3.5, 7))

sub_rc <-c(5,4,17, 7)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

lines(sub_rc[1]+dat_K_BIC$K-raw_k+1,sub_rc[4]+((dat_K_BIC$BIC)/10000+10),lwd=3)


for(i in 0:6){
  segments(sub_rc[1],sub_rc[4]-0.5*i,sub_rc[1]+0.2,sub_rc[4]-0.5*i,font=2)
  text(sub_rc[1]-1,sub_rc[4]-0.5*i,as.numeric(-105000-5000*i),cex=1.6,font=1,family="Times New Roman")
}


for(i in 0:4){
  segments(sub_rc[1]+3*i+1,sub_rc[2],sub_rc[1]+3*i+1,sub_rc[2]+0.1,font=2)
  text(sub_rc[1]+3*i+1,sub_rc[2]-0.15,raw_k+3*i,cex=1.6,font=1,family="Times New Roman")
}

segments(sub_rc[1]+cl-raw_k+1,sub_rc[2],sub_rc[1]+cl-raw_k+1,sub_rc[4],lwd=3,lty=2,col="red")

#+((dat_K_BIC$BIC[6])/10000+20)


text(sub_rc[1]+cl-raw_k+1,sub_rc[2]-0.15,cl,col="red",cex=1.6,font=1,family="Times New Roman")


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[2]-0.4,"No. of subModules",cex=2,font=1,family="Times New Roman")


text(sub_rc[1]-2.2,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],"BIC",cex=2,font=1,srt=90,family="Times New Roman")

dev.off()




