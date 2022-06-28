library(splines)
library(orthogonalsplinebasis)
library(MASS)
library(grplasso)
library(parallel)
library(glmnet)
library(ggplot2)


data_all_1 <- list()
for (m in 1:length(data_cluster[,1])) {
  data_raw1 <-matrix(NA,ncol = 10,nrow = 6)
  rownames(data_raw1) <- c("tumor sizeo","H","N","tumor sizee","H*N","H/N")
  
  
  data_raw1[1,] <- as.numeric(data[[m]][1,])
  data_raw1[2,] <- as.numeric(data[[m]][2,])
  data_raw1[3,] <- as.numeric(data[[m]][3,])
  
  data_raw2 <- data_raw1
  for (i in 1:3) {
    for (j in 1:10) {
      data_raw1[i,j] <- (data_raw2[i,j]-min(data_raw2[i,]))/(max(data_raw2[i,])-min(data_raw2[i,]))
    }
  }
  data_raw1[1:3,] <- data_raw1[1:3,]+1
  
  data_raw1[4,] <- apply(data_raw1[1:3,],2,sum)
  
  data_raw1[1,] <- data_raw1[1,order(data_raw1[4,],decreasing = F)]
  data_raw1[2,] <- data_raw1[2,order(data_raw1[4,],decreasing = F)]
  data_raw1[3,] <- data_raw1[3,order(data_raw1[4,],decreasing = F)]
  data_raw1[4,] <- data_raw1[4,order(data_raw1[4,],decreasing = F)]
  
  colnames(data_raw1) <- data_raw1[4,]
  
  data_raw1 <- log(data_raw1)
  
  data_raw1[4,] <- data_raw1[1,]
  data_raw1[5,] <- data_raw1[2,]+data_raw1[3,]
  data_raw1[6,] <- data_raw1[2,]-data_raw1[3,]
  
  data_raw1<- data_raw1-min(data_raw1[6,])
  data_all_1[[m]] <- data_raw1
}

data_final<- list()
for (j in 1:length(data_cluster[,1])) {
  t<- as.numeric(colnames(data_all_1[[j]]))
  
  par_1 <- matrix(NA,ncol = 2,nrow = 6)
  for (i in 1:6) {
    res <- optim(par = c(5,0.5),method="BFGS",rrloss,t=t,yy=data_all_1[[j]][i,])
    par_1[i,] <- res$par
  }
  
  tt <- seq(as.numeric(colnames(data_all_1[[j]]))[1],as.numeric(colnames(data_all_1[[j]]))[10],length=100)
  data1 <- matrix(NA,ncol = 100,nrow = 6)
  for (m in 1:6) {
    data1[m,] <- mix(par_1[m,],tt)
  }
  
  rownames(data1) <- rownames(data_raw1)
  colnames(data1) <- tt
  
  data_final[[j]] <- data1
}

lasso <- matrix(0,ncol = 6,nrow = 6)
colnames(lasso) <- rownames(data_raw1)
rownames(lasso) <- rownames(data_raw1)

lasso[1,2:3] <- 1
lasso[1,5] <- 1
lasso[2,3] <- 1
lasso[3,2] <- 1
lasso[4,2:3] <- 1
lasso[4,6] <- 1
lasso[5,2] <- 1
lasso[6,2] <- 1

smL <- function(times,para,DS1){
  
  sum((DS1-Legendre.model(t=times,mu=para))^2)
}


LMall <- function(NX,nt,nstep=30,order){
  
  stp <- (max(nt)-min(nt))/nstep
  res <- c()
  for(j in 1:nstep){
    
    tg1 <- Legendre.model11((j-1)*stp+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg2 <- Legendre.model11(j*stp/2+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg3 <- Legendre.model11(j*stp/2+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg4 <- Legendre.model11(j*stp+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tmp1 <- rbind(tg1,tg2,tg3,tg4)
    res <- rbind(res,tmp1)
  }
  res
}


ode.optim <- function(y.c,connect,effect,LL,nstep,order,times){
  self <- y.c
  indexx <- which(connect[y.c,]==1)
  para <- rep(0.000001,length(indexx)*(order-1))
  res <- optim(para,fitPKM,NG=(effect),self=y.c,nconnect=connect[y.c,],nt=times,order=order,nstep=nstep,
               LL=LL,method="BFGS",control=list(maxit=100,trace=T)
  )
  cat("Gene=",y.c," ",res$value,"\n")
  A <- ode.sovle.ind(NG=(effect),res$par,nconnect=connect[y.c,],nt=times,order=order,nstep=nstep,LL=LL,self=self)
  return(A)
}


optim.parallel <- function(connect,effect,n.cores,proc,order,times,nstep){
  
  diag(connect) <- 1
  nt1 <- min(times)
  nt2 <- max(times)
  
  LL <- LMall(NX=1,nt=seq(nt1,nt2,(nt2-nt1)/nstep),nstep=nstep,order=order)
  
  nx <- dim(effect)[2]
  
  grp <- floor(nx/n.cores)
  grp.i <- c()
  if(n.cores==1){
    grp.i <- c(grp.i,rep(1,nx))
  }else{
    for(ii in 1:n.cores){
      if(ii==n.cores){
        grp.i <- c(grp.i,rep(ii,nx-grp*(ii-1)))
      }else{
        grp.i <- c(grp.i,rep(ii,grp))
      }
    }
  }
  
  grp.ii <- unique(grp.i)
  
  res.list <- mclapply(grp.ii, function(i)
  {
    y.c <- 	which(grp.i==i)
    A <- sapply(y.c, proc, connect=connect,effect=effect,LL=LL,nstep=nstep,order=order,times=times);
    return (unlist(A));
  }, mc.cores=n.cores )
  
  res1 <- do.call("c", res.list)
  res2 <- parallel.data.optim(res1,connect,times)
  return(res2)
}

parallel.data.optim <- function(rd,nm,ntt){
  
  nrd <- matrix(rd,nrow=length(ntt))
  nn <- dim(nm)[1]
  ki <- 0
  allist <- list()
  for(i in 1:nn){
    iii <- (which(nm[i,]==1))
    iiil <- length(iii)
    tmp.d <- nrd[,(ki+1):(ki+iiil)]
    if(is.matrix(tmp.d)){
      colnames(tmp.d) <- iii
    }else{
      names(tmp.d) <- iii
    }
    
    allist[[i]] <- tmp.d
    ki <- ki + iiil
  }
  
  return(allist)
}




interType <- function(con,alle,sme){
  
  diag(con) <- 0
  nn <- dim(con)[1]
  connfest <- matrix(0,nrow=nn,ncol=nn)
  indp <- c()
  inter <- list()
  for(i in 1:nn){
    al <- alle[[i]]
    index <- which(as.numeric(colnames(al))==i)
    if(is.matrix(al)){
      lindp <- al[,index]
      linter <- al[,-index]
      indp <- cbind(indp,lindp)
      inter[[i]] <- linter
      rcor <- cor(sme[i,],linter)
    }else{
      indp <- cbind(indp,al)
      inter[[i]] <- 0
      rcor <- 0
    }
    
    
    connfest[i,which(con[i,]==1)] <- as.numeric(rcor)
  }
  
  return(list(connfest=connfest,connect=con,indp=indp,inter=inter))
  
}

Legendre.model <-function( t, mu, tmin=NULL, tmax=NULL )
{
  u <- -1;
  v <- 1;
  if (is.null(tmin)) tmin<-min(t);
  if (is.null(tmax)) tmax<-max(t);
  ti    <- u + ((v-u)*(t-tmin))/(tmax - tmin);
  np.order <- length(mu)-1;
  L <- mu[1] + ti*mu[2];
  if (np.order>=2)
    L <- L + 0.5*(3*ti*ti-1)* mu[3] ;
  if (np.order>=3)
    L <- L + 0.5*(5*ti^3-3*ti)*mu[4] ;
  if (np.order>=4)
    L <- L + 0.125*(35*ti^4-30*ti^2+3)* mu[5];
  if (np.order>=5)
    L <- L + 0.125*(63*ti^5-70*ti^3+15*ti)*mu[6];
  if (np.order>=6)
    L <- L + (1/16)*(231*ti^6-315*ti^4+105*ti^2-5)* mu[7];
  if (np.order>=7)
    L <- L + (1/16)*(429*ti^7-693*ti^5+315*ti^3-35*ti)* mu[8];
  if (np.order>=8)
    L <- L + (1/128)*(6435*ti^8-12012*ti^6+6930*ti^4-1260*ti^2+35)* mu[9];
  if (np.order>=9)
    L <- L + (1/128)*(12155*ti^9-25740*ti^7+18018*ti^5-4620*ti^3+315*ti)* mu[10];
  if (np.order>=10)
    L <- L + (1/256)*(46189*ti^10-109395*ti^8+90090*ti^6-30030*ti^4+3465*ti^2-63)* mu[11];
  if (np.order>=11)
  {
    for(r in 11:(np.order))
    {
      kk <- ifelse(r%%2==0, r/2, (r-1)/2);
      for (k in c(0:kk) )
      {
        L <- L + (-1)^k*factorial(2*r-2*k)/factorial(k)/factorial(r-k)/factorial(r-2*k)/(2^r)*ti^(r-2*k)*mu[r+1];
      }
    }
  }
  return(L);
}

Legendre.model11 <- function(t, np.order,tmin = NULL, tmax = NULL)
{
  u <- -1;
  v <- 1;
  if (is.null(tmin))
    tmin <- min(t);
  if (is.null(tmax))
    tmax <- max(t);
  ti    <- u + ((v - u) * (t - tmin)) / (tmax - tmin);
  L <- rep(NA,np.order)
  L[1] <- 1;
  if (np.order >= 2)
    L[2] <- 0.5 * (6 * ti) 
  if (np.order >= 3)
    L[3] <- 0.5 * (15 * ti ^ 2 - 3) 
  if (np.order >= 4)
    L[4] <-  0.125 * (35 * 4 * ti ^ 3 - 60 * ti) 
  if (np.order >= 5)
    L[5] <-  0.125 * (63 * 5 * ti ^ 4 - 210 * ti ^ 2 + 15)
  if (np.order >= 6)
    L[6] <-(1 / 16) * (231 * 6 * ti ^ 5 - 315 * 4 * ti ^ 3 + 105 * 2 * ti) 
  if (np.order >= 7)
    L[7] <- (1 / 16) * (429 * 7 * ti ^ 6 - 693 * 5 * ti ^ 4 + 315 * 3 *ti ^ 2 - 35)
  if (np.order>=8)
    L[8] <-  (1/128) * (6435 * 8 * ti ^ 7 - 12012 * 6 * ti ^ 5 + 6930 * 4 * ti ^ 3 - 1260 * 2 * ti)
  if (np.order>=9)
    L[9] <-  (1/128) * (12155 * 9 * ti ^ 8 - 25740 * 7 * ti ^ 6 + 18018 * 5 * ti ^ 4 - 4620 * 3 * ti ^ 2 + 315)
  if (np.order>=10)
    L[10] <-  (1/256) * (46189 * 10 * ti ^ 9 - 109395 * 8 * ti ^ 7 + 90090 * 6 * ti ^ 5 - 30030 * 4 * ti ^ 3 + 3465 * 2 * ti)
  if (np.order>=11)
    L[11] <-  (1/512) * (176358 * 11 * ti ^ 10 - 461890 * 9 * ti ^ 8 + 437580 * 7 * ti ^ 6 - 180180 * 5 * ti ^ 4 + 30030 * 3 * ti^ 2 - 1386)
  if (np.order>=12)
    L[12] <-  660.1943 * 12 * ti ^ 11 -1894.471 * 10 * ti ^ 9 + 2029.79 * 8 * ti ^ 7  - 997.0898 * 6 * ti ^ 5 + 219.9463 * 4 * ti^ 3 - 17.5957* 2 * ti
  if (np.order>=13)
    L[13] <-  1269.604 * 13 * ti ^ 12 -3961.166 * 11 * ti ^ 10 + 4736.177 * 9 * ti ^ 8  - 2706.387 * 7 * ti ^ 6 + 747.8174 * 5 * ti^ 4 - 87.97852* 3 * ti^ 2 + 2.932617
  if (np.order>=14)
    L[14] <-  2448.523 * 14 * ti ^ 13 -8252.429 * 12 * ti ^ 11 + 10893.21 * 10 * ti ^ 9  - 7104.265 * 8 * ti ^ 7 + 2368.088 * 6 * ti^ 5 - 373.9087* 4 * ti^ 3 + 21.99463* 2 * ti 
  if (np.order>=15)
    L[15] <-  4733.811 * 15 * ti ^ 14 -17139.66 * 13 * ti ^ 12 + 24757.29 * 11 * ti ^ 10  - 18155.34 * 9 * ti ^ 8 + 7104.265 * 7 * ti^ 6 - 1420.853* 5 * ti^ 4 + 124.6362* 3 * ti ^ 2 - 3.14209
  if (np.order>=16)
    L[16] <-  9171.759 * 16 * ti ^ 15 -35503.58 * 14 * ti ^ 13 + 55703.9 * 12 * ti ^ 11  - 45388.36 * 10 * ti ^ 9 + 20424.76 * 8 * ti^ 7 - 4972.986* 6 * ti^ 5 + 592.0221* 4 * ti ^ 3 - 26.70776* 2 * ti 
  if (np.order>=17)
    L[17] <-  17804 * 17 * ti ^ 16 -73374.07 * 15 * ti ^ 14 + 124262.5 * 13 * ti ^ 12  - 111407.8 * 11 * ti ^ 10 + 56735.45 * 9 * ti^ 8 - 16339.81* 7 * ti^ 6 + 2486.493* 5 * ti ^ 4 - 169.1492* 3 * ti ^ 2 +3.33847
  if (np.order>=18)
    L[18] <-  34618.89 * 18 * ti ^ 17 -151334 * 16 * ti ^ 15 + 275152.8 * 14 * ti ^ 13  - 269235.5 * 12 * ti ^ 11 + 153185.7 * 10 * ti^ 9 - 51061.91* 8 * ti^ 7 + 9531.556* 6 * ti ^ 5 - 888.0331* 4 * ti ^ 3 +31.71547* 2 * ti 
  if (np.order>=19)
    L[19] <-  67415.74 * 19 * ti ^ 18 -311570 * 17 * ti ^ 16 + 605336.1 * 15 * ti ^ 14  - 642023.1 * 13 * ti ^ 12 + 403853.3 * 11 * ti^ 10 - 153185.7* 9 * ti^ 8 + 34041.27* 7 * ti ^ 6 - 4084.952* 5 * ti ^ 4 +222.0083* 3 * ti ^ 2 -3.523941
  if (np.order>=20)
    L[20] <-  131460.7 * 20 * ti ^ 19 -640449.5 * 18 * ti ^ 17 + 1324173 * 16 * ti ^ 15  - 1513340 * 14 * ti ^ 13 + 1043288 * 12 * ti^ 11 - 444238.6* 10 * ti^ 9 + 114889.3* 8 * ti ^ 7 - 17020.64* 6 * ti ^ 5 +1276.548* 4 * ti ^ 3 -37.00138* 2 * ti 
  if (np.order>=21)
    L[21] <-  256661.4 * 21 * ti ^ 20 -1314607 * 19 * ti ^ 18 + 2882023 * 17 * ti ^ 16  - 3531127 * 15 * ti ^ 14 + 2648345 * 13 * ti^ 12 - 1251945* 11 * ti^ 10 + 370198.8* 9 * ti ^ 8 - 65651.02* 7 * ti ^ 6 +6382.738* 5 * ti ^ 4 -283.6773* 3 * ti^ 2 + 3.700138 
  if (np.order>=22)
    L[22] <-  501656.3 * 22 * ti ^ 21 -2694944 * 20 * ti ^ 19 + 6244383 * 18 * ti ^ 17  - 8165732 * 16 * ti ^ 15 + 6620863 * 14 * ti^ 13 - 3442849* 12 * ti^ 11 + 1147616* 10 * ti ^ 9 - 237985* 8 * ti ^ 7 +28722.32* 6 * ti ^ 5 -1772.983* 4 * ti^ 3 + 42.55159* 2 * ti
  if (np.order>=23)
    L[23] <-  981501.4 * 23 * ti ^ 22 -5518219 * 21 * ti ^ 20 + 13474721 * 19 * ti ^ 18  - 18733149 * 17 * ti ^ 16 + 16331463 * 15 * ti^ 14 - 9269209* 13 * ti^ 12 + 3442849* 11 * ti ^ 10 - 819725.9* 9 * ti ^ 8 +118992.5* 7 * ti ^ 6 -9574.107* 5 * ti^ 4 + 354.5966* 3 * ti^ 2 -3.868326
  if (np.order>=24)
    L[24] <-  1922107 * 24 * ti ^ 23 -11287266 * 22 * ti ^ 21 + 28970650 * 20 * ti ^ 19  - 42669950 * 18 * ti ^ 17 + 39807941 * 16 * ti^ 15 - 24497195* 14 * ti^ 13 + 10041643* 12 * ti ^ 11 - 2705096* 10 * ti ^ 9 +461095.8* 8 * ti ^ 7 -46274.85* 6 * ti^ 5 + 2393.527* 4 * ti^ 3 -48.35408* 2 * ti
  if (np.order>=25)
    L[25] <-  3767330 * 25 * ti ^ 24 -23065284 * 23 * ti ^ 22 + 62079965 * 21 * ti ^ 20  - 96568835 * 19 * ti ^ 18 + 96007388 * 17 * ti^ 16 - 63692706* 15 * ti^ 14 + 28580061* 13 * ti ^ 12 - 8607122* 11 * ti ^ 10 +1690685* 9 * ti ^ 8 -204931.5* 7 * ti^ 6 + 13882.46* 5 * ti^ 4 -435.1867* 3 * ti ^ 2 + 4.029506
  if (np.order>=26)
    L[26] <-  7389762 * 26 * ti ^ 25 -47091621 * 24 * ti ^ 23 + 132625380 * 22 * ti ^ 21  - 217279879 * 20 * ti ^ 19 + 229350983 * 18 * ti^ 17 - 163212560* 16 * ti^ 15 + 79615883* 14 * ti ^ 13 - 26538628* 12 * ti ^ 11 +5917397* 10 * ti ^ 9 -845342.4* 8 * ti^ 7 + 71726.02* 6 * ti^ 5 -3155.104* 4 * ti ^ 3 + 54.39834* 2 * ti
  
  return(L);
}

fitPKM <- function(para,NG,self,nconnect,nt,order,nstep,LL){
  
  odes <- ode.sovle.ind(NG,para,nconnect,nt,order,nstep,LL,self=self)
  sum((NG[,self]-(rowSums(odes)))^2
      +1*sum(para^2)
  )
}
ode.sovle.ind <- function(NG,fitpar,nconnect,nt,order,nstep,LL,self){
  
  stp <- (max(nt)-min(nt))/nstep
  index <- which(nconnect==1)
  
  ind.par <- matrix(fitpar[1:(length(index)*(order-1))],ncol=order-1,byrow=T)
  allrep <- matrix(rep(NG[1,which(nconnect==1)[1]]/length(which(nconnect==1)),length(index)),nrow=1)
  #allrep <- matrix(rep(0,length(index)),nrow=1)
  
  #allrep[which(index==self)] <- NG[1,self]
  nn <- 1
  for(j in 1:nstep){
    tg1 <- (rowSums(t(apply(ind.par,1,"*",LL[nn,])))*NG[j,index])
    tg2 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+1,])))*NG[j,index])
    tg3 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+2,])))*NG[j,index])
    tg4 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+3,])))*NG[j,index])
    tmp <- allrep[j,] +stp*(tg1+2*tg2+2*tg3+tg4)/6
    allrep <- rbind(allrep,tmp)
    nn <- nn + 4
  }
  self_name <- colnames(NG)[self]
  no_name <- which(colnames(allrep)==self_name)
  if(length(which(allrep[,no_name]<0))==0){
    return(allrep)
  } else{return(allrep*1)}
}

ode.sovle.ind <- function(NG,fitpar,nconnect,nt,order,nstep,LL,self){
  
  stp <- (max(nt)-min(nt))/nstep
  index <- which(nconnect==1)
  
  ind.par <- matrix(fitpar[1:(length(index)*(order-1))],ncol=order-1,byrow=T)
  allrep <- matrix(rep(NG[1,which(nconnect==1)[1]]/length(which(nconnect==1)),length(index)),nrow=1)
  #allrep <- matrix(rep(0,length(index)),nrow=1)
  
  #allrep[which(index==self)] <- NG[1,self]
  nn <- 1
  for(j in 1:nstep){
    tg1 <- (rowSums(t(apply(ind.par,1,"*",LL[nn,])))*NG[j,index])
    tg2 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+1,])))*NG[j,index])
    tg3 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+2,])))*NG[j,index])
    tg4 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+3,])))*NG[j,index])
    tmp <- allrep[j,] +stp*(tg1+2*tg2+2*tg3+tg4)/6
    allrep <- rbind(allrep,tmp)
    nn <- nn + 4
  }
  self_name <- colnames(NG)[self]
  no_name <- which(colnames(allrep)==self_name)
  if(length(which(allrep[,no_name]<0))==0){
    allrep[,no_name] <-allrep[,no_name]
  } else{
    allrep[,no_name] <-allrep[,no_name]*1}
  if(max(abs(allrep[,-no_name])) < 2){
    allrep[,-no_name] <-allrep[,-no_name]
  } else{
    allrep[2:100,-no_name] <-allrep[2:100,-no_name]*1
  }
  return(allrep)
}

F2015_H.od_all <- list()
for (i in 1:length(data_final)) {
  F2015_H.od <- optim.parallel(connect=lasso,effect=t(data_final[[i]]),
                               n.cores=1,proc=ode.optim,order=5,times=as.numeric(colnames(data_final[[i]])),nstep=99)
  F2015_H.od_all[[i]] <-  F2015_H.od
  
}

