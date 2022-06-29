setwd("/home/lbjiang/workdir/smm/liver-cancer/fig8-8/")

library(pbapply)
library(parallel)
library(ggplot2)



dat <- read.csv(file = "data_cluster.csv", row.names = 1, check.names = F)



df <- log(dat+1)

T1 <- log(colSums(dat[,grep("H",colnames(df))]))
T2 <- log(colSums(dat[,grep("N",colnames(df))]))

TT <- c(T1,T2)

T1_fit <- seq(min(T1),max(T1),length=20)
T2_fit <- seq(min(T2),max(T2),length=20)
TT_fit <- c(T1_fit,T2_fit)


f1 <- function(y){
  # x=c(T1,T2)
  linear_equation <- function(par,x){
    y=par[1]+par[2]*x
    y
  }
  
  
  par_est <- function(par,x,y){
    sum( (y -  linear_equation(par,x))^2 )
  }
  
  
  tmp <- c(0.1,0.1)
  r1 <- optim(tmp,par_est,x=T1,y=as.numeric(y[grep("H",colnames(y))]), method = "BFGS")
  
  result1 <- linear_equation(r1$par,T1_fit)
  tmp <- c(0.1,0.1)
  r2 <- optim(tmp,par_est,x=T2,y=as.numeric(y[grep("N",colnames(y))]), method = "BFGS")
  result2 <- linear_equation(r2$par,T2_fit)
  
  
  result1[which(result1 < 0)] <- 0
  result2[which(result2 < 0)] <- 0
  #r1$par
  
  
  result3 <- c(result1,result2)
  return(result3)
}




df_fitted <-  t(pbsapply(1:nrow(df),function(c)f1(df[c,])))





##############################





get_init_par <- function(data,k){
  
  
  init_cluster <- kmeans(data,centers = k,iter.max = 10000)
  pro <- table(init_cluster$cluster)/nrow(data) #????????????????????????
  
  cuM <- init_cluster$centers
  
  linear_equation <- function(par,x){
    y=par[1]+par[2]*x
    y
  }
  
  loss <- function(par,x,y){
    sum((y-linear_equation(par,x))^2)
  }
  
  
  f21 <- function(y,t){
    
    
    par <- c(0.1,0.1)
    r1 <- optim(par,loss,x=t,y= y, method = "Nelder-Mead",
                control = list(maxit = 200000000))
    return(r1$par)
  }
  
  # y <- cuM[1,1:10]
  #t <- T1
  init_curve_para <- cbind(t(sapply(1:k,function(c)f21(cuM[c,1:20],T1_fit))),
                           t(sapply(1:k,function(c)f21(cuM[c,21:40],T2_fit))))
  
  init_sd_para <- c(0.8,0.9,0.8,0.9)#SAD1
  init_pro <- pro
  
  return_object <- list(init_sd_para,init_curve_para,init_pro)
  names(return_object)<-c("init_sd_par","init_curve","init_pro")
  return(return_object)
}



#input <- get_init_par(df_fitted,20)

#data=df_fitted
#k=20




get_cluster <- function(data,k,EI,input){
  requiredPackages = c("mvtnorm","reshape2","ggplot2")
  for(packages in requiredPackages){
    if(!require(packages,character.only = TRUE)) install.packages(packages)
    require(packages,character.only = TRUE)
  }
  Delta <- 100; iter <- 0; itermax <- 5;
  SAD1_get_matrix <- function(par,data){
    p <-  ncol(data)/2
    v2 <- par[1]
    phi <- par[2]
    tmp <- (1-phi^2)
    sigma <- array(dim=c(p,p))
    for(i in 1:p){
      sigma[i,i:p] <- phi^( c(i:p) - i ) * (1-phi^(2*i ))/tmp
      sigma[i:p,i] <- sigma[i,i:p]}
    sigma <- sigma*abs(v2)
    return(sigma)
  } 
  AR1_get_matrix <- function(par,data){
    n <- ncol(data)
    rho <- par[2]
    exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                      (1:n - 1))
    return(par[1]*rho^exponent)
  }
  
  
  get_SAD <- function(par,data){
    n <- ncol(data)/2
    sig1 <- SAD1_get_matrix(par[1:2],data)
    sig2 <- SAD1_get_matrix(par[3:4],data)
    sig12 <- array(0, dim=c(n,n))
    sigma1 <- cbind(sig1,sig12)
    sigma2 <- cbind(sig12,sig2)
    sigma <- rbind(sigma1,sigma2)
    return(sigma)
  }
  
  linear_equation <- function(par,x){
    y=par[1]+par[2]*x
    y
  }
  
  power_equation2 <- function(par,x){
    a=par[1]; b=par[2]; x <- as.numeric(x)
    return(y = b*log(x)+a)
  }
  mle <- function(par,data,prob,EI){
    #par <- old_par
    #prob <- pro
    # data <- df_fitted
    T1_fit <- EI[1:20]
    T2_fit <- EI[21:40]
    par1 <- par[1:4]
    par2 <- matrix(par[-c(1:4)],nrow = k,ncol = 4)
    temp_S <- sapply(1:k, function(c) dmvnorm(data,
                                              c(linear_equation(par2[c,][1:2],T1_fit),linear_equation(par2[c,][3:4],T2_fit)),
                                              get_SAD(par1,data))*prob[c] )
    LL <- sum(-log(rowSums(temp_S)))
    #LL
    return(LL)
  }
  
  T1_fit <- EI[1:20]
  T2_fit <- EI[21:40]
  
  while ( Delta > 0.5 && iter <= itermax ) {
    # initiation
    if(iter == 0){
      init_sd_para <- input[[1]]
      init_curve_para <- input[[2]] 
      pro <- input[[3]]
    }
    #E step, calculate the posterior probability
    old_par <- c(init_sd_para,init_curve_para)
    LL_mem <- mle(old_par,df_fitted,pro,EI)
    mvn.c <- sapply(1:k, function(c) dmvnorm(df_fitted,
                                             c(linear_equation(init_curve_para[c,][1:2],T1_fit),linear_equation(init_curve_para[c,][3:4],T2_fit)),
                                             get_SAD(init_sd_para[1:4],df_fitted))*pro[c] )
    omega <- mvn.c/rowSums(mvn.c)
    #M step, calculate parameters
    pro <- colSums(omega)/sum(omega)
    new_par <- try(optim(old_par, mle, data=df_fitted, prob=pro,EI=EI, method = "Nelder-Mead",control=list(maxit=100000000)))
    if ('try-error' %in% class(new_par))
      break
    L_Value <- new_par$value
    init_sd_para <- new_par$par[1:4]
    init_curve_para <- matrix(new_par$par[-c(1:4)],nrow = k)
    Delta <- abs(L_Value-LL_mem)
    #if (Delta > 1000000)
    #break
    cat('\n',"iter=",iter,"LL=",L_Value,'\n')
    iter <- iter+1; LL_mem <- L_Value
  } 
  
  BIC <- 2*(L_Value)+log(nrow(data))*length(old_par)
  #plot-----------
  cluster <- apply(omega,1,which.max)
  #clustered_df <- cbind(row.names(data),data,cluster)
  clustered_df <- data.frame(cbind(row.names(df),df_fitted,cluster))
  colnames(clustered_df) <- c("row.names(data)",as.numeric(colSums(df)),"cluster")
  long_df <- melt(clustered_df,id.vars=c("row.names(data)","cluster"))
  long_df[,4] <- as.numeric(long_df[,4])
  long_df[,3] <- as.numeric(as.character(long_df[,3]))
  colnames(long_df) <- c("gene","cluster","time","fpkm")
  p1 <-  ggplot()+geom_line(long_df,mapping=aes(as.numeric(as.character(time)),fpkm,group=gene,
                                                colour= as.character(cluster)))+
    facet_wrap(cluster,scales = "fixed")+ 
    theme(legend.position="none") + xlab("Total_fpkm")+ylab("individual_fpkm")
  
  clustered_df <- cbind(row.names(df),df,cluster)
  colnames(clustered_df) <- c("row.names(data)",as.numeric(colSums(df)),"cluster")
  long_df <- melt(clustered_df,id.vars=c("row.names(data)","cluster"))
  long_df[,4] <- as.numeric(long_df[,4])
  long_df[,3] <- as.numeric(as.character(long_df[,3]))
  colnames(long_df) <- c("gene","cluster","time","fpkm")
  p2 <-  ggplot()+geom_line(long_df,mapping=aes(as.numeric(as.character(time)),fpkm,group=gene,
                                                colour= as.character(cluster)))+
    facet_wrap(cluster,scales = "fixed")+ 
    theme(legend.position="none") + xlab("Total_fpkm")+ylab("individual_fpkm")
  
  
  clustered_df <- clustered_df[,-1]
  return_object <- list(init_sd_para,init_curve_para,pro,LL_mem,BIC,clustered_df,p1,p2)
  names(return_object)<-c("sd_par", "curve_par", "pro", "LL", "BIC", "clustered_data","plot1",'plot2')
  
  return(return_object)
}





get_BIC <- function(rep,k,EI){
  input <- pblapply(1:rep, function(c) get_init_par(df_fitted,k) ) 
  core.number <- detectCores()
  cl <- makeCluster(getOption("cl.cores", core.number))
  clusterExport(cl, c("df","get_init_par","get_cluster","input","k","EI","rep","df_fitted")
                ,envir=environment()
  )
  result <- pblapply(1:rep, function(c) get_cluster(df_fitted,k,EI,input[[c]]),cl=cl)#
  stopCluster(cl)
  
  out_df <- matrix(NA,nrow = 3,ncol = rep)
  out_df[1,] <- k#sapply(1:rep,function(c)length(table(result[[c]]$clustered_data[,(length(df_fitted)+1)])))
  out_df[2,] <- sapply(1:rep,function(c)result[[c]]$BIC)
  out_df[3,] <- sapply(1:rep,function(c)result[[c]]$LL)
  rownames(out_df) <- c("cluster_number","BIC","LL")
  colnames(out_df) <- paste0("rep",1:rep)
  return(list(t(out_df),result))
}




#####################################
get_con_param<-function(parm.id)
{
  for (e in commandArgs())
  {
    ta = strsplit(e,"=", fixed=TRUE);
    if(! is.na( ta[[1]][2]))
    {
      temp = ta[[1]][2];
      if( ta[[1]][1] == parm.id) {
        return (as.character(temp));
      }
    }
  }
  
  return(NA);
}


test1 <- get_con_param("test1")
#test2 <- get_con_param("test2")
test3 <- get_con_param("test3")

test1 <- as.numeric(test1)
#test2 <- as.numeric(test2)
test3 <- as.numeric(test3)


nm <- 36
n1 <- test1
#n2 <- test2
if(n1 >nm)
  n1 <- nm



rep=2
#n1=30
for(k in n1){
  BIC <- get_BIC(rep,k,EI=TT_fit)
  save.image(paste0("rep_",rep,"_k_",k,".RData"))
}






