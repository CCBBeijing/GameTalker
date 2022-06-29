get_interaction <- function(data, col, reduction = FALSE ){
  data <- t(data)
  n <- nrow(data)
  clean_data <- data
  gene_list <- list()
  m <- clean_data[,col]
  M <- clean_data[,-col]
  
  
  name <- colnames(clean_data)
  
  cor1 <- cor(M,m)
  
  
  return_obj = list(ind.name = name[col],
                    dep.name =  name[-col],
                    coefficent = cor1)
  return(return_obj)
}



LOP_rk4 <- function(x0, y0, h, d_LOP_fit){
  f <- function(x,y){dy=do.call(sum,polynomial.values(polynomials=d_LOP_fit,x=x));dy}
  k1 <- f(x0,y0)
  k2 <- f(x0+h/2,y0+h/2*k1)
  k3 <- f(x0+h/2,y0+h/2*k2)
  k4 <- f(x0+h,y0+h*k3)
  y <- y0+h/6*(k1+2*(1-1/sqrt(2))*k2+2*(1+1/sqrt(2))*k3+k4)
  return(y)
}

get_effect <- function(pars,effect,times,order,y0){
  #Legendre polynomials
  LOP <-  legendre.polynomials(order, normalized=F)
  #derivatives of LOP
  d_LOP_fit <-  sapply(1:length(pars),function(c)
    pars[c]*polynomial.derivatives(LOP)[[c+1]])
  h <- scaleX(times,u=-1,v=1)[2]-scaleX(times,u=-1,v=1)[1] #per step h
  #dy_LOP, the increasment of each step
  dy <- sapply(1:length(times),function(c)LOP_rk4(scaleX(times,u=-1,v=1)[c], y0, h, d_LOP_fit))
  #dy_LOP*y= main or sub effect
  dy_fit <- effect*c(0,dy[1:(length(times)-1)])
  return(cumsum(dy_fit))
}

ode_optimize <- function(pars,ind,dep,times,data,order,effect){
  ind_pars <- matrix(pars,ncol=order)[1,]
  dep_pars <- matrix(pars,ncol=order)[-1,]
  inital_value <- data[,ind][1]
  ind_effect <- get_effect(ind_pars,data[,ind],times,order,inital_value)+inital_value
  if ( is.null(nrow(dep_pars)) ) {
    dep_effect <- get_effect(dep_pars,data[,dep],times,order,0)
    y <- ind_effect+dep_effect
  }else{
    dep_effect <- sapply(1:length(dep), function(c)
      get_effect(dep_pars[c,],data[,dep[c]],times,order,0))
    y <- ind_effect+rowSums(dep_effect)
  }
  #ssr <- sum((data[,ind]-y)^2)
  ssr <- sum((data[,ind]-y)^2)

  #add penalty
  alpha=1e-7
  ridge <- sum((data[,ind]-y)^2+alpha*(sum(ind_pars^2)+sum(dep_pars^2)))
  return(ridge)
}

get_value <- function(effect,relationship,times,order){
  #input
  ind <- relationship[[1]]
  dep <- relationship[[2]]
  ind_no <- as.numeric(which(colnames(effect)==ind))
  dep_no <- as.numeric(sapply(1:length(dep), function(c) which(colnames(effect)==dep[c])))
  init_pars <- rep(0.001,(length(ind_no)+length(dep_no))*order)
  result <- optim(init_pars,ode_optimize,ind=ind_no,dep=dep_no,
                  times=times,data=effect,order=order,
                  method = "BFGS",
                  #upper = 1,
                  #lower = -1,
                  control=list(maxit=1e7,trace=T))
  par_after <- matrix(result$par,length(ind)+length(dep),order)
  return(par_after)
}

get_ode_par <- function(data, times, order, reduction, parallel = TRUE) {
  cat('Start variable selection',sep="\n")
  relationship <- pblapply(1:nrow(data),function(c)
    get_interaction(data, c, reduction = reduction))
  cat('Finish variable selection',sep="\n")
  
  cat('Start ODE solving',sep="\n")
  if (parallel == TRUE) {
    core.number <- detectCores()
    cl <- makeCluster(getOption("cl.cores", core.number))
    clusterEvalQ(cl, {require(orthopolynom)})
    clusterExport(cl, c("data","times","order","reduction","relationship",
                        "get_value","ode_optimize","get_effect","LOP_rk4"),envir=environment())
    lop_par <- pblapply(1:nrow(data),function(c)
      get_value(t(data),relationship[[c]],times,order),cl=cl)
    stopCluster(cl)
  }
  else{
    lop_par <- pblapply(1:nrow(data),function(c)
      get_value(t(data),relationship[[c]],times,order))
  }
  cat('Finish ODE solving',sep="\n")
  return_obj <- list(lop_par = lop_par,
                     relationship = relationship,
                     dataset = data,
                     times = times,
                     order = order)
  return(return_obj)
}

get_ode_output <- function(relationship, par, effect, times, order){
  effect <- t(effect)
  output <- list()
  output[[1]] <- relationship[[1]]
  output[[2]] <- relationship[[2]]
  output[[3]] <- par[1,]
  output[[4]] <- par[2:nrow(par),]
  ind_no <- as.numeric(which(colnames(effect)==output[[1]]))
  dep_no <- as.numeric(sapply(1:length(output[[2]]),
                              function(c) which(colnames(effect)==output[[2]][c])))
  inital_value <- effect[,ind_no][1]
  ind_effect <- get_effect(as.numeric(output[[3]]),effect[,ind_no],times,order,inital_value)+inital_value
  if (length(dep_no)==1) {
    dep_effect <- get_effect(as.numeric(output[[4]]),effect[,dep_no],times,order,0)
  }else{
    dep_effect <- sapply(1:length(dep_no), function(c)
      get_effect(as.numeric(output[[4]][c,]),effect[,dep_no[c]],times,order,0))
    colnames(dep_effect) <- dep_no
  }
  all_effect <- cbind(ind_effect,dep_effect)
  effect_mean <- apply(all_effect,2,mean)
  output[[5]] <- effect_mean
  output[[6]] <- all_effect
  return(output)
}

get_ode_output <- function(relationship, par, effect, times, order){
  effect <- t(effect)
  output <- list()
  output[[1]] <- relationship[[1]]
  output[[2]] <- relationship[[2]]
  output[[3]] <- par[1,]
  output[[4]] <- par[2:nrow(par),]
  ind_no <- as.numeric(which(colnames(effect)==output[[1]]))
  dep_no <- as.numeric(sapply(1:length(output[[2]]),
                              function(c) which(colnames(effect)==output[[2]][c])))
  inital_value <- effect[,ind_no][1]
  ind_effect <- get_effect(as.numeric(output[[3]]),effect[,ind_no],times,order,inital_value)+inital_value
  if (length(dep_no)==1) {
    dep_effect <- get_effect(as.numeric(output[[4]]),effect[,dep_no],times,order,0)
  }else{
    dep_effect <- sapply(1:length(dep_no), function(c)
      get_effect(as.numeric(output[[4]][c,]),effect[,dep_no[c]],times,order,0))
    colnames(dep_effect) <- dep_no
  }
  all_effect <- cbind(ind_effect,dep_effect)
  effect_mean <- apply(all_effect,2,mean)
  output[[5]] <- effect_mean
  output[[6]] <- all_effect
  return(output)
}

get_ode_output <- function(relationship, par, effect, times, order){
  effect <- t(effect)
  output <- list()
  output[[1]] <- relationship[[1]]
  output[[2]] <- relationship[[2]]
  output[[3]] <- par[1,]
  output[[4]] <- par[2:nrow(par),]
  ind_no <- as.numeric(which(colnames(effect)==output[[1]]))
  dep_no <- as.numeric(sapply(1:length(output[[2]]),
                              function(c) which(colnames(effect)==output[[2]][c])))
  inital_value <- effect[,ind_no][1]
  ind_effect <- get_effect(as.numeric(output[[3]]),effect[,ind_no],times,order,inital_value)+inital_value
  if (length(dep_no)==1) {
    dep_effect <- get_effect(as.numeric(output[[4]]),effect[,dep_no],times,order,0)
  }else{
    dep_effect <- sapply(1:length(dep_no), function(c)
      get_effect(as.numeric(output[[4]][c,]),effect[,dep_no[c]],times,order,0))
    colnames(dep_effect) <- dep_no
  }
  all_effect <- cbind(ind_effect,dep_effect)
  effect_mean <- apply(all_effect,2,mean)
  output[[5]] <- effect_mean
  output[[6]] <- all_effect
  return(output)
}

get_all_net <- function(input){
  effect <- input$dataset
  n = nrow(effect)
  par = input$lop_par
  order = input$order
  times = input$times
  relationship = input$relationship
  
  net <- lapply(1:n,function(c)
    get_ode_output(relationship = relationship[[c]],
                   par = par[[c]],
                   effect = effect,
                   times = times,
                   order = order))
  return(net)
}

get_curve_data <- function(input1, input2, i){
  times <- input1$times
  cluster_mean <- input1$dataset
  d1 <- data.frame(input2[[i]][[6]],check.names = F)
  colnames(d1)[-1] <- input2[[i]][[2]]
  d1$sum <- rowSums(d1)
  d1$origin <- as.numeric(cluster_mean[i,])
  d1$x <- times
  return(d1)
}

get_decomposition_plot <- function(input1, input2, i){
  col <- c('#ff7171','#0dceda','#9ede73')
  plot_data <- get_curve_data(input1, input2, i)
  ind_name <- input2[[i]][[1]]
  
  d <- plot_data
  p1 <- ggplot(d,aes(x=x))+geom_line(d,mapping=aes(x=x,y=origin),color=col[2],size=1.5)+
    geom_line(d,mapping=aes(x=x,y=ind_effect),color=col[1],size=1.4)
  for (j in 1:(ncol(d)-4)) {
    p1 <- p1+geom_line(aes_string(y=d[,1+j]),color=col[3],size=1.2,alpha=1)+
      annotate('text',x=max(as.numeric(d$x))+0.3,y=d[nrow(d),(j+1)],
               label=colnames(d)[j+1],size=3)
    
  }
  p1 <- p1+ theme(panel.grid = element_blank(),
                  panel.background = element_rect(color = 'black', fill = 'transparent'),
                  legend.title = element_blank()) +
    xlab("Times") + ylab('Effect')+
    theme(plot.margin = unit(c(0,0,0,0),"lines"))+
    geom_hline(yintercept = 0, size = 0.6) +
    annotate('text',x=mean(as.numeric(d$x)),y=round(max(d[,-ncol(d)])),label=ind_name,size=5)
  
  return(p1)
}
