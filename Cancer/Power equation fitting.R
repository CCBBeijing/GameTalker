mix <- function(par,t){
  alpha <- par[1]
  beta <- par[2]
  y <- par[1]+par[2]*t
  return(y)
}



rrloss <- function(yy,par,t){
  sum((yy-mix(par,t))^2)
}

library(ggplot2) 

data_s <- data_all[[6]]
t<- as.numeric(colnames(data_s))
res <- optim(par = c(0.5,1),method="Nelder-Mead",rrloss,t=t,yy=data_s[1,])
tt <- ggplot()
brt1 <- data.frame(x=t,y=as.numeric(data_s[1,])) 
brtt <- data.frame(x=t,y=mix(res$par,t))
tt <-tt+ geom_point(data = brt1,aes(x,y),alpha=1,col="black",size=2)
tt <-tt+ geom_line(data = brtt,aes(x,y),alpha=1,col="red",size=2)
tt <- tt+xlab(NULL)+ylab(NULL)
tt <- tt+ ggtitle(NULL)
tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( color="black", size=70),axis.text.y =element_text( color="black", size=70),
        plot.margin=unit(c(0,0,6,0),"mm")      
  )
#tt <- tt+geom_hline(yintercept = c(0),linetype=3,size=1)
tt <- tt +scale_y_continuous(breaks = NULL,limits = c(0,0.8))
tt <- tt+ scale_x_continuous(limits=c(3,5.5),breaks = seq(3,5.5,by = 0.5),labels = NULL)+theme(axis.text.x = element_text( family = "serif",  size=25),axis.ticks.length.x = unit(-0.4,"cm"))
tt
