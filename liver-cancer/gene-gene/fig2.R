

N_22

i <- 10
res1[[i]][[1]]
res1[[i]][[2]]




N_21
52    66    86   100   110   170   214   237   251   255   297   301   324   374
[15]   377   412   435   440   464   504   537   597   599   629   659   662   674   683
i <- 537
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]



N_20

1    2    7   22   23   33   34   35   39   40   42   45   57   62   70   78   91
[18]   92   94   95   97  100  101  104  105  109  110  115  116  119  120  140  142  146

147  148  156  161  165  168  174  176  192  194  201  212  217  218  220  221
[45]  222  223  233  234  235  238  240  243  245  248  253  261  262  265  267  268  270  272  274  280  285  288
[67]  293  296  300  305  309  311  322  328  332  345  354  357  358  361  367  373  375  385  397  407  413  414


417  424  427  428  437  438  442  445  446  451  455  458  460  468  470  481  494  501  509  512  513  517
[111]  523  528  529  533  547  549  554  559  572  575  580  603  605  612  632  661  663  664  667  668  669  671
[133]  676  677  681  685  706  710  722  724  729  738  739  746  756  757  769  776  780  786  797  806  814  815



i <- 661
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]



N_12
21    29    63    82   199   227   264   279   315   404   557   582   610   636
[15]   673   691   702   732   800   820   971  1043  1183  1247  1442  1453  1463  1494

1575  1679  1848  1914  1942  2002  2033  2115
[37]  2167  2233  2243  2280  2375  2410  2429  2453  2457  2634  2754  2777  2841  2891  2908  2917  2981  2985
[55]  3020  3064  3085  3133  3174  3308  3373  3388  3444  3460  3472  3585  3639  3640  3741  3761  3796  3813

i <-2841
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]




N_11
24    25    65    70    79   123   124   127   131   133   157   160   171   177
[15]   179   181   182   202   205   206   254   289   327   349   363   364   366   369
i <- 20094
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]





N_10
22    30    57    72   104   164   256   368   370   499   620   621   630   764
[15]   790   809   811   818   822   823   858   900   940  1041  1057  1066  1128  1166

1182  1264  1348  1390  1392  1570  1672  1684  1695  1785  1789  1944  1951  2086
[43]  2183  2208  2222  2228  2229  2262  2319  2384  2436  2463  2502  2542  2544  2635
[57]  2715  2811  2832  2934  2955  2977  2986  2996  3029  3058  3160  3264  3276  3319


3381  3383
[73]  3393  3438  3448  3539  3575  3586  3641  3655  3658  3808  3835  3858  3880  3883  3923  4025  4124  4175
[91]  4301  4347  4461  4514  4598  4617  4630  4670  4684  4731  4796  4845  4952  5040  5107  5109  5112  5283
[109]  5443  5446  5455  5529  5553  5578  5599  5614  5648  5739  5957  5980  6146  6211  6216  6295  6323  6347

#2502
#2934
i <-3641
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]


N_02


826   843   867   880   899   902   915   960   989   993  1002  1027  1042  1053  1056  1063  1091  1100
[73]  1109  1111  1122  1152  1207  1211  1229  1251  1268  1282  1290  1292  1297  1309  1336  1353  1371  1372
[91]  1374  1381  1414  1416  1422  1423  1464  1475  1485  1500  1509  1511  1514  1525  1541  1551  1552  1555
[109]  1580  1631  1643  1648  1696  1711  1713  1716  1723  1755  1768  1771  1775  1782  1786  1794  1840  1859


i <- 1251
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]




N_01

28   114   162   191   196   209   516   532   548   577   606   631   680   682
[15]   697   873  1006  1108  1121  1244  1284  1322  1395  1397  1468  1636  1884  2270
[29]  2343  2366  2367  2454  2497  2604  2623  2819  2833  2896  2992  3009  3042  3074
516

3126  3129  3141  3144  3153  3158  3159  3221  3225  3257  3278  3360
[55]  3371  3390  3471  3522  3571  3628  3716  3762  3811  3979  4099  4205  4206  4257  4364  4377  4458  4521
[73]  4539  4588  4676  4685  4917  4932  4962  5007  5019  5091  5116  5132  5237  5321  5380  5422  5466  5489


i <-5489
res1[[i]][[1]][[6]]
res1[[i]][[2]][[6]]

N_00
i <- 17
res1[[i]][[1]]
res1[[i]][[2]]



d_fit_22 <- dat_fit_fun(10)
d_fit_21 <- dat_fit_fun(537)#255
d_fit_20 <- dat_fit_fun(40)#40

d_fit_12 <- dat_fit_fun(2841)#1043
d_fit_11 <- dat_fit_fun(20094)#70
d_fit_10 <- dat_fit_fun(3641)#2934


d_fit_02 <- dat_fit_fun(1251)#825
d_fit_01 <- dat_fit_fun(1395)
d_fit_00 <- dat_fit_fun(17)

i=10
rownames(dat)[10] #NFYA
t_22 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_22 <- res1[[i]]
g_22 <- "NFYA"


i=537#255  
rownames(dat)[537] #FLT4
t_21 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_21 <- res1[[i]]
g_21 <- "FLT4"

i=40  #40
rownames(dat)[40] #CASP10
t_20 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_20 <- res1[[i]]
g_20 <- "CASP10"

i=2841#1043  
rownames(dat)[2841] #TRIP4
t_12 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_12 <- res1[[i]]
g_12 <- "TRIP4"

i=20094#70
rownames(dat)[20094] #IGHJ2
t_11 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_11 <- res1[[i]]
g_11 <- "IGHJ2"

i=3641#2934
rownames(dat)[3641] #LAMTOR3
t_10 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_10 <- res1[[i]]
g_10 <- "LAMTOR3"

i=1251#825
rownames(dat)[1251] #NOTCH3
t_02 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_02 <- res1[[i]]
g_02 <- "NOTCH3"

i=1395
rownames(dat)[1395] #UBE2A
t_01 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_01 <- res1[[i]]
g_01 <- "UBE2A"

i=17
rownames(dat)[17] #ANKIB1
t_00 <- as.numeric(rownames(res1[[i]][[1]][[6]]))
d_00 <- res1[[i]]
g_00 <- "ANKIB1"



library(showtext)
showtext_auto(enable=TRUE)
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")
font_add("Times New Roman1",regular = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf")

pdf("fig8-31-1.pdf",height = 12,width =18)

#dd <- 2
kk <- 0.5
#ra <- 0.35

rax <- 1
len <- 16.8

ra <- 0.55
dd <- 17

par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(c(0,0), c(0,0), type="n",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",xlim=c(-10,130),ylim=c(10, 190))

ymax <- 198-10
segments(10,ymax,130,ymax ,lwd=3)

text(70,ymax+5,"Tumor",cex=2.4,font=1,family="Times New Roman")

text(10+20,ymax-4,"Cooperation",cex=2.4,font=1,family="Times New Roman")

text(10+20+40,ymax-4,"Neutrality",cex=2.4,font=1,family="Times New Roman")
text(10+20+40+40,ymax-4,"Competition",cex=2.4,font=1,family="Times New Roman")


text(-2,180-54/2,"Cooperation",cex=2.4,srt=90,font=1,family="Times New Roman")
text(-2,180-54-54/2,"Neutrality",cex=2.4,srt=90,font=1,family="Times New Roman")
text(-2,180-54-54-54/2,"Competition",cex=2.4,srt=90,font=1,family="Times New Roman")

segments(-6,180,-6,18 ,lwd=3)

text(-10,180-54-54/2,"TME",cex=2.4,srt=90,font=1,family="Times New Roman")

##########################################################################
##########################################################################
sub_rc <-c(10,126,50, 180)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Synergism (NFYA)",cex=1.8,font=1,family="Times New Roman")


text(sub_rc[1]-12,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],"Cooperation",cex=2,font=1,adj=90,family="Times New Roman")


####################

sub_rc <-c(10,126,30, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd+4/ra*i,4*i,cex=2,font=1,family="Times New Roman")
}

text(sub_rc[1]-6,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],expression("Individual Expression"),srt=90,cex=2,family="Times New Roman")


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd-4/ra*i,-4*i,cex=2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}


times <- t_22/rax
l1 <- d_22[[1]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_22[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_22[[1]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(10+20,126,30+20, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_22/rax
l1 <- d_22[[2]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_22[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_22[[2]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")

text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_22,cex=2,font=1,family="Times New Roman1")

##########################################################################
#########################################################################
sub_rc <-c(10+40,126,50+40, 180)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Commensalism to Tumor (TRIP4)",cex=1.8,font=1,family="Times New Roman")


##EFEDF5

####################

sub_rc <-c(10+40,126,50+20, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}


times <- t_12/rax
#l1 <- d_12[[1]][[6]][,1]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=2)

l1 <- d_fit_12[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_12[[1]][[6]][,2]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20,126,50+40, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_12/rax
l1 <- d_12[[2]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_12[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_12[[2]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")


text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_12,cex=2,font=1,family="Times New Roman1")




##########################################################################
#########################################################################
sub_rc <-c(10+40+40,126,50+40+40, 180)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Altruism for Tumor (NOTCH3)",cex=1.8,font=1,family="Times New Roman")




####################

sub_rc <-c(10+40+40,126,50+20+40, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}


times <- t_02/rax
l1 <- d_02[[1]][[6]][,1]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_02[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_02[[1]][[6]][,2]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20+40,126,50+40+40, 180-8)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_02/rax
l1 <- d_02[[2]][[6]][,1]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_02[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_02[[2]][[6]][,2]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")

text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_02,cex=2,font=1,family="Times New Roman1")

##########################################################################
#########################################################################

##########################################################################
#########################################################################



##########################################################################
##########################################################################
sub_rc <-c(10,126-54,50, 180-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Commensalism from Tumor (FLT4)",cex=1.8,font=1,family="Times New Roman")


####################

sub_rc <-c(10,126-54,30, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd+4/ra*i,4*i,cex=2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd-4/ra*i,-4*i,cex=2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}
text(sub_rc[1]-6,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],expression("Individual Expression"),srt=90,cex=2,family="Times New Roman")


times <- t_21/rax
l1 <- d_21[[1]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_21[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_21[[1]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(10+20,126-54,30+20, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_21/rax
#l1 <- d_21[[2]][[6]][,1]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=2)

l1 <- d_fit_21[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_21[[2]][[6]][,2]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")

text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_21,cex=2,font=1,family="Times New Roman1")

##########################################################################
#########################################################################
sub_rc <-c(10+40,126-54,50+40, 180-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Co-existence (IGHJ2)",cex=1.8,font=1,family="Times New Roman")




####################

sub_rc <-c(10+40,126-54,50+20, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}


times <- t_11/rax
#l1 <- d_21[[1]][[6]][,1]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=2)

l1 <- d_fit_11[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_21[[1]][[6]][,2]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20,126-54,50+40, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_11/rax
#l1 <- d_22[[2]][[6]][,1]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=2)

l1 <- d_fit_11[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_21[[2]][[6]][,2]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_11,cex=2,font=1,family="Times New Roman1")



##########################################################################
#########################################################################
sub_rc <-c(10+40+40,126-54,50+40+40, 180-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Amensalism from Tumor (UBE2A)",cex=1.8,font=1,family="Times New Roman")




####################

sub_rc <-c(10+40+40,126-54,50+20+40, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}


times <- t_01/rax
l1 <- d_01[[1]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_01[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_01[[1]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20+40,126-54,50+40+40, 180-8-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  #text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-2,4+4*i,cex=1.2,font=1,family="Times New Roman")
}

times <- t_01/rax
#l1 <- d_01[[2]][[6]][,1]*10/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=2)

l1 <- d_fit_01[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_01[[2]][[6]][,2]*10/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_01,cex=2,font=1,family="Times New Roman1")



##########################################################################
#########################################################################

##########################################################################
#########################################################################







##########################################################################
##########################################################################
sub_rc <-c(10,126-54-54,50, 180-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Predation toward Tumor (CASP10)",cex=1.8,font=1,family="Times New Roman")


####################

sub_rc <-c(10,126-54-54,30, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd+4/ra*i,4*i,cex=2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  text(sub_rc[1]-2,sub_rc[2]+dd-4/ra*i,-4*i,cex=2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}
text(sub_rc[1]-6,(sub_rc[4]-sub_rc[2])/2+sub_rc[2],expression("Individual Expression"),srt=90,cex=2,family="Times New Roman")
text(sub_rc[3],sub_rc[2]-9,expression("Expression Index"),cex=2,family="Times New Roman")



times <- t_20/rax
l1 <- d_20[[1]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_20[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_20[[1]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(10+20,126-54-54,30+20, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#98FB9833")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}

times <- t_20/rax
l1 <- d_20[[2]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_20[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_20[[2]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")

text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_20,cex=2,font=1,family="Times New Roman1")

##########################################################################
#########################################################################
sub_rc <-c(10+40,126-54-54,50+40, 180-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Amensalism to Tumor (LAMTOR3)",cex=1.8,font=1,family="Times New Roman")




####################

sub_rc <-c(10+40,126-54-54,50+20, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}
text(sub_rc[3],sub_rc[2]-9,expression("Expression Index"),cex=2,family="Times New Roman")


times <- t_10/rax
#l1 <- d_10[[1]][[6]][,1]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=2)

l1 <- d_fit_10[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

#l1 <- d_10[[1]][[6]][,2]*100/ra
#lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=3)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20,126-54-54,50+40, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#FF666633")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}

times <- t_10/rax
l1 <- d_10[[2]][[6]][,1]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_10[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_10[[2]][[6]][,2]*100/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_10,cex=2,font=1,family="Times New Roman1")



##########################################################################
#########################################################################
sub_rc <-c(10+40+40,126-54-54,50+40+40, 180-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1)

text(sub_rc[1]+20,sub_rc[4]-4,"Antagonism (ANKIB1)",cex=1.8,font=1,family="Times New Roman")




####################

sub_rc <-c(10+40+40,126-54-54,50+20+40, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")


for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}




for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.5,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}

text(sub_rc[3],sub_rc[2]-9,expression("Expression Index"),cex=2,family="Times New Roman")

times <- t_00/rax
l1 <- d_00[[1]][[6]][,1]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_00[1,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_00[[1]][[6]][,2]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")



sub_rc <-c(50+20+40,126-54-54,50+40+40, 180-8-54-54)
rect(sub_rc[1],sub_rc[2],sub_rc[3],sub_rc[4],border="black",lwd=1,col="#AFEEEE4C")



for(i in 0:3){
  segments(sub_rc[1],sub_rc[2]+dd+4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd+4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd+4/ra*i,4*i,cex=1.2,font=1,family="Times New Roman")
}


for(i in 0:2){
  segments(sub_rc[1],sub_rc[2]+dd-4/ra*i,sub_rc[1]+0.4,sub_rc[2]+dd-4/ra*i,font=2)
  #text(sub_rc[1]-1.2,sub_rc[2]+dd-4/ra*i,-4*i,cex=1.2,font=1,family="Times New Roman")
}
for(i in 0:4){
  segments(sub_rc[1]+kk+4/rax*i,sub_rc[2],sub_rc[1]+kk+4/rax*i,sub_rc[2]+0.8,font=2)
  text(sub_rc[1]+kk+4/rax*i,sub_rc[2]-3,2+4*i,cex=2,font=1,family="Times New Roman")
}

times <- t_00/rax
l1 <- d_00[[2]][[6]][,1]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="red",lwd=3,lty=1)

l1 <- d_fit_00[2,]/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="blue",lwd=3,lty=1)

l1 <- d_00[[2]][[6]][,2]*10/ra
lines(times+sub_rc[1]+kk,l1+sub_rc[2]+dd,col="green3",lwd=3,lty=1)

segments(sub_rc[1],sub_rc[2]+dd,sub_rc[3],sub_rc[2]+dd,lwd=1,lty=2,col="gray")

text((sub_rc[3]-sub_rc[1])/2+sub_rc[1],sub_rc[4]-3,g_00,cex=2,font=1,family="Times New Roman1")

dev.off()


