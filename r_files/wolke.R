###########################
# Einbinden der Libraries #
###########################
library(Hmisc)
source("gausfit.R")
source("linearfit.R")
source("expfit.R")
source("sqrtfit.R")

######################
# Einlesen der Daten #
######################
m1ch1 = array(dim=c(2500,2,11))
#m1ch1_unscaled = array(dim=c(2500,2,11))
m1ch2 = array(dim=c(2500,2,11))
offset=0
for(i in c(1:11)) {
  j=i+offset
  if(j<10)
    pre="000"
  else
    pre="00"
  path1=paste("../oszi_data/ALL",pre,j,"/F",pre,j,"CH1.CSV",sep="")
  path2=paste("../oszi_data/ALL",pre,j,"/F",pre,j,"CH2.CSV",sep="")
  a1=read.table(path1,colClasses=c("NULL","NULL","NULL","real","real"),dec=".",sep=",")
  a1_scale=read.table(path1,colClasses=c("NULL","character","NULL","NULL","NULL"),sep=",",dec=".")
  a2=read.table(path2,colClasses=c("NULL","NULL","NULL","real","real"),dec=".",sep=",")
  a2_scale=read.table(path1,colClasses=c("NULL","character","NULL","NULL","NULL"),sep=",",dec=".")
  ch1vscale=as.numeric(a1_scale[9,])
  ch1hscale=as.numeric(a1_scale[12,])
  ch2vscale=as.numeric(a2_scale[9,])
  ch2hscale=as.numeric(a2_scale[12,])
  ch1offset=as.numeric(a1_scale[10,])
  ch2offset=as.numeric(a2_scale[10,])
  m1ch1[,1,i]         =a1[[1]]#*ch1hscale
  #m1ch1_unscaled[,1,i]=a1[[1]]
  m1ch1[,2,i]         =a1[[2]]#+ch2offset#*ch1vscale
  #m1ch1_unscaled[,2,i]=a1[[2]]
  m1ch2[,1,i]=a2[[1]]*ch2hscale
  m1ch2[,2,i]=a2[[2]]*ch2vscale
}
m2ch1 = array(dim=c(2500,2,11))
m2ch2 = array(dim=c(2500,2,11))
offset=12
for(i in c(1:11)) {
  j=i*2+offset
  k=i*2+offset-1
  if(j<10)
    prej="000"
  else
    prej="00"
  if(j<10)
    prek="000"
  else
    prek="00"
  path1=paste("../oszi_data/ALL",prej,j,"/F",prej,j,"CH1.CSV",sep="")
  path2=paste("../oszi_data/ALL",prek,k,"/F",prek,k,"CH2.CSV",sep="")
  a1=read.table(path1,colClasses=c("NULL","NULL","NULL","real","real"),dec=".",sep=",")
  a1_scale=read.table(path1,colClasses=c("NULL","character","NULL","NULL","NULL"),sep=",",dec=".")
  a2=read.table(path2,colClasses=c("NULL","NULL","NULL","real","real"),dec=".",sep=",")
  a2_scale=read.table(path1,colClasses=c("NULL","character","NULL","NULL","NULL"),sep=",",dec=".")
  ch1vscale=as.numeric(a1_scale[9,])
  ch1hscale=as.numeric(a1_scale[12,])
  ch2vscale=as.numeric(a2_scale[9,])
  ch2hscale=as.numeric(a2_scale[12,])
  ch1offset=as.numeric(a1_scale[10,])
  ch2offset=as.numeric(a2_scale[10,])
  m2ch1[,1,i]=a1[[1]]#*ch1hscale
  m2ch1[,2,i]=a1[[2]]#*ch1vscale
  m2ch2[,1,i]=a2[[1]]#*ch2hscale
  m2ch2[,2,i]=a2[[2]]#*ch2vscale
}

bereich=array(dim=c(11,2))
bereich[1,] =c(2,3)*10^(-5)
bereich[2,] =c(1.9,2.8)*10^(-5)
bereich[3,] =c(1.7,2.6)*10^(-5)
bereich[4,] =c(1.7,2.3)*10^(-5)
bereich[5,] =c(1.4,2.2)*10^(-5)
bereich[6,] =c(1.1,2.1)*10^(-5)
bereich[7,] =c(1,2)*10^(-5)
bereich[8,] =c(0.7,1.6)*10^(-5)
bereich[9,] =c(0.5,1.4)*10^(-5)
bereich[10,]=c(0.3,1.1)*10^(-5)
bereich[11,]=c(0.1,0.7)*10^(-5)

mu=c()
smu=c()
A=c()
sA=c()
sigma=c()
ssigma=c()
C=c()

colors=c("blue","cadetblue1","chartreuse","green","darkgreen","darkmagenta","deeppink","red","darkorange","yellow","gray31")

plot(c(),c(),type="n",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",ylim=c(-0.01,0.18),xlim=c(0,3*10^(-5)))


for(i in c(1:11)){
  
  x=m1ch1[,1,i]
  y=m1ch1[,2,i]

  try({
    fit=gausfit(data.frame(x=x,y=y,sy=1),bereich=bereich[i,])
    printfitdata(fit)
    
    mu[i]=fit["mu","Estimate"]
    smu[i]=fit["mu","Std. Error"]
    A[i]=fit["N","Estimate"]
    sA[i]=fit["N","Std. Error"]
    sigma[i]=fit["sig","Estimate"]
    ssigma[i]=fit["sig","Std. Error"]
    C[i]=fit["C","Estimate"]
  })
  
  y=y-C[i]
  fit["C","Estimate"]=0;
  

  points(x,y,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col=colors[i],ylim=c(-0.01,0.18),xlim=c(0,3*10^(-5)))
  plotgaus(fit,bereich[i,])
  grid()
  
  
}

offset=0#3
xvals=(c(9.58,8.9,7.9,6.98,6.17,5.15,4.14,3.21,2.35,1.11,0)+offset)*0.001
sxvals=0.02*0.001*xvals/xvals

plot(mu,xvals,type="p",pch=4,xlab="t",ylab="x_c(t)",cex=0.6,bty="l",col="black")
#with(data=data.frame(x=mu,y=xvals,sy=sxvals),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
plotCI(mu,xvals,uiw=sxvals,err="y",pch=4,cex=0.6,add=TRUE)
plotCI(mu,xvals,uiw=smu,err="x",pch=4,cex=0.6,add=TRUE)
grenzen=c(min(mu),max(mu))
grid()
xct=linearfit(data.frame(x=mu,y=xvals,sy=sxvals),grenzen,TRUE)
plotlinear(xct,grenzen)
plotlindata(xct,"\nmu_n*E (Slope)")
plot(mu,A,type="p",pch=4,xlab="t",ylab="A(t)",cex=0.6,bty="l",col="black")
plotCI(mu,A,uiw=sA,err="y",pch=4,cex=0.6,add=TRUE)
plotCI(mu,A,uiw=smu,err="x",pch=4,cex=0.6,add=TRUE)
grid()
At=expfit(data.frame(x=mu,y=A,sy=sA),grenzen,TRUE)
plotexp(At,grenzen)
printexpdata(At,"\nLebensdauer")
plot(mu,sigma,type="p",pch=4,xlab="t",ylab="sigma(t)",cex=0.6,bty="l",col="black")
plotCI(mu,sigma,uiw=ssigma,err="y",pch=4,cex=0.6,add=TRUE)
plotCI(mu,sigma,uiw=smu,err="x",pch=4,cex=0.6,add=TRUE)
grid()
sigmat=sqrtfit(data.frame(x=mu,y=sigma,sy=ssigma),grenzen,TRUE)
plotsqrt(sigmat,grenzen)
printsqrtdata(sigmat,"\n2D_n (B)")






###############
# Messreihe 2 #
###############

bereich=array(dim=c(11,2))
bereich[1,] =c(0.5,2)*10^(-5)
bereich[2,] =c(0.5,1.8)*10^(-5)
bereich[3,] =c(0.5,1.6)*10^(-5)
bereich[4,] =c(0.5,1.5)*10^(-5)
bereich[5,] =c(0.3,1.4)*10^(-5)
bereich[6,] =c(0.3,1.2)*10^(-5)
bereich[7,] =c(0.3,1.2)*10^(-5)
bereich[8,] =c(0.3,1.1)*10^(-5)
bereich[9,] =c(0.3,1.1)*10^(-5)
bereich[10,]=c(0.3,1.0)*10^(-5)
bereich[11,]=c(0.3,1.0)*10^(-5)


mu=c()
smu=c()
A=c()
sA=c()
sigma=c()
ssigma=c()
U=c(20,23.2,26,29.4,32.4,35.4,38.6,41.6,44.4,47.6,50)
sU=0.1*U/U

plot(c(),c(),type="n",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",ylim=c(-0.01,0.12),xlim=c(0,2*10^(-5)))

for(i in c(1:11)){
  
  x=m2ch1[,1,i]
  y=m2ch1[,2,i]
  
  try({
    fit=gausfit(data.frame(x=x,y=y,sy=1),bereich=bereich[i,])
    printfitdata(fit)
    
    mu[i]=fit["mu","Estimate"]
    smu[i]=fit["mu","Std. Error"]
    A[i]=fit["N","Estimate"]
    sA[i]=fit["N","Std. Error"]
    sigma[i]=fit["sig","Estimate"]
    ssigma[i]=fit["sig","Std. Error"]
    C[i]=fit["C","Estimate"]
  })
  y=y-C[i]
  fit["C","Estimate"]=0;

  points(x,y,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col=colors[i],ylim=c(-0.01,0.12),xlim=c(0,2*10^(-5)))
  plotgaus(fit,bereich[i,])
  grid()
  
}

plot(mu,1/U,type="p",pch=4,xlab="t",ylab="1/U",cex=0.6,bty="l",col="black")
plotCI(mu,1/U,uiw=sU/U^2,err="y",pch=4,cex=0.6,add=TRUE)
plotCI(mu,1/U,uiw=smu,err="x",pch=4,cex=0.6,add=TRUE)
grenzen=c(min(mu),max(mu))
grid()
Ut=linearfit(data.frame(x=mu,y=1/U,sy=sU/U^2),grenzen,TRUE)
plotlinear(Ut,grenzen)
plotlindata(Ut,"\nmu_n/(x*l) (Slope)")
