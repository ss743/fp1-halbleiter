###########################
# Einbinden der Libraries #
###########################
library(Hmisc)
source("gausfit.R")

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
  j=i*2+offset-1
  k=i*2+offset
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
A=c()
sigma=c()

for(i in c(1:11)){
  
  x=m1ch1[,1,i]
  y=m1ch1[,2,i]
  plot(x,y,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="black")#,xlim=c(0,2*10^(-10)))
  grid()
  
  try({
  fit=gausfit(data.frame(x=x,y=y,sy=1),bereich=bereich[i,])
  plotgaus(fit,bereich[i,])
  printfitdata(fit)
  
  mu[i]=fit["mu","Estimate"]
  A[i]=fit["N","Estimate"]
  sigma[i]=fit["sig","Estimate"]
})
  
  #x=m1ch1_unscaled[,1,i]
  #y=m1ch1_unscaled[,2,i]
  #plot(x,y,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="red")#,xlim=c(0,2*10^(-10)))
  #grid()
}

offset=3
xvals=c(9.58,8.9,7.9,6.98,6.17,5.15,4.14,3.21,2.35,1.11,0)+offset

plot(mu,xvals,type="p",pch=4,xlab="t",ylab="x_c(t)",cex=0.6,bty="l",col="black")
grid()
plot(mu,A,type="p",pch=4,xlab="t",ylab="A(t)",cex=0.6,bty="l",col="black")
grid()
plot(mu,sigma,type="p",pch=4,xlab="t",ylab="sigma(t)",cex=0.6,bty="l",col="black")
grid()
