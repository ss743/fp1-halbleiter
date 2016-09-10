###########################
# Einbinden der Libraries #
###########################
library(Hmisc)
source("konstfit.R")

######################
# Einlesen der Daten #
######################
germanium = read.table("../data/Bandlücke-Ge.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_untergrund = read.table("../data/Bandlücke-Ge_Untergrund.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_fehler = read.table("../data/Bandlücke-Ge_Fehler.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_lampe = read.table("../data/Bandlücke-Ge_Lampenleistung.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
germanium = subset(germanium,E>-1 & E<1)
ge_lampe = subset(ge_lampe,E>-2 & E<2)

par(mfrow=c(1,1))


x=ge_fehler$t
y1=ge_fehler$U_pyro
y2=ge_fehler$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

ge_fehler_pyro_data=data.frame(x=ge_fehler$t,y=ge_fehler$U_pyro)
ge_fehler_sample_data=data.frame(x=ge_fehler$t,y=ge_fehler$U_sample)
ge_fehler_pyro=konstfit(ge_fehler_pyro_data)
ge_fehler_sample=konstfit(ge_fehler_sample_data)
abline(a=ge_fehler_pyro[1], b=0,col="blue")
abline(a=ge_fehler_sample[1], b=0,col="red")
ge_pyro_err=ge_fehler_pyro[2]
ge_samp_err=ge_fehler_sample[2]

x=ge_untergrund$t
y1=ge_untergrund$U_pyro
y2=ge_untergrund$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=c(0,0.1))
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

ge_untergrund_pyro_data=data.frame(x=ge_untergrund$t,y=ge_untergrund$U_pyro)
ge_untergrund_sample_data=data.frame(x=ge_untergrund$t,y=ge_untergrund$U_sample)
ge_untergrund_pyro=konstfit(ge_untergrund_pyro_data)
ge_untergrund_sample=konstfit(ge_untergrund_sample_data)
abline(a=ge_untergrund_pyro[1], b=0,col="blue")
abline(a=ge_untergrund_sample[1], b=0,col="red")

x=germanium$E
y1=germanium$U_pyro-ge_untergrund_pyro[1]
y2=germanium$U_sample-ge_untergrund_sample[1]
sy1=sqrt(ge_pyro_err^2+ge_untergrund_pyro[2]^2)*y1/y1
sy2=sqrt(ge_samp_err^2+ge_untergrund_sample[2]^2)*y2/y2
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="Channel",ylab="Counts",cex=0.6,bty="l",col="blue",ylim=lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="Channel",ylab="Counts",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

x=ge_lampe$E
y1=ge_lampe$U_pyro
y2=ge_lampe$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

