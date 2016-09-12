###########################
# Einbinden der Libraries #
###########################
library(Hmisc)
source("konstfit.R")
source("linearfit.R")

######################
# Einlesen der Daten #
######################
germanium = read.table("../data/Bandlücke-Ge.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_untergrund = read.table("../data/Bandlücke-Ge_Untergrund.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_fehler = read.table("../data/Bandlücke-Ge_Fehler.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
ge_lampe = read.table("../data/Bandlücke-Ge_Lampenleistung.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
germanium = subset(germanium,E>-1 & E<1)
ge_lampe = subset(ge_lampe,E>-2 & E<2)

silizium = read.table("../data/Bandlücke-Si.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
si_untergrund = read.table("../data/Bandlücke-Si_Untergrund.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
si_fehler = read.table("../data/Bandlücke-Si_Fehler.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
si_fehler2 = read.table("../data/Bandlücke-Si_Fehler2.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
si_lampe = read.table("../data/Bandlücke-Si_Lampenleistung.txt",sep="\t",skip=6,col.names=c("t","alpha","U_pyro","U_sample","lambda","E"),dec=",")
silizium = subset(silizium,E>-2 & E<2)
si_lampe = subset(si_lampe,E>-3 & E<3)


par(mfrow=c(1,1))

 ##############################
 # Berechnen der Fehlerbalken #
 ##############################
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

 #############################
 # Berechnen des Untergrunds #
 #############################
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

 ############################
 # Plot des Lampenspektrums #
 ############################
x=ge_lampe$E
y1=ge_lampe$U_pyro
sy1=ge_pyro_err
lim=c(min(y1),max(y1))
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

 ##################################
 # Umrechnung des Lampenspektrums #
 ##################################
lampe_alt=ge_lampe
material=germanium

source("lampe.R")

ge_lampe=lampe

 ###########################
 # Umrechnung der Spektren #
 ###########################
x=germanium$E
y1_ol=germanium$U_pyro-ge_untergrund_pyro[1]
y1=y1_ol/ge_lampe$U
y2_ol=germanium$U_sample-ge_untergrund_sample[1]
y2=y2_ol/ge_lampe$U
sy1_ol=sqrt(ge_pyro_err^2+ge_untergrund_pyro[2]^2)*y1_ol/y1_ol
sy1=sqrt((sy1_ol/y1_ol)^2+(ge_pyro_err/ge_lampe$U)^2)*y1
sy2_ol=sqrt(ge_samp_err^2+ge_untergrund_sample[2]^2)*y2_ol/y2_ol
sy2=sqrt((sy2_ol/y2_ol)^2+(ge_pyro_err/ge_lampe$U)^2)*y2
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
lim=c(0,0.7)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

 ######################################################
 # Berechnung des Schnittpunktes im negativen Bereich #
 ######################################################
lim=c(0,0.6)
x_lim=c(-0.8,-0.5)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim,xlim=x_lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

grenzen1=c(-0.72,-0.63)
grenzen2=c(-0.70,-0.62)

daten1=data.frame(x=x,y=y1,sy=sy1)
daten2=data.frame(x=x,y=y2,sy=sy2)
fit1=linearfit(daten1,grenzen1)
fit2=linearfit(daten2,grenzen2)

blueline<-data.frame(x=grenzen1,y=fit1[1]+fit1[2]*grenzen1)
redline<-data.frame(x=grenzen2,y=fit2[1]+fit2[2]*grenzen2)

schnittpunkt1=abs((fit1[1]-fit2[1])/(fit2[2]-fit1[2]))
errorschnitt1=sqrt((fit1[3]^2+fit2[3]^2)/(fit1[1]-fit2[1])^2+(fit1[4]^2+fit2[4]^2)/(fit2[2]-fit1[2])^2)

lines(blueline,col="blue",xlim=grenzen1)
lines(redline,col="red",xlim=grenzen2)

 ######################################################
 # Berechnung des Schnittpunktes im positiven Bereich #
 ######################################################
lim=c(0,0.7)
x_lim=c(0.5,0.8)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim,xlim=x_lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

grenzen1=c(0.65,0.77)
grenzen2=c(0.64,0.72)

daten1=data.frame(x=x,y=y1,sy=sy1)
daten2=data.frame(x=x,y=y2,sy=sy2)
fit1=linearfit(daten1,grenzen1)
fit2=linearfit(daten2,grenzen2)

blueline<-data.frame(x=grenzen1,y=fit1[1]+fit1[2]*grenzen1)
redline<-data.frame(x=grenzen2,y=fit2[1]+fit2[2]*grenzen2)

schnittpunkt2=(fit1[1]-fit2[1])/(fit2[2]-fit1[2])
errorschnitt2=sqrt((fit1[3]^2+fit2[3]^2)/(fit1[1]-fit2[1])^2+(fit1[4]^2+fit2[4]^2)/(fit2[2]-fit1[2])^2)

lines(blueline,col="blue",xlim=grenzen1)
lines(redline,col="red",xlim=grenzen2)

 ######################################
 # Gewichtetes Mittel aus den Punkten #
 ######################################
schnittpunkt=(schnittpunkt1/errorschnitt1^2+schnittpunkt2/errorschnitt2^2)/(1/errorschnitt1^2+1/errorschnitt2^2)
errorschnitt=sqrt(1/(1/errorschnitt1^2+1/errorschnitt2^2))

cat("Bandlückenenergie: E_{Ge} = ")
cat(schnittpunkt)
cat(" +- ")
cat(errorschnitt)
cat("eV")


############
# Silizium #
############

par(mfrow=c(1,2))

 ##############################
 # Berechnen der Fehlerbalken #
 ##############################
x=si_fehler$t
y1=si_fehler$U_pyro
y2=si_fehler$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

si_fehler_pyro_data=data.frame(x=si_fehler$t,y=si_fehler$U_pyro)
si_fehler_sample_data=data.frame(x=si_fehler$t,y=si_fehler$U_sample)
si_fehler_pyro=konstfit(si_fehler_pyro_data)
si_fehler_sample=konstfit(si_fehler_sample_data)
abline(a=si_fehler_pyro[1], b=0,col="blue")
abline(a=si_fehler_sample[1], b=0,col="red")

x=si_fehler2$t
y1=si_fehler2$U_pyro
y2=si_fehler2$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

si_fehler_pyro2_data=data.frame(x=si_fehler2$t,y=si_fehler2$U_pyro)
si_fehler_sample2_data=data.frame(x=si_fehler2$t,y=si_fehler2$U_sample)
si_fehler_pyro2=konstfit(si_fehler_pyro2_data)
si_fehler_sample2=konstfit(si_fehler_sample2_data)
abline(a=si_fehler_pyro2[1], b=0,col="blue")
abline(a=si_fehler_sample2[1], b=0,col="red")
si_pyro_err=(si_fehler_pyro[2]+si_fehler_pyro2[2])/2
si_samp_err=(si_fehler_sample[2]+si_fehler_sample2[2])/2

par(mfrow=c(1,1))

 #############################
 # Berechnen des Untergrunds #
 #############################
x=si_untergrund$t
y1=si_untergrund$U_pyro
y2=si_untergrund$U_sample
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
plot(x,y1,type="p",pch=4,xlab="t",ylab="U",cex=0.6,bty="l",col="blue",ylim=c(0,0.1))
points(x,y2,type="p",pch=4,cex=0.6,bty="l",col="red")
grid()

si_untergrund_pyro_data=data.frame(x=si_untergrund$t,y=si_untergrund$U_pyro)
si_untergrund_sample_data=data.frame(x=si_untergrund$t,y=si_untergrund$U_sample)
si_untergrund_pyro=konstfit(si_untergrund_pyro_data)
si_untergrund_sample=konstfit(si_untergrund_sample_data)
abline(a=si_untergrund_pyro[1], b=0,col="blue")
abline(a=si_untergrund_sample[1], b=0,col="red")

 ############################
 # Plot des Lampenspektrums #
 ############################
x=si_lampe$E
y1=si_lampe$U_pyro
sy1=si_pyro_err
lim=c(min(y1),max(y1))
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

 ##################################
 # Umrechnung des Lampenspektrums #
 ##################################
lampe_alt=si_lampe
material=silizium

source("lampe.R")

si_lampe=lampe

 ###########################
 # Umrechnung der Spektren #
 ###########################
x=silizium$E
y1_ol=silizium$U_pyro-si_untergrund_pyro[1]
y1=y1_ol/si_lampe$U
y2_ol=silizium$U_sample-si_untergrund_sample[1]
y2=y2_ol/si_lampe$U
sy1_ol=sqrt(si_pyro_err^2+si_untergrund_pyro[2]^2)*y1_ol/y1_ol
sy1=sqrt((sy1_ol/y1_ol)^2+(si_pyro_err/si_lampe$U)^2)*y1
sy2_ol=sqrt(si_samp_err^2+si_untergrund_sample[2]^2)*y2_ol/y2_ol
sy2=sqrt((sy2_ol/y2_ol)^2+(si_pyro_err/si_lampe$U)^2)*y2
lim=c(min(c(min(y1),min(y2))),max(c(max(y1),max(y2))))
lim=c(0,20)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

 ######################################################
 # Berechnung des Schnittpunktes im negativen Bereich #
 ######################################################
lim=c(0,6)
x_lim=c(-1.3,-1)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim,xlim=x_lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

grenzen1=c(-1.20,-1.075)
grenzen2=c(-1.15,-1.05)

daten1=data.frame(x=x,y=y1,sy=sy1)
daten2=data.frame(x=x,y=y2,sy=sy2)
fit1=linearfit(daten1,grenzen1)
fit2=linearfit(daten2,grenzen2)

blueline<-data.frame(x=grenzen1,y=fit1[1]+fit1[2]*grenzen1)
redline<-data.frame(x=grenzen2,y=fit2[1]+fit2[2]*grenzen2)

schnittpunkt1=abs((fit1[1]-fit2[1])/(fit2[2]-fit1[2]))
errorschnitt1=sqrt((fit1[3]^2+fit2[3]^2)/(fit1[1]-fit2[1])^2+(fit1[4]^2+fit2[4]^2)/(fit2[2]-fit1[2])^2)

lines(blueline,col="blue",xlim=grenzen1)
lines(redline,col="red",xlim=grenzen2)

 ######################################################
 # Berechnung des Schnittpunktes im positiven Bereich #
 ######################################################
lim=c(0,20)
x_lim=c(1,1.2)
plot(x,y1,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="blue",ylim=lim,xlim=x_lim)
with(data=data.frame(x=x,y=y1,sy=sy1),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
points(x,y2,type="p",pch=4,xlab="E",ylab="U",cex=0.6,bty="l",col="red")
with(data=data.frame(x=x,y=y2,sy=sy2),expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
grid()

grenzen1=c(1.05,1.20)
grenzen2=c(1.07,1.17)

daten1=data.frame(x=x,y=y1,sy=sy1)
daten2=data.frame(x=x,y=y2,sy=sy2)
fit1=linearfit(daten1,grenzen1)
fit2=linearfit(daten2,grenzen2)

blueline<-data.frame(x=grenzen1,y=fit1[1]+fit1[2]*grenzen1)
redline<-data.frame(x=grenzen2,y=fit2[1]+fit2[2]*grenzen2)

schnittpunkt2=(fit1[1]-fit2[1])/(fit2[2]-fit1[2])
errorschnitt2=sqrt((fit1[3]^2+fit2[3]^2)/(fit1[1]-fit2[1])^2+(fit1[4]^2+fit2[4]^2)/(fit2[2]-fit1[2])^2)

lines(blueline,col="blue",xlim=grenzen1)
lines(redline,col="red",xlim=grenzen2)

 ######################################
 # Gewichtetes Mittel aus den Punkten #
 ######################################
schnittpunkt=(schnittpunkt1/errorschnitt1^2+schnittpunkt2/errorschnitt2^2)/(1/errorschnitt1^2+1/errorschnitt2^2)
errorschnitt=sqrt(1/(1/errorschnitt1^2+1/errorschnitt2^2))

cat("\nBandlückenenergie: E_{Si} = ")
cat(schnittpunkt)
cat(" +- ")
cat(errorschnitt)
cat("eV")

