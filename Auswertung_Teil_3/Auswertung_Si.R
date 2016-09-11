setwd("C:/Users/Saskia/Documents/Physik/FP/fp1-halbleiter/Auswertung_Teil_3")

source("gausfit.R")

peak_am=read.table("Americium_Si.mca", skip=12, stringsAsFactors = FALSE)
peak_co1=read.table("Cobalt_Si.mca", skip=12, stringsAsFactors = FALSE)

input_data = data.frame(x=1:100,y=peak_am[[1]][1:100],sy=sqrt(peak_am[[1]][1:100]))
input_data_2 = data.frame(x=1:1000,y=peak_co1[[1]][1:1000],sy=sqrt(peak_co1[[1]][1:1000]))



plottype <- "p"
pointsize<- 0.6
weighted <- TRUE

plot(input_data$x,input_data$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
with(data=input_data,expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
bereich=c(48,58)
fit1=gausfit(input_data,bereich,weighted)
plotgaus(fit1,bereich)
printfitdata(fit1,"Peak 1")

plot(input_data_2$x,input_data_2$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
with(data=input_data_2,expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
bereich=c(600,650)
fit2=gausfit(input_data_2,bereich,weighted)
plotgaus(fit2,bereich)
printfitdata(fit2,"Peak 2")

bereich=c(695,708)
fit3=gausfit(input_data_2,bereich,weighted)
plotgaus(fit3,bereich)
printfitdata(fit3,"Peak 3")