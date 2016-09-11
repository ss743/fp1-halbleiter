setwd("C:/Users/Saskia/Documents/Physik/FP/fp1-halbleiter/Auswertung_Teil_3")

source("gausfit.R")

peak_am=read.table("Americium_CdTe.mca", skip=12, stringsAsFactors = FALSE)
peak_co1=read.table("Cobalt_CdTe.mca", skip=12, stringsAsFactors = FALSE)

input_data = data.frame(x=1:400,y=peak_am[[1]][1:400],sy=sqrt(peak_am[[1]][1:400]))
input_data_2 = data.frame(x=1:800,y=peak_co1[[1]][1:800],sy=sqrt(peak_co1[[1]][1:800]))



plottype <- "p"
pointsize<- 0.6
weighted <- TRUE

plot(input_data$x,input_data$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
with(data=input_data,expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
bereich=c(275,340)
fit1=gausfit(input_data,bereich,weighted)
plotgaus(fit1,bereich)
printfitdata(fit1,"Peak 1")

plot(input_data_2$x,input_data_2$y,type=plottype,pch=4,xlab="Channel",ylab="Counts",cex=pointsize,bty="l")
with(data=input_data_2,expr=Hmisc::errbar(x,y,y+sy,y-sy,add=T,pch=4,type="n"))
bereich=c(600,665)
fit2=gausfit(input_data_2,bereich,weighted)
plotgaus(fit2,bereich)
printfitdata(fit2,"Peak 2")

bereich=c(680,750)
fit3=gausfit(input_data_2,bereich,weighted)
plotgaus(fit3,bereich)
printfitdata(fit3,"Peak 3")