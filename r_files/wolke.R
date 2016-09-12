###########################
# Einbinden der Libraries #
###########################
library(Hmisc)

######################
# Einlesen der Daten #
######################
ch1 = c()
ch2 = c()
for(i in c(0:34)) {
  if(i<10)
    pre="000"
  else
    pre="00"
  path1=paste("../oszi_data/ALL",pre,i,"/F",pre,i,"CH1.CSV",sep="")
  path2=paste("../oszi_data/ALL",pre,i,"/F",pre,i,"CH2.CSV",sep="")
  ch1=read.csv(path1)
  ch2=read.csv(path2)
}