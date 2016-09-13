sqrtfit <- function(input, bereich, weighted=FALSE){ #--- Fitten der Exponentialfunktion
  
  thesqrt <- y ~ A + sqrt(B*x)
  
  #daten=input[bereich[1]:bereich[2],]
  daten=subset(input,x>=bereich[1] & x <= bereich[2])
  
  #---Startwerte
  ymin=min(daten$y)
  xmin=daten$x[which.min(daten$y)]
  ymax=max(daten$y)
  xmax=daten$x[which.max(daten$y)]
  B_est=(ymin-ymax)^2/((sqrt(xmin)-sqrt(xmax))^2)
  err=daten$sy
  startvalues=list(A=ymin,B=B_est)
  #---Startwerte

  #plot (function(x){ymin + sqrt(B_est*x)},bereich[1],bereich[2],add=TRUE,col="green")
  
    
  #---Durchführen des Fits
  nlc<-nls.control(maxiter=5000)
  if(weighted)
    fit = nls(thesqrt,daten,weights=1/err^2,start=startvalues,control=nlc)
  else
    fit = nls(thesqrt,daten,start=startvalues,control=nlc)
  #return(fit)
  return(summary(fit)$parameters)
  
}

plotsqrt <- function(fitdata,bereich){ #--- Plotten der gefitteten Exponentialfunktion in vorhandenen Graph
  
  B<-fitdata["B","Estimate"]
  A<-fitdata["A","Estimate"]
  
  plot (function(x){A + sqrt(B*x)},bereich[1],bereich[2],add=TRUE,col="red")
  
}

printsqrtdata <- function(fitdata,title="",factor=1,error=0){ #--- Ausgabe der Exponentialfit-Daten
  
  B<-fitdata["B","Estimate"]
  sB<-fitdata["B","Std. Error"]
  
  A<-fitdata["A","Estimate"]
  sA<-fitdata["A","Std. Error"]
  
  cat(title)
  cat("\n")
  

  cat(" A = ")
  cat(A)
  cat("+-")
  cat(sA)
  cat("\n")
  cat(" B = ")
  cat(B)
  cat("+-")
  cat(sB)
  cat("\n")
  
  
}
