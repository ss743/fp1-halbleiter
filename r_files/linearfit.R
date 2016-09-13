linearfit <- function(input,bereich,weighted=TRUE){
  data=subset(input,x>=bereich[1] & x <= bereich[2])
  x=data$x
  y=data$y
  
  if(weighted){
    err=data$sy
    fit=lm(y~x,weights=1/err^2)
  } else {
    fit=lm(y~x)
  }
  intercept=fit$coefficients[["(Intercept)"]]
  slope=fit$coefficients[["x"]]
  
  intererr=summary(fit)$coefficients[["(Intercept)","Std. Error"]]
  slopeerr=summary(fit)$coefficients[["x","Std. Error"]]
  
  return(c(intercept,slope,intererr,slopeerr))
}

plotlinear <- function(fitdata,grenzen){
  
  line<-data.frame(x=grenzen,y=fitdata[1]+fitdata[2]*grenzen)
  lines(line,col="red",xlim=grenzen)
  
}

plotlindata <- function(fitdata,title=""){
  
  cat(title)
  cat("\n")
  
  cat(" Intercept: ")
  cat(fitdata[1])
  cat("+-")
  cat(fitdata[3])
  cat("\n")
  
  cat(" Slope:     ")
  cat(fitdata[2])
  cat("+-")
  cat(fitdata[4])
  cat("\n")
  

}