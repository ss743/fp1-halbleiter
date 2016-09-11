linearfit <- function(input,bereich){
  data=subset(input,x>=bereich[1] & x <= bereich[2])
  x=data$x
  y=data$y
  err=data$sy
  
  fit=lm(y~x,weights=1/err^2)
  
  intercept=fit$coefficients[["(Intercept)"]]
  slope=fit$coefficients[["x"]]
  
  intererr=summary(fit)$coefficients[["(Intercept)","Std. Error"]]
  slopeerr=summary(fit)$coefficients[["x","Std. Error"]]
  
  return(c(intercept,slope,intererr,slopeerr))
}