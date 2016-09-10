mittelwert <- function(input,bereich){
  
  daten=input[bereich[1]:bereich[2],]
  return(mittelwert(daten))
  
}

mittelwert <- function(input){
  
  a=sum(input$y)/length(input$y)
  
  return(a)
  
}

konstfit <- function(input,bereich){
  
  daten=input[bereich[1]:bereich[2],]

  return(kosntfit(daten))
  
}

konstfit <- function(input){
  
  mw=mittelwert(input)
  n=length(input$y)
  einheit=c(1:n)/c(1:n)
  a=sqrt(1/((n-1))*sum((input$y-mw*einheit)^2))
  
  print(mw)
  
  return(c(mw,a))
  
}