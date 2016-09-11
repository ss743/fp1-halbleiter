lampe=data.frame(alpha=material$alpha,E=material$E,U=rep(NA,length(material$E)))

for(i in c(1:nrow(lampe))){
  energy=lampe$E[i]
  #cat("\n")
  #cat(i)
  #cat(": ")
  #cat(energy)
  diff=c()
  for(j in c(1:nrow(lampe_alt))){
    #cat("\n ")
    #cat(j)
    #cat(": ")
    #cat(lampe_alt$E[j]-energy)
    diff[j]=lampe_alt$E[j]-energy
  }
  diff_g0=c()
  for(k in c(1:length(diff))){
    if(diff[k]>0)
      diff_g0[k]=diff[k]
  }
  diff_l0=c()
  for(k in c(1:length(diff))){
    if(diff[k]<0)
      diff_l0[k]=diff[k]
  }
  a=which.min(diff_g0)
  b=which.max(diff_l0)
  #cat("\n minDiff:")
  #cat(a)
  #cat(",")
  #cat(b)
  #cat(": ")
  #cat(diff[a])
  #cat(",")
  #cat(diff[b])
  
  lampe$U[i]=(lampe_alt$U_pyro[a]+diff[b]*(lampe_alt$U_pyro[b]-lampe_alt$U_pyro[a])/(diff[a]-diff[b]))
  
}
