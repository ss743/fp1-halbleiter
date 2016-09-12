lampe=data.frame(alpha=material$alpha,E=material$E,U=rep(NA,length(material$E)))

for(i in c(1:nrow(lampe))){
  energy=lampe$E[i]
  diff=c() # create vector with difference values
  for(j in c(1:nrow(lampe_alt))){
    diff[j]=lampe_alt$E[j]-energy
  }
  diff_g0=c() # create vector with all entries > 0
  for(k in c(1:length(diff))){
    if(diff[k]>0)
      diff_g0[k]=diff[k]
  }
  diff_l0=c() # create vector with all entries < 0
  for(k in c(1:length(diff))){
    if(diff[k]<0)
      diff_l0[k]=diff[k]
  }
  a=which.min(diff_g0) #left next entry
  b=which.max(diff_l0) #right next entry

  lampe$U[i]=(lampe_alt$U_pyro[a]+diff[b]*(lampe_alt$U_pyro[b]-lampe_alt$U_pyro[a])/(diff[a]-diff[b]))
  
}
