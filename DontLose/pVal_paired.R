RNuGen = function(mean,sd){
  Outer_Sim = rnorm(1000000,mean,sd)
  PVal_dist = NuVal_mat = matrix(NA, 10000,100)
  
  temp.means = temp.sd = matrix(NA, 10000,100)
  SdEst = numeric(10000)
  
  for (i in 1:100){
    vec1 = sample(Outer_Sim,10000)
    vec2 = sample(Outer_Sim,10000)
    
    for (j in 1:10000){
      temp.means[j,i] = mean(c(vec1[j],vec2[j]))
      temp.sd[j,i] = sd(c(vec1[j],vec2[j]))
    }
    l.mod = loess(temp.sd[ ,i] ~ temp.means[ ,i])
    SdEst[i] = predict(l.mod)
  
    
    for (k in 1:10000){
      NuVal_mat[k,i] = abs(vec1[k] - vec2[k])/SdEst[i] 
    }
  }
  
  return(NuVal_mat)
}

v = as.vector(NuVal_mat)

v_unique = v[!duplicated(v)]
vs = sort(v_unique)
#v_rank = rank(vs)
num = max(v_rank)


  
  # adjusted_p = dim(v_unique)
  # adjusted_p[1] = vs[1]
  # 
  # for (i in 1:num){
  #   adjusted_p[i] = min(adjusted_p[i-1],(vs[i]*num)/v_rank[i])
  # }
  # 
  # FDR_P = matrix(NA,10000,100)
  # for (i in 1:n){
  #   for (j in 1:s){
  #     FDR_P[i,j] = adjusted_p[match(PVal_dist[i,j],v_unique)]
  #   }
  # }
  # 

m = sd = vector("numeric", length = 10000)
for (i in 1:10000){
  m[i] = mean(c(vec1[i],vec2[i]))
  sd[i] = sd(c(vec1[i],vec2[i]))
}
