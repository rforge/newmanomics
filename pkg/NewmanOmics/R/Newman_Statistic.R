require(matrixStats)
require(stats)

input1 = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Bank_Testing.csv", row.names = 1)
MatB = data.matrix(input1)

input2 = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Paired_Testing.csv", row.names = 1)
MatP = data.matrix(input2)
#RN = rownames(input)

nml_str = "Normal"
tum_str = "Cancer"


## Preliminary functions

NuVals_Bank = function(matrix,mean,sd,s){
  MatNu = matrix(NA,n,s)  
  for (i in 1:n){
    for (j in 1:s){
      MatNu[i,j] = abs((matrix[i,j]) - (mean[i]))/sd[i]
    }
  }
  return(MatNu)
}

NuVals_Anova = function(matrix,sd){
  nu_vals = numeric(n)
  for (i in 1:n){
    nu_vals[i] = (max(matrix[i, ]) - min(matrix[i, ]))/sd[i]
  }
  return(nu_vals)
}

NuVals_IterBank = function(MatA,n,s){
  temp.means = temp.sd = MatsdEst = MatNu = matrix(NA,n,s)
  
  for (j in 1:s){
    temp.means[ ,j] = rowMeans(MatA[ ,-j])
    temp.sd[ ,j] = rowSds(MatA[ ,-j])
    l.mod = loess(temp.sd[ ,j] ~ temp.means[ ,j])
    MatsdEst[ ,j] = predict(l.mod)
    for (k in 1:n){
      MatNu[k,j] <<- abs(MatA[k,j] - temp.means[k,j])/MatsdEst[k,j]
    }
  }
  return(MatNu)
}

PValues_Banked = function(matrix){
  Back_dist = NuVals_Bank(matrix = Bank,mean = temp.means, sd = sdEst, s = s_norm)
  
  LogNu_Norm = log(Back_dist,2)
  LogNu_Tum = log(MatNu_Bank,2)
  
  mean_dist = mean(LogNu_Norm)
  sd_dist = sd(LogNu_Tum)
  
  Mat_P = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      if (LogNu_Norm[i,j] <= mean_dist){
        Mat_P[i,j] = pnorm(LogNu_Tum[i,j],mean_dist,sd_dist)
      }else {
        Mat_P[i,j] = pnorm(LogNu_Tum[i,j],mean_dist,sd_dist,lower.tail = F)
      }
    }
  }
  return(MatP)
}

FDR_Correction = function(matrix){
  
  ## Benjamini Hochberg method employed. Could also use built in p.adjust(p, "BH")
  ## Percent correction only factors in during analysis, b/c (rank/total)*percentageFDR only gives the adjusted cutoff

  
  v = as.vector(matrix)
  v_unique = v[!duplicated(v)]
  vs = sort(v_unique,decreasing = T)
  v_rank = rank(vs)
  num = max(v_rank)
  
  adjusted_p = dim(v_unique)
  adjusted_p[1] = vs[1]
  
  for (i in 1:num){
    adjusted_p[i] = min(adjusted_p[i-1],(vs[i]*num)/v_rank[i])
  }
  
  FDR_P = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      FDR_P[i,j] = adjusted_p[match(matrix[i,j],v_unique)]
    }
  }
  return(FDR_P)
}

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

Nu2P = function(matrix,vec,s){
  MatP = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      MatP[i,j] = length(vec[vec <= matrix[i,j]])/10000
    }
  }
  return(MatP)
}


## Actual Statistic
Bank_Stat = function(MatA){
  bank_idx = grep(nml_str,colnames(MatA))
  Bank = MatA[ ,bank_idx]
  
  n = dim(Bank)[1]
  s = dim(MatA)[2] - dim(Bank)[2]
  s_norm = dim(Bank)[2]
  
  temp.means  = unname(rowMeans(MatA))
  temp.sd = rowSds(MatA)
  
  loess.model = loess(temp.sd ~ temp.means)
  sdEst = predict(loess.model)
  
  MatNu_Bank = NuVals_Bank(matrix = MatA,mean = temp.means, sd = sdEst, s = s)
  
  MatP_Banked = PValues_Banked(matrix = MatNu_Bank)
  FDR_Adjusted_Bank = FDR_Correction(MatP_Banked)
  
  write.csv(FDR_Adjusted_Bank,"C:/Users/anous/Documents/R/BMI Research/Output/GSE6631_Head&NeckSq_Banked.csv", row.names = RN)
  
}

Paired_Stat = function(matrix){
  nml_idx = grep(nml_str,colnames(matrix))
  Mat_Nml = matrix[ ,nml_idx]
  Mat_Tum = matrix[ , -nml_idx]
  
  n = dim(Mat_Nml)[1]
  s = dim(Mat_Nml)[2]
  
  temp.means = temp.sd = MatsdEst = MatNu = matrix(NA,n,s)
  
  for (i in 1:s){
    for (j in 1:n){
      temp.means[j,i] = mean(c(Mat_Nml[j,i],Mat_Tum[j,i]))
      temp.sd[j,i] = sd(c(Mat_Nml[j,i],Mat_Tum[j,i])) 
    } 
    l.mod = loess(temp.sd[ ,i] ~ temp.means[ ,i])
    MatsdEst[ ,i] = predict(l.mod)
   
    for (k in 1:n){
      MatNu[k,i] = abs((Mat_Nml[k,i]) - (Mat_Tum[k,i]))/MatsdEst[k,i] 
    }
  }
  
  m = mean(MatNu)
  sd = sd(MatNu)
  
  RandNu = RNuGen(m,sd)
  v = as.vector(RandNu)
  
  PVals_paired = Nu2P(MatNu,v)
}

Pilot_Stat = function(MatA){
  n = dim(MatA)[1]
  s = dim(MatA)[2]
  
  temp.means  = unname(rowMeans(MatA))
  temp.sd = rowSds(MatA)
  
  loess.model = loess(temp.sd ~ temp.means)
  sdEst = predict(loess.model)
  
  NuVals_Anova(matrix = MatA, sd = sdEst)
  
  
  ## F measure = between group var/within group var
  ## p val = area under curve (correction needed?)
}





