require(matrixStats)
require(stats)

input1 = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Bank_Testing.csv", row.names = 1)
mat = data.matrix(input1)


nmlStr = "Normal"
tumStr = "Cancer"


## Actual Statistic
bankStat = function(mat){
  bankIdx = grep(nmlStr,colnames(mat))
  bank = mat[ ,bankIdx]

  n = dim(bank)[1]
  s = dim(mat)[2] - dim(bank)[2]
  sNorm = dim(bank)[2]

  temp.means  = unname(rowMeans(mat))
  temp.sd = rowSds(mat)

  loess.model = loess(temp.sd ~ temp.means)
  sdEst = predict(loess.model)

  matNuBank = nuValsBank(matrix = mat,mean = temp.means, sd = sdEst, s = s)

  matPBanked = pValuesBanked(matrix = matNuBank)
  fdrAdjustedBank = fdrCorrection(matPBanked)

  write.csv(fdrAdjustedBank,"C:/Users/anous/Documents/R/BMI Research/Output/GSE6631_Head&NeckSq_Banked.csv", row.names = RN)

}


nuValsBank = function(matrix,mean,sd,s){
  matNu = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      matNu[i,j] = abs((matrix[i,j]) - (mean[i]))/sd[i]
    }
  }
  return(matNu)
}

nuValsIterBank = function(mat,n,s){
  temp.means = temp.sd = matsdEst = matNu = matrix(NA,n,s)

  for (j in 1:s){
    temp.means[ ,j] = rowMeans(mat[ ,-j])
    temp.sd[ ,j] = rowSds(mat[ ,-j])
    l.mod = loess(temp.sd[ ,j] ~ temp.means[ ,j])
    matsdEst[ ,j] = predict(l.mod)
    for (k in 1:n){
      matNu[k,j] <<- abs(mat[k,j] - temp.means[k,j])/matsdEst[k,j]
    }
  }
  return(matNu)
}


pValuesBanked = function(matrix){
  backDist = nuValsBank(matrix = bank,mean = temp.means, sd = sdEst, s = sNorm)

  logNuNorm = log(backDist,2)
  logNuTum = log(matNuBank,2)

  meanDist = mean(logNuNorm)
  sdDist = sd(logNuTum)

  matP = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      if (logNuNorm[i,j] <= meanDist){
        matP[i,j] = pNorm(logNuTum[i,j],meanDist,sdDist)
      }else {
        matP[i,j] = pNorm(logNuTum[i,j],meanDist,sdDist,lower.tail = F)
      }
    }
  }
  return(matP)
}

fdrCorrection = function(matrix){

  ## Benjamini Hochberg method employed. Could also use built in p.adjust(p, "BH")
  ## Percent correction only factors in during analysis, b/c (rank/total)*percentageFDR only gives the adjusted cutoff


  v = as.vector(matrix)
  vUnique = v[!duplicated(v)]
  vs = sort(vUnique,decreasing = T)
  vRank = rank(vs)
  num = max(vRank)

  adjustedP = dim(vUnique)
  adjustedP[1] = vs[1]

  for (i in 1:num){
    adjustedP[i] = min(adjustedP[i-1],(vs[i]*num)/vRank[i])
  }

  fdrP = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      fdrP[i,j] = adjustedP[match(matrix[i,j],vUnique)]
    }
  }
  return(fdrP)
}
