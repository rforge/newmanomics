require(matrixStats)
require(stats)

input = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Bank_Testing.csv", row.names = 1)
mat = data.matrix(input)


nmlStr = "Normal"
tumStr = "Cancer"

Paired_Stat = function(matrix){
  nmlIdx = grep(nmlStr,colnames(matrix))
  matNml = matrix[ ,nmlIdx]
  matTum = matrix[ , -nmlIdx]

  n = dim(matNml)[1]
  s = dim(matNml)[2]

  temp.means = temp.sd = matsdEst = matNu = matrix(NA,n,s)

  for (i in 1:s){
    for (j in 1:n){
      temp.means[j,i] = mean(c(matNml[j,i],matTum[j,i]))
      temp.sd[j,i] = sd(c(matNml[j,i],matTum[j,i]))
    }
    l.mod = loess(temp.sd[ ,i] ~ temp.means[ ,i])
    matsdEst[ ,i] = predict(l.mod)

    for (k in 1:n){
      matNu[k,i] = abs((matNml[k,i]) - (matTum[k,i]))/matsdEst[k,i]
    }
  }

  m = mean(matNu)
  sd = sd(matNu)

  randNu = rNuGen(m,sd)
  v = as.vector(randNu)

  pValsPaired = nu2P(matNu,v)
}

rNuGen = function(mean,sd){
  outerSim = rnorm(1000000,mean,sd)
  pValDist = nuValMat = matrix(NA, 10000,100)
  temp.means = temp.sd = matrix(NA, 10000,100)
  sdEst = numeric(10000)
  for (i in 1:100){
    vec1 = sample(outerSim,10000)
    vec2 = sample(outerSim,10000)
    for (j in 1:10000){
      temp.means[j,i] = mean(c(vec1[j],vec2[j]))
      temp.sd[j,i] = sd(c(vec1[j],vec2[j]))
    }
    l.mod = loess(temp.sd[ ,i] ~ temp.means[ ,i])
    sdEst[i] = predict(l.mod)
    for (k in 1:10000){
      nuValMat[k,i] = abs(vec1[k] - vec2[k])/sdEst[i]
    }
  }
  return(nuValMat)
}

nu2P = function(matrix,vec,s){
  matP = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      matP[i,j] = length(vec[vec <= matrix[i,j]])/10000
    }
  }
  return(matP)
}
