require(matrixStats)
require(stats)

input = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Bank_Testing.csv", row.names = 1)
mat = data.matrix(input)

pilotStat = function(mat){
  n = dim(mat)[1]
  s = dim(mat)[2]

  temp.means  = unname(rowMeans(mat))
  temp.sd = rowSds(mat)

  loess.model = loess(temp.sd ~ temp.means)
  sdEst = predict(loess.model)

  nuValsAnova(matrix = mat, sd = sdEst)


  ## F measure = between group var/within group var
  ## p val = area under curve (correction needed?)
}

nuValsAnova = function(matrix,sd){
  nuVals = numeric(n)
  for (i in 1:n){
    nuVals[i] = (max(matrix[i, ]) - min(matrix[i, ]))/sd[i]
  }
  return(nuVals)
}
