require(matrixStats)
require(stats)
#require(gdata)
#require(Matrix)
#require(datasets)

input = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Pilot_Testing.csv", row.names = 1)
MatA = data.matrix(input)

### ANOVA method

Pilot_Stat = function(MatA){
  n = dim(MatA)[1]
  s = dim(MatA)[2]
  
  Get_Means(MatA)
  
  NuVals_Anova(matrix = MatA, sd = sdEst)
}

Get_Means = function(matrix){
  temp.means  <<- unname(rownames(MatA))
  temp.sd = rowSds(MatA)
  
  loess.model = loess(sd ~ means)
  sdEst <<- predict(loess.model)
}

#plot(sd ~ means)
#points(means[order(means)],sdEst[order(means)], col = "green")

NuVals_Anova = function(matrix,sd){
  nu_vals = numeric(n)
  for (i in 1:n){
    nu_vals[i] <<- (max(MatA[i, ]) - min(MatA[i, ]))/sdEst[i]
  }
}

RN = rownames(input)
write.csv(nu_vals, "C:/Users/anous/Documents/R/BMI Research/Output/PilotTest_Anova_Nu.csv", row.names = RN)


### Iterative bank method

Iter_Bank = function(MatA,n,s){
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
}


write.csv(MatNu,"C:/Users/anous/Documents/R/BMI Research/Output/Pilot_Test_iteratedNu.csv", row.names = RN)


heatmap(MatNu)
