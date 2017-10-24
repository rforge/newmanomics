## Code requires samples in file to be marked as 'Tumor' or 'cancer' and arranged in ascending order

require(matrixStats)
require(stats)

input = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/Bank_Testing.csv", row.names = 1)
MatA = data.matrix(input)

nml_str = "Normal"
tum_str = "Cancer"

Bank_Stat = function(MatA){
  bank_idx = grep(nml_str,colnames(MatA))
  Bank = MatA[ ,bank_idx]
  
  n = dim(Bank)[1]
  s = dim(MatA)[2] - dim(Bank)[2]
  
  Get_Means(Bank)
  NuVals_Bank(matrix = MatA,mean = temp.means, sd = sdEst)
  
}

NuVals_Bank = function(matrix,mean,sd){
  MatNu = matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      MatNu[i,j] <<- abs((MatA[i,j]) - (temp.means[i]))/sdEst[i]
    }
  }
}

#temp.means = unname(rowMeans(Bank))
#temp.sd = rowSds(Bank)

#l.mod = loess(temp.sd ~ temp.means)
#sdEst = predict(l.mod)

CN = paste(colnames(MatA)[-bank_idx],"_NormalBank")
RN = rownames(input)
           
write.csv(MatNu,"C:/Users/anous/Documents/R/BMI Research/Output/BankedTest_Nu.csv", row.names = RN)

heatmap(MatNu)

## optimally adjusted FDR
PValue_FDR = function(MatNu){
  FDR_P = Mat_P = matrix(NA,n,s)
  L_MatNu = log(MatNu,2)
  
  mean_dist = mean(L_MatNu)
  sd_dist = sd(L_MatNu)
  
  for (i in 1:n){
    for (j in 1:s){
      if (L_MatNu[i,j] <= mean_dist){
        Mat_P[i,j] = pnorm(L_MatNu[i,j],mean_dist,sd_dist)
      }else {
        Mat_P[i,j] = 1 - pnorm(L_MatNu[i,j],mean_dist,sd_dist)
      }
    }
  }
  
  v = as.vector(Mat_P)
  v_unique = v[!duplicated(v)]
  vs = sort(v_unique,decreasing = T)
  v_rank = rank(vs)
  num = max(v_rank)
  
  adjusted_p = dim(v_unique)
  adjusted_p[1] = vs[1]
  
  for (i in 1:num){
    adjusted_p[i] = min(adjusted_p[i-1],(vs[i]*num)/v_rank[i])
  }
  
  
  for (i in 1:n){
    for (j in 1:s){
      FDR_P[i,j] = adjusted_p[match(Mat_P[i,j],v_unique)]
    }
  }
  FDR_Final <<- FDR_P
}
