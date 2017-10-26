## Code requires sample names to be marked as 'Tumor' or 'Cancer' and arranged in an ascending order

require(matrixStats)
require(stats)

input = read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/GSE6631_Head&NeckSq.csv", row.names = 1)
MatA = data.matrix(input)

nml_str = "Normal"
tum_str = "Cancer"


Paired_Stat = function(MatA){
  nml_idx = grep(nml_str,colnames(MatA))
  Mat_Nml = MatA[ ,nml_idx]
  Mat_Tum = MatA[ , -nml_idx]
  
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
}


## Convert to p_values

FDR_P = Mat_P = matrix(NA,n,s)

for (i in 1:n){
  for (j in 1:s){
    if (MatNu[i,j] <= mean_dist){
      Mat_P[i,j] = pnorm(MatNu[i,j],mean_dist,sd_dist)
    }else {
      Mat_P[i,j] = 1 - pnorm(MatNu[i,j],mean_dist,sd_dist)
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

##could not really figure out a way to not have the third for-loop

#heatmap(MatNu)

#RN = rownames(input)
#write.csv(MatNu,"C:/Users/anous/Documents/R/BMI Research/Output/GSE6631_Head&NeckSq.csv", row.names = RN)




