# global.r

# PAIRED

pilotStat = function(mat){
  RN = rownames(mat)
  
  n = dim(mat)[1]
  s = dim(mat)[2]
  
  temp.means  = unname(rowMeans(mat)) 
  temp.sd = rowSds(mat)
  
  loess.model = loess(temp.sd ~ temp.means)
  sdEst = predict(loess.model)
  
  nuValsPilot = nuValsAnova(matrix = mat, sd = sdEst, n)
  
  pValsPilot = nuToPValPilot(mat,n,s,temp.means,nuValsPilot)
  
  return(pValsPilot)
}

nuValsAnova = function(matrix,sd, n){
  nuVals = numeric(n)
  for (i in 1:n){
    nuVals[i] = (max(matrix[i, ]) - min(matrix[i, ]))/sd[i]
  }
  return(nuVals)
}

nuToPValPilot = function(matrix,n,s,mean, nuVals){
  wg = numeric(n)
  for (i in 1:n){
    wg[i] = sum(abs(matrix[i,] - mean[i])^2)/(s-2)
  }
  fStat = wg/nuVals
  pValsPilot = pf(fStat,1,1)
  
  return(pValsPilot)
}

finStep = function(pValsPilot, p, RN, mat1){
  binVec = c()
  
  for (i in 1:NROW(pValsPilot)){
    if (is.nan(pValsPilot[i])){
      pValsPilot[i] = 1
    }
  }
  
  for (i in 1:NROW(pValsPilot)){
    binVec[i] = (pValsPilot[i] < p)
  }
  
  v1 = c()
  v2 = c()
  j=1
  for (i in 1:length(binVec)){
    if (binVec[i] == "TRUE"){
      v1[j] <- RN[i]
      v2[j] <- i
      j=j+1
    }
  }
  
  sig_mat = mat1[c(v2),]
  heatmap(sig_mat)
}

# BANKED

# input1 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Output/bankObj_full.csv",row.names = 1)
# input2 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/LuAd_AllTumors.csv", row.names = 1)
# input3 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/LuAd_Normals.csv", row.names = 1)
# RN <- rownames(input2)
# 
# bankObj <- data.matrix(input1)
# testSet <- data.matrix(input2)
# bankMatrix <- data.matrix(input3)
# 
# ## Actual Statistic
# bankStat <- function(bankObj, testSet, bankMatrix){
#   
#   X = median(testSet)
#   Y = median(bankMatrix)
#   
#   NX = (0.05/0.95)*X
#   NY = (0.05/0.95)*Y
#   
#   testSet = log(testSet+NX,10)
#   bankMatrix = log(bankMatrix+NY,10)
#   
#   n <- dim(testSet)[1]
#   s <- dim(testSet)[2]
#   
#   if(missing(bankObj)){
#     sBank <- dim(bankMatrix)[2] 
#     
#     temp.means  <- unname(rowMeans(bankMatrix))
#     temp.sd <- rowSds(bankMatrix)
#     
#     loess.model <- loess(temp.sd ~ temp.means)
#     sdEst <- predict(loess.model)
#     
#     Back_dist_iter <<- nuValsIterBank(bankMatrix, n, sBank)
#     
#     bankObj <- as.matrix(c(temp.means,sdEst,Back_dist_iter))
#     dim(bankObj) <- c(n,sBank+2)
#     
#     
#     write.csv(bankObj,"C:/Users/anous/Documents/R/BMI Research/Output/bankObj.csv")
#   } else if(missing(bankMatrix)) {
#     sBank <- dim(bankObj)[2]-2
#     
#     temp.means <- bankObj[1,]
#     sdEst <- bankObj[2,]
#     Back_dist_iter <- bankObj[,3:(sBank+2)]
#     
#     
#   } else{print("error")}
#   
#   matNuBank <<- nuValsBank(matrix = testSet,mean = temp.means, sd = sdEst, n, s = s)
#   
#   matPBanked <- pValuesBanked(matrix = matNuBank, Background = Back_dist_iter, n, s)
#   
#   print("P vals done")
#   
#   
#   #fdrAdjustedBank <- fdrCorrection(matPBanked,n,s)
#   
#   write.csv(matPBanked,"C:/Users/anous/Documents/R/BMI Research/Output/LuAdFullBanked_uncorr.csv", row.names = F)
#   #return(binarySigBank)
#   #return(fdrAdjustedBank)
#   #return(Back_dist_iter)
#   #return(matNuBank)
#   return(matPBanked)
#   
# }
# 
# 
# nuValsBank <- function(matrix,mean,sd,n,s){
#   matNu <- matrix(0,n,s)
#   for (i in 1:n){
#     for (j in 1:s){
#       matNu[i,j] <- ((matrix[i,j]) - (mean[i]))/sd[i]
#     }
#   }
#   return(matNu)
# }
# 
# nuValsIterBank <- function(matrix,n,s){
#   temp.means = temp.sd = matsdEst = matNu = matrix(NA,n,s)
#   
#   for (j in 1:s){
#     temp.means[ ,j] <- rowMeans(matrix[ ,-j])
#     temp.sd[ ,j] <- rowSds(matrix[ ,-j])
#     l.mod <- loess(temp.sd[ ,j] ~ temp.means[ ,j])
#     matsdEst[ ,j] <- predict(l.mod)
#     for (k in 1:n){
#       matNu[k,j] <- (matrix[k,j] - temp.means[k,j])/matsdEst[k,j]
#     }
#   }
#   return(matNu)
# }
# 
# pValuesBanked <- function(matrix, Background, n, s){
#   
#   
#   meanDist = mean(Background)
#   sdDist = sd(Background)
#   
#   matP = matrix(NA,n,s)
#   
#   for (i in 1:n){
#     for (j in 1:s){
#       matP[i,j] <- pnorm(matrix[i,j],meanDist,sdDist)
#     }
#   }
#   return(matP)
# }
# 
# #fdrCorrection <- function(matrix,n,s){
# 
# ## Benjamini Hochberg method employed. Could also use built in p.adjust(p, "BH")
# ## Percent correction only factors in during analysis, b/c (rank/total)*percentageFDR only gives the adjusted cutoff
# 
# 
# v <- as.vector(matrix)
# vUnique <- v[!duplicated(v)]
# vs <- sort(vUnique,decreasing = T)
# vRank <- rank(vs)
# num <- max(vRank)
# 
# adjustedP <- length(vUnique)
# adjustedP[1] <- vs[1]
# 
# ##### if statement
# 
# # breakPt = floor(0.9*num)
# # for (i in 1:breakPt){adjustedP[i] = 0.499}
# # 
# # for (i in breakPt+1:num){
# #   adjustedP[i] <- min(adjustedP[i-1],(vs[i]*num)/vRank[i])
# #   if(i%%((num-breakPt)/10) == 0)print((i/(num-breakPt))*100)
# # }
# # 
# 
# for (i in 1:num){
#   adjustedP[i] <- min(adjustedP[i-1],(vs[i]*num)/vRank[i])
#   if(i%%(num/10) == 0)print((i/num)*100)
# }
# 
# fdrP <- matrix(NA,n,s){
# for (i in 1:n){
#   for (j in 1:s){
#     fdrP[i,j] <- adjustedP[match(matrix[i,j],vUnique)]
#   }
# }
# return(fdrP)
# }
# 
# #fdrCutoff <- function(matrix,alpha){
# 
# v <- as.vector(matrix)
# w <- sort(v)
# threshold <- alpha*(1:length(v))/length(v)
# c <- cbind(v,threshold,v<=threshold)
# 
# sig_value <- c[,1][c[,3] == 1]
# cutoff <- max(sig_value)
# 
# return(cutoff)
# 
# 
# ## If we want to input a cutoff then use this method, else use FDR correction on p-values
# # sigP <- fdrCutoff(matPBanked,alpha = 0.05)
# # binarySigBank <- matrix(0,n,s)
# # 
# # for (i in 1:n){
# #   for (j in 1:s){
# #     if (matPBanked[i,j] <= sigP){binarySigBank[i,j] = 1}
# #   }
# #   if(i%%(n/10) == 0)print((i/n)*100)
# # }
# # }
# 
# 
# require('tictoc')
# 
# v <- as.vector(bank_w_bank)
# 
# BH_vec = p.adjust(v, method = 'fdr')
# fdr =  as.matrix(BH_vec)
# dim(fdr) = c(n,s)
# 
# 
# count2 = 0
# binMat2 = matrix(NA,n,s)
# 
# for (i in 1:n){
#   for (j in 1:s){
#     if (pvalues[i,j] <= 0.05){
#       count2 = count2 + 1
#       binMat2[i,j] = 1}
#     else {binMat2[i,j] = 0}
#   }
# }
# 