# BANKED

roam <- function(M) {
  as.vector(matrixMean(M))
}

stand <- function(M, mu) {
  sqrt(as.vector(matrixVar(M, mu)))
}

### INTERNAL
### Get the background distribution of "unusual" values in the bank
nuValsIterBank <- function(matrix) {
  n <- nrow(matrix)
  s <- ncol(matrix)
  temp.means = temp.sd = matsdEst = matNu = matrix(NA,n,s)

  for (j in 1:s){
    temp.means[ ,j] <- roam(matrix[ ,-j])
    temp.sd[ ,j] <- stand(matrix[ ,-j], temp.means[,j])
    l.mod <- loess(temp.sd[ ,j] ~ temp.means[ ,j])
    matsdEst[ ,j] <- predict(l.mod)
  }
  matNu <- (matrix - temp.means)/matsdEst
  return(matNu)
}

### EXTERNAL
### prepare a "bank" of normal samples against which to test an individual
createBank <- function(bankMatrix) {
  ## warn if data appears to be on the raw scale
  if (diff(range(bankMatrix)) > 100) {
    warning("createBank: Data appears to be on the raw scale and not on a log scale.")
  }
  ## Compute mean and standard deviation
  bankMeans  <- roam(bankMatrix)
  bankSD <- stand(bankMatrix, bankMeans)
  ## Fit a loess model, since we assume SD is a smooth function of the mean
  loess.model <- loess(bankSD ~ bankMeans)
  sdEst <- predict(loess.model)
  ## bundle the results
  rowStats <- data.frame(mean=bankMeans, SD=bankSD, sdEst=sdEst)
  ##
  background <- nuValsIterBank(bankMatrix)
  ## return a list
  list(rowStats = rowStats, background = background)
}

### EXTERNAL
### Primary interface; compute the actual Newman banked statistic
bankStat <- function(bankObj, testSet, bankMatrix){
  n <- dim(testSet)[1]
  s <- dim(testSet)[2]

  if(missing(bankObj)){
    bankObj <- createBank(bankMatrix)
  } else if (!missing(bankMatrix)) {
    stop("You should only supply one of 'bankObj' or 'bankMatrix'.")
  }
  temp.means <- bankObj$rowStats$mean
  sdEst <- bankObj$rowStats$sdEst
  Back_dist_iter <- bankObj$background

  matNuBank <- nuValsBank(matrix = testSet, mean = temp.means, sd = sdEst)

  matPBanked <- pValuesBanked(matrix = matNuBank, Background = Back_dist_iter, n, s)

  print("P vals done")

  return(list(nu.statistics = matNuBank, p.values = matPBanked))

}
### If an input is missing, for example the bankObj, run your code as
###       pvals_uncorr = bankStat(,testSet,bankMatrix)

nuValsBank<- function(matrix, mean, sd){
  centered <- sweep(matrix, 1, mean, "-")
  scaled <- sweep(centered, 1, sd, '/')
  return(scaled)
}

pValuesBanked <- function(matrix, Background, n, s){


  meanDist = mean(Background)
  sdDist = sd(Background)

  matP = matrix(NA,n,s)

  for (i in 1:n){
    for (j in 1:s){
      matP[i,j] <- pnorm(matrix[i,j],meanDist,sdDist)
    }
  }
  return(matP)
}

## If pvals_uncorr is your output, fdr gives you a matrix of corrected p values

# v <- as.vector(pvals_uncorr)
#
# BH_vec = p.adjust(v, method = 'fdr')
# fdr =  as.matrix(BH_vec)
# dim(fdr) = c(n,s)


### The following four functions were written in an effort to find p values efficiently. Even though they work for smaller sets, the run time is significantly less for the built in FDR function ###
altPValues <- function(matrix,Background,n,s){
  matP <- matrix(NA,n,s)
  v = as.vector(Background)
  vec = v[!duplicated(v)]

  for (i in 1:n){
    for (j in 1:s){
      matP[i,j] <- length(vec[vec <= matrix[i,j]])/length(vec)
    }
  }
  return(matP)
}

empiricalP <- function(matrix,background,n,s){
  pdf = approxfun(density(background))
  min = min(background)

  pf = function(q){integrate(pdf,min,q,subdivisions = 2000)$value}


  empirMatP = matrix(NA,n,s)

  for (i in 1:n){
    for (j in 1:s){
      if (matrix[i,j] >= min){empirMatP[i,j] = pf(matrix[i,j])}
      else {empirMatP[i,j] = 0.04}
    }
  }
  return(empirMatP)
}

fdrCorrection <- function(matrix,n,s){

  ## Benjamini Hochberg method employed. Could also use built in p.adjust(p, "BH")
  ## Percent correction only factors in during analysis, b/c (rank/total)*percentageFDR only gives the adjusted cutoff


  v <- as.vector(matrix)
  vUnique <- v[!duplicated(v)]
  vs <- sort(vUnique,decreasing = T)
  vRank <- rank(vs)
  num <- max(vRank)

  adjustedP <- length(vUnique)
  adjustedP[1] <- vs[1]

  ##### if statement

  # breakPt = floor(0.9*num)
  # for (i in 1:breakPt){adjustedP[i] = 0.499}
  #
  # for (i in breakPt+1:num){
  #   adjustedP[i] <- min(adjustedP[i-1],(vs[i]*num)/vRank[i])
  #   if(i%%((num-breakPt)/10) == 0)print((i/(num-breakPt))*100)
  # }
  #

  for (i in 1:num){
    adjustedP[i] <- min(adjustedP[i-1],(vs[i]*num)/vRank[i])
    if(i%%(num/10) == 0)print((i/num)*100)
  }

  fdrP <- matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      fdrP[i,j] <- adjustedP[match(matrix[i,j],vUnique)]
    }
  }
  return(fdrP)
}

fdrCutoff <- function(matrix,alpha){

  v <- as.vector(matrix)
  w <- sort(v)
  threshold <- alpha*(1:length(v))/length(v)
  c <- cbind(v,threshold,v<=threshold)

  sig_value <- c[,1][c[,3] == 1]
  cutoff <- max(sig_value)

  return(cutoff)


  ## If we want to input a cutoff then use this method, else use FDR correction on p-values
  # sigP <- fdrCutoff(matPBanked,alpha = 0.05)
  # binarySigBank <- matrix(0,n,s)
  #
  # for (i in 1:n){
  #   for (j in 1:s){
  #     if (matPBanked[i,j] <= sigP){binarySigBank[i,j] = 1}
  #   }
  #   if(i%%(n/10) == 0)print((i/n)*100)
  # }
}
#####################


