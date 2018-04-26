# BANKED

# input1 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Output/LuSc_BankObj.csv",row.names = 1)
#Bank Object consisting of row means, estimate of std deviation and background distribution
# input2 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/TrisomyPatient_19193.csv", row.names = 1) 
#Dataset to be tested against the bank
# input3 <- read.csv(file = "C:/Users/anous/Documents/R/BMI Research/Input/HapMap_209Patients.csv", row.names = 1) 
#Dataset that comprises the bank
# RN <- rownames(input2)
#
# bankObj <- data.matrix(input1)
# testSet <- data.matrix(input2)
# bankMatrix <- data.matrix(input3)

## Actual Statistic

## keeping the log bc it helps with nonNormal data
#  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4120293/

bankStat <- function(bankObj, testSet, bankMatrix){

  #### In case you want to log normalize the data before running the statistic
  # X = median(testSet)
  # NX = (0.05/0.95)*X
  # testSet = log(testSet+NX,10)
  ### testSet = log(testSet,2) ##If you want to do a simpler log transform


  n <- dim(testSet)[1]
  s <- dim(testSet)[2]

  if(missing(bankObj)){
    sBank <- dim(bankMatrix)[2]

    temp.means  <- unname(rowMeans(bankMatrix))
    temp.sd <- rowSds(bankMatrix)

    loess.model <- loess(temp.sd ~ temp.means)
    sdEst <- predict(loess.model)

    Back_dist_iter <<- nuValsIterBank(bankMatrix, n, sBank)

    bankObj <- as.matrix(c(temp.means,sdEst,Back_dist_iter))
    dim(bankObj) <- c(n,sBank+2)


    write.csv(bankObj,"C:/Users/anous/Documents/R/BMI Research/Output/18842_bankObj.csv") #Saves the bankObject for future use

  } else if(missing(bankMatrix)) {
    sBank <- dim(bankObj)[2]-2

    temp.means <- bankObj[,1]
    sdEst <- bankObj[,2]
    Back_dist_iter <- bankObj[,3:(sBank+2)]


  } else{print("error")}

  matNuBank <<- nuValsBank(matrix = testSet,mean = temp.means, sd = sdEst, n, s = s)
  #write.csv(matNuBank,"C:/Users/anous/Documents/R/BMI Research/Output/NuVals_18842.csv") ## Saves Nu values if you want to

  matPBanked <- pValuesBanked(matrix = matNuBank, Background = Back_dist_iter, n, s)

  print("P vals done")

  #write.csv(matPBanked,"C:/Users/anous/Documents/R/BMI Research/Output/18842_uncorrP.csv", row.names = F) ##Saves uncorrected P Values if you want it to

  return(list(nu.statistics = matNuBanked, p.values = matPBanked))

} ### If an input is missing, for example the bankObj, run your code as pvals_uncorr = bankStat(,testSet,bankMatrix)

nuValsBank <- function(matrix,mean,sd,n,s){
  matNu <- matrix(0,n,s)
  for (i in 1:n){
    for (j in 1:s){
      matNu[i,j] <- ((matrix[i,j]) - (mean[i]))/sd[i]
    }
  }
  return(matNu)
}

nuValsIterBank <- function(matrix,n,s){
  temp.means = temp.sd = matsdEst = matNu = matrix(NA,n,s)

  for (j in 1:s){
    temp.means[ ,j] <- rowMeans(matrix[ ,-j])
    temp.sd[ ,j] <- rowSds(matrix[ ,-j])
    l.mod <- loess(temp.sd[ ,j] ~ temp.means[ ,j])
    matsdEst[ ,j] <- predict(l.mod)
    for (k in 1:n){
      matNu[k,j] <- (matrix[k,j] - temp.means[k,j])/matsdEst[k,j]
    }
  }
  return(matNu)
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


