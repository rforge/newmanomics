
pairedStat <- function(baseData, perturbedData = NULL, pairing = NULL){

  if (is.list(baseData)) {
    x <- baseData
    baseData <- do.call(cbind, lapply(x, function(entry) {entry[,1]}))
    perturbedData <- do.call(cbind, lapply(x, function(entry) {entry[,2]}))
    rm(x)
  } else if (is.null(perturbedData)) {
    if (is.null(pairing)) {
      stop("You must supply at least one of 'perturbedData' or 'pairing'.")
    }
    pos <- which(pairing > 0)
    pos <- pos[order(pairing[pos])]
    neg <- which(pairing < 0)
    neg <- neg[order(pairing[order(neg)])]  
    x <- baseData
    baseData <- x[,neg]
    perturbedData <- x[,pos]
    rm(x, pos, neg)
  }
  
  ## KRC: Do we need to check that the two matrices are the same size?
  ## Or just let the first computation throw its own error?

  ## Matrix computation of mean of two things
  temp.means <- (baseData + perturbedData) / 2
  ## Similar computation for SD of two things.
  temp.sd <- abs(baseData - perturbedData) / sqrt(2)
  ## For each column, perform loess fit
  n <- dim(baseData)[1]
  s <- dim(baseData)[2]
  MatsdEst <- matrix(NA, n, s) # set aside storage
  for (i in 1:s) {
    l.mod <- loess(temp.sd[ ,i] ~ temp.means[ ,i])
    MatsdEst[ ,i] <- predict(l.mod)
  }

  ## compute the matrix of nu-statistics
  matNu <- abs(baseData - perturbedData) / MatsdEst

  ## empirical p-values via simulation
  m <- mean(matNu)
  sd <- sd(matNu)
  randNu <- randNuGen(m, sd)
  pValsPaired <- nu2PValPaired(matNu, as.vector(randNu))

  ## KRC: should we make this a proper object, or just leave it a list?
  return(list(nu.statistics=matNu, p.values=pValsPaired))
}

### Generating 1 million Nu values based on the overall mean and std deviation
### of the Nu values obtained from the paired statistic. This will later be
### used to estimate the p-values.
randNuGen <- function(mu=0, sigma=1) {
  ## magic numbers:  ngenes = 10000, ntimes = 100
  A <- matrix(rnorm(10000*100, mu, sigma), ncol=100)
  B <- matrix(rnorm(10000*100, mu, sigma), ncol=100)
  sdest <- mean( abs(A-B)/sqrt(2) )
  abs(A-B)/sdest
}

### originally written by Chao Liu on stackoverflow at
### https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
NearestValueSearch <- function(x, w){
  ## A simple binary search algorithm
  ## Assume the w vector is sorted so we can use binary search
  left <- 1
  right <- length(w)
  while(right - left > 1){
    middle <- floor((left + right) / 2)
    if(x < w[middle]){
      right <- middle
    }
    else{
      left <- middle
    }
  }
  if(abs(x - w[right]) < abs(x - w[left])){
    return(right)
  }
  else{
    return(left)
  }
}

nu2PValPaired <- function(nuMatrix, vec){
  vec <- sort(vec)
  MatP <- matrix(sapply(nuMatrix, function(x) {
    1 - NearestValueSearch(x, vec)/length(vec)
  }), nrow(nuMatrix), ncol(nuMatrix))
  return(MatP)
}
