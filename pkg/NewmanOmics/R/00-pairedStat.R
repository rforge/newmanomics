setClass("NewmanPaired",
         slots = c(
           nu.statistics = "matrix",
           p.values = "matrix",
           pairedMean = "matrix",
           difference = "matrix",
           smoothSD = "matrix")
)

validNewmanPair <- function(object) {
  all((dim(object@nu.statistics) == dim(object@p.values))
      & (dim(object@nu.statistics) == dim(object@pairedMean))
      & (dim(object@nu.statistics) == dim(object@difference))
      & (dim(object@nu.statistics) == dim(object@smoothSD)))
}
setValidity("NewmanPaired", validNewmanPair)

setMethod("[", signature = "NewmanPaired", function(x, i, j, ..., drop=FALSE) {
  new("NewmanPaired",
      nu.statistics = x@nu.statistics[i,j, drop=FALSE],
      p.values = x@p.values[i,j, drop=FALSE],
      pairedMean = x@pairedMean[i,j, drop=FALSE],
      difference = x@difference[i,j, drop=FALSE],
      smoothSD = x@smoothSD[i,j, drop=FALSE])
})

setMethod("dim", signature = "NewmanPaired", function(x) {
  dim(x@nu.statistics)
})

setMethod("plot", signature = c("NewmanPaired", "missing"),
          function(x, y, which = NULL, ask = NULL,
                   high=0.99, low=0.01, colset=c("red", "blue", "orange"), ...) {
  M <- dim(x)[2]
  if (is.null(which)) {
    which <- 1:M
  }
  if (any(which < 1) || any(which > M)) {
    stop("'which' must be between", 1, "and", M, "\n")
  }
  if (is.null(ask)) {
    ask <- prod(par("mfcol")) < length(which) && dev.interactive()
  }
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  for (W in which) {
    X <- x[,W]
    bigp <- X@p.values > 0.99
    smallp <- X@p.values < 0.01
    plot(X@pairedMean, X@difference,
         main=colnames(X@pairedMean),
         xlab="Mean log expression", ylab="Difference in log expression")
    points(X@pairedMean[smallp], X@difference[smallp], col=colset[1], pch=16)
    points(X@pairedMean[bigp], X@difference[bigp], col=colset[2], pch=16)
    points(X@pairedMean, X@smoothSD, col=colset[3])
    points(X@pairedMean, -X@smoothSD, col=colset[3])
    legend("topleft",
           c(paste("P <", round(low, 3)),
             paste("P >", round(high, 3)),
             "Smoothed SD"),
           col=colset, pch=16)
  }
  invisible(x)
})

setMethod("hist", signature = "NewmanPaired", function(x, breaks=101, xlab="P-value", ...) {
  if (dim(x)[2] > 1) {
    warning("Multiple pairs in 'x'; only showing the first one.")
    x <- x[,1]
  }
  hist(x@p.values, breaks=breaks, xlab=xlab, ...)
})

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
    neg <- neg[order(abs(pairing[neg]))]  
    x <- baseData
    baseData <- x[,neg,drop=FALSE] # 'drop' in case there is only one pair
    perturbedData <- x[,pos, drop=FALSE]
    rm(x, pos, neg)
  }
  
  ## KRC: Do we need to check that the two matrices are the same size?
  ## Or just let the first computation throw its own error?

  ## Matrix computation of mean of two things
  pairedMean <- (baseData + perturbedData) / 2
  ## Similar computation for SD of two things.
  pooledSD <- abs(baseData - perturbedData) / sqrt(2)
  colnames(pairedMean) <- colnames(pooledSD) <- colnames(perturbedData)
  ## For each column, perform loess fit
  n <- dim(baseData)[1]
  s <- dim(baseData)[2]
  smoothSD <- matrix(NA, n, s) # set aside storage
  for (i in 1:s) {
    l.mod <- loess(pooledSD[ ,i] ~ pairedMean[ ,i])
    smoothSD[ ,i] <- predict(l.mod)
  }
  colnames(smoothSD) <- colnames(pairedMean)
  
  ## compute the matrix of nu-statistics
  ## KRC: Why is there an absolute value?
  matNu <- abs(baseData - perturbedData) / smoothSD
  colnames(matNu) <- colnames(pairedMean)

  ## empirical p-values via simulation
  m <- mean(matNu)
  sd <- sd(matNu)
  randNu <- randNuGen(m, sd)
  pValsPaired <- nu2PValPaired(matNu, as.vector(randNu))
  colnames(pValsPaired) <- colnames(pairedMean)

  new("NewmanPaired",
      nu.statistics = matNu,
      p.values = pValsPaired,
      pairedMean = pairedMean,
      difference = perturbedData - baseData,
      smoothSD = smoothSD)
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
