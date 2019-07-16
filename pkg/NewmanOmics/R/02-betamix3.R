setClass("MixOf3Beta",
         slots=c(
           input = "numeric", # input data vector
           mle = "numeric",   # parameters: L and M
           psi = "numeric",   # proportions: alpha, beta, gamma
           Z = "matrix"))     # latent indicator variable

validBetaMix <- function(object) {
  (all(object@input >= 0) & all(object@input <= 1) &
   length(object@mle) == 2 &
   length(object@psi) == 3 &
   ncol(object@Z) == 3 &
   nrow(object@Z) == length(object@input))
}
setValidity("MixOf3Beta", validBetaMix)

setMethod("plot", signature = c("MixOf3Beta", "missing"), function(x, y, ...) {
  who <- apply(x@Z, 1, function(w) {sample(1:3, 1, prob=w)})
  opar <- par(mfrow=c(3,1))
  hist(x@input[who==1], breaks=57, main="Right", xlab="P")
  hist(x@input[who==2], breaks=57, main="Left", xlab="P")
  hist(x@input[who==3], breaks=57, main="Uniform", xlab="P")
  par(opar)

})

setMethod("hist", signature = "MixOf3Beta", function(x, lcol="red", breaks=101, ...) {
  tt <- seq(0, 1, length=501)
  dd <- dMix3(x, tt)
  hist(x@input, breaks=breaks, prob=TRUE, ...)
  lines(tt, dd, lwd=2, col=lcol)
})

setMethod("image", signature = "MixOf3Beta", function(x, ...) {
  image(1:nrow(x@Z), 1:3, x@Z[order(x@Z[,1], x@Z[,2]),],
        xlab='', ylab='', yaxt='n', ...)
  mtext(c("Right", "Left", "Uniform"), side=2, at=1:3, las=2, line=1)
})

### Assumes a mixture of three beta distributions
###      alpha*Beta(L,1) + beta*Beta(1, M) + gamma*Beta(1,1)
###       right-peak        left-peak         uniform

### Note that dbeta(x; L, 1) = L*x^(L-1)
###       and dbeta(x; 1, M) = M*(1-x)^(M-1)
###       and dbeta(x; 1, 1) = 1, is uniform on the interval [0,1].

## likelihood = P(q | L, M, z)
##     params = c(L, M)
##     q = observed data vector
##     z = latent indicator matrix
logLikelihood <- function(param, q, z) {
  L <- param[1]
  M <- param[2]
  N <- length(q)
  like <- z[,1]*L*q^(L-1) + z[,2]*M*(1-q)^(M-1) + z[,3]
  -sum(log(like)) # negate because 'nlm' finds minima instead of maxima
}

## datavec is the vector of observed data, assumed to be between 0 and 1
fitMix3 <- function(datavec, forever=100, epsilon=0.001, print.level=0) {
  if (any(is.na(datavec))) {
    warning("Data contains missing values; we are ignoring them.")
    datavec <- datavec[!is.na(datavec)]
  }
  if (any(datavec > 1) | any (datavec < 0)) {
    stop("Data values must be in the inteval [0,1].")
  }
  ## initialize cluster assignments
  N <- length(datavec)
  Z <- matrix(0, nrow=N, ncol=3) # binary cluster assignment indicators
  temp <- data.frame(dbeta(datavec, 8, 1), # right peak beta
                     dbeta(datavec, 1, 8), # left peak beta
                     dbeta(datavec, 1, 1)) # uniform
  who <- apply(temp, 1, function(x) {sample(1:3, 1, prob=x)})
  Z[who == 1, 1] <- 1
  Z[who == 2, 2] <- 1
  Z[who == 3, 3] <- 1
  ## loop control
  lastlike <- -10^5
  currlike <- 0
  count <- 0
  mle <- c(2,2)
  while ((count < forever) & (abs(lastlike - currlike) > epsilon)) {
    count = count + 1
    ## M-step
    maxlike <- nlm(logLikelihood, mle, q=datavec, z=Z, stepmax=10000, print.level=print.level)
    mle <- maxlike$estimate
    lastlike <- currlike
    currlike <- maxlike$minimum
    if (any(mle < 0)) stop("One of the mle's is negative!")
    ## E-step
    psi <- apply(Z, 2, mean) # tentative proportions of mixture components
    Z[,1] <- psi[1]*dbeta(datavec, mle[1], 1)
    Z[,2] <- psi[2]*dbeta(datavec, 1, mle[2])
    Z[,3] <- psi[3]
    Z <- sweep(Z, 1, apply(Z, 1, sum), "/")
    if (print.level > 0) {
      print("MLE (L,M) =")
      print(mle)
      print("Psi (p1, p2, p3) =")
      print(psi)
      }
  }
  new("MixOf3Beta",
      input = datavec,
      mle = mle,
      psi = psi,
      Z = Z)
}

# the density function for the fitted distribution
dMix3 <- function(object, x) {
  psi <- object@psi
  L <- object@mle[1]
  M <- object@mle[2]
  psi[1]*L*x^(L-1) + psi[2]*M*(1-x)^(M-1) + psi[3]
}

# the cumulative probability function for the fitted distribution
# should be
#          alpha * x^L  +  beta * (1 - (1 - x)^M)  +  gamma * x
pMix3 <- function(object, x) {
  psi <- object@psi
  L <- object@mle[1]
  M <- object@mle[2]
  psi[1]*x^(L) + psi[2]*(1 - (1-x)^M) + psi[3]*x
}

# compute FDR given cutoff on nominal p-value
# note consistency with above description of CDF
computeFDR <- function(object, alpha) {
  mle <- object@mle
  psi <- object@psi
  A <- psi[2] * (1 - (1 - alpha)^mle[2])
  B <- psi[1] * alpha^mle[1] + psi[3] * alpha
  B/(A+B)
}

# compute cutoff on nominal p-value given desired FDR
computeCutoff <- function(object, fdr) {
  f <- function(x) abs(computeFDR(object, x) - fdr)
  optimize(f, c(0,1))
}

