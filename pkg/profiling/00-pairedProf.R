library(NewmanOmics)

csvfile <- system.file("extdata", "LungNormalTumorPair.csv",
                       package="NewmanOmics")

lung <- read.csv(csvfile)
summary(lung)

set.seed(12345)
Rprof("myProile.out")
for (ignore in 1:10) {
  picked <- sample(nrow(lung), 1000)
  normal <- log(1 + as.matrix(lung[picked, 2, drop=FALSE]))
  tumor  <- log(1 + as.matrix(lung[picked, 3, drop=FALSE]))
  ps <- pairedStat(normal, tumor)
}
Rprof()
summaryRprof("myProile.out") # all tie is spent inside loess

altmeth <- function(mu = 0, sigma = 1) {
  A <- matrix(rnorm(10000*100, mu, sigma), ncol=100)
  B <- matrix(rnorm(10000*100, mu, sigma), ncol=100)
  sdest <- mean( abs(A-B)/sqrt(2) )
  abs(A-B)/sdest
}

foo <- function(X, Y, ...) {
  q <- c(0.25, 0.75)
  y <- quantile(Y, q)
  x <- quantile(X, q)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  abline(int, slope, ...)
}

oneway <- NewmanOmics:::randNuGen(0,1)
twoway <- altmeth(0, 1)
par(ask=T)
for (i in 1:100) {
  qqplot(oneway[,i], twoway[,i])
  foo(oneway[,i], twoway[,i])
  abline(0,1, col='red')
}
par(ask=F)

N <- 2
barf <- matrix(rnorm(5000*4), ncol=4)
sad <- apply(barf, 1, sd)
summary(sad)
hist(sad, breaks=123)
