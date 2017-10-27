#' Paired Statistic, used for a one to one comparison of individual samples to another set of similar individual samples
#' @title Paired Statistic
#' @param normalMat First set of samples
#' @param tumorMat Second set of samples
#' @return A list containing two matrices: the \code{nu.statistics} and the \code{p.values}.
#' @examples #Datasets need to be arranged so that there is a one-one organizational
#' correspondence between the normals and tumors/ before and after treatment patients.
#' input1 <- read.csv(file = "filepath", row.names = 1)
#' input2 <- read.csv(file = "filepath.csv", row.names = 1)
#' RN <- rownames(input2)
#'
#'
#' normalMat <- data.matrix(input1)
#' tumorMat <- data.matrix(input2)
#'
#' pairedStat(normalMat, tumorMat)
#'
#' @export
pairedStat <- function(normalMat, tumorMat){
  ## KRC: Why is this advice buried here? Doesn't it belong somewhere
  ## in the user's docuemntation, like man pages or vignette?
  ##
  ## In case you want to log normalize the dataset before analysis.
  ## Do so either by this method (recommended) or a simple log normalization
  ## as mentioned in the banked version
  # X = median(normalMat)
  # NX = (0.05/0.95)*X
  # normalMat = log(normalMat+NX,10)
  #
  # Y = median(tumorMat)
  # NY = (0.05/0.95)*X
  # tumorMat = log(tumorMat+NX,10)

  ## KRC: Do we need to check that the two matrices are the same size?
  ## Or just let the first computation throw its own error?

  ## Matrix computation of mean of two things
  temp.means <- (normalMat + tumorMat) / 2
  ## Similar comput2tion for SD of two things.
  temp.sd <- abs(normalMat - tumorMat) / sqrt(2)
  ## For each column, perform loess fit
  n <- dim(normalMat)[1]
  s <- dim(normalMat)[2]
  MatsdEst <- matrix(NA, n, s) # set aside storage
  for (i in 1:s) {
    l.mod <- loess(temp.sd[ ,i] ~ temp.means[ ,i])
    MatsdEst[ ,i] <- predict(l.mod)
  }

  ## compute the matrix of nu-statistcis
  matNu <- abs(normalMat - tumorMat) / MatsdEst

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

nu2PValPaired <- function(nuMatrix, vec){
  MatP <- matrix(sapply(nuMatrix, function(x) mean(x < vec)),
                 nrow(nuMatrix), ncol(nuMatrix))
  return(MatP)
}
