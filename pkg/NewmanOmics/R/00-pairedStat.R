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

  #### In case you want to log normalize the dataset before analysis. Do so either by this method (recommended) or a simple log normalization as mentioned in the banked version
  # X = median(normalMat)
  # NX = (0.05/0.95)*X
  # normalMat = log(normalMat+NX,10)
  #
  # Y = median(tumorMat)
  # NY = (0.05/0.95)*X
  # tumorMat = log(tumorMat+NX,10)

  n <- dim(normalMat)[1]
  s <- dim(normalMat)[2]

  MatsdEst = matrix(0,n,s)
  matNu = matrix(0,n,s)

  temp.means <- (normalMat + tumorMat) / 2
  temp.sd <- abs(normalMat - tumorMat) / sqrt(2)
  for (i in 1:s){
    l.mod <- loess(temp.sd[ ,i] ~ temp.means[ ,i])
    MatsdEst[ ,i] <- predict(l.mod)
  }

  matNu <- abs(normalMat - tumorMat) / MatsdEst

  m <- mean(matNu)
  sd <- sd(matNu)

  randNu <- randNuGen(m,sd)

  v <- as.vector(randNu)

  pValsPaired <- nu2PValPaired(matNu,v,n,s)

  return(list(nu.statistics=randNu, p.values=pValsPaired))
}
randNuGen <- function(mu=0, sigma=1) {
  ## magic numbers:  ngenes = 10000, ntimes = 100
  nuValMat <- matrix(NA, 10000, 100) 

  for (i in 1:100){
    vec1 <- rnorm(10000, mu, sigma)
    vec2 <- rnorm(10000, mu, sigma)
    means <- (vec1 + vec2)/2
    sds <- abs(vec1 - vec2)/sqrt(2)
    l.mod <- loess(sds ~ means)
    SdEst <- predict(l.mod)
    nuValMat[,] <- abs(vec1 - vec2)/SdEst
  }

  return(nuValMat)
} ## Generating 1 million Nu values based on the overall mean and std deviation of the Nu values obtained from the paired statistic. This will later be used to estimate the p_values

nu2PValPaired <- function(matrix,vec,n,s){
  MatP <- matrix(NA,n,s)
  for (i in 1:n){
    for (j in 1:s){
      MatP[i,j] <- length(vec[vec <= matrix[i,j]])/1000000
    }
  }
  return(MatP)
}

