\name{fitMix3}
\alias{fitMix3}
\alias{computeFDR}
\alias{computeCutoff}
\title{Compute FDR from Three-Component Beta Mixture}
\description{
  Provides functions to fit a beta-mixture model to a set of p-values
  that has peaks at both zero and one, and to estimate false discovery
  rates.
}
\usage{
fitMix3(datavec, forever=100, epsilon=0.001, print.level=0)
computeFDR(object, x)
computeCutoff(object, fdr)
}
\details{
  We have observed empirically that the set of p-value sobtained when
  computing the Newman paired test statistic often has peaks both at zero
  (representing genes of interest) and at one (representing "boring"
  genes that change much less than expected). We attribute the latter
  phenomenonh to the fact that we use locally smoothed instead of
  gene-by-gene estimates of the standard deviation; genes whose SD is
  increased by the smoothing process contribute to the boring peak near
  one.

  To estimate p-values in this context, we fit a three-component beta
  mixture model, combining (1) a right-peaked distribution Beta(L,1),
  (2) a left-peaked dfistribution Beta(1,M), and (3) a uniform
  distribution. Specfically, we look for models of the form
  \deqn{alpha*Beta(L,1) + beta*Beta(1, M) + gamma*Beta(1,1)}.
  
  Model-fitting uses an expectation-maximization (EM) algorithm. In
  addition to the parameters \code{mle=c(L,M)} and \code{psi=c(alpha,
  beta, gamma)}, we introduce a matrix \code{Z} of latent variables that
  indicate which distribution each point is likely to arise
  form. \code{Z} has three columns (one for each mixture component) and
  one row for each p-value; the entries in each row are nonegative and
  sum to one. The M-step of the algorithm uses the \code{\link{nlm}}
  optimization function to compute the maximum-likelihood \code{mle}
  values given \code{psi} and \code{Z}. The E-step first updates
  \code{psi} from the \code{Z}-matrix, and then updates the values of
  \code{Z} based on the current \code{mle}. 
  
  We are able to use the mixture distribution to compute the
  relationship between a cutoff on the nominal p-values and the false
  discovery rate (FDR). 
}
\arguments{
\item{datavec}{A numeric vector containing p-values.}
\item{forever}{An integer; maximum number of iterations while fitting
  the mixture model.}
\item{epsilon}{A real number; change in the log likelihood that should
  be used to terminate the model-fitting loop.}
\item{print.level}{An integer; how much detail should \code{link{nlm}}
  print while fitting the model.}
\item{object}{An object of the \code{\link{MixOf3Beta}} class.}
\item{x}{A real number between 0 and 1; the cu}
\item{fdr}{A real number beteen 0 and 1; the targeted FDR value.}
}
\value{
  The model-fitting function, \code{fitMix3}, returns an object of the
  \code{\link{MixOf3Beta}} class.

  The \code{computeFDR} function returns a real number in [0,1], the
  false discovery rate assiociated with the nominal cutoff.

  The \code{computeCutoff} function returns a real number in [0,1], the
  cutoff required to achieve the desired FDR.
}
\examples{
set.seed(98765)
ds <- c(rbeta(3000, 20, 1),
        rbeta(1000, 1, 40),
        runif(6000))
fit <- fitMix3(ds)
computeFDR(fit, 0.01)
computeCutoff(fit, 0.01)
computeFDR(fit, 0.0016438)
computeCutoff(fit, 0.05)
computeFDR(fit, 0.00702114)
}
