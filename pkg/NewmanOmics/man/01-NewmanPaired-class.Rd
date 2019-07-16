\name{NewmanPaired-class}
\alias{NewmanPaired}
\alias{NewmanPaired-class}
\alias{plot,NewmanPaired,missing-method}
\alias{hist,NewmanPaired-method}
\alias{dim,NewmanPaired-method}
\alias{[,NewmanPaired-method}%] to make emacs more-or-less happy
\docType{class}
\title{Class "NewmanPaired"}
\description{
  Represents the reusults of computing the Newman Paired test statistic
  on one or more paired samples.
}
\section{Creating Objects}{
  In practice, users will use the \code{\link{pairedStat}} function to
  construct an object of the \code{NewmanPaired} class. Hand
  construction is strongly discouraged.
}
\section{Slots}{
  \describe{
    \item{\code{pairedMean}:}{A matrix of size N (number of features) by S (number
    of sample pairs). The mean expression of each feature in each paired
    sample. Also called "A" in the M-versus-A plots of the microarray era.}
    \item{\code{difference}:}{A matrix of size N (number of features) by S (number
    of sample pairs). The difference (perturbed - base) in expression of
    each feature in each paired sample. Also called "M" in the M-versus-A
    plots of the microarray era.}
    \item{\code{smoothSD}:}{A matrix of size N (number of features) by S (number
    of sample pairs). The results of fitting a loess smooth to the
    relationship between the \code{PairedMean} and the observed estimate
    of standard deviation (i.e., \code{abs(difference)/sqrt(2)}).}
    \item{\code{nuStatistics}:}{A matrix of size N (number of features) by S (number
    of sample pairs). The Newman paired statistics, nu.}
    \item{\code{pValues}:}{A matrix of size N (number of features) by S (number
    of sample pairs). Empirical p-values for the Newman statistics.}
  }
}
\section{Methods}{
  \describe{
    \item{x[i,j]}{Select a subset of features or sample pairs.}
    \item{dim(x)}{The dimension, N by S, of the object.}
    \item{plot(x, y, high=0.99, low=0.01, ...)}{Plot the results of the
      analysis of one sample pair.}
    \item{hist(x, breaks=101, xlab="P-value", ...)}{Plot a histogram of the p-values for one sample-pair.}
  }
}
\references{
  Abrams ZB, Joglekar A, Gershkowitz GR, Sinicropi-yao S, Asiaee A,
  Carbone DP, Coombes KR. Personalized Transcriptomics: Selecting Drugs
  Based on Gene Expression Profiles. Preprint.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com}
}
\seealso{
  \code{\link{pairedStat}}, \code{\link{bankStat}}
}
\examples{
showClass("NewmanPaired")
}
\keyword{classes}
\keyword{htest}
\keyword{multivariate}


