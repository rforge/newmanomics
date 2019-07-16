\name{MixOf3Beta-class}
\alias{MixOf3Beta}
\alias{MixOf3Beta-class}
\alias{plot,MixOf3Beta,missing-method}
\alias{hist,MixOf3Beta-method}
\alias{image,MixOf3Beta-method}
\docType{class}
\title{Class "MixOf3Beta"}
\description{
  Represents the results of fitting a beta-mixture model to a set of
  p-values that has peaks at both zero and one.
}
\details{
  Given a set of p-values (or any data on the interval [0,1]) that has
  peaks at both ends of the interval, we fit a three-componet mixture
  model. One component is uniform, and represents the expected
  distribution under the null hypothesis that nothing interesting is
  happening anywhere. The second component has the distribution
  Beta(1,M); this has a peak at zero and represents the features of
  interest. The final component has the distribution Beta(L,1). In the
  context of the Newman paired statistic, this represents genes or
  features whose variabilirt is smaller than the locally smoothed
  estimate of the standard deviation; we can think of these as
  "extra boring".
}
\section{Creating Objects}{
  In practice, users will use the \code{\link{fitMix3}} function to
  construct an object of the \code{MixOf3Beta} class. Hand
  construction is strongly discouraged.
}
\section{Slots}{
  \describe{
    \item{\code{input}:}{A numeric vector containing the input p-values.}
    \item{\code{mle}:}{ A numeric vactor of length 2 containing the bea
      parameters \code{L} and {M} (in that order).}
    \item{\code{psi}:}{A numeric vector of length three containing the
      mixing parameters, in the order (right-peak component, left-peak
      component, and uniform-component).}
    \item{\code{Z}:}{A matrix of size N (number of features) by 3. This
      contains the latent indicator matrix. Each row corresponds to a
      gene or feature, and the entries show the proabbiltiy that the
      feature arose from the right, left, or uniform comnponent.}
  }
}
\section{Methods}{
  \describe{
    \item{plot(x, y, ...)}{Plot the decompositon of the data into thre pieces.}
    \item{hist(x, lcol = "red", breaks=101, ...)}{Plot a histogram of
      the p-values along with the fitted model of the distribution.}
    \item{image(x)}{Plot a (sorted) image of the latent variable Z-matrix.}
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
  \code{\link{pairedStat}}, \code{\link{NewmanPaired-class}}
}
\examples{
set.seed(98765)
ds <- c(rbeta(3000, 20, 1),
        rbeta(1000, 1, 40),
        runif(6000))
fit <- fitMix3(ds)
image(fit, col=topo.colors(64))
hist(fit, col="skyblue", lcol="blue")
plot(fit)
}
\keyword{classes}
\keyword{htest}
\keyword{multivariate}


