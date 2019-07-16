\name{bankStat}
\alias{bankStat}
\alias{createBank}
\title{Newman Banked Statistic}
\description{
The Newman Banked Statistic is used to compare an individual sample to a cohort of similar samples.
}
\usage{
bankStat(bankObj, testSet, bankMatrix)
createBank(bankMatrix)
}
\arguments{
\item{bankObj}{Compressed representation of the cohort being compared to.}

\item{testSet}{Matrix containing data from one or more individual
  samples to be compared to the bank.}

\item{bankMatrix}{Data for the bank of "normal" or "untreated"
   or "baseline" control samples.}
}
\value{
A list containing two matrices: the \code{nu.statistics} and the \code{p.values}.
}
\examples{
data(GSE6631)
HN <- as.matrix(log2(1 + GSE6631))
bankMatrix <- HN[,seq(1, ncol(HN), 2)] # odd columns are normal
testSet <- HN[, seq(2, 6, 2)]   # evn columns are tumor
bs <- bankStat(testSet = testSet, bankMatrix = bankMatrix)
summary(bs$nu.statistics)
summary(bs$p.values)
}
