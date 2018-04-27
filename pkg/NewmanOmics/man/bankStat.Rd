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
\dontrun{
# Bank Object consisting of row means, estimate of std deviation and background distribution
input1 <- read.csv(file = "filepath.csv",row.names = 1) 
# Dataset to be tested against the bank
input2 <- read.csv(file = "filepath.csv", row.names = 1)
# Dataset that comprises the bank
input3 <- read.csv(file = "filepath.csv", row.names = 1)

bankObj <- data.matrix(input1)
testSet <- data.matrix(input2)
bankMatrix <- data.matrix(input3)

bankStat(bankObj, testSet, bankMatrix)
}}
