\name{pairedStat}
\alias{pairedStat}
\title{Paired Statistic}
\usage{
pairedStat(normalMat, tumorMat)
}
\arguments{
\item{normalMat}{First set of samples}

\item{tumorMat}{Second set of samples}
}
\value{
A list containing two marices: the \code{nu.statistics} and the \code{p.values}.
}
\description{
Paired Statistic, used for a one to one comparison of individual samples to another set of similar individual samples
}
\examples{
# Datasets need to be arranged so that there is a one-once organizational
# correspondence between the normals and tumors/ before and after treatment patients
\dontrun{
input1 <- read.csv(file = "filepath", row.names = 1)
input2 <- read.csv(file = "filepath.csv", row.names = 1)
RN <- rownames(input2)


normalMat <- data.matrix(input1)
tumorMat <- data.matrix(input2)

pairedStat(normalMat, tumorMat)
}
}
