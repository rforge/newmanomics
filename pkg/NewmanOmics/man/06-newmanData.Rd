\name{newman-data}
\alias{newman-data}
\alias{LungPair}
\alias{GSE6631}
\docType{data}
\title{Datasets to Illustrate the Newman Tests}
\description{
  These data sets contain paired normal and tumor samples usewd to
  ilustrate te Newman pairted test and the Newman bank test. 
}
\usage{
data(LungPair)
data(GSE6631)
}
\format{\code{LungPair} is a data matrix containing normalized second
  generation sequencing data from The Cancer Genome Atlas (TCGA), with
  20,531 row (genes) and 2 columns (samples). The first column contains
  data for the normal sample and the second column contains data for the
  tumor sample from the patient with barcode TCGA.38.4625.

  \code{GSE6631} is a data matrix containing normalized Affymetrix
  microarray data from paired head-and-neck cancer samples in the Gene
  Expression Omnibus set GSE6631. The matrix contains 200 rows (a rando
  subset of genes) and 44 columns (samples). The odd numbered colukmns
  are derived from normal mucosa; te even numbered columns are derived
  from paired tumor samples from the same patient.
}
\source{
  The full squamous cell lung cancer (LUSC) data from TCGA was
  downloaded from   \url{http://firebrowse.org/}, and the data for this
  pair were separated and saved as a binary R data file. The
  head-and-neck cancer data were downloaded from the Gene Expression
  Omnibus at 
  \url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE6631}. A
  subset of 2000 genes was randomly selected before saving the binary R
  data file.
}
\references{
  Kuriakose MA, Chen WT, He ZM, Sikora AG et al. Selection and
  validation of differentially expressed genes in head and neck
  cancer. Cell Mol Life Sci 2004 Jun;61(11):1372-83. 
}
\examples{
data(LungPair)
data(GSE6631)
}
\keyword{datasets}

