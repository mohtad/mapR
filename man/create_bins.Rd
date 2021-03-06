% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_bins.R
\name{create_bins}
\alias{create_bins}
\title{Create Bins from a vector of numbers.}
\usage{
create_bins(x, nbins, bins = NULL)
}
\arguments{
\item{x}{Vector of number.}

\item{nbins}{Number of bins to be generated. If 'bins' parameter is provided value, nbins is not used.}

\item{bins}{List containing the breaks intervals and the bins' labels. If 'bins' parameter is not provided value, nbins is used and bins are automatically generated. Bins contain two vectors: breaks and labels.}
}
\value{
vector of bins as string associated to x numbers.
}
\description{
Create Bins from a vector of numbers.
}
\examples{
#auto generate the bins
data<-data.frame(x =floor(exp(rnorm(200000 * 1.3))))
data$bin <- create_bins(data$x, nbins=6)

#pre-define the bins
data<-data.frame(x =floor(exp(rnorm(200000 * 1.3))))
data$bin <- create_bins(data$x, nbins=NULL, bins=list(breaks=c(-1,0,1,35,61,92,130), labels=c("0","1","2 to 35","36 to 61","62 to 92","93 to 130")))

}
