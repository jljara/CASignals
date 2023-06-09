% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalise.signal.R
\name{normalise.signal}
\alias{normalise.signal}
\title{Normalises a signal.}
\usage{
normalise.signal(signal, signal.baseline.value, signal.min.value)
}
\arguments{
\item{signal}{The original signal to be normalised}

\item{signal.baseline.value}{The signal's baseline value to be considered
in the normalization operation}

\item{signal.min.value}{The signal's minimum value to be considered
in the normalization operation}
}
\value{
a normalised version of the given signal (zero at the
    selected minimum value, and one at the baseline value)
}
\description{
Create a normalised version of a signal.
}
\examples{
normalise.signal(c(10, 8, 9, 11, 10, 11, 12, 10, 9, 8, 4, 5, 6, 7, 8), 10, 4)

}
