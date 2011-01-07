\name{corr}
\alias{corr}
\title{
Function to Compute Correlations
}
\description{
computes correlations on complete cases only and prints a correlation matrix where all entries less than cutoff in absolute values are "erased"
}
\usage{
corr(x, rnd = 2, cutoff = 0.25)
}
\arguments{
  \item{x}{\code{x} is a dataframe or matrix}
  \item{rnd}{\code{rnd} number of decimal places to be displayed}
  \item{cutoff}{\code{cutoff} display only corr > cutoff is absolute value}
}
\details{
combine variables into a dataframe or matrix using data.frame(x,y) or cbind(x,y)
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}

\examples{
corr(cbind(rnorm(100),rnorm(100)))
}
\keyword{statistics}
\keyword{univariate}
