\name{normPlot}
\alias{normPlot}
\title{
Normal Probability Plot
}
\description{
computes normal prob plot and includes line thru actual and expected 1st and 3 quartiles. Also computes Anderson Darling statistic and displays p-value.
}
\usage{
normPlot(x)
}
\arguments{
  \item{x}{must be a vector}
}
\value{
  \item{AD}{list of AD test statistic (AD) and pvalue(p)}
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\examples{
normPlot(rnorm(1000))
}
\keyword{statistics}
\keyword{univariate}
