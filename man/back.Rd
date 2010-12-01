\name{back}
\alias{back}
\title{
Lag (or backup)
}
\description{
Creates lagged version of input variable
}
\usage{
back(y, noperiods = 1)
}
\arguments{
  \item{y}{input vector -- not a data.frame!}
  \item{noperiods}{number of periods to lag}
}
\value{
returns lagged values as a vector
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\note{
do not use the \code{lag} function built into R.  this leads not lags!
}
\examples{
back(1:10)
}
\keyword{Statistics}
\keyword{ts}
