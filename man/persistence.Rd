\name{persistence}
\alias{persistence}
\docType{data}
\title{
Persistence: Sales, Advertising Time Series data
}
\description{
Sales and Advertising data which can be used to test for persistence (unit roots) and long-run effects of advertising
}
\usage{data(persistence)}
\format{
  A data frame with 76 monthly observations. Time series start in 1980. All variables measured in \$ except as noted.  Variables with INF ending are inflation adjusted.
  \describe{
    \item{\code{MONTH}}{month indicator}
    \item{\code{SALES}}{a numeric vector}
    \item{\code{ADBUDG}}{a numeric vector}
    \item{\code{PRINT}}{a numeric vector}
    \item{\code{NEWSPAP}}{a numeric vector}
    \item{\code{TVRADIO}}{a numeric vector}
    \item{\code{MARGIN}}{gross margin in percent}
    \item{\code{INFLAT}}{a numeric vector}
    \item{\code{SALINF}}{a numeric vector}
    \item{\code{ADVINF}}{a numeric vector}
    \item{\code{PRINF}}{a numeric vector}
    \item{\code{TVINF}}{a numeric vector}
  }
}
\source{
Dekimpe and Hanssens(1995), "The persistence of Marketing Effects on Sales," Marketing Science
}
\examples{
data(persistence)
}
\keyword{datasets}
