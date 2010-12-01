\name{stocks}
\alias{stocks}
\docType{data}
\title{
STOCKS dataset
}
\description{
Daily closing prices for the stocks of Apple, Google, DemandTec, and for the
S&P 500 from Jan 1st, 2009 to Dec 31st, 2009.

}
\usage{data(stocks)}
\format{
  A data frame with 252 observations on the following 5 variables.
  \describe{
    \item{\code{Day}}{Day of measurement. 1 represents Jan 1st 2009.}
    \item{\code{Apple}}{Apple's stock price}
    \item{\code{Google}}{Google's stock price}
    \item{\code{DMAN}}{DemandTec's stock price}
    \item{\code{GSPC}}{S&P value}
  }
}
\source{
Yahoo Finance
}
\examples{
data(stocks)
}
\keyword{datasets}
