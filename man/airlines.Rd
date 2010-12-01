\name{airlines}
\Rdversion{1.1}
\alias{airlines}
\docType{data}
\title{
AIRLINES dataset
}
\description{
This dataset contains monthly returns on airline stocks from CRSP.
The series span from Dec 1980 to Apr 1985.

}
\usage{data(airlines)}
\format{
  A data frame with 53 observations on the following 9 variables.
  \describe{
    \item{\code{date}}{Date in format YYMMDD}
    \item{\code{delta}}{Returns for Delta Airlines}
    \item{\code{sp500}}{S&P 500 index}
    \item{\code{amr}}{Returns for American Airlines}
    \item{\code{klm}}{Returns for KLM}
    \item{\code{pan}}{Returns for Pan American Airlines}
    \item{\code{ual}}{Returns for United Airlines}
    \item{\code{u}}{Returns for "u" }
    \item{\code{air}}{Returns for "air"}
  }
}
\source{
CSRP
}
\examples{
data(airlines)
}
\keyword{datasets}
