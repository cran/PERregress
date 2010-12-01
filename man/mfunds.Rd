\name{mfunds}
\Rdversion{1.1}
\alias{mfunds}
\docType{data}
\title{
MFUNDS dataset
}
\description{
Time series of monthly returns of several mutual funds.
}
\usage{data(mfunds)}
\format{
  A data frame with 180 observations on the following 9 variables.
  \describe{
    \item{\code{drefus}}{R22 Drefus (growth)}
    \item{\code{fidel}}{R30 Fidelity Trend fund (growth)}
    \item{\code{keystne}}{R55 Keystone Speculative fund (max capital gain)}
    \item{\code{Putnminc}}{R92 Putnam Income Fund (income)}
    \item{\code{scudinc}}{R99 Scudder Income}
    \item{\code{windsor}}{R129 Windsor Fund (growth)}
    \item{\code{eqmrkt}}{equally weighted market}
    \item{\code{valmrkt}}{value weighted market}
    \item{\code{tbill}}{tbill rate}
  }
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Partial extract from R. Korajczyk file of 130 mutual funds.
}
\examples{
data(mfunds)
}
\keyword{datasets}
