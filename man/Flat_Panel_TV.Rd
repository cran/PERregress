\name{Flat_Panel_TV}
\alias{Flat_Panel_TV}
\docType{data}
\title{
Flat_Panel_TV dataset
}
\description{
This dataset contains information on flat panel TV (LED and Plasma) collected from Amazon on 
"Black Friday" 11/26/10. All units are 1080p.
}
\usage{data(Flat_Panel_TV)}
\format{
  A data frame with 70 observations on the following 5 variables.
  \describe{
    \item{\code{Price}}{TV price in dollars}
    \item{\code{Size}}{TV diagonal size in inches}
    \item{\code{Brand}}{TV brand (\code{LG}, \code{Panasonic}, or \code{Samsung})}
    \item{\code{Type}}{TV technology, either \code{LED} or \code{Plasma}}
    \item{\code{Model}}{TV model}
  }
}
\examples{
data(Flat_Panel_TV)
}
\keyword{datasets}
