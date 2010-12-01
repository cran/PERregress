\name{return_pd}
\alias{return_pd}
\docType{data}
\title{
RETURN_PD dataset
}
\description{
Data to test the relationship between stock returns and the Price/Dividend Ratio 
over various time periods.
}
\usage{data(return_pd)}
\format{
  A data frame with 49 observations on the following 8 variables.
  \describe{
    \item{\code{X1yrt}}{Return on CRSP VW market index over one year.}
    \item{\code{pd1yr}}{Price/Dividend Ratio one year prior to the time period 
			over which X1yrt is computed.}
    \item{\code{X2yrt}}{Two year return on CRSP VW.}
    \item{\code{pd2yr}}{Price/Dividend Ratio one year prior to the time period 
			over which X2yrt is computed.}
    \item{\code{X3yrt}}{Three year return on CRSP VW.}
    \item{\code{pd3yr}}{Price/Dividend Ratio one year prior to the time period 
			over which X3yrt is computed.}
    \item{\code{X5yrt}}{Five year return on CRSP VW.}
    \item{\code{pd5yr}}{Price/Dividend Ratio one year prior to the time period 
			over which X5yrt is computed.}
  }
}
\source{
John Cochrane. See his article in Chicago Fed Bulletin circa 1999.
}
\examples{
data(return_pd)
}
\keyword{datasets}
