\name{doffing}
\Rdversion{1.1}
\alias{doffing}
\docType{data}
\title{
DOFFING dataset
}
\description{
Data On Doffing (removing of empty spindles) from Spinning Frames
}
\usage{data(doffing)}
\format{
  A data frame with 60 observations on the following 5 variables.
  \describe{
    \item{\code{frdoff}}{number of frames doffed}
    \item{\code{cost}}{labor cost (units of labor)}
    \item{\code{cumfr}}{cumulative frames doffed}
    \item{\code{cumavc}}{cum average cost= cumcost/cumfr}
    \item{\code{unitc}}{average cost for batch= cost/frdoff}
  }
}
\details{
Application: estimation of learning curves with real cost data
}
\source{
Womer and Patterson, Journal of Business and Economic Statistics, (1983)
}
\examples{
data(doffing)
}
\keyword{datasets}
