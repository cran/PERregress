\name{tvdemo}
\alias{tvdemo}
\docType{data}
\title{
tvdemo
}
\description{
Survey data collected by a British marketing research company on product usage and consumption
}
\usage{data(tvdemo)}
\format{
  A data frame with 1000 observations on the following 16 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{sex}}{a factor with two levels: "male" and "female"}
    \item{\code{social}}{a factor with 6 levels from highest (A) to lowest (F)}
    \item{\code{edu}}{a factor with 9 levels corresponding to years of ed}
    \item{\code{Region}}{a factor with 11 levels indicating geographic region of GB}
    \item{\code{income}}{a numeric vector from 1:19 corresponding to different income levels}
    \item{\code{colas}}{1 if reg cola consumer, 0 if not}
    \item{\code{restrntEve}}{1 if reg rest consumer, 0 if not}
    \item{\code{juice}}{1 if juice consumer, 0 if not}
    \item{\code{cigs}}{1 if reg cig consumer, 0 if not}
    \item{\code{antique}}{1 if reg view antique shows, 0 if not}
    \item{\code{news}}{1 if reg news viewer, 0 if not}
    \item{\code{enders}}{1 if reg enders viewer, 0 if not}
    \item{\code{friends}}{1 if reg friends viewer, 0 if not}
    \item{\code{simpsons}}{1 if reg simpsons viewer, 0 if not}
    \item{\code{football}}{1 if reg soccer viewer, 0 if not }
  }
}
\details{
income variable is ordinal with 1 as highest level of income.  
}
\source{
from Zvi Gilula sometimes called the bmbrx dataset.
}
\examples{
data(tvdemo)
}
\keyword{datasets}
