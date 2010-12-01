\name{slots}
\alias{slots}
\docType{data}
\title{
SLOTS dataset
}
\description{
Data collected by a casino operator of all the casino's slots machine.  
This dataset contains the winnings (for several months) and various 
attributes of 2,401 slot machines.
}
\usage{data(slots)}
\format{
  A data frame with 30439 observations on the following 7 variables.
  \describe{
    \item{\code{slot_number}}{Machine id}
    \item{\code{month}}{Month}
    \item{\code{winnings}}{Casino profits (\$)}
    \item{\code{days_active}}{No. of days slot was operational within a month}
    \item{\code{hold}}{Expected hold (\%) of slot machine}
    \item{\code{denomination}}{25 cents, 1 dollar, etc}
    \item{\code{location}}{Floor location of machine in casino}
  }
}
\source{
confidential but THE major player
}
\examples{
data(slots)
}
\keyword{datasets}
