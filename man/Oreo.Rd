\name{Oreo}
\alias{Oreo}
\docType{data}
\title{
Oreo Sales and Marketing Time Series
}
\description{
Data on Sales of Oreo cookies and various marketing variables.
}
\usage{data(Oreo)}
\format{
  A data frame with 54 observations on the following 7 variables.
  \describe{
    \item{\code{Period}}{month}
    \item{\code{Sales}}{monthly sales (\$)}
    \item{\code{Advertising}}{monthly Advert expenditures (\$)}
    \item{\code{Promotion}}{monthly Promotional expenditures (\$)}
    \item{\code{Oreo.price}}{price in cents}
    \item{\code{Hydrox.price}}{competitor price in cents}
    \item{\code{Share}}{Oreo \$ market share}
  }
}
\details{
quite a bit of share data is missing
}
\source{
from Prof Little's BRANDAID examples
}
\examples{
data(Oreo)
}
\keyword{datasets}
