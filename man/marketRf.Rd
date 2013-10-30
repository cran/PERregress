\name{marketRf}
\alias{marketRf}
\docType{data}
\title{
Market Indices and Risk Free Rate data
}
\description{
Monthly returns on various market indices, the level of the S&P index, and t-bill returns (so-called risk-free rate).  Includes returns from July 1926 through June 2013.
}
\usage{data(marketRf)}
\format{
  A data frame with 1044 observations on the following 8 variables.
  \describe{
    \item{\code{date}}{Last trading date in month}
    \item{\code{vwretd}}{Value-weighted return (includes distributions)}
    \item{\code{vwretx}}{Value-weighted return (excluding dividends)}
    \item{\code{ewretd}}{Equal-weighted return (includes distributions)}
    \item{\code{ewretx}}{Equal-weighted return (excluding dividends)}
    \item{\code{spindx}}{Level of S&P composite index}
    \item{\code{sprtrn}}{Monthly return on S&P composite index}
    \item{\code{RF}}{Monthly return on 1 month T-bill with .01 = 1\%}
  }
}

\source{
\emph{Center for Research in Security Prices}

\emph{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
}

\examples{
#Merge Vanguard mutual fund returns with market and risk free returns
data(Vanguard)
data(marketRf)
VanguardRf<-merge(Vanguard,marketRf,by="date",all.x=TRUE)

#Obtain summary statistics for the risk free returns
summary(marketRf$RF)

#Plot the distribution of risk free returns
hist(marketRf$RF)
}
\keyword{datasets}
