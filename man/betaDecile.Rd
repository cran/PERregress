\name{Monthly NYSE/AMEX Beta Decile Returns}
\alias{betaDecile}
\docType{data}
\title{
Beta Decile Portfolios
}
\description{
Includes beta decile returns between January 1926 through June 2013. 
}
\usage{data(betaDecile)}
\format{
  A data frame with 1050 observations on the following 11 variables.
  \describe{
    \item{\code{date}}{Last trading date in the month}
    \item{\code{BET1RET}}{Monthly return for decile 1, highest decile in beta}
    \item{\code{BET2RET}}{Monthly return for decile 2}
    \item{\code{BET3RET}}{Monthly return for decile 3}
    \item{\code{BET4RET}}{Monthly return for decile 4}
    \item{\code{BET5RET}}{Monthly return for decile 5}
    \item{\code{BET6RET}}{Monthly return for decile 6}
    \item{\code{BET7RET}}{Monthly return for decile 7}
    \item{\code{BET8RET}}{Monthly return for decile 8}
    \item{\code{BET9RET}}{Monthly return for decile 9}
    \item{\code{BET10RET}}{Monthly return for decile 10, lowest decile in beta}
  }
}
\details{
The beta of a stock or portfolio is a number describing the correlated volatility of an asset in relation to the volatility of the benchmark that the asset is being compared to. This benchmark is generally the overall financial market.

For classification into the deciles, annual betas are computed (from daily data) using the Scholes and Williams method.
}
\source{
\emph{Center for Research in Security Prices}
}
\references{
Levinson, Mark (2006). Guide to Financial Markets. London: The Economist (Profile Books). pp. 155-6.

Myron Scholes and Joseph Williams, "Estimating Betas from Nonsynchronous Data,"
\emph{Journal of Financial Economics}, vol 5, 1977, 309-327.
}
\examples{
#Merge beta decile returns with market returns
data(marketRf)
data(betaDecile)
betaDecileMarket<-merge(betaDecile,marketRf,by="date")

#Boxplot of monthly decile returns
boxplot(betaDecile[,-1])
}

\keyword{datasets}
