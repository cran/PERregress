\name{Fama/French Risk Factors}
\alias{riskFactors}
\docType{data}
\title{
Fama/French Risk Factors
}
\description{
Monthly Fama/French risk factors from July 1926 through June 2013.
}
\usage{data(riskFactors)}
\format{
  A data frame with 1044 observations on the following 4 variables.
  \describe{
    \item{\code{date}}{Last trading date in month}
    \item{\code{RmRf}}{Excess return on the market}
    \item{\code{SMB}}{Small Minus Big}
    \item{\code{HML}}{High Minus Low}
  }
}
\details{
The Fama/French factors are constructed using 6 value-weight portfolios formed on size and book-to-market.

The portfolios, which are constructed at the end of each June, are the intersections of 2 portfolios formed on size (market equity, ME) and 3 portfolios formed on the ratio of book equity to market equity (BE/ME). The size breakpoint for year t is the median NYSE market equity at the end of June of year t. BE/ME for June of year t is the book equity for the last fiscal year end in t-1 divided by ME for December of t-1. The BE/ME breakpoints are the 30th and 70th NYSE percentiles.

The portfolios for July of year t to June of t+1 include all NYSE, AMEX, and NASDAQ stocks for which we have market equity data for December of t-1 and June of t, and (positive) book equity data for t-1. 

SMB (Small Minus Big) is the average return on the three small portfolios minus the average return on the three big portfolios.   
      
\emph{SMB = 1/3 (Small Value + Small Neutral + Small Growth)- 1/3 (Big Value + Big Neutral + Big Growth).}
  
HML (High Minus Low) is the average return on the two value portfolios minus the average return on the two growth portfolios.   
     
\emph{HML = 1/2 (Small Value + Big Value)- 1/2 (Small Growth + Big Growth).}
  
RmRf, the excess return on the market, is the value-weight return of all CRSP firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a CRSP share code of 10 or 11 at the beginning of month t, good shares and price data at the beginning of t, and good return data for t minus the one-month Treasury bill rate (from Ibbotson Associates).

RmRf includes all NYSE, AMEX, and NASDAQ firms. SMB and HML for July of year t to June of t+1 include all NYSE, AMEX, and NASDAQ stocks for which we have market equity data for December of t-1 and June of t, and (positive) book equity data for t-1.

}
\source{
Data:
\emph{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}

Factors:
\emph{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html}

Portfolios:
\emph{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/six_portfolios.html}
}
\references{
See Fama and French, 1993, "Common Risk Factors in the Returns on Stocks and Bonds," \emph{Journal of Financial Economics}, for a complete description of the factor returns.
}
\examples{
#Merge risk factors with with risk free returns
data(marketRf)
data(riskFactors)
riskFactorsRf<-merge(riskFactors,marketRf,by="date")
}
\keyword{datasets}
