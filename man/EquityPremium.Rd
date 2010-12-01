\name{EquityPremium}
\alias{EquityPremium}
\docType{data}
\title{
EQUITYPREMIUM dataset
}
\description{
Data used to test several candidate predictors of equity premimum.
The dependent variable is always the equity premium, i.e., the total 
rate of return on the stock market minus the prevailing short-term 
interest rate.
}
\usage{data(EquityPremium)}
\format{
  A data frame with 1656 observations on the following 18 variables.
  \describe{
    \item{\code{yyyymm}}{The date}
    \item{\code{Index}}{The price level of S&P500.}
    \item{\code{D12}}{Twelve-month moving sums of dividends paid on the S&P 
    500 index.}
    \item{\code{E12}}{Twelve-monthmoving sums of earnings on the S&P 500 index}
    \item{\code{b.m}}{The Book to Market Ratio (b/m) as the ratio of book value 
    to market value for the Dow Jones Industrial Average.}
    \item{\code{tbl}}{Treasury Bills rates.}
    \item{\code{AAA}}{Yields on AAA-rated bonds.}
    \item{\code{BAA}}{Yields on BAA-rated bonds.}
    \item{\code{lty}}{Long-term government bond returns.}
    \item{\code{ntis}}{Net Equity Expansion equal to the ratio of twelve-month 
    moving sums of net issues by NYSE listed stocks divided by the total market 
    capitalization of NYSE stocks.}
    \item{\code{Rfree}}{The risk-free rate.}
    \item{\code{infl}}{Inflation as the Consumer Price Index (All Urban 
    Consumers).}
    \item{\code{ltr}}{Long-term government bond returns.}
    \item{\code{corpr}}{Long-term corporate bond returns.}
    \item{\code{svar}}{Stock Variance computed as sum of squared daily returns 
    on S&P 500.}
    \item{\code{csp}}{Cross-sectional beta premium measuring the relative 
    valuations of high- and low-beta stocks.}
    \item{\code{CRSP_SPvw}}{The return (including dividends) on S&P500 as calculated by CRSP.}
    \item{\code{CRSP_SPvwx}}{The return (excluding dividends) on S&P500 as calculated by CRSP.}
  }
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Welch and Goyal "A Comprehensive Look at the Empirical Performance of Equity 
Premium Prediction", July 2008, Review of Financial Studies 21(4) 1455-1508.

data availble at \url{http://www.bus.emory.edu/agoyal/Research.html}
}
\examples{
data(EquityPremium)
}
\keyword{datasets}
