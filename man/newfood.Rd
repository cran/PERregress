\name{newfood}
\alias{newfood}
\docType{data}
\title{
NEWFOOD dataset
}
\description{
Data from a  6-month test market on the Newfood product.
The purpose of this test was to study certain marketing variables in order to 
determine an optimal introductory marketing plan for use when the product is
introduced on a national scale and to obtain improved sales forecasts.
It was planned to study the effect of marketing variables by conducting
sales audits in a panel of grocery outlets in four different cities.
}
\usage{data(newfood)}
\format{
  A data frame with 72 observations on the following 8 variables.
  \describe{
    \item{\code{sales}}{average unit sales per month}
    \item{\code{price}}{price in cents}
    \item{\code{adv}}{1 if high 0 if low}
    \item{\code{locat}}{1 if in instant breakfast section 0 if bread section}
    \item{\code{income}}{average income of families in 000}
    \item{\code{svol}}{monthly dollar value of total store sales in 000}
    \item{\code{city}}{1,2,3,4 city index}
    \item{\code{indx}}{1 if sales figure is for first 2 months, 2 for 2nd 
    two months,3 for 3rd two months of experiment}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
A controlled store test was designed including three variables:

l. price (24, 29, and 34 cents)

2. Two levels of advertising expenditures, high and low, were examined. 
All advertising was in the form of television commercials.  The high level 
was used in two of the four cities and was meant to simulate the effect of 
a $6 million nationwide advertising campaign. The low level was used in the 
other two cities and was designed to simulate the effect of a $3 million 
nationwide advertising campaign.

3. Two shelf locations (in the bread section or the instant breakfast section)

Prices and location were varied across stores within cities while
advertising was varied across cities.

\strong{Design}

The test design in summarized below.  Each cell contained 3 stores and each
store was audited monthly. The sales for each store in the cell were
averaged on a bi-monthly basis.

\preformatted{
______________________________________________________________________
______________________________________________________________________

                        p=24 cts       p=29 cts      p=34 cts

City   Adv Level        L1    L2       L1    L2      L1    L2
____________________________________________________________________

3      low

4      low

1      high

2      high
______________________________________________________________________
               p=price, L=location, three stores per cell
}

\strong{Control Measures}


The store locations could not be perfectly matched. In order to control
for differences in stores, two variables were contructed:

INCOME= average income of families in trading area of each store

SVOL= average weekly dollar sales of all stores in experimental cell
}

\examples{
data(newfood)
}
\keyword{datasets}
