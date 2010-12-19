\name{detergent}
\alias{detergent}
\docType{data}
\title{
DETERGENT dataset
}
\description{
Actual sales data for the laundry detergent category across 86 stores in the Dominick's 
Finer Foods (DFF) chain in Chicago.   
The data include weekly sales and price information for three products-Tide 
128 oz, Tide 64 oz, Wisk 64 oz-across the 86 stores in the DFF chain.  
The data are available for up to 300 weeks (the exact number of weeks 
included in the data varies across stores). 
The data are from the years 1989 to 1994, and resulted from a partnership 
between the Booth School and Dominick's Finer Foods on research into 
shelf-management and store pricing. 
}
\usage{data(detergent)}
\format{
  A data frame with 14745 observations on the following 10 variables.
  \describe{
    \item{\code{store}}{Store id number - stores as a factor}
    \item{\code{week}}{Week}
    \item{\code{acv}}{ACV (all commodity volume), in $1,000}
    \item{\code{promoflag}}{= 1 if any product in the category was on promotion}
    \item{\code{q_tide128}}{Tide 128 oz:  unit sales}
    \item{\code{p_tide128}}{Tide 128 oz:  price ($)}
    \item{\code{q_tide64}}{Tide 64 oz:  unit sales}
    \item{\code{p_tide64}}{Tide 64 oz:  price ($)}
    \item{\code{q_wisk64}}{Wisk 64 oz:  unit sales}
    \item{\code{p_wisk64}}{Wisk 64 oz:  price ($)}
  }
}
\source{
The Kilts Center for Marketing at the Booth School of Business:  
\url{http://research.chicagobooth.edu/marketing/databases/dominicks/index.aspx}
}
\examples{
data(detergent)
}
\keyword{datasets}
