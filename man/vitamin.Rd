\name{vitamin}
\alias{vitamin}
\docType{data}
\title{
VITAMIN dataset
}
\description{
Data used to identify the relationship between price and contents of 
the product.
}
\usage{data(vitamin)}
\format{
  A data frame with 150 observations on the following 11 variables.
  \describe{
    \item{\code{unitc}}{price in $ of one tablet or capsule of vitamin}
    \item{\code{captab}}{0 if vitamin in capsule form, 1 if in tablet form}
    \item{\code{VA}}{quantity of A in International Units (IU) }
    \item{\code{VC}}{quantity of C in mg}
    \item{\code{B1}}{quantity of B1 in mg}
    \item{\code{B2}}{quantity of B2 in mg}
    \item{\code{NA.}}{quantity of Niacin in mg}
    \item{\code{VE}}{quantity of E in IU}
    \item{\code{B6}}{quantity of B6 in mg}
    \item{\code{B12}}{quantity of B12 in micrograms}
    \item{\code{FE}}{quantity of iron in mg}
  }
}

\source{
Data on vitamins collected by Larry E. Jones from Evanston drugstores in 1984. See also JPE article on Hedonic pricing by LEJ.
}
\examples{
data(vitamin)
}
\keyword{datasets}
