\name{AndersonDarling}
\alias{AndersonDarling}
\title{
Anderson-Darling test for Normality
}
\description{
Computes A-D test for Normality
}
\usage{
AndersonDarling(x)
}

\arguments{
  \item{x}{input vector  -- not a data frame\!}
}

\value{
	\item{\code{AD}}{value of AD test stat}
	\item{\code{p}}{p value}
}
\references{
borrowed from the nortest package
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\examples{
AndersonDarling(rnorm(100))
}
\keyword{Statistics}
\keyword{Distribution}
