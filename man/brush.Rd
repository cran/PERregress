\name{brush}
\alias{brush}
\title{
Static Brushing
}
\description{
Illustrate Computation of a Mulitple Regression coefficient via interlinked graphs.  A static version of brushing.
}
\usage{
brush(x1, x2, y, window = 0.3 * (quantile(x2, probs = c(0.75)) - quantile(x2, probs = c(0.25))), addreg = FALSE)
}
\arguments{
  \item{x1}{indep variable, must be a vector}
  \item{x2}{indep variable, must be a vector}
  \item{y}{dependent variable, must be a vector}
  \item{window}{selection window defined in terms of units of x2}
  \item{addreg}{if TRUE, add regression lines to plots}
}
\details{
computes multiple regression coefficient on x1 in a multiple regression of y on x1 and x2.
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\examples{
data(multi)
brush(multi$p1,multi$p2,multi$Sales)
}
\keyword{Statistics}
\keyword{regression}
