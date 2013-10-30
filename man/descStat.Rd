\name{descStat}
\alias{descStat}
\title{
Descriptive Statistics
}
\description{
Compute Descriptive statistics for continuous variables. Mean, Standard Deviation (SD), Median, Inter-quartile Range (IQR), Standard Error of the mean (SE Mean), and Confidence Interval (CI) for the mean are displayed.
}
\usage{
descStat(df, CONF = 0.95, rnd = 3)
}
\arguments{
  \item{df}{input dataframe -- must be a dataframe}
  \item{CONF}{confidence level (1-alpha)}
  \item{rnd}{decimal digits to display}
}
\details{
use \code{descStat(data.frame(x))} to compute descriptive statistics for a vector or matrix of variables
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\seealso{
\code{\link{summary}}
}
\examples{
data(Flat_Panel_TV)
descStat(Flat_Panel_TV)
}
\keyword{statistics}
\keyword{univar}
