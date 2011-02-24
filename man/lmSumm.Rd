\name{lmSumm}
\alias{lmSumm}
\title{
Summarize Linear Model Fits including HAC variance estimator
}
\description{
Summarizes output from lm() command. Computes HAC std errors if desired. For heteroskedasticity only, uses H3 method as documented in sandwich package.  For L>0, uses N-W (84) approach. 
}
\usage{
lmSumm(lmfit, L = 0, HAC = FALSE)
}
\arguments{
  \item{lmfit}{output from lm()}
  \item{L}{Truncation lag for HAC estimators, default is 0 which is only an adjustment for heteroskedasticity }
  \item{HAC}{logical, if TRUE then compute HAC, default is FALSE}
}
\value{
called for effect. does not return anything.
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}
\examples{
y=rnorm(100)
X=matrix(rnorm(2*100),ncol=2)
lmSumm(lm(y~X))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ statistics }
\keyword{ regression }
