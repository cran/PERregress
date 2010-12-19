\name{residualPlots}
\alias{residualPlots}
\title{
Residual Plots for Linear Regression
}
\description{
Provides four plots based on residuals.  Clockwise from upper left: 1. histogram of standardized residuals, 2. residuals vs. fitted values, 3. Standardized Residuals vs. Leverage (hat) values, and 4. Normal Probability Plot of Residuals.
}
\usage{
residualPlots(out, bigres = 3, bighat = 3, cutoff = 1)
}
\arguments{
  \item{out}{output from \code{lm}}
  \item{bigres}{cut-off for large std residuals}
  \item{bighat}{multiple of mean leverage value to flag for large leverage}
  \item{cutoff}{cut-off for Cooks Distance values}
}
\details{
yellow lines (if present) on leverage vs. std residuals plot indicate large residuals. Green line (if present) delimits large leverage values.  Large red values (if present) indicate large Cooks Distance values. P-value for Anderson-Darling test of normality is indicated on normal probability plot.
}
\value{
   \item{large_res}{indices of observations with large std residuals, if any}
   \item{large_lever}{indices of observations with large leverage values, if any}
   \item{large_CooksD}{indices of obserations with large Cooks Distance, if any}
}
\author{
Peter E. Rossi, Anderson School UCLA, \email{perossichi@gmail.com}
}

\examples{
data(Flat_Panel_TV)
out=residualPlots(lm(Flat_Panel_TV$Price~Flat_Panel_TV$Size))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Statistics}
\keyword{regression}
