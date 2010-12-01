\name{bank}
\alias{bank}
\docType{data}
\title{
BANK dataset
}
\description{
Each of the following 16 variables have been measured at 233 bank branches.
The first variable corresponds to the sales of a type of bank acoount at 
each branch.
Variables in columns 2 to 16 can be used to characterize each branch.
}
\usage{data(bank)}
\format{
  A data frame with 233 observations on the following 16 variables.
  \describe{
    \item{\code{newacc}}{a numeric vector with the number of the new accounts 
          sold in a given time period}
    \item{\code{nohh}}{a numeric vector with the number of households serviced}
    \item{\code{nosalep}}{a numeric vector with the number of people selling the new account}
    \item{\code{manhatt}}{1 if the branch is in Manhattan and 0 othewise }
    \item{\code{borough}}{1 of the branch is in the boroughs and 0 otherwise}
    \item{\code{suburb}}{1 if the branch is in the suburbs and 0 otherwise 
         note: if all of c4,c5, and c6 are 0 then the branch is in a rural area}
    \item{\code{ddepbal}}{demand deposits balance}
    \item{\code{noddep}}{number of demand deposits}
    \item{\code{nowbal}}{now accounts balance}
    \item{\code{nonow}}{number of now accounts}
    \item{\code{mmbal}}{balance of money market accounts}
    \item{\code{nomm}}{number of money market accounts}
    \item{\code{savebal}}{passbook saving balance}
    \item{\code{othbal}}{other time balance}
    \item{\code{consloan}}{consumer loans}
    \item{\code{homeloan}}{shelter loans}
  }
}
\details{
Data collected by a bank, which has introduced a new type of account, to answer 
two questions: 
(i)  How can we assess branch performance in selling the new account? 
(ii) Different branches have different numbers of people selling the new account.  
     Can we determine the effect of the sales force in selling the new account?   
     (a) do we need any sales people? 
     (b) if we need sales people, how many per branch? 

}
\examples{
data(bank)
}
\keyword{datasets}
