back=function(y,noperiods=1)
{
# function to compute "lags" for vectors
pandterm = function(message) {
        stop(message, call. = FALSE) }
if (missing(y)) pandterm("Requires Input Variable")
if (!is.vector(y)) pandterm("Input must be a Variable/Vector - subset dataframe if necessary")
if(!is.numeric(noperiods)) pandterm("noperiods must be numeric variable")
N=length(y)
if(!(noperiods > 0 & noperiods < N)) pandterm("number of lags must be > 0 and < N")
lagy=double(N)
lagy=NA
lagy[(noperiods+1):N]=y[1:(N-noperiods)]
return(lagy)
}