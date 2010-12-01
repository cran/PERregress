normPlot=function(x)
{
#
# function to compute normal probability plot
#
pandterm = function(message) {
        stop(message, call. = FALSE) }
if(missing(x)) pandterm("missing argument -- must be output from lm")
if(!is.vector(x)) pandterm("input must be a vector - extract from data frame or lm object)")

AD=AndersonDarling(x)
qqnorm(x,ylab="",xlab="",main="")
mtext(side=1,line=3,"Quantiles Under Normality",cex=.85)
mtext(side=1,line=2,paste("A-D p-value= ",round(AD$p,2)),cex=.75,col="blue")
qqline(x,lwd=2,col="green")
return(AD)
}