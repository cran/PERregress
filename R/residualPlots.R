residualPlots=function(out,bigres=3,bighat=3,cutoff=1)
{
#
# function to compute residual plots
#
pandterm = function(message) {
        stop(message, call. = FALSE) }
if(missing(out)) pandterm("missing argument -- must be output from lm")
if(is.null(attributes(out)$class)) pandterm("input must come from lm, e.g. regression.plots(lm(y~x))")
if(attributes(out)$class!="lm") pandterm("input must come from lm, e.g. residual.plots(lm(y~x))")
par.orig=par()
par(mfrow=c(2,2))
hist(rstandard(out),col="magenta",xlab="Standardized Residuals",main="")
plot(out$fitted,out$resid,pch=20,col="blue",xlab="Fitted",ylab="Residuals")
qqnorm(out$residuals,ylab="",xlab="",main="")
mtext(side=2,line=3,"Residuals",cex=.85)
mtext(side=1,line=3,"Quantiles Under Normality",cex=.85)
mtext(side=1,line=2,paste("A-D p-value= ",round(AndersonDarling(out$residuals)$p,2)),cex=.75,col="blue")
qqline(out$residuals,lwd=2,col="green")
plot(lm.influence(out)$hat,rstandard(out),pch=20,col="blue",xlab="Leverage",ylab="Standardized Residuals")
#
# add lines to show large resids and large influence
#
N=length(out$residuals)
kp1=length(out$coefficients)
if(max(rstandard(out)) > bigres) abline(h=bigres,lty=2,lwd=2,col="orange")
if(min(rstandard(out)) < -bigres) abline(h=-bigres,lty=2,lwd=2,col="orange")
if(max(lm.influence(out)$hat)>bighat*(kp1/N)) abline(v=bighat*(kp1/N),lty=2,lwd=2,col="orange")
#
# paint points with large Cooks distance
#
cook=cooks.distance(out)
ncutoff=length(cook>cutoff)
if(ncutoff > 0) points(lm.influence(out)$hat[cook>cutoff],rstandard(out)[cook>cutoff],pch=20,cex=1.5,col="red")
par=par.orig

return(list(large_res=which(abs(rstandard(out)) > bigres),large_lever=which(lm.influence(out)$hat > bighat*(kp1/N)),large_CooksD=which(cook>cutoff),
       AD=AndersonDarling(out$residuals)))
}