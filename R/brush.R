brush=function(x1,x2,y,window=.3*(quantile(x2,probs=c(.75))-quantile(x2,probs=c(.25))),addreg=FALSE)
{
	# function to do static brushing to illustrate a regression of y on x1, x2 -- mr coef on x1
	pandterm = function(message) {
	        stop(message, call. = FALSE) }
	if (missing(y) | missing(x1) | missing(x2 )) pandterm("Requires 3 Input Variables")
	if (!is.vector(y)) pandterm("First input must be a Variable/Vector - subset dataframe if necessary")
	if (!is.vector(x1)) pandterm("Second input must be a Variable/Vector - subset dataframe if necessary")
	if (!is.vector(x2)) pandterm("Third input must be a Variable/Vector - subset dataframe if necessary")
	par.orig=par()
	par(mfrow=c(2,3))
	low=c((quantile(x2,probs=c(.2))-.5*window),(quantile(x2,probs=c(.2))+.5*window))
	high=c((quantile(x2,probs=c(.8))-.5*window),(quantile(x2,probs=c(.8))+.5*window))
	med=c((median(x2)-.5*window),(median(x2)+.5*window))
	
	x1low=x1[x2>=low[1] & x2<=low[2]]
	x1med=x1[x2>=med[1] & x2<=med[2]]
	x1high=x1[x2>=high[1] & x2<=high[2]]
	x2low=x2[x2>=low[1] & x2<=low[2]]
	x2med=x2[x2>=med[1] & x2<=med[2]]
	x2high=x2[x2>=high[1] & x2<=high[2]]
	ylow=y[x2>=low[1] & x2<=low[2]]
	ymed=y[x2>=med[1] & x2<=med[2]]
	yhigh=y[x2>=high[1] & x2<=high[2]]
	
	plot(x1,y,pch=20,col="black")
	if(addreg) abline(lm(y~x1)$coef,col="red")
	points(x1low,ylow,pch=20,col="yellow")
	if(addreg) abline(lm(ylow~x1low)$coef,lwd=2,col="yellow",lty=2)
	plot(x1,y,pch=20,col="black")
	if(addreg) abline(lm(y~x1)$coef,col="red")
	points(x1med,ymed,pch=20,col="blue")
	if(addreg) abline(lm(ymed~x1med)$coef,lwd=2,col="blue",lty=2)
	plot(x1,y,pch=20,col="black")
    if(addreg) abline(lm(y~x1)$coef,col="red")
	points(x1high,yhigh,pch=20,col="green")
	if(addreg) abline(lm(yhigh~x1high)$coef,lwd=2,col="green",lty=2)
	
	plot(x2,x1,pch=20,col="black")
	points(x2low,x1low,pch=20,col="yellow")
	plot(x2,x1,pch=20,col="black")
	points(x2med,x1med,pch=20,col="blue")
	plot(x2,x1,pch=20,col="black")
	points(x2high,x1high,pch=20,col="green")
	par=par.orig
}