lmSumm=function(lmfit,L=0,HAC=FALSE){
#
#  function to "clean" lm summary and include heteroskedastic/autocorrelation consistent std errors
#
HACVar=function(qrX,res,L){
	# 2/11 P. Rossi
	#  function  to compute heteroskedastic autocorrelation
	#   consistent Var-Cov matrix using QR decomposition of X, and residuals
	#
	#  (X'X)-1 Omegahat (X'X)-1
	# if L=0, Phi0 = X' diag(e_i^2/(1-hii)^2) X  -- hii is i,i, element of hat matrix use "H3" method from sandwich
	# if Lne0, Omegahat = omega_0*Phi_0 + sum(l=1 to L) omega_l*Phi_l ; omega_l = 1 - (l/L+1)
	#        phi_l=sum(t=l+1 to T) x_t e_t e_t-l (x_t-l)'  x_t is the "t"th row of X and e_t is the lsq residual
	# or
	#        phi_l= (XP*ep)'(XM*em)+ t(   )
	#        XP=X[(l+1):T,] ep[(l+1):T] ; XM=X[1:(T-l),]  em[1:T-l]
	#
	#  in terms of X=QR  this is R-1(R-1)' OmegaH R-1(R-1)'
	# note the hat matrix is QQ'
	#
	#
	k=ncol(qrX$qr)
	R=qr.R(qrX)
	Q=qr.Q(qrX)
	X=Q%*%R
	n=nrow(X)
	RI=backsolve(R,diag(k))
	lambda=res**2
	lambda=lambda/((1-rowSums(Q*Q))**2)    # here we are fetching diag elements of QQ'
	if(L==0)
	{
	  QRIt=Q%*%t(RI)
	  V=crossprod(QRIt,QRIt*lambda)       # for L=0, V=(X'X)-1X' Lambda X (X'X)-1 = t(QRI) Lambda QRI
	}
	else
	{
		phi0=crossprod(X,X*res**2)
		Omegahat=phi0
		for(l in 1:L )
		{
			omega_l=1-l/(L+1)	 		# N-W weights
			mat=crossprod((X[(l+1):n,]*res[(l+1):n]),(X[1:(n-l),]*res[1:(n-l)]))
			mat=mat+t(mat)
			Omegahat=Omegahat+omega_l*mat
			   
		}
	   XpXI=crossprod(t(RI))
	   V=crossprod(XpXI,Omegahat%*%XpXI)
	}
	std_err=sqrt(diag(V))
	return(list(V=V,std_err=std_err))
}
pval=function(x){1-pf(x[1],df1=x[2],df2=x[3])}

pandterm = function(message) {
        stop(message, call. = FALSE) }
if (missing(lmfit)) pandterm("Requires output from the lm command as in lmSummary(lm(y~x))")
if (attributes(lmfit)$class != "lm") pandterm("input not from lm command")
cat("Multiple Regression Analysis:",fill=TRUE)
n=length(lmfit$res)
k=length(lmfit$coef)
cat("    ",k," regressors(including intercept) and ",n," observations",sep="",fill=TRUE)
if(HAC==TRUE)
{
	cat("    with heteroskedastic|autocorrelation consistent standard errors",fill=TRUE)
	cat("    Lag truncation =",L,fill=TRUE)
}
cat("",fill=TRUE)
print(lmfit$call,fill=TRUE)
cat("",fill=TRUE)
#
# compute std errors
#
slm=summary(lmfit)
s=slm$sigma
rsq=slm$r.squared
adjrsq=slm$adj.r.squared
fstat=slm$fstat  # note this is a vector of three nums fstat, df1, df2
if(HAC)
{
	if(n<(L+1)) pandterm("make sure N > L+1")
	std_err=HACVar(lmfit$qr,lmfit$res,L=L)$std_err
	}
else
{
	s=sqrt(sum(lmfit$res**2)/(length(lmfit$res)-k))
	RI=backsolve(qr.R(lmfit$qr),diag(k))
	V=s**2*crossprod(t(RI))
	std_err=sqrt(diag(V))
}
t=lmfit$coef/std_err
t_pval=2*(1-pt(abs(t),df=(n-k)))
output=matrix(double(k*4),ncol=4)
rownames(output)=names(lmfit$coef)
coef=lmfit$coef
names(coef)=NULL
colnames(output)=c("Estimate","Std Error","t value","p value")
output[,1]=signif(coef,4)
output[,2]=signif(std_err,4)
output[,3]=round(t,2)
output[,4]=round(t_pval,3)
cat("Coefficients:",fill=TRUE)
print(output)
cat("---",fill=TRUE)
cat("Standard Error of the Regression: ",signif(s,4),fill=TRUE)
cat("Multiple R-squared: ",round(rsq,3)," Adjusted R-squared: ",round(adjrsq,3),fill=TRUE)
if(!HAC) 
   {cat("Overall F stat: ",round(fstat[1],2)," on ",fstat[2]," and ",fstat[3]," DF,"," pvalue= ",round(pval(fstat),3),
         sep="",fill=TRUE)}
if(HAC & (n-k) < (50+20*L))
   {cat("Warning: Sample Size of",n,"too small for accurate use of HAC",fill=TRUE)}
}