descStat= function(df,CONF=.95,rnd=3) 
{
# function to compute descriptive stats
pandterm = function(message) {
        stop(message, call. = FALSE) }
if (missing(df)) pandterm("Requires a dataframe")
if (!is.data.frame(df)) pandterm("Input not a dataframe - try data.frame(input)")
if(!is.numeric(CONF)) pandterm("Confidence must be numeric variable")
if(!(CONF < 1 & CONF >0)) pandterm("Confidence must be in (0,1)")
numeric=sapply(df,is.numeric)
dfn=df[,numeric]
means=sapply(dfn,mean)
sds=sapply(dfn,sd)
medians=sapply(dfn,median)
N=dim(dfn)[1]
semeans=sds/sqrt(N)
per25=sapply(dfn,quantile,prob=c(.25))
per75=sapply(dfn,quantile,prob=c(.75))
IQR=per75-per25
Zstar=-qnorm((1-CONF)/2)
cil=means-Zstar*semeans
ciu=means+Zstar*semeans
mat=cbind(means,medians,sds,IQR,semeans,cil,ciu)
mat=round(mat,digits=rnd)
confc=as.character(CONF*100)
lab=paste(confc,"% CI",sep="")
colnames(mat)=c("Mean","Median","SD","IQR","SE Mean",
                paste(lab,"-L",sep=""),paste(lab,"-U",sep=""))
print(mat)
cat("Sample size = ",nrow(dfn),fill=TRUE)
return()
}