descStat= function(df,CONF=.95,rnd=3) 
{
# function to compute descriptive stats
pandterm = function(message) {
        stop(message, call. = FALSE) }
if (missing(df)) pandterm("Requires a dataframe,matrix, or vector as input")
if (!is.data.frame(df)){
 	if(!is.matrix(df) & !is.vector(df))
		{pandterm("Input not a dataframe, matrix, or vector")}
	else
		{if(!is.data.frame(df)) df=data.frame(df)}
}
if(!is.numeric(CONF)) pandterm("Confidence must be numeric variable")
if(!(CONF < 1 & CONF >0)) pandterm("Confidence must be in (0,1)")
numeric=sapply(df,is.numeric)
dfn=df[,numeric,drop=FALSE]
numNA=function(vec){sum(is.na(vec))}
nmiss=sapply(dfn,numNA)
means=sapply(dfn,mean,na.rm=TRUE)
sds=sapply(dfn,sd,na.rm=TRUE)
medians=sapply(dfn,median,na.rm=TRUE)
N=dim(dfn)[1]
semeans=sds/sqrt(c(rep(N,length(sds)))-nmiss)
per25=sapply(dfn,quantile,prob=c(.25),na.rm=TRUE)
per75=sapply(dfn,quantile,prob=c(.75),na.rm=TRUE)
IQR=per75-per25
Zstar=-qnorm((1-CONF)/2)
cil=means-Zstar*semeans
ciu=means+Zstar*semeans

mat=cbind(means,medians,sds,IQR,semeans,cil,ciu,nmiss)
mat=round(mat,digits=rnd)
confc=as.character(CONF*100)
lab=paste(confc,"% CI",sep="")
colnames(mat)=c("Mean","Median","SD","IQR","SE Mean",
                paste(lab,"-L",sep=""),paste(lab,"-U",sep=""),"NMissing")
print(mat)
cat("Number of Observations = ",N,fill=TRUE)
invisible(mat)
}