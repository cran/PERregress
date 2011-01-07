corr=function(x,rnd=2,cutoff=.25)
{
# function to compute correlations
pandterm = function(message) {
        stop(message, call. = FALSE) }
if (missing(x)) pandterm("Requires an input")
if (!is.data.frame(x) & !is.matrix(x)) pandterm("Input not a dataframe/matrix - try data.frame(input)/cbind(input)")
if(rnd<0) pandterm("rnd must be > 0")
if(!is.double(cutoff)) pandterm("cutoff must be a number")
if(cutoff > 1 || cutoff < 0) pandterm("cutoff must be in (0,1)")
if(is.matrix(x)) 
    {numeric=apply(x,2,is.numeric)}
else
    {numeric=sapply(x,is.numeric)}
xnum=x[,numeric]
mat=cor(xnum,use="complete.obs")
mat=round(mat,rnd)
coln=colnames(mat)
vmat=as.vector(mat)
vmat[abs(vmat)<cutoff]=0
neg=vmat<0
cmat=as.character(vmat)
cmat[cmat=="0"]=paste(rep(" ",rnd+3),collapse="")
cmat[cmat=="1"]=paste(c("1.",rep("0",rnd)),collapse="")
addsp=function(x){paste(" ",x,collapse="",sep="")}
cmat[!neg]=sapply(cmat[!neg],addsp)                   # add a space in front of positive correlations for alignment
cmat=matrix(cmat,ncol=ncol(xnum))
colnames(cmat)=sapply(coln,addsp)
rownames(cmat)=coln
cat("Full Correlation Matrix",fill=TRUE)
print(mat)
cat(" ",fill=TRUE)
cat("Correlation Matrix trimmed with cutoff =",cutoff,fill=TRUE)
print(cmat,quote=FALSE)
}
