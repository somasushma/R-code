library("pracma")
library("numbers")
library("MASS")

aliquot=function(x){
  ali=vector(mode = "list", length = length(x))
  for (j in 1:length(x)) {
    ali[[j]]=sum(head(divisors(x[j]), -1))
  }
return(ali)  
}


n=300
ali=vector(mode = "list", length = n)
f=rep(NA, n)
for (j in 1:n) {
  m=1
  k=j
  ali[[j]][m]=k
  repeat{
    k=unlist(aliquot(k))
    if(k %in% ali[[j]]) break
    m=m+1
    ali[[j]][m]=k
    if(k==0) || k>10^10) break 
    }
  f[j]=m
  if(k>10^10) f[j]=Inf
}

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f, type="h", col="darkred", xlab="n",ylab= expression(paste(f,"[n]")), main="aliquot map")
