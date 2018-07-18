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

ali.divs=aliquot=function(x){
  ali=vector(mode = "list", length = length(x))
  for (j in 1:length(x)) {
    ali[[j]]=head(divisors(x[j]), -1)
  }
  return(ali)  
}

#aliquot simple
n=1000
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
    if(k==0 || k>10^15) break 
  }
  f[j]=m
  if(k>10^15) f[j]=Inf
}

#plot aliquot
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f, type="h", col="darkred", xlab="n",ylab= expression(paste(f,"[n]")), main="aliquot map")
points(x=which(f==Inf), y=rep(0,length(which(f==Inf))), col="blue", pch=16)

#termination
cong=unlist(lapply(ali, function(x) tail(x,1)))

#semiperfect
n=1000
f.sp=rep(NA,n)
m=1
for (j in 2:n) {
  te=unlist(ali.divs(j))
  ye=unlist(sapply(length(te):(ceiling(length(te)/2)), function(x) apply(combn(te, x),MARGIN = 2, sum)))
if(j %in% ye){
  f.sp[m]=j
  m=m+1
}
}
f.sp=c(na.omit(f.sp))
