library("pracma")
library("numbers")
library("MASS")
library("gmp")

# arbitary precision divisor function
big.divisors <- function(MyN) {
  myRle <- function (x1) {
    n1 <- length(x1)
    y1 <- x1[-1L] != x1[-n1]
    i <- c(which(y1), n1)
    list(lengths = diff(c(0L, i)), values = x1[i], uni = sum(y1)+1L)
  }
  
  if (MyN==1L) return(MyN)
  else {
    pfacs <- myRle(factorize(MyN))
    unip <- pfacs$values
    pv <- pfacs$lengths
    n <- pfacs$uni
    myf <- unip[1L]^(0L:pv[1L])
    if (n > 1L) {
      for (j in 2L:n) {
        myf <- c(myf, do.call(c,lapply(unip[j]^(1L:pv[j]), function(x) x*myf)))
      }
    }
  }
  myf[order(asNumeric(myf))]  ## 'order' is faster than 'sort.list'
}

# arbitary precision aliquot function
aliquot.big=function(x){
  ali=vector(mode = "list", length = length(x))
  for (j in 1:length(x)) {
    ali[[j]]=sum(head(big.divisors(x[j]), -1))
  }
  return(ali)  
}

#abundance
abu.big=function(x){
  abu= sapply(1:length(x), function(j) as.character(sum(big.divisors(x[j]))))
  return(abu)  
}

#is.abundant
is.abundant=function(x){
  y=sapply(1:length(x), function(j) as.character(sum(big.divisors(x[j]))-2*x[j]))
 y= as.numeric(y)>0
 return(y)
}

#aliquot map
n=1000
ali.b=vector(mode = "list", length = n)
f=rep(NA, n)
for (j in 1:n) {
  m=1
  k=j
  ali.b[[j]][m]=as.character(k)
  repeat{
    k=as.character(as.bigz(unlist(aliquot.big(as.bigz(k)))))
    if(k %in% ali.b[[j]]) break
    m=m+1
    ali.b[[j]][m]=k
    if(as.numeric(k)==0 || as.numeric(k)>10^49) break 
  }
  f[j]=m
  if(as.numeric(k)>10^45) f[j]=Inf
}


#plot
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f, type="h", col="darkred", xlab="n",ylab= expression(paste(f,"[n]")), main="aliquot map")
points(x=which(f==Inf), y=rep(0,length(which(f==Inf))), col="blue", pch=16)

#infinities
which(f==Inf)
#convergence style
cong=unlist(lapply(ali.b, function(x) tail(x,1)))

#abundant overrepresentation
fabu=f[which(is.abundant(c(1:n)))]
fabu=fabu[fabu !=Inf]
fnabu=f[which(!is.abundant(c(1:n)))]
fnabu=fnabu[fabu !=Inf]

boxplot(list(fabu, fnabu), horizontal = T, col=c("lightblue", "pink"), log = "x")
