library("pracma")
library("numbers")
library("MASS")
library("gmp")


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

aliquot.big=function(x){
  ali=vector(mode = "list", length = length(x))
  for (j in 1:length(x)) {
    ali[[j]]=sum(head(big.divisors(x[j]), -1))
  }
  return(ali)  
}


n=1000
ali=vector(mode = "list", length = n)
f=rep(NA, n)
for (j in 1:n) {
  m=1
  k=j
  ali[[j]][m]=k
  repeat{
    k=asNumeric(as.bigz(unlist(aliquot.big(k))))
    if(k %in% ali[[j]]) break
    m=m+1
    ali[[j]][m]=k
    if(k==0 || k>10^40) break 
  }
  f[j]=m
  if(k>10^40) f[j]=Inf
}
